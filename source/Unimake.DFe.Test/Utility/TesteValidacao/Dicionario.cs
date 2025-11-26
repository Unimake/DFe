using Org.BouncyCastle.X509;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq.Expressions;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.Security;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;
using Xunit.Abstractions;
using Org.BouncyCastle.Asn1.X509;


namespace Unimake.DFe.Test.Utility.TesteValidacao
{

    public class InformacaoXML
    {
        public string TagRaiz { get; set; }
        public string Versao { get; set; }
        public string SchemaArquivo { get; set; }
        public string SchemasEspecifico { get; set; }
        public string SchemaArquivoEspecifico { get; set; }
        public string TargetNS { get; set; }
        public string TagAssinatura { get; set; }
        public string TagAtributoID { get; set; }
        public string TagLoteAssinatura { get; set; }
        public string TagLoteAtributoID { get; set; }
        public string TagExtraAssinatura { get; set; }
        public string TagExtraAtributoID { get; set; }

        public override string ToString()
        {
            return
                $"TagRaiz: {TagRaiz}\n" +
                $"Versão: {Versao}\n" +
                $"SchemaArquivo: {SchemaArquivo}\n" +
                $"SchemasEspecifico: {SchemasEspecifico}\n" +
                $"SchemaArquivoEspecifico: {SchemaArquivoEspecifico}\n" +
                $"TargetNS: {TargetNS}\n" +
                $"TagAssinatura: {TagAssinatura}\n" +
                $"TagAtributoID: {TagAtributoID}\n" +
                $"TagLoteAssinatura: {TagLoteAssinatura}\n" +
                $"TagLoteAtributoID: {TagLoteAtributoID}\n" +
                $"TagExtraAssinatura: {TagExtraAssinatura}\n" +
                $"TagExtraAtributoID: {TagExtraAtributoID}";
        }

    }

    public class DicionarioServico
    {
        private static readonly Dictionary<string, InformacaoXML> dicionario = new();


        // Esse metodo irá ser chamado pelo UniNFe, onde ele passará o documento XML a ser validado, tag raiz e 
        // o certificado digital para assinar o XML caso necessário utilizando o mesmo formato que o UniNFe faz para pegar  
        // (CertificadoDigital = Empresas.Configuracoes[emp].X509Certificado)
        // precisa passar também o tipo de DFe para montar o schema corretamente é possivel utilizar o DectectTypeXML para isso dependendo do tipo de servico

        [Theory]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "consStatServ", @"..\..\..\Utility\TesteValidacao\XMLteste\consStatServ.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "consSitNFe", @"..\..\..\Utility\TesteValidacao\XMLteste\consSitNFe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "inutNFe", @"..\..\..\Utility\TesteValidacao\XMLteste\inutNFe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "envEvento", @"..\..\..\Utility\TesteValidacao\XMLteste\envEvento_110001.xml")]
        [InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "infNFe", @"..\..\..\Utility\TesteValidacao\XMLteste\infNFe.xml")]
        public static void CaregarServicoValidar(string caminhoServicoValidacao, string tagRaiz, string caminhoArquivo)
        {
            // Carregar certificado
            var CertificadoCaminho = @"C:\Projetos\Unimake_PV.pfx";
            var CertificadoSenha = "12345678";
            var certificado = new CertificadoDigital().CarregarCertificadoDigitalA1(CertificadoCaminho, CertificadoSenha);

            // Carregar config
            XmlDocument xmlConfig = new();
            xmlConfig.Load(caminhoServicoValidacao);

            if (!File.Exists(caminhoArquivo))
            {
                throw new Exception("Arquivo XML não encontrado");
            }

            XmlDocument xml = new();
            xml.Load(caminhoArquivo);

            var versao = xml.DocumentElement.GetAttribute("versao");
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                
                string tagRaizServico = servico.Attributes["tagRaiz"]?.Value;
                string versaoServico = servico.Attributes["versao"]?.Value;

                if (tagRaizServico != tagRaiz || versaoServico != versao)
                    continue;

                // se não for evento
                if (tagRaiz != "envEvento")
                {
                    var inform = MontarInformacaoGeral(servico);

                  
                    AssinarSeNecessario(xml, inform, certificado);
                    ValidarSchemaGeral(xml, inform);

                    Console.WriteLine(inform);
                    return;
                }

                // se for evento
                XmlNodeList eventos = xml.GetElementsByTagName("evento");

                foreach (XmlNode eventoNode in eventos)
                {
                    var tpEventoNode = eventoNode.SelectSingleNode("*[local-name()='infEvento']/*[local-name()='tpEvento']");
                    string tpEvento = tpEventoNode?.InnerText;

                    if (String.IsNullOrWhiteSpace(tpEvento))
                        throw new Exception("Tag <tpEvento> não encontrada");

                    // Procura schema pelo ID específico
                    XmlNode schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");
                    if (schemasEspecificos == null)
                    {
                        throw new Exception("Configuração de SchemasEspecificos não encontrada");
                    }

                    XmlNode tipoCorreto = null;

                    foreach (XmlNode tipo in schemasEspecificos.SelectNodes("*[local-name()='Tipo']"))
                    {
                        string id = tipo.SelectSingleNode("*[local-name()='ID']")?.InnerText;

                        if (id == tpEvento)
                        {
                            tipoCorreto = tipo;
                            break;
                        }
                    }

                    if (tipoCorreto == null)
                        throw new Exception($"Não existe Schema Específico configurado para o evento {tpEvento}");

                   
                    var inform = MontarInformacaoEspecifica(servico, tipoCorreto);

                    AssinarSeNecessario(xml, inform, certificado);

                    ValidarSchemaGeral(xml, inform);

                    ValidarSchemaEspecifico(eventoNode, inform);

                    Console.WriteLine(inform);
                }

                return;
            }
        }



        private static InformacaoXML MontarInformacaoGeral(XmlNode servico)
        {
            return new InformacaoXML
            {
                TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                Versao = servico.Attributes["versao"]?.Value,
                SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText,
                TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText,
                TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText,
                TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText,
                TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText
            };
        }


        private static InformacaoXML MontarInformacaoEspecifica(XmlNode servico, XmlNode tipo)
        {
            return new InformacaoXML
            {
                TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                Versao = servico.Attributes["versao"]?.Value,
                SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText,
                TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText
            };
        }


        private static void AssinarSeNecessario(XmlDocument xml, InformacaoXML info, X509Certificate2 cert)
        {
            if (String.IsNullOrWhiteSpace(info.TagAssinatura))
                return;


            //| TipoDFe         | Algoritmo |
            //| --------------- | --------- | 
            //| NFe             | SHA1      | 
            //| CTe  CTeOS      | SHA1      | 
            //| MDFe            | SHA1      | 
            //| NFSe            | SHA1      | 
            //| Reinf           | SHA256    |
            //| eSocial         | SHA256    | 
            //| DARE            | SHA256    |

            try
            {
                AssinaturaDigital.Assinar(xml, info.TagAssinatura, info.TagAtributoID, cert, AlgorithmType.Sha1);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Erro ao assinar XML: {ex.Message}");
            }
        }


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info)
        {
            
                string tipoDFe = "NFe";
                string schema = $"{tipoDFe}.{info.SchemaArquivo}";

                var validar = new ValidarSchema();
                validar.Validar(xml, schema, info.TargetNS);

            
            if(!validar.Success)
            {
                throw new Exception($"Erro ao validar schema geral {validar.ErrorMessage}");
            }
        }



        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info)
        {
           
                var infEvento = eventoNode.SelectSingleNode("*[local-name()='infEvento']");
                var detEvento = infEvento.SelectSingleNode("*[local-name()='detEvento']");

                XmlDocument xmlEspecifico = new();
                xmlEspecifico.LoadXml(detEvento.OuterXml);

                var validarEspecifico = new ValidarSchema();

                string tipoDFe = "NFe";
                string schemaEspecifico = $"{tipoDFe}.{info.SchemaArquivoEspecifico}";
                validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNS);

            if(!validarEspecifico.Success)
            {
                throw new Exception($"Erro ao validar schema específico {validarEspecifico.ErrorMessage}");
            }
        }
    }
}