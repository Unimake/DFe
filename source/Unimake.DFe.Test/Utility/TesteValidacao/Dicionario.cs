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
        [InlineData(@"..\..\..\Utility\TesteValidacao\ServicoValidacao.xml", "envEvento", @"..\..\..\Utility\TesteValidacao\XMLteste\envEvento_110001.xml")]
        public static void CaregarServicoValidar(string caminhoServicoValidacao, string tagRaiz, string caminhoArquivo)
        {

            // o certificado sera passado pelo UniNFe, aqui é apenas um exemplo de carregamento do certificado
            var CertificadoCaminho = @"C:\Projetos\Unimake_PV.pfx";
            var CertificadoSenha = "12345678";
            var certificado = new CertificadoDigital().CarregarCertificadoDigitalA1(CertificadoCaminho, CertificadoSenha);

            // Carregar o XML de configuração dos serviços
            XmlDocument doCaminhoServicoValidacao = new();
            doCaminhoServicoValidacao.Load(caminhoServicoValidacao);

            // verificar se o arquivo a ser validado existe
            if (!File.Exists(caminhoArquivo))
            {
                throw new Exception("Arquivo 'caminhoArquivo' não encontrado");
            }


            XmlDocument docCaminhoArquivo = new();
            docCaminhoArquivo.Load(caminhoArquivo);

            // so funciona para NFe
            var versao = docCaminhoArquivo.DocumentElement.GetAttribute("versao");



            XmlNodeList servicos = doCaminhoServicoValidacao.GetElementsByTagName("Servico");
            var informacaoXML = new InformacaoXML();

            foreach (XmlNode servico in servicos)
            {
                var nodeScheamaEspecifico = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");
                if (nodeScheamaEspecifico == null)
                {
                    informacaoXML.TagRaiz = servico.Attributes["tagRaiz"]?.Value;
                    informacaoXML.Versao = servico.Attributes["versao"]?.Value;
                    informacaoXML.SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
                    informacaoXML.TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText;
                    informacaoXML.TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText;
                    informacaoXML.TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText;
                    informacaoXML.TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText;
                    informacaoXML.TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText;
                    informacaoXML.TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText;
                    informacaoXML.TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText;

                }


                else if (nodeScheamaEspecifico != null)
                {

                    var tpEvento = docCaminhoArquivo.SelectSingleNode("//*[local-name()='tpEvento']")?.InnerText;

                    if (!tpEvento.IsNullOrEmpty())
                    {
                        var nodeTipo = nodeScheamaEspecifico.SelectNodes("*[local-name()='Tipo']");

                        foreach (XmlNode tipo in nodeTipo)
                        {

                            var idEspecificoList = tipo.SelectNodes("*[local-name()='ID']");

                            foreach (XmlNode id in idEspecificoList)
                            {
                                var idConteudo = id.InnerText;

                                if (tpEvento == idConteudo)
                                {

                                    // TODO: DECIDIR COMO MANDAR PARA O VALIDAR E QUAL SCHEMA MANDAR
                                    // VERIFICAR UM EVENTO USANDO O VALIDAR NA DLL PARA VER COMO ELE PASSA O SCHEMA 
                                    informacaoXML.TagRaiz = servico.Attributes["tagRaiz"]?.Value;
                                    informacaoXML.Versao = servico.Attributes["versao"]?.Value;
                                    informacaoXML.SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
                                    informacaoXML.SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText;
                                    informacaoXML.TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText;
                                    informacaoXML.TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText;
                                    informacaoXML.TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText;
                                    informacaoXML.TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText;
                                    informacaoXML.TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText;
                                    informacaoXML.TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText;
                                    informacaoXML.TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText;

                                }
                            }

                        }

                    }


                }
                if (informacaoXML.TagRaiz == tagRaiz && informacaoXML.Versao == versao)
                {
                    string chave = $"{informacaoXML.TagRaiz}-{informacaoXML.Versao}"; // Exemplo de chave: "consStatServ-4.00"
                    dicionario[chave] = informacaoXML;

                    Console.WriteLine(dicionario[chave]);

                    if (!string.IsNullOrWhiteSpace(informacaoXML.TagAssinatura)) // caso tenha que assinar o XML
                    {

                        //|  TipoDFe          | Algoritmo |
                        //| ---------------   | --------- |
                        //| **NFe * *         | SHA1      |
                        //| **CTe / CTeOS * * | SHA1      |
                        //| **MDFe * *        | SHA1      |
                        //| **NFSe * *        | SHA1      |
                        //| **Reinf * *       | SHA256    |
                        //| **eSocial * *     | SHA256    |
                        //| **DARE * *        | SHA256    |

                        try
                        {
                            AssinaturaDigital.Assinar(docCaminhoArquivo, informacaoXML.TagAssinatura, informacaoXML.TagAtributoID, certificado, AlgorithmType.Sha1);
                            Console.WriteLine($"assinado.xml: {docCaminhoArquivo.OuterXml}");
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine($"Erro ao assinar o XML: {ex.Message}");

                        }

                    }
            
                    try
                        {
                            // TipoDFe tipoDFe = TipoDFe.NFe; Definir o tipo de DFe conforme necessário com o DetectTpeXML 
                            string tipoDFeString = "NFe"; // Exemplo: "NFe", "CTe", etc.
                            string schemaMontado = $"{tipoDFeString}.{informacaoXML.SchemaArquivo}";
                            var validar = new ValidarSchema();
                            validar.Validar(docCaminhoArquivo, schemaMontado, informacaoXML.TargetNS);


                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine($"Erro ao validar o XML: {ex.Message}");

                        }




                }


            }



        }
    }
}