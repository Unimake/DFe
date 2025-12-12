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
using Unimake.Business.DFe.Utility;
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


        // Esse metodo irá ser chamado pelo UniNFe, onde ele passará o documento XML a ser validado e 
        // o certificado digital para assinar o XML caso necessário. Será utilizando o mesmo formato que o UniNFe faz para pegar  
        // (CertificadoDigital = Empresas.Configuracoes[emp].X509Certificado)
        // precisa passar também o tipo de DFe para montar o schema corretamente é possivel utilizar o DectectTypeXML para isso dependendo do tipo de servico

        [Theory]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\distDFeInt.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\consReciCTe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\enviCTe_ModalAereo.xml")]
        [InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFe\envEvento_211110.xml")]







        public static void CaregarServicoValidar(string caminhoServicoValidacao, string caminhoArquivo)
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

            XmlDocument xml = new(); // passar direto o XmlDocument  quando for chamar o xml do UniNFe
            xml.Load(caminhoArquivo);

            var tagRaiz = xml.DocumentElement.Name; // extraindo a tag raiz


            var versao = ObterVersao(xml);
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {

                string tagRaizServico = servico.Attributes["tagRaiz"]?.Value;
                string versaoServico = servico.Attributes["versao"]?.Value;

                if (tagRaiz == "NFe")
                {
                    xml = MontarEnvio(xml, versao);
                    tagRaiz = "enviNFe";

                }

                if (tagRaizServico != tagRaiz || versaoServico != versao)
                    continue;

                
                if (!String.IsNullOrWhiteSpace(servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText)) // serviço sem schemaEspecífico
                {
                    var inform = MontarInformacaoGeral(servico);


                    AssinarSeNecessario(xml, inform, certificado);
                    ValidarSchemaGeral(xml, inform);

                    Console.WriteLine(inform);
                    return;
                }


                var eventosProcessados = ObterEvento(servico, xml);


                foreach (var (tipoCorreto, eventoNode) in eventosProcessados)
                {

                    var inform = MontarInformacaoEspecifica(servico, tipoCorreto);

                    AssinarSeNecessario(xml, inform, certificado);

                    ValidarSchemaGeral(xml, inform);

                    ValidarSchemaEspecifico(eventoNode, inform);

                    Console.WriteLine(inform);
                }
             }

                return;
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

            //TipoDFe tipoDFe = TipoDFe.NFe; // TODO: Detectar o tipo de DFe conforme o XML recebido

            TipoDFe tipoDFe = DetectarTipoDFe(xml);

            string schema = $"{tipoDFe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS);


            if (!validar.Success)
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

            TipoDFe tipoDFe = TipoDFe.NFe;
            string schemaEspecifico = $"{tipoDFe.ToString()}.{info.SchemaArquivoEspecifico}";
            validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNS);

            if (!validarEspecifico.Success)
            {
                throw new Exception($"Erro ao validar schema específico {validarEspecifico.ErrorMessage}");
            }
        }


        private static List<(XmlNode NodetipoCorreto, XmlNode eventoNode)> ObterEvento(XmlNode servico, XmlDocument  xml)
        {
            var lista = new List<(XmlNode NodetipoCorreto, XmlNode eventoNode)>();
            XmlNodeList eventos = null;


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

                lista.Add((tipoCorreto, eventoNode));

            }
            return lista;
        }



        //private static string ObterModelo(XmlDocument document)
        //{

        //    if (document.GetElementsByTagName("mod").Count == 0)
        //    {
        //        throw new Exception("Tag obrigatória <mod> não foi localizada no grupo de tag <NFe><infNFe><ide>.");
        //    }

        //    var modeloDoc = document.GetElementsByTagName("mod")[0].InnerText;

        //    return modeloDoc;

        //}


        private static List<string> ObterListaTagId()
        {
            List<string> tagsId = new List<string>();

            // so para poder utilizar o config também  --> depois ver como fazer isso melhor
            var caminhoConfig = @"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml";
            XmlDocument xmlDocument = new();
            xmlDocument.Load(caminhoConfig);


            XmlNodeList servicos = xmlDocument.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                var tagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText;
                if (!String.IsNullOrEmpty(tagAtributoID) && !tagsId.Contains(tagAtributoID))
                {
                    tagsId.Add(tagAtributoID);

                }
            }

            return tagsId;

        }



        private static string ObterVersao(XmlDocument xml)
        {

            var versao = xml.DocumentElement.GetAttribute("versao");
            if (!string.IsNullOrEmpty(versao))
            {
                return versao;
            }

            var tags = ObterListaTagId();

            foreach (var tag in tags)
            {

                var node = xml.GetElementsByTagName(tag);

                if (node.Count > 0)
                {
                    versao = ((XmlElement)node[0]).GetAttribute("versao");

                    if (!string.IsNullOrEmpty(versao))
                    {
                        return versao;
                    }

                }

            }
            return string.Empty;

        }



        private static XmlDocument MontarEnvio(XmlDocument xml, string versao)
        {
            var nfeNode = xml.GetElementsByTagName("NFe")[0];

            var xmlNFe = $"<enviNFe versao=\"{versao}\" xmlns=\"http://www.portalfiscal.inf.br/nfe\">" + "<idLote>000000000000001</idLote>"
                         + "<indSinc>0</indSinc>" + nfeNode.OuterXml +
                         "</enviNFe>";

            var doc = new XmlDocument();
            doc.LoadXml(xmlNFe);

            return doc;

        }



        private static TipoDFe DetectarTipoDFe(XmlDocument xml)
        {
            var tipoDFe = TipoDFe.Desconhecido;

            switch (xml.DocumentElement.Name)
            {
                #region NFe

                case "consStatServ":
                case "consSitNFe":
                case "consReciNFe":
                case "ConsCad":
                case "envEvento": 
                case "inutNFe":
                case "NFe":
                case "enviNFe":
                case "nfeProc":
                    tipoDFe = TipoDFe.NFe;
                    break;

                case "distDFeInt":
                    var ns = xml.GetElementsByTagName("distDFeInt")[0].NamespaceURI.ToLower();

                    if (ns.Contains("/nfe"))
                    {
                        tipoDFe = TipoDFe.NFe;
                    }
                    else if (ns.Contains("/cte"))
                    {
                        tipoDFe = TipoDFe.CTe;
                    }
                    break;

                #endregion

                #region CTe

                case "consStatServCte":
                case "consSitCTe":
                case "consReciCTe":
                case "eventoCTe":
                case "CTe":
                case "enviCTe":
                case "CTeOS":   // verificar como fazer a validação depois de documento com a tag raiz.
                case "cteProc": // verificar como fazer a validação depois de documento com a tag raiz.
                case "CTeSimp":
                    tipoDFe = TipoDFe.CTe;
                    break;

                #endregion

                #region MDFe

                case "consStatServMDFe":
                case "consSitMDFe":
                case "consReciMDFe":
                case "eventoMDFe":
                case "MDFe":
                case "enviMDFe":
                case "consMDFeNaoEnc":
                case "mdfeProc":
                    tipoDFe = TipoDFe.MDFe;
                    break;

                #endregion

                #region NF3e

                case "consStatServNF3e":
                case "consSitNF3e":
                case "consReciNF3e":
                case "eventoNF3e":
                case "NF3e":
                    tipoDFe = TipoDFe.NF3e;
                    break;

                #endregion

                #region NFCom

                case "consStatServNFCom":
                case "consSitNFCom":
                case "eventoNFCom":
                case "NFCom":
                    tipoDFe = TipoDFe.NFCom;
                    break;

                #endregion

                default:
                    tipoDFe = TipoDFe.Desconhecido;
                    break;
            }

            return tipoDFe;
        }


    }
}

