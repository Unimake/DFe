using Newtonsoft.Json;
using Org.BouncyCastle.Asn1.X509;
using Org.BouncyCastle.X509;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.Security;
using Unimake.DFe.Test.Utility.TesteValidacao.Extractors;
using Unimake.DFe.Test.Utility.TesteValidacao.Isoladores;
using Unimake.DFe.Test.Utility.TesteValidacao.Vinculadores;
using Xunit;
using Xunit.Abstractions;


namespace Unimake.DFe.Test.Utility.TesteValidacao
{
    public class ValidarXML
    {


        // Esse metodo irá ser chamado pelo UniNFe, onde ele passará o documento XML a ser validado e 
        // o certificado digital para assinar o XML caso necessário e o padrão caso NFSe. Será utilizando o mesmo formato que o UniNFe faz para pegar o certificado
        // (CertificadoDigital = Empresas.Configuracoes[emp].X509Certificado) e o padrão por meio do metodo BuscaPadraoNFSe(UFCod)

        [Theory]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NF3e\eventoNF3e.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFe\NFe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTe_ModalRodoviarioValido.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFe\inutNFe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTeOS-nfe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTeOS-nfe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTeOS.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\4_00_CTeOS_ModalRodoOS.xml")]
        [InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFSe\ISSNET\CancelarNfse-ped-cannfse.xml", PadraoNFSe.ISSNET)]
        [InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFSe\ISSNET\CancelarNfse-101-ped-cannfse.xml", PadraoNFSe.ISSNET)]


        // todo: colocar CTeOS de forma correta no Config e testar novos serviços do mesmo padrão
        public static void ValidarServico(string caminhoServicoValidacao, string caminhoArquivo, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            #region VaiSerApagada
            var CertificadoCaminho = @"C:\Projetos\Unimake_PV.pfx";
            var CertificadoSenha = "12345678";
            var certificado = new CertificadoDigital().CarregarCertificadoDigitalA1(CertificadoCaminho, CertificadoSenha);

            XmlDocument xmlConfig = new();
            xmlConfig.Load(caminhoServicoValidacao);

            if (!File.Exists(caminhoArquivo))
                throw new Exception("Arquivo XML não encontrado");

            XmlDocument xml = new();
            xml.Load(caminhoArquivo);
            #endregion

            TipoDFe tipoDFe = padraoNFSe != PadraoNFSe.None
                ? TipoDFe.NFSe
                : DetectarTipoDFe(xml);

            string tagRaiz = xml.DocumentElement.Name;
            string versao = ObterVersao(xml, xmlConfig);
            XmlNode servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

            var inform = MontarInformacaoGeral(servico);
            AssinarSeNecessario(xml, inform, certificado);

            // Se não tem schemas específicos, valida só o geral mesmo
            if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null)
            {
                ValidarSchemaGeral(xml, inform, tipoDFe, padraoNFSe);
                Console.WriteLine(inform);
                return;
            }

            // Se chegou aqui, tem schemas específicos para validar, então precisa vincular o XML geral com os específicos para depois validar cada um
            bool isEvento = servico.SelectSingleNode(".//*[local-name()='TagEvento']") != null;

            var vinculador = VinculadorFactory.Criar(tipoDFe, isEvento);
            var nodes = vinculador.Vincular(servico, xml);

            foreach (var (tipoCorreto, node) in nodes)
            {
                MontarInformacaoEspecifica(servico, tipoCorreto, inform);
                ValidarSchemaGeral(xml, inform, tipoDFe);
                ValidarSchemaEspecifico(node, inform, tipoDFe);
                Console.WriteLine(inform);
            }

        }



        private static XmlNode ObterServico(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            return padraoNFSe == PadraoNFSe.None
                ? TratarDFe(xml, versao, tipoDFe, tagRaiz, xmlConfig)
                : TratarNFSe(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);
        }



        private static XmlNode TratarNFSe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {

            string pathServicosNFSe = string.Empty;
            XmlNode servicoNFSe = null;
            tipoDFe = TipoDFe.NFSe;

            if (versao.IsNullOrEmpty())
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}']";
                XmlNodeList servicosNFSe = xmlConfig.SelectNodes(pathServicosNFSe);

                foreach (XmlNode servico in servicosNFSe)
                {
                    var identificador = servico.Attributes["tagIdentificadora"]?.Value;

                    if (!string.IsNullOrEmpty(identificador))
                    {
                        var nodeServicoNFSe = xml.DocumentElement.SelectSingleNode($"//*[local-name()='{identificador}']");

                        if (!(nodeServicoNFSe is null))
                        {
                            servicoNFSe = servico;
                            break;
                        }
                    }
                }
            }
            else
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}' and @versao='{versao}']";
                servicoNFSe = xmlConfig.SelectSingleNode(pathServicosNFSe);

            }
            if (servicoNFSe is null)
            {
                throw new Exception($"Serviço '{tagRaiz}' não encontrado para NFSe de padrão {padraoNFSe}.");
            }

            return servicoNFSe;
        }




        private static XmlNode TratarDFe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {

            string pathServico = $"//{tipoDFe}/Servico[@tagRaiz='{tagRaiz}' and @versao='{versao}']";
            XmlNode servico = xmlConfig.SelectSingleNode(pathServico);

            if (servico is null)
                throw new Exception($"Serviço '{tagRaiz}' com versão '{versao}' não encontrado para o tipo de DFe '{tipoDFe}'.");

            return servico;

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
                TagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText, // salvar tag evento no inform para poder fazer verificações futuras
                TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText,
                TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText,
                TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText,
                TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText
            };
        }


        private static void MontarInformacaoEspecifica(XmlNode servico, XmlNode tipo, InformacaoXML inform)
        {
            inform.SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
            inform.SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText;

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


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            string schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoDFe.ToString()}.{info.SchemaArquivo}"
                : $"{tipoDFe.ToString()}.{padraoNFSe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new Exception($"Erro ao validar schema geral {validar.ErrorMessage}");
            }
        }


        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info, TipoDFe tipoDFe)
        {

            //Isolando cada XML dependendo do tipoDFe
            var isolador = IsoladorFactory.CriarIsolador(tipoDFe);
            var xmlEspecifico = isolador.Isolar(eventoNode);

            var validarEspecifico = new ValidarSchema();
            string schemaEspecifico = $"{tipoDFe.ToString()}.{info.SchemaArquivoEspecifico}";
            validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNS);

            if (!validarEspecifico.Success)
            {
                throw new Exception($"Erro ao validar schema específico {validarEspecifico.ErrorMessage}");
            }
        }



        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig)
        {
            // Versão como atributo da raiz
            var versao = xml.DocumentElement.GetAttribute("versao");
            if (!string.IsNullOrEmpty(versao))
                return versao;

            // Buscar em cada Servico configurado pela TagVersao
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                var tagVersao = servico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count > 0)
                {
                    versao = ((XmlElement)nodeVersao[0]).GetAttribute("versao");
                    if (!string.IsNullOrEmpty(versao))
                        return versao;
                }
            }

            return string.Empty;
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
                case "cteProc":
                case "CTeSimp":
                case "CTeOS":
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

