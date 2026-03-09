using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Metadata.Ecma335;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.Security;
using Unimake.DFe.Test.Utility.TesteValidacao.Isoladores;
using Unimake.DFe.Test.Utility.TesteValidacao.Vinculadores;
using Xunit;


namespace Unimake.DFe.Test.Utility.TesteValidacao
{
    public class ValidarXML
    {


        string nome;

        // Esse metodo irá ser chamado pelo UniNFe, onde ele passará o documento XML a ser validado e 
        // o certificado digital para assinar o XML caso necessário e o padrão caso NFSe. Será utilizando o mesmo formato que o UniNFe faz para pegar o certificado
        // (CertificadoDigital = Empresas.Configuracoes[emp].X509Certificado) e o padrão por meio do metodo BuscaPadraoNFSe(UFCod)

        [Theory]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFSe\GISSONLINE\2.04\EnviarLoteRpsEnvio-env-loterps.xml", PadraoNFSe.GISSONLINE)]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFSe\PRONIM\1.01\ConsultarNfseEnvio-ped-sitnfse.xml", PadraoNFSe.PRONIM)]
        [InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFSe\NACIONAL\1.01\GerarNfseEnvio_IndicativoDecisaoJudicial-env-loterps.xml", PadraoNFSe.NACIONAL)]


        public static void ValidarServico(string caminhoServicoValidacao, string caminhoArquivo, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            try
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
                string versao = ObterVersao(xml, xmlConfig, tipoDFe);
                XmlNode servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

                if (servico is null)
                    return;

                var inform = MontarInformacaoGeral(servico);
                AssinarSeNecessario(xml, inform, certificado);

                // Se não tem schemas específicos, valida só o geral mesmo
                if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null)
                {
                    ValidarSchemaGeral(xml, inform, tipoDFe, padraoNFSe);
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
                }
            }
            catch (Exception ex)
            {
                throw new Exception($"Erro ao validar o arquivo: {ex.Message}");
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

            return servicoNFSe;
        }




        private static XmlNode TratarDFe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {

            string pathServico = $"//{tipoDFe}/Servico[@tagRaiz='{tagRaiz}' and @versao='{versao}']";
            XmlNode servico = xmlConfig.SelectSingleNode(pathServico);

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
                TagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText,
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
            if (!string.IsNullOrEmpty(info.TagAssinatura))
            {
                Assinar(xml, info.TagAssinatura, info.TagAtributoID, cert);
            }

            if (!string.IsNullOrEmpty(info.TagLoteAssinatura))
            {
                Assinar(xml, info.TagLoteAssinatura, info.TagLoteAtributoID, cert);
            }

            if (!string.IsNullOrEmpty(info.TagExtraAssinatura)) 
            {
                Assinar(xml, info.TagExtraAssinatura, info.TagExtraAtributoID, cert);
            }

        }


        private static void Assinar(XmlDocument xml, string tagAssinatura, string tagID, X509Certificate2 cert)
        {

            var usarCertificado = true; 

            if (string.IsNullOrWhiteSpace(tagAssinatura))
                return;

            if (usarCertificado)
            {

                if (!AssinaturaDigital.EstaAssinado(xml, tagAssinatura))
                {
                    AssinaturaDigital.Assinar(xml, tagAssinatura, tagID, cert, AlgorithmType.Sha1);

                    //| TipoDFe         | Algoritmo |
                    //| --------------- | --------- | 
                    //| NFe             | SHA1      | 
                    //| CTe  CTeOS      | SHA1      | 
                    //| MDFe            | SHA1      | 
                    //| NFSe            | SHA1      | 
                    //| Reinf           | SHA256    |
                    //| eSocial         | SHA256    | 
                    //| DARE            | SHA256    |

                }
            }
           
        }


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            if (string.IsNullOrEmpty(info.SchemaArquivo))
                return;

            string schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoDFe.ToString()}.{info.SchemaArquivo}"
                : $"{tipoDFe.ToString()}.{padraoNFSe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new Exception($"Erro ao validar schema geral {validar.ErrorMessage}. Verifique se esteja utilizando o layout mais recente para o XML.");
            }
        }


        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info, TipoDFe tipoDFe)
        {
            if (string.IsNullOrEmpty(info.SchemaArquivo))
                return;

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



        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig, TipoDFe tipoDFe)
        {
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                var tagVersao = servico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count > 0)
                {
                    var versaoAtributo = ((XmlElement)nodeVersao[0]).GetAttribute("versao");
                    var versaoValor = nodeVersao[0].InnerText; // Em alguns casos a versão pode estar no valor do nó ao invés de um atributo, então verificamos os dois

                    if (!string.IsNullOrEmpty(versaoAtributo))
                        return versaoAtributo;

                    if (!string.IsNullOrEmpty(versaoValor))
                        return versaoValor;

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
                    throw new Exception("Tipo do DFe não identificado");
            }

            return tipoDFe;
        }

    }
}

