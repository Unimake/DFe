using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Isoladores;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Vinculadores;
using Unimake.Business.DFe.Xml.Validar.QRCode;
using Unimake.Exceptions;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Nova classe de validação de XML que centraliza toda a lógica de validação.
    /// </summary>
    public class ValidarEstruturaXML
    {
        /// <summary>
        /// Classe de resultado da validação, contendo um boolean para indicar se a validação foi bem-sucedida,
        /// Descrição da validação, mensagem de retorno sobre a validação, status da valiação e o xml assinado.
        /// </summary>
        public class ResultadoValidacao
        {
            /// <summary>
            /// Boolean retornado para informar a situação da validação.
            /// </summary>
            public bool Validado { get; set; }

            /// <summary>
            /// Descrição do serviço validado, com o tipo e o serviço.
            /// </summary>
            public string Descricao { get; set; }

            /// <summary>
            /// Mensagem de retorno da validação, caso a validação tenha retornado false, para informar o motivo do erro.
            /// </summary>
            public string MensagemRetorno { get; set; }

            /// <summary>
            /// Status da validação
            /// </summary>
            public string StatusValidacao { get; set; }

            /// <summary>
            /// Xml após a assinatura 
            /// </summary>
            public XmlDocument XmlAssinado { get; set; }
        }

        /// <summary>
        /// Guarda as configurações do XML de configuracção para o acesso durante a validação, evitando múltiplas consultas ao XML de configuração.
        /// </summary>
        public class InformacaoXML
        {
            /// <summary>
            /// Tag raiz do XML, utilizada para identificar o tipo de documento e 
            /// buscar a configuração correta no XML de serviços.
            /// </summary>
            public string TagRaiz { get; set; }

            /// <summary>
            /// Tag que guarda a descrição do serviço do XML de Configuracao.
            /// </summary>
            public string Descricao { get; set; }

            /// <summary>
            /// Tag ou atributo que contém a versão do layout do XML. 
            /// A versão é crucial para validar contra o schema correto.
            /// </summary>
            public string Versao { get; set; }

            /// <summary>
            /// Schema que deve ser utilizado para validar o XML geral.
            /// </summary>
            public string SchemaArquivo { get; set; }

            /// <summary>
            /// Caso o XML contenha partes específicas que exigem validação 
            /// contra schemas diferentes (ex: eventos, modais)
            /// </summary>
            public string SchemaArquivoEspecifico { get; set; }

            /// <summary>
            /// Tag que caso não seja null no arquivo de configuração, indica que o XML 
            /// possui eventos e que deve ser feita a vinculação do XML geral 
            /// com os específicos de cada evento para validação.
            /// </summary>
            public string TagEvento { get; set; }

            /// <summary>
            /// Tag que contem a Target Namespace do XML, utilizada para 
            /// validar o XML contra o schema correto.
            /// </summary>
            public string TargetNS { get; set; }

            /// <summary>
            /// Tag que contem a Target Namespace Específico (se tiver) do XML, utilizada para 
            /// validar o XML contra o schema correto.
            /// </summary>
            public string TargetNSEspecifico { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita a assinatura digital no XML.
            /// </summary>
            public string TagAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para 
            /// referenciar a assinatura digital no XML.
            /// </summary>
            public string TagAtributoID { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita a assinatura digital 
            /// do lote no XML, caso o serviço trabalhe com lotes.
            /// </summary>
            public string TagLoteAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para referenciar a 
            /// assinatura digital do lote no XML, caso o serviço trabalhe com lotes.
            /// </summary>
            public string TagLoteAtributoID { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita uma assinatura digital extra no XML, 
            /// caso o serviço exija mais de uma assinatura.
            /// </summary>
            public string TagExtraAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para referenciar 
            /// a assinatura digital extra no XML.
            /// </summary>
            public string TagExtraAtributoID { get; set; }

            /// <summary>
            /// Tag que indica se o serviço deve ser assinado utilizando a 
            /// canonicalização exclusiva, padrão é false.
            /// </summary>
            public bool AssinaCanonicalizacaoExclusiva { get; set; }

            /// <summary>
            /// Tag que indica se o serviço utiliza certificado digital para assinatura, padrão é true, ou seja, se a tag estiver 
            /// presente e for diferente de "false" o serviço será assinado, caso contrário, não será assinado.
            /// </summary>
            public bool UsaCertificadoDigital { get; set; }

            /// <summary>
            /// Tag que indica se o serviço deve ser assinado ou não dependendo do Tipo Ambiente.
            /// </summary>
            public TipoAmbiente? NaoAssina { get; set; }

            /// <summary>
            /// Tag que indica se o serviço utiliza QRcode.
            /// </summary>
            public bool GerarQRCode { get; set; }
        }

        /// <summary>
        /// Retorna o XML de configuração de serviços, que contém as regras de validação para cada tipo de documento e serviço.
        /// </summary>
        /// <returns></returns>
        /// <exception cref="Exception"></exception>
        private static XmlDocument CarregarConfigValidacao()
        {
            var assembly = typeof(ValidarEstruturaXML).Assembly;
            var resourceName = "Unimake.Business.DFe.Servicos.Config.ValidacaoConfig.xml";

            using (var stream = assembly.GetManifestResourceStream(resourceName))
            {
                if (stream == null)
                    throw new Exception($"Recurso não encontrado: {resourceName}");

                var xmlConfig = new XmlDocument();
                xmlConfig.Load(stream);
                return xmlConfig;
            }
        }

        /// <summary>
        /// Valida o XML de acordo com as regras definidas no XML de configuração de serviços.
        /// </summary>
        /// <param name="xml">Documento XML para a validação</param>
        /// <param name="configuracao">Configurações com as informações de validação do DFe</param>
        /// <exception cref="Exception"></exception>
        public ResultadoValidacao ValidarServico(XmlDocument xml, Configuracao configuracao)
        {
            var certificado = configuracao.CertificadoDigital;
            var tipoAmbiente = configuracao.TipoAmbiente;
            var padraoNFSe = configuracao.PadraoNFSe;
            var codigoUF = (UFBrasil)configuracao.CodigoUF;

            var tipoDFe = padraoNFSe != PadraoNFSe.None
            ? TipoDFe.NFSe
            : DetectarTipoDFe(xml);

            try
            {
                configuracao.TipoDFe = tipoDFe;
                var xmlConfig = CarregarConfigValidacao();
                var tagRaiz = xml.DocumentElement.Name;
                var versao = ObterVersao(xml, xmlConfig, tipoDFe, padraoNFSe);
                var servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

                if (servico is null)
                {
                    throw new Exception($"Não foi possível encontrar a configuração para o tipo de DFe com tag raiz: {tagRaiz} ou versão: {versao}. Verfique se a versão e/ou tag raiz estão corretas paraa validação");
                }

                AtribuirUrl(servico, codigoUF, configuracao);

                var inform = MontarInformacaoGeral(servico, codigoUF);

                try
                {
                    if (DeveNormalizarXmlPeloObjeto(tipoDFe, tagRaiz))
                    {
                        AssinarSeNecessario(xml, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                        ValidarSchemas(xml, servico, inform, tipoDFe, padraoNFSe);

                        var xmlNormalizado = NormalizarXmlPeloObjeto(xml, tipoDFe, tagRaiz);
                        AssinarSeNecessario(xmlNormalizado, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                        ValidarSchemas(xmlNormalizado, servico, inform, tipoDFe, padraoNFSe);

                        SubstituirConteudoXml(xml, xmlNormalizado);

                        return new ResultadoValidacao
                        {
                            Validado = true,
                            Descricao = inform.Descricao,
                            MensagemRetorno = "XML normalizado, assinado e validado com sucesso.",
                            StatusValidacao = "1",
                            XmlAssinado = xml
                        };
                    }

                    AssinarSeNecessario(xml, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                    ValidarSchemas(xml, servico, inform, tipoDFe, padraoNFSe);

                    return new ResultadoValidacao
                    {
                        Validado = true,
                        Descricao = inform.Descricao,
                        MensagemRetorno = "XML assinado e validado com sucesso.",
                        StatusValidacao = "1",
                        XmlAssinado = xml
                    };
                }

                catch (Exception ex)
                {
                    var status = ObterStatus(ex);

                    return new ResultadoValidacao
                    {
                        Validado = false,
                        Descricao = inform.Descricao,
                        MensagemRetorno = ex.Message,
                        StatusValidacao = status,
                        XmlAssinado = xml,
                    };
                }
            }
            catch (Exception ex)
            {
                var status = ObterStatus(ex);
                return new ResultadoValidacao
                {
                    Validado = false,
                    MensagemRetorno = ex.Message,
                    StatusValidacao = status,
                    XmlAssinado = xml,

                };
            }
        }

        private static bool DeveNormalizarXmlPeloObjeto(TipoDFe tipoDFe, string tagRaiz)
        {
            if (tipoDFe == TipoDFe.MDFe)
            {
                return tagRaiz == "MDFe" ||
                    tagRaiz == "enviMDFe" ||
                    tagRaiz == "eventoMDFe" ||
                    tagRaiz == "consStatServMDFe" ||
                    tagRaiz == "consSitMDFe" ||
                    tagRaiz == "consReciMDFe" ||
                    tagRaiz == "consMDFeNaoEnc";
            }

            if (tipoDFe == TipoDFe.CTe)
            {
                return tagRaiz == "CTe" ||
                    tagRaiz == "enviCTe" ||
                    tagRaiz == "CTeSimp" ||
                    tagRaiz == "CTeOS" ||
                    tagRaiz == "eventoCTe" ||
                    tagRaiz == "consStatServCte" ||
                    tagRaiz == "consStatServCTe" ||
                    tagRaiz == "consSitCTe" ||
                    tagRaiz == "consReciCTe" ||
                    tagRaiz == "distDFeInt";
            }

            if (tipoDFe == TipoDFe.NFe || tipoDFe == TipoDFe.NFCe)
            {
                return tagRaiz == "NFe" ||
                    tagRaiz == "enviNFe" ||
                    tagRaiz == "envEvento" ||
                    tagRaiz == "inutNFe" ||
                    tagRaiz == "consStatServ" ||
                    tagRaiz == "consSitNFe" ||
                    tagRaiz == "consReciNFe" ||
                    tagRaiz == "ConsCad" ||
                    tagRaiz == "nfceDownloadXML" ||
                    tagRaiz == "nfceListagemChaves" ||
                    tagRaiz == "distDFeInt";
            }

            if (tipoDFe == TipoDFe.NFCom)
            {
                return tagRaiz == "NFCom" ||
                    tagRaiz == "eventoNFCom" ||
                    tagRaiz == "consStatServNFCom" ||
                    tagRaiz == "consSitNFCom";
            }

            if (tipoDFe == TipoDFe.NFGas)
            {
                return tagRaiz == "NFGas" ||
                    tagRaiz == "eventoNFGas" ||
                    tagRaiz == "consStatServNFGas" ||
                    tagRaiz == "consSitNFGas";
            }

            if (tipoDFe == TipoDFe.NF3e)
            {
                return tagRaiz == "NF3e" ||
                    tagRaiz == "eventoNF3e" ||
                    tagRaiz == "consStatServNF3e" ||
                    tagRaiz == "consSitNF3e" ||
                    tagRaiz == "consReciNF3e";
            }

            if (tipoDFe == TipoDFe.CCG)
            {
                return tagRaiz == "consGTIN";
            }

            if (tipoDFe == TipoDFe.CIOT)
            {
                return tagRaiz == "ConsultarSituacaoTransportador" ||
                    tagRaiz == "ConsultarFrotaTransportador" ||
                    tagRaiz == "DeclaracaoOperacaoTransporte" ||
                    tagRaiz == "CancelamentoOperacaoTransporte" ||
                    tagRaiz == "RetificacaoOperacaoTransporte" ||
                    tagRaiz == "EncerramentoOperacaoTransporte" ||
                    tagRaiz == "ConsultarExcecao" ||
                    tagRaiz == "ConsultarCIOTGerado" ||
                    tagRaiz == "GerarIdOperacaoTransporte";
            }

            return false;
        }

        private static XmlDocument NormalizarXmlPeloObjeto(XmlDocument xml, TipoDFe tipoDFe, string tagRaiz)
        {
            switch (tipoDFe)
            {
                case TipoDFe.MDFe:
                    return NormalizarMDFePeloObjeto(xml, tagRaiz);

                case TipoDFe.CTe:
                    return NormalizarCTePeloObjeto(xml, tagRaiz);

                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    return NormalizarNFePeloObjeto(xml, tagRaiz);

                case TipoDFe.NFCom:
                    return NormalizarNFComPeloObjeto(xml, tagRaiz);

                case TipoDFe.NFGas:
                    return NormalizarNFGasPeloObjeto(xml, tagRaiz);

                case TipoDFe.NF3e:
                    return NormalizarNF3ePeloObjeto(xml, tagRaiz);

                case TipoDFe.CCG:
                    return NormalizarCCGPeloObjeto(xml, tagRaiz);

                case TipoDFe.CIOT:
                    return NormalizarCIOTPeloObjeto(xml, tagRaiz);

                default:
                    return xml;
            }
        }

        private static XmlDocument NormalizarMDFePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "MDFe")
            {
                var mdfe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.MDFe>(xml);
                mdfe.Signature = null;
                mdfe.InfMDFeSupl = null;
                return mdfe.GerarXML();
            }

            if (tagRaiz == "enviMDFe")
            {
                var enviMDFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.EnviMDFe>(xml);

                if (enviMDFe.MDFe != null)
                {
                    enviMDFe.MDFe.Signature = null;
                    enviMDFe.MDFe.InfMDFeSupl = null;
                }

                return enviMDFe.GerarXML();
            }

            if (tagRaiz == "eventoMDFe")
            {
                var eventoMDFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.EventoMDFe>(xml);
                eventoMDFe.Signature = null;
                return eventoMDFe.GerarXML();
            }

            if (tagRaiz == "consStatServMDFe")
            {
                var consStatServMDFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.ConsStatServMDFe>(xml);
                return consStatServMDFe.GerarXML();
            }

            if (tagRaiz == "consSitMDFe")
            {
                var consSitMDFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe>(xml);
                return consSitMDFe.GerarXML();
            }

            if (tagRaiz == "consReciMDFe")
            {
                var consReciMDFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.ConsReciMDFe>(xml);
                return consReciMDFe.GerarXML();
            }

            if (tagRaiz == "consMDFeNaoEnc")
            {
                var consMDFeNaoEnc = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc>(xml);
                return consMDFeNaoEnc.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarNFePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "NFe")
            {
                var nfe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.NFe>(xml);
                nfe.Signature = null;
                nfe.InfNFeSupl = null;
                return XMLUtility.Serializar(nfe);
            }

            if (tagRaiz == "enviNFe")
            {
                var enviNFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.EnviNFe>(xml);

                if (enviNFe.NFe != null)
                {
                    foreach (var nfe in enviNFe.NFe)
                    {
                        nfe.Signature = null;
                        nfe.InfNFeSupl = null;
                    }
                }

                return enviNFe.GerarXML();
            }

            if (tagRaiz == "envEvento")
            {
                var envEvento = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.EnvEvento>(xml);

                if (envEvento.Evento != null)
                {
                    foreach (var evento in envEvento.Evento)
                    {
                        evento.Signature = null;
                    }
                }

                return envEvento.GerarXML();
            }

            if (tagRaiz == "inutNFe")
            {
                var inutNFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.InutNFe>(xml);
                inutNFe.Signature = null;
                return inutNFe.GerarXML();
            }

            if (tagRaiz == "consStatServ")
            {
                var consStatServ = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.ConsStatServ>(xml);
                return consStatServ.GerarXML();
            }

            if (tagRaiz == "consSitNFe")
            {
                var consSitNFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.ConsSitNFe>(xml);
                return consSitNFe.GerarXML();
            }

            if (tagRaiz == "consReciNFe")
            {
                var consReciNFe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.ConsReciNFe>(xml);
                return consReciNFe.GerarXML();
            }

            if (tagRaiz == "ConsCad")
            {
                var consCad = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.ConsCad>(xml);
                return consCad.GerarXML();
            }

            if (tagRaiz == "distDFeInt")
            {
                var distDFeInt = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.DistDFeInt>(xml);
                return distDFeInt.GerarXML();
            }

            if (tagRaiz == "nfceDownloadXML")
            {
                var nfceDownloadXML = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.NFCeDownloadXML>(xml);
                return nfceDownloadXML.GerarXML();
            }

            if (tagRaiz == "nfceListagemChaves")
            {
                var nfceListagemChaves = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.NFCeListagemChaves>(xml);
                return nfceListagemChaves.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarNFComPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "NFCom")
            {
                var nfCom = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.NFCom>(xml);
                nfCom.Signature = null;
                nfCom.InfNFComSupl = null;
                return nfCom.GerarXML();
            }

            if (tagRaiz == "eventoNFCom")
            {
                var eventoNFCom = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.EventoNFCom>(xml);
                eventoNFCom.Signature = null;
                return eventoNFCom.GerarXML();
            }

            if (tagRaiz == "consStatServNFCom")
            {
                var consStatServNFCom = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom>(xml);
                return consStatServNFCom.GerarXML();
            }

            if (tagRaiz == "consSitNFCom")
            {
                var consSitNFCom = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.ConsSitNFCom>(xml);
                return consSitNFCom.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarNFGasPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "NFGas")
            {
                var nfGas = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFGas.NFGas>(xml);
                nfGas.Signature = null;
                nfGas.InfNFGasSupl = null;
                return nfGas.GerarXML();
            }

            if (tagRaiz == "eventoNFGas")
            {
                var eventoNFGas = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFGas.EventoNFGas>(xml);
                eventoNFGas.Signature = null;
                return eventoNFGas.GerarXML();
            }

            if (tagRaiz == "consStatServNFGas")
            {
                var consStatServNFGas = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFGas.ConsStatServNFGas>(xml);
                return consStatServNFGas.GerarXML();
            }

            if (tagRaiz == "consSitNFGas")
            {
                var consSitNFGas = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFGas.ConsSitNFGas>(xml);
                return consSitNFGas.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarNF3ePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "NF3e")
            {
                var nf3e = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.NF3e>(xml);
                nf3e.Signature = null;
                nf3e.InfNF3eSupl = null;
                return nf3e.GerarXML();
            }

            if (tagRaiz == "eventoNF3e")
            {
                var eventoNF3e = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.EventoNF3e>(xml);
                eventoNF3e.Signature = null;
                return eventoNF3e.GerarXML();
            }

            if (tagRaiz == "consStatServNF3e")
            {
                var consStatServNF3e = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.ConsStatServNF3e>(xml);
                return consStatServNF3e.GerarXML();
            }

            if (tagRaiz == "consSitNF3e")
            {
                var consSitNF3e = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.ConsSitNF3e>(xml);
                return consSitNF3e.GerarXML();
            }

            if (tagRaiz == "consReciNF3e")
            {
                var consReciNF3e = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.ConsReciNF3e>(xml);
                return consReciNF3e.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarCCGPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "consGTIN")
            {
                var consGTIN = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CCG.ConsGTIN>(xml);
                return consGTIN.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarCIOTPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "ConsultarSituacaoTransportador")
            {
                var consultarSituacaoTransportador = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador>(xml);
                return consultarSituacaoTransportador.GerarXML();
            }

            if (tagRaiz == "ConsultarFrotaTransportador")
            {
                var consultarFrotaTransportador = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.ConsultarFrotaTransportador>(xml);
                return consultarFrotaTransportador.GerarXML();
            }

            if (tagRaiz == "DeclaracaoOperacaoTransporte")
            {
                var declaracaoOperacaoTransporte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte>(xml);
                return declaracaoOperacaoTransporte.GerarXML();
            }

            if (tagRaiz == "CancelamentoOperacaoTransporte")
            {
                var cancelamentoOperacaoTransporte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporte>(xml);
                return cancelamentoOperacaoTransporte.GerarXML();
            }

            if (tagRaiz == "RetificacaoOperacaoTransporte")
            {
                var retificacaoOperacaoTransporte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporte>(xml);
                return retificacaoOperacaoTransporte.GerarXML();
            }

            if (tagRaiz == "EncerramentoOperacaoTransporte")
            {
                var encerramentoOperacaoTransporte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporte>(xml);
                return encerramentoOperacaoTransporte.GerarXML();
            }

            if (tagRaiz == "ConsultarExcecao")
            {
                var consultarExcecao = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.ConsultarExcecao>(xml);
                return consultarExcecao.GerarXML();
            }

            if (tagRaiz == "ConsultarCIOTGerado")
            {
                var consultarCIOTGerado = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.ConsultarCIOTGerado>(xml);
                return consultarCIOTGerado.GerarXML();
            }

            if (tagRaiz == "GerarIdOperacaoTransporte")
            {
                var gerarIdOperacaoTransporte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte>(xml);
                return gerarIdOperacaoTransporte.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarCTePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "CTe")
            {
                var cte = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(xml);
                cte.Signature = null;
                cte.InfCTeSupl = null;
                return cte.GerarXML();
            }

            if (tagRaiz == "enviCTe")
            {
                var enviCTe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.EnviCTe>(xml);

                if (enviCTe.CTe != null)
                {
                    foreach (var cte in enviCTe.CTe)
                    {
                        cte.Signature = null;
                        cte.InfCTeSupl = null;
                    }
                }

                return enviCTe.GerarXML();
            }

            if (tagRaiz == "CTeSimp")
            {
                var cteSimp = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTeSimp.CTeSimp>(xml);
                cteSimp.Signature = null;
                cteSimp.InfCTeSupl = null;
                return cteSimp.GerarXML();
            }

            if (tagRaiz == "CTeOS")
            {
                var cteOS = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTeOS.CTeOS>(xml);
                cteOS.Signature = null;
                cteOS.InfCTeSupl = null;
                return cteOS.GerarXML();
            }

            if (tagRaiz == "eventoCTe")
            {
                var eventoCTe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.EventoCTe>(xml);
                eventoCTe.Signature = null;
                return eventoCTe.GerarXML();
            }

            if (tagRaiz == "consStatServCte" || tagRaiz == "consStatServCTe")
            {
                var consStatServCte = new Unimake.Business.DFe.Xml.CTe.ConsStatServCte().LerXML<Unimake.Business.DFe.Xml.CTe.ConsStatServCte>(xml);
                return consStatServCte.GerarXML();
            }

            if (tagRaiz == "consSitCTe")
            {
                var consSitCTe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.ConsSitCTe>(xml);
                return consSitCTe.GerarXML();
            }

            if (tagRaiz == "consReciCTe")
            {
                var consReciCTe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.ConsReciCTe>(xml);
                return consReciCTe.GerarXML();
            }

            if (tagRaiz == "distDFeInt")
            {
                var distDFeInt = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.DistDFeInt>(xml);
                return distDFeInt.GerarXML();
            }

            return xml;
        }

        private static void SubstituirConteudoXml(XmlDocument destino, XmlDocument origem)
        {
            destino.LoadXml(origem.OuterXml);
        }

        private void ValidarSchemas(XmlDocument xml, XmlNode servico, InformacaoXML inform, TipoDFe tipoDFe, PadraoNFSe padraoNFSe)
        {
            if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null)
            {
                ValidarSchemaGeral(xml, inform, tipoDFe, padraoNFSe);
                return;
            }

            bool isEvento = servico.SelectSingleNode(".//*[local-name()='TagEvento']") != null;
            var vinculador = VinculadorFactory.Criar(tipoDFe, isEvento);
            var nodes = vinculador.Vincular(servico, xml);

            var xmlGeralValidado = false;

            foreach (var (tipoCorreto, node) in nodes)
            {
                var informEspecifica = CopiarInformacao(inform);
                MontarInformacaoEspecifica(servico, tipoCorreto, informEspecifica);

                if (!xmlGeralValidado)
                {
                    ValidarSchemaGeral(xml, informEspecifica, tipoDFe, padraoNFSe);
                    xmlGeralValidado = true;
                }

                ValidarSchemaEspecifico(node, isEvento, informEspecifica, tipoDFe);
            }
        }

        private static InformacaoXML CopiarInformacao(InformacaoXML origem) =>
            new InformacaoXML
            {
                TagRaiz = origem.TagRaiz,
                Descricao = origem.Descricao,
                Versao = origem.Versao,
                SchemaArquivo = origem.SchemaArquivo,
                SchemaArquivoEspecifico = origem.SchemaArquivoEspecifico,
                TagEvento = origem.TagEvento,
                TargetNS = origem.TargetNS,
                TargetNSEspecifico = origem.TargetNSEspecifico,
                TagAssinatura = origem.TagAssinatura,
                TagAtributoID = origem.TagAtributoID,
                TagLoteAssinatura = origem.TagLoteAssinatura,
                TagLoteAtributoID = origem.TagLoteAtributoID,
                TagExtraAssinatura = origem.TagExtraAssinatura,
                TagExtraAtributoID = origem.TagExtraAtributoID,
                AssinaCanonicalizacaoExclusiva = origem.AssinaCanonicalizacaoExclusiva,
                UsaCertificadoDigital = origem.UsaCertificadoDigital,
                NaoAssina = origem.NaoAssina,
                GerarQRCode = origem.GerarQRCode
            };

        private static XmlNode ObterServico(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {
            if (tipoDFe == TipoDFe.NFSe)
            {
                return TratarNFSe(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);
            }

            if (tipoDFe == TipoDFe.ESocial || tipoDFe == TipoDFe.EFDReinf)
            {
                return TratarESocialEFDReinf(xml, versao, tipoDFe, tagRaiz, xmlConfig);
            }

            return TratarDFe(xml, versao, tipoDFe, tagRaiz, xmlConfig);
        }

        private static XmlNode TratarESocialEFDReinf(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {
            XmlNodeList nodeListDFe = xmlConfig.SelectNodes($"//{tipoDFe}/Servico");
            XmlNode nodeServicoCorreto = null;

            foreach (XmlNode nodeServico in nodeListDFe)
            {
                var tagIdentificadora = nodeServico.Attributes["tagIdentificadora"]?.Value;

                if (!tagIdentificadora.IsNullOrEmpty())
                {
                    var nodeServicoESocial = xml.DocumentElement.SelectSingleNode($"//*[local-name()='{tagIdentificadora}']");

                    if (!(nodeServicoESocial is null))
                    {
                        nodeServicoCorreto = nodeServico;
                        break;
                    }
                }
            }

            return nodeServicoCorreto;
        }

        private static XmlNode TratarNFSe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {
            string pathServicosNFSe = string.Empty;
            XmlNode servicoNFSe = null;

            if (versao.IsNullOrEmpty())
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}']";
                var servicosNFSe = xmlConfig.SelectNodes(pathServicosNFSe);

                foreach (XmlNode servico in servicosNFSe)
                {
                    // Aceita tag identificadora com ou sem namespace (Ex: "ns:tagIdentificadora" ou "tagIdentificadora")
                    var identificador = servico.Attributes["tagIdentificadora"]?.Value?.Split(':').Last();

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

            if (servico is null && tipoDFe == TipoDFe.CTe && tagRaiz == "consStatServCte")
            {
                pathServico = $"//{tipoDFe}/Servico[@tagRaiz='consStatServCTe' and @versao='{versao}']";
                servico = xmlConfig.SelectSingleNode(pathServico);
            }

            return servico;
        }

        private static InformacaoXML MontarInformacaoGeral(XmlNode servico, UFBrasil codigoMunicipio)
        {
            #region verifica variáveis para a assinatura

            var usaCertificado = VerificarUtilizacaoCertificadoDigital(servico, codigoMunicipio);
            var ambiente = VerificarAmbienteAssinatura(servico, codigoMunicipio);
            var canonizalizacaoExclusiva = VerificarAssinaCanonicalizacaoExclusiva(servico, codigoMunicipio);

            #endregion

            return new InformacaoXML
            {
                TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                Versao = servico.Attributes["versao"]?.Value,
                Descricao = servico.SelectSingleNode("*[local-name()='Descricao']")?.InnerText,
                SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText,
                TagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText,
                TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText,
                TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText,
                TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText,
                TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText,
                NaoAssina = ambiente,
                UsaCertificadoDigital = usaCertificado,
                AssinaCanonicalizacaoExclusiva = canonizalizacaoExclusiva,
                GerarQRCode = servico.SelectSingleNode("*[local-name()='GerarQrCode']")?.InnerText?.Trim() == "true"

            };
        }

        private static bool VerificarUtilizacaoCertificadoDigital(XmlNode servico, UFBrasil codigoMunicipio)
        {
            string valorCert = null;
            var nodeExcecao = VerificarExcecao(servico, codigoMunicipio, "UsaCertificadoDigital");

            if (nodeExcecao != null)
            {
                valorCert = nodeExcecao.InnerText;
            }

            return valorCert?.Trim() != "false";
        }

        private static TipoAmbiente? VerificarAmbienteAssinatura(XmlNode servico, UFBrasil codigoMunicipio)
        {
            string ambiente = null;
            var nodeExcecao = VerificarExcecao(servico, codigoMunicipio, "NaoAssina");

            if (nodeExcecao != null)
            {
                ambiente = nodeExcecao.InnerText?.Trim().ToLower();
            }

            return ambiente?.ToLower() == "homologação" ? TipoAmbiente.Homologacao : (ambiente?.ToLower() == "produção" ? TipoAmbiente.Producao : (TipoAmbiente?)null);
        }

        private static bool VerificarAssinaCanonicalizacaoExclusiva(XmlNode servico, UFBrasil codigoMunicipio)
        {
            string valorCanonicalizacao = null;
            var nodeExcecao = VerificarExcecao(servico, codigoMunicipio, "AssinaCanonicalizacaoExclusiva");

            if (nodeExcecao != null)
            {
                valorCanonicalizacao = nodeExcecao.InnerText;
            }

            return valorCanonicalizacao?.Trim() == "true";
        }

        private static XmlNode VerificarExcecao(XmlNode servico, UFBrasil codigoMunicipio, string nomeTag)
        {
            var nodeTag = servico.SelectSingleNode($"*[local-name()='{nomeTag}']");
            XmlNode nodeExcecao = null;

            if (nodeTag != null)
            {
                nodeExcecao = nodeTag.SelectSingleNode(
                    $"*[local-name()='Excecao' and @codMunicipio='{codigoMunicipio}']"
                );
            }

            return nodeExcecao;
        }

        private static void MontarInformacaoEspecifica(XmlNode servico, XmlNode tipo, InformacaoXML inform)
        {
            inform.SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
            inform.SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText;
            inform.TargetNSEspecifico = tipo.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText ?? inform.TargetNS; // Caso o nó específico tenha uma TargetNS diferente da geral, utiliza a específica, caso contrário, mantém a geral
            inform.TagAtributoID = tipo.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText ?? inform.TagAtributoID; // Caso o nó específico tenha um atributo ID diferente da geral, utiliza o específico, caso contrário, mantém o geral
        }

        private void AssinarSeNecessario(XmlDocument xml, InformacaoXML inform, X509Certificate2 cert, Configuracao configuracao, TipoAmbiente tipoAmbiente, TipoDFe tipoDFe)
        {
            if (!string.IsNullOrEmpty(inform.TagAssinatura))
            {
                Assinar(xml, inform.TagAssinatura, inform.TagAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.AssinaCanonicalizacaoExclusiva, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }

            if (!string.IsNullOrEmpty(inform.TagLoteAssinatura))
            {
                Assinar(xml, inform.TagLoteAssinatura, inform.TagLoteAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.AssinaCanonicalizacaoExclusiva, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }

            if (!string.IsNullOrEmpty(inform.TagExtraAssinatura))
            {
                Assinar(xml, inform.TagExtraAssinatura, inform.TagExtraAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.AssinaCanonicalizacaoExclusiva, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }
        }

        private void Assinar(XmlDocument xml,
            string tagAssinatura,
            string tagID,
            TipoAmbiente? tagNaoAssina,
            bool usaCertificado,
            bool assinaCanonicalizacaoExclusiva,
            bool gerarQrCode,
            X509Certificate2 cert,
            TipoAmbiente tipoAmbiente,
            TipoDFe tipoDFe,
            Configuracao configuracao)
        {

            if (string.IsNullOrWhiteSpace(tagAssinatura))
                return;

            var algoritmoAssinatura = AlgoritmoAssinatura(tipoDFe);

            if (usaCertificado)
            {
                if (tagNaoAssina is null || tagNaoAssina != tipoAmbiente)
                {
                    try
                    {
                        if (!AssinaturaDigital.EstaAssinado(xml, tagAssinatura))
                        {
                            AssinaturaDigital.Assinar(xml, tagAssinatura, tagID, cert, algoritmoAssinatura, exclusiveC14N: assinaCanonicalizacaoExclusiva);


                        }


                    }
                    catch (Exception ex)
                    {

                        throw new AssinaturaException(
                            $"Ocorreu um erro ao assinar o XML: {ex.Message}");
                    }

                    MontarQRCode(xml, gerarQrCode, tipoDFe, configuracao);
                }
            }
        }

        private static AlgorithmType AlgoritmoAssinatura(TipoDFe tipoDFe)
        {
            //| TipoDFe         | Algoritmo |
            //| --------------- | --------- | 
            //| NFe             | SHA1      | 
            //| CTe  CTeOS      | SHA1      | 
            //| MDFe            | SHA1      | 
            //| NFSe            | SHA1      | 
            //| Reinf           | SHA256    |
            //| eSocial         | SHA256    | 
            //| DARE            | SHA256    |

            switch (tipoDFe)
            {
                case TipoDFe.ESocial:
                case TipoDFe.EFDReinf:
                    return AlgorithmType.Sha256;

                default:
                    return AlgorithmType.Sha1;

            }
        }

        private static void MontarQRCode(XmlDocument xml, bool gerarQrCode, TipoDFe tipoDFe, Configuracao configuracao)
        {
            var geradorQrCode = QrCodeFactory.Criar(configuracao, gerarQrCode, tipoDFe);
            geradorQrCode?.GerarQrCode(xml, configuracao);
        }

        private static void AtribuirUrl(XmlNode servico, UFBrasil codigoUF, Configuracao configuracao)
        {
            foreach (XmlNode grupoUF in servico?.SelectNodes("GrupoUrl/Grupo"))
            {
                foreach (XmlNode uf in grupoUF.SelectNodes("UF"))
                {
                    if (uf.InnerText.Equals(codigoUF.ToString(), StringComparison.OrdinalIgnoreCase))
                    {
                        var urls = grupoUF.SelectSingleNode("Urls");

                        if (urls != null)
                        {
                            configuracao.UrlChaveHomologacao = urls["UrlChaveHomologacao"]?.InnerText;
                            configuracao.UrlChaveProducao = urls["UrlChaveProducao"]?.InnerText;
                            configuracao.UrlQrCodeHomologacao = urls["UrlQrCodeHomologacao"]?.InnerText;
                            configuracao.UrlQrCodeProducao = urls["UrlQrCodeProducao"]?.InnerText;
                        }

                        return;
                    }
                }
            }
        }

        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            // Caso não possua Schema para a validação retornar sem validar e deixar a validação por conta da prefeitura ao enviar
            if (string.IsNullOrEmpty(info.SchemaArquivo))
            {
                return;
            }

            var tipoBusca = tipoDFe == TipoDFe.NFCe ? TipoDFe.NFe : tipoDFe;

            string schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoBusca.ToString()}.{info.SchemaArquivo}"
                : $"{tipoBusca.ToString()}.{padraoNFSe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema geral: {validar.ErrorMessage}.");
            }
        }

        private static void ValidarSchemaEspecifico(XmlNode node, bool isEvento, InformacaoXML info, TipoDFe tipoDFe)
        {
            // Caso não possua Schema para a validação retornar sem validar e deixar a validação por conta da prefeitura ao enviar
            if (string.IsNullOrEmpty(info.SchemaArquivoEspecifico))
            {
                return;
            }

            //Isolando cada XML dependendo do tipoDFe
            var isolador = IsoladorFactory.CriarIsolador(tipoDFe, isEvento);
            var xmlEspecifico = isolador.Isolar(node);

            // Em casos que não possuir o xml especifico  Ex: CTe de complemento
            if (xmlEspecifico is null)
            {
                return;
            }

            var validarEspecifico = new ValidarSchema();
            string schemaEspecifico = $"{tipoDFe.ToString()}.{info.SchemaArquivoEspecifico}";
            validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNSEspecifico);

            if (!validarEspecifico.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema específico: {validarEspecifico.ErrorMessage}.");
            }
        }

        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig, TipoDFe tipoDFe, PadraoNFSe padraoNFSe)
        {
            var servicoValidacao = xmlConfig.SelectSingleNode("ServicosValidacao");

            var nodeDFe = padraoNFSe == PadraoNFSe.None
            ? servicoValidacao.SelectSingleNode(tipoDFe.ToString())
            : servicoValidacao.SelectSingleNode($"{tipoDFe}/Padrao[@nome='{padraoNFSe}']");

            if (nodeDFe is null && padraoNFSe != PadraoNFSe.None)
            {
                throw new PadraoNaoImplementadoException($"Não foi possível encontrar a configuração para o padrão: {padraoNFSe}");
            }

            if (nodeDFe is null)
            {
                throw new Exception($"Não foi possível encontrar a configuração para o tipo de DFe: {tipoDFe}");
            }

            foreach (XmlNode nodeServico in nodeDFe.SelectNodes("Servico"))
            {
                var tagVersao = nodeServico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count == 0 && tipoDFe == TipoDFe.CTe && tagVersao == "consStatServCTe")
                {
                    nodeVersao = xml.GetElementsByTagName("consStatServCte");
                }

                if (nodeVersao.Count > 0)
                {

                    var elemento = (XmlElement)nodeVersao[0];

                    var versaoAtributo = elemento.GetAttribute("versao");

                    versaoAtributo = string.IsNullOrWhiteSpace(versaoAtributo) ? elemento.GetAttribute("Versao") : versaoAtributo; // caso tenha atributo com primeira letra em uppercase

                    if (!string.IsNullOrEmpty(versaoAtributo))
                        return versaoAtributo;

                    var versaoValor = elemento.ChildNodes.Cast<XmlNode>().FirstOrDefault(x => x.NodeType == XmlNodeType.Text)?.Value; // pegando somente o valor direto do node para evitar erros

                    if (!string.IsNullOrEmpty(versaoValor))
                        return versaoValor;

                }
            }

            return string.Empty;
        }

        private string ObterStatus(Exception ex)
        {
            switch (ex)
            {
                case AssinaturaException _:
                    return "4";

                case ValidarXMLException _:
                    return "2";

                case PadraoNaoImplementadoException _:
                    return "5";

                default:
                    return "3";
            }
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
                    var modeloDoc = xml.GetElementsByTagName("mod")[0]?.InnerText;

                    if (modeloDoc == ((int)ModeloDFe.NFCe).ToString())
                    {
                        tipoDFe = TipoDFe.NFCe;
                        break;
                    }

                    tipoDFe = TipoDFe.NFe;
                    break;

                case "nfceDownloadXML":
                case "nfceListagemChaves":
                    tipoDFe = TipoDFe.NFCe;
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
                case "consStatServCTe":
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

                #region DCe

                case "consStatServDCe":
                case "consSitDCe":
                case "eventoDCe":
                case "DCe":
                    tipoDFe = TipoDFe.DCe;
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

                #region CCG

                case "consGTIN":
                    tipoDFe = TipoDFe.CCG;
                    break;

                #endregion

                #region CIOT

                case "ConsultarSituacaoTransportador":
                case "ConsultarFrotaTransportador":
                case "DeclaracaoOperacaoTransporte":
                case "CancelamentoOperacaoTransporte":
                case "RetificacaoOperacaoTransporte":
                case "EncerramentoOperacaoTransporte":
                case "ConsultarExcecao":
                case "ConsultarCIOTGerado":
                case "GerarIdOperacaoTransporte":
                    tipoDFe = TipoDFe.CIOT;
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

                #region NFGas

                case "consStatServNFGas":
                case "consSitNFGas":
                case "eventoNFGas":
                case "NFGas":
                    tipoDFe = TipoDFe.NFGas;
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

                #region DARE
                case "DARE":
                    tipoDFe = TipoDFe.DARE;
                    break;
                #endregion

                #region GNRE
                case "TConsultaConfigUf":
                case "TConsLote_GNRE":
                case "TLote_GNRE":
                case "TResultLote_GNRE":
                    tipoDFe = TipoDFe.GNRE;
                    break;
                #endregion

                case "eSocial":
                    tipoDFe = TipoDFe.ESocial;
                    break;

                case "Reinf":
                    tipoDFe = TipoDFe.EFDReinf;
                    break;

                default:
                    throw new Exception($"Não foi possível identificar o tipo do DFe pela tag raiz '{xml.DocumentElement.Name}'. " +
                        "Verifique se o XML está correto ou se o padrão caso seja NFSe foi devidamente configurado.");
            }

            return tipoDFe;
        }
    }
}

