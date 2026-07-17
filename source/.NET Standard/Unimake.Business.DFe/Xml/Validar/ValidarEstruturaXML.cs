using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar;
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
            /// Algoritmo utilizado para assinar o XML. O padrão é SHA1.
            /// </summary>
            public AlgorithmType SignatureAlgorithmType { get; set; } = AlgorithmType.Sha1;

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
            return CatalogoValidacaoXML.CriarCopia();
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
            var codigoConfiguracao = tipoDFe == TipoDFe.NFSe
                ? configuracao.CodigoMunicipio
                : configuracao.CodigoUF;

            try
            {
                configuracao.TipoDFe = tipoDFe;
                var xmlConfig = CarregarConfigValidacao();
                var tagRaiz = xml.DocumentElement.Name;
                var versao = tipoDFe == TipoDFe.NFSe
                    ? (!string.IsNullOrWhiteSpace(configuracao.SchemaVersao)
                        ? configuracao.SchemaVersao
                        : DefinirVersaoNFSe(xml, padraoNFSe, configuracao.CodigoMunicipio))
                    : ObterVersao(xml, xmlConfig, tipoDFe);

                var servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

                if (servico is null)
                {
                    throw new Exception($"Não foi possível encontrar a configuração para o tipo de DFe com tag raiz: '{tagRaiz}' ou versão: '{versao}'. Verfique se a versão e/ou tag raiz estão corretas para a validação");
                }

                AtribuirUrl(servico, codigoUF, configuracao);

                var inform = MontarInformacaoGeral(servico, codigoConfiguracao);

                try
                {
                    if (DeveNormalizarXmlPeloObjeto(tipoDFe, tagRaiz))
                    {
                        AssinarSeNecessario(xml, servico, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                        ValidarSchemas(xml, servico, inform, tipoDFe, padraoNFSe);

                        var xmlNormalizado = NormalizarXmlPeloObjeto(xml, tipoDFe, tagRaiz);
                        AssinarSeNecessario(xmlNormalizado, servico, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                        ValidarSchemas(xmlNormalizado, servico, inform, tipoDFe, padraoNFSe);

                        SubstituirConteudoXml(xml, xmlNormalizado);

                        return CriarResultadoSucesso(xml, inform.Descricao, "XML normalizado, assinado e validado com sucesso.");
                    }

                    AssinarSeNecessario(xml, servico, inform, certificado, configuracao, tipoAmbiente, tipoDFe);
                    ValidarSchemas(xml, servico, inform, tipoDFe, padraoNFSe);

                    return CriarResultadoSucesso(xml, inform.Descricao, "XML assinado e validado com sucesso.");
                }

                catch (Exception ex)
                {
                    return CriarResultadoFalha(xml, inform.Descricao, ex, ObterStatus(ex));
                }
            }
            catch (Exception ex)
            {
                return CriarResultadoFalha(xml, null, ex, ObterStatus(ex));
            }
        }

        private static ResultadoValidacao CriarResultadoSucesso(XmlDocument xml, string descricao, string mensagem)
        {
            return new ResultadoValidacao
            {
                Validado = true,
                Descricao = descricao,
                MensagemRetorno = mensagem,
                StatusValidacao = "1",
                XmlAssinado = xml
            };
        }

        private static ResultadoValidacao CriarResultadoFalha(XmlDocument xml, string descricao, Exception exception, string status)
        {
            return new ResultadoValidacao
            {
                Validado = false,
                Descricao = descricao,
                MensagemRetorno = exception.Message,
                StatusValidacao = status,
                XmlAssinado = xml
            };
        }

        private static bool DeveNormalizarXmlPeloObjeto(TipoDFe tipoDFe, string tagRaiz)
        {
            return RegistroNormalizacaoXML.DeveNormalizar(tipoDFe, tagRaiz);
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

                case TipoDFe.BPe:
                    return NormalizarBPePeloObjeto(xml, tagRaiz);

                case TipoDFe.NF3e:
                    return NormalizarNF3ePeloObjeto(xml, tagRaiz);

                case TipoDFe.DCe:
                    return NormalizarDCePeloObjeto(xml, tagRaiz);

                case TipoDFe.CCG:
                    return NormalizarCCGPeloObjeto(xml, tagRaiz);

                case TipoDFe.CIOT:
                    return NormalizarCIOTPeloObjeto(xml, tagRaiz);

                case TipoDFe.GNRE:
                    return NormalizarGNREPeloObjeto(xml, tagRaiz);

                case TipoDFe.DARE:
                    return NormalizarDAREPeloObjeto(xml, tagRaiz);

                case TipoDFe.EFDReinf:
                    return NormalizarEFDReinfPeloObjeto(xml, tagRaiz);

                case TipoDFe.ESocial:
                    return NormalizarESocialPeloObjeto(xml, tagRaiz);

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

        private static XmlDocument NormalizarBPePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "BPe")
            {
                var bpe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPe.BPe>(xml);
                bpe.Signature = null;
                return bpe.GerarXML();
            }

            if (tagRaiz == "BPeTM")
            {
                var bpeTM = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPeTM.BPeTM>(xml);
                bpeTM.Signature = null;
                return bpeTM.GerarXML();
            }

            if (tagRaiz == "BPeTA")
            {
                var bpeTA = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPeTA.BPeTA>(xml);
                bpeTA.Signature = null;
                return bpeTA.GerarXML();
            }

            if (tagRaiz == "eventoBPe")
            {
                var eventoBPe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPe.EventoBPe>(xml);
                eventoBPe.Signature = null;
                return eventoBPe.GerarXML();
            }

            if (tagRaiz == "consStatServBPe")
            {
                var consStatServBPe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPe.ConsStatServBPe>(xml);
                return consStatServBPe.GerarXML();
            }

            if (tagRaiz == "consSitBPe")
            {
                var consSitBPe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.BPe.ConsSitBPe>(xml);
                return consSitBPe.GerarXML();
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

        private static XmlDocument NormalizarDCePeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "DCe")
            {
                var dce = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DCe.DCe>(xml);
                dce.Signature = null;
                dce.InfDCeSupl = null;
                return dce.GerarXML();
            }

            if (tagRaiz == "eventoDCe")
            {
                var eventoDCe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DCe.EventoDCe>(xml);
                eventoDCe.Signature = null;
                return eventoDCe.GerarXML();
            }

            if (tagRaiz == "consStatServDCe")
            {
                var consStatServDCe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DCe.ConsStatServDCe>(xml);
                return consStatServDCe.GerarXML();
            }

            if (tagRaiz == "consSitDCe")
            {
                var consSitDCe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DCe.ConsSitDCe>(xml);
                return consSitDCe.GerarXML();
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

        private static XmlDocument NormalizarGNREPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "TConsultaConfigUf")
            {
                var consultaConfigUf = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.GNRE.TConsultaConfigUf>(xml);
                return consultaConfigUf.GerarXML();
            }

            if (tagRaiz == "TConsLote_GNRE")
            {
                if (xml.GetElementsByTagName("incluirPDFGuias").Count == 0 &&
                    xml.GetElementsByTagName("incluirArquivoPagamento").Count == 0 &&
                    xml.GetElementsByTagName("incluirNoticias").Count == 0)
                {
                    var consLoteConsGNRE = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.GNRE.TConsLoteConsGNRE>(xml);
                    return consLoteConsGNRE.GerarXML();
                }

                var consLoteGNRE = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE>(xml);
                return consLoteGNRE.GerarXML();
            }

            if (tagRaiz == "TLote_GNRE")
            {
                var loteGNRE = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.GNRE.TLoteGNRE>(xml);
                return loteGNRE.GerarXML();
            }

            if (tagRaiz == "TLote_ConsultaGNRE")
            {
                var loteConsultaGNRE = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.GNRE.TLoteConsultaGNRE>(xml);
                return loteConsultaGNRE.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarDAREPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz == "Dare")
            {
                var dare = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DARE.DARE>(xml);
                return dare.GerarXML();
            }

            if (tagRaiz == "DareLote")
            {
                var dareLote = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DARE.DARELote>(xml);
                return dareLote.GerarXML();
            }

            if (tagRaiz == "Receitas")
            {
                var receitas = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DARE.Receitas>(xml);
                return receitas.GerarXML();
            }

            return xml;
        }

        private static XmlDocument NormalizarEFDReinfPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz != "Reinf")
            {
                return xml;
            }

            if (xml.GetElementsByTagName("envioLoteEventos").Count > 0)
            {
                var reinfEnvioLoteEventos = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos>(xml);
                RemoverAssinaturasEventosEFDReinf(reinfEnvioLoteEventos);
                return reinfEnvioLoteEventos.GerarXML();
            }

            if (xml.GetElementsByTagName("ConsultaLoteAssincrono").Count > 0)
            {
                var reinfConsultaLoteAssincrono = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.EFDReinf.ReinfConsultaLoteAssincrono>(xml);
                return reinfConsultaLoteAssincrono.GerarXML();
            }

            if (xml.GetElementsByTagName("ConsultaReciboEvento").Count > 0)
            {
                var reinfConsulta = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.EFDReinf.ReinfConsulta>(xml);
                return reinfConsulta.GerarXML();
            }

            return xml;
        }

        private static void RemoverAssinaturasEventosEFDReinf(Unimake.Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos reinfEnvioLoteEventos)
        {
            var eventos = reinfEnvioLoteEventos?.EnvioLoteEventos?.Eventos?.Evento;

            if (eventos == null)
            {
                return;
            }

            foreach (var evento in eventos)
            {
                var propriedades = evento.GetType().GetProperties();

                foreach (var propriedade in propriedades)
                {
                    var valorEvento = propriedade.GetValue(evento);

                    if (valorEvento != null)
                    {
                        var propriedadeAssinatura = valorEvento.GetType().GetProperty("Signature");
                        propriedadeAssinatura?.SetValue(valorEvento, null);
                    }
                }
            }
        }

        private static XmlDocument NormalizarESocialPeloObjeto(XmlDocument xml, string tagRaiz)
        {
            if (tagRaiz != "eSocial")
            {
                return xml;
            }

            if (xml.GetElementsByTagName("envioLoteEventos").Count > 0)
            {
                var eSocialEnvioLoteEventos = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos>(xml);
                RemoverAssinaturasEventosESocial(eSocialEnvioLoteEventos);
                return eSocialEnvioLoteEventos.GerarXML();
            }

            if (xml.GetElementsByTagName("consultaLoteEventos").Count > 0)
            {
                var consultaLoteEventos = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos>(xml);
                return consultaLoteEventos.GerarXML();
            }

            if (xml.GetElementsByTagName("consultaEvtsEmpregador").Count > 0)
            {
                var consultaEvtsEmpregador = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsEmpregadorESocial>(xml);
                return consultaEvtsEmpregador.GerarXML();
            }

            if (xml.GetElementsByTagName("consultaEvtsTabela").Count > 0)
            {
                var consultaEvtsTabela = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTabelaESocial>(xml);
                return consultaEvtsTabela.GerarXML();
            }

            if (xml.GetElementsByTagName("consultaEvtsTrabalhador").Count > 0)
            {
                var consultaEvtsTrabalhador = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTrabalhadorESocial>(xml);
                return consultaEvtsTrabalhador.GerarXML();
            }

            if (xml.GetElementsByTagName("solicDownloadEvtsPorId").Count > 0)
            {
                var downloadEventosPorID = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorID>(xml);
                downloadEventosPorID.Signature = null;
                return downloadEventosPorID.GerarXML();
            }

            if (xml.GetElementsByTagName("solicDownloadEventosPorNrRecibo").Count > 0)
            {
                var downloadEventosPorNrRec = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorNrRec>(xml);
                downloadEventosPorNrRec.Signature = null;
                return downloadEventosPorNrRec.GerarXML();
            }

            return xml;
        }

        private static void RemoverAssinaturasEventosESocial(Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos eSocialEnvioLoteEventos)
        {
            var eventos = eSocialEnvioLoteEventos?.EnvioLoteEventos?.Eventos?.Evento;

            if (eventos == null)
            {
                return;
            }

            foreach (var evento in eventos)
            {
                var propriedades = evento.GetType().GetProperties();

                foreach (var propriedade in propriedades)
                {
                    var valorEvento = propriedade.GetValue(evento);

                    if (valorEvento != null)
                    {
                        var propriedadeAssinatura = valorEvento.GetType().GetProperty("Signature");
                        propriedadeAssinatura?.SetValue(valorEvento, null);
                    }
                }
            }
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
            ValidadorEstruturalXML.Validar(xml, servico, inform, tipoDFe, padraoNFSe);
        }

        private static XmlNode ObterServico(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {
            return ResolvedorServicoValidacao.Resolver(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);
        }

        private static XmlNode TratarESocialEFDReinf(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {
            return ResolvedorServicoValidacao.ResolverPorTagIdentificadora(xml, tipoDFe, xmlConfig);
        }

        private static XmlNode TratarNFSe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {
            return ResolvedorServicoValidacao.ResolverNFSe(xml, versao, tagRaiz, xmlConfig, padraoNFSe);
        }

        private static XmlNode TratarDFe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {

            return ResolvedorServicoValidacao.ResolverDFe(versao, tipoDFe, tagRaiz, xmlConfig);
        }

        private static InformacaoXML MontarInformacaoGeral(XmlNode servico, int codigoConfiguracao)
        {
            return LeitorConfiguracaoValidacao.Ler(servico, codigoConfiguracao);
        }

        private static AlgorithmType VerificarAlgoritmoAssinatura(XmlNode servico, int codigoConfiguracao)
        {
            return LeitorConfiguracaoValidacao.ObterAlgoritmoAssinatura(servico, codigoConfiguracao);
        }

        private void AssinarSeNecessario(XmlDocument xml, XmlNode servico, InformacaoXML inform, X509Certificate2 cert, Configuracao configuracao, TipoAmbiente tipoAmbiente, TipoDFe tipoDFe)
        {
            CoordenadorAssinaturaXML.AssinarSeNecessario(
                xml,
                servico,
                inform,
                cert,
                configuracao,
                tipoAmbiente,
                tipoDFe,
                AssinarEventosEFDReinf,
                AssinarEventosESocial,
                Assinar);
        }

        private static void AssinarEventosEFDReinf(XmlDocument xml, XmlNode servico, TipoAmbiente? tagNaoAssina, X509Certificate2 cert, TipoAmbiente tipoAmbiente)
        {
            if (tagNaoAssina != null && tagNaoAssina == tipoAmbiente)
            {
                return;
            }

            var eventos = xml.SelectNodes("/*[local-name()='Reinf']/*[local-name()='envioLoteEventos']/*[local-name()='eventos']/*[local-name()='evento']");

            foreach (XmlNode evento in eventos)
            {
                var reinfEvento = evento.SelectSingleNode("*[local-name()='Reinf']");

                if (reinfEvento is null)
                {
                    throw new AssinaturaException("Não foi encontrado o evento EFDReinf para assinatura em lote.");
                }

                var eventoEspecifico = reinfEvento.ChildNodes
                    .Cast<XmlNode>()
                    .FirstOrDefault(x => x.NodeType == XmlNodeType.Element && x.LocalName != "Signature")?.LocalName;

                if (string.IsNullOrWhiteSpace(eventoEspecifico))
                {
                    throw new AssinaturaException("Não foi possível identificar o evento EFDReinf para assinatura em lote.");
                }

                var tipoEvento = EncontrarTipoEventoEFDReinf(servico, eventoEspecifico);
                var tagAtributoID = tipoEvento.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText;

                var xmlEventoEspecifico = new XmlDocument();
                xmlEventoEspecifico.LoadXml(reinfEvento.OuterXml);

                if (!AssinaturaDigital.EstaAssinado(xmlEventoEspecifico, "Reinf"))
                {
                    try
                    {
                        AssinaturaDigital.Assinar(xmlEventoEspecifico, "Reinf", tagAtributoID, cert, AlgorithmType.Sha256, true, "id");
                    }
                    catch (Exception ex)
                    {
                        throw new AssinaturaException($"Ocorreu um erro ao assinar o evento EFDReinf '{eventoEspecifico}': {ex.Message}");
                    }

                    evento.RemoveChild(reinfEvento);
                    evento.AppendChild(xml.ImportNode(xmlEventoEspecifico.DocumentElement, true));
                }
            }
        }

        private static XmlNode EncontrarTipoEventoEFDReinf(XmlNode servico, string eventoEspecifico)
        {
            var tipos = servico.SelectNodes("*[local-name()='SchemasEspecificos']/*[local-name()='Tipo']");

            foreach (XmlNode tipo in tipos)
            {
                var evento = tipo.SelectSingleNode("*[local-name()='Evento']")?.InnerText;

                if (evento == eventoEspecifico)
                {
                    return tipo;
                }
            }

            throw new AssinaturaException($"Não existe configuração de assinatura para o evento EFDReinf '{eventoEspecifico}'.");
        }

        private static void AssinarEventosESocial(XmlDocument xml, XmlNode servico, TipoAmbiente? tagNaoAssina, X509Certificate2 cert, TipoAmbiente tipoAmbiente)
        {
            if (tagNaoAssina != null && tagNaoAssina == tipoAmbiente)
            {
                return;
            }

            var eventos = xml.SelectNodes("/*[local-name()='eSocial']/*[local-name()='envioLoteEventos']/*[local-name()='eventos']/*[local-name()='evento']");

            foreach (XmlNode evento in eventos)
            {
                var eSocialEvento = evento.SelectSingleNode("*[local-name()='eSocial']");

                if (eSocialEvento is null)
                {
                    throw new AssinaturaException("Não foi encontrado o evento eSocial para assinatura em lote.");
                }

                var eventoEspecifico = eSocialEvento.ChildNodes
                    .Cast<XmlNode>()
                    .FirstOrDefault(x => x.NodeType == XmlNodeType.Element && x.LocalName != "Signature")?.LocalName;

                if (string.IsNullOrWhiteSpace(eventoEspecifico))
                {
                    throw new AssinaturaException("Não foi possível identificar o evento eSocial para assinatura em lote.");
                }

                var tipoEvento = EncontrarTipoEventoESocial(servico, eventoEspecifico);
                var tagAtributoID = tipoEvento.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText;

                var xmlEventoEspecifico = new XmlDocument();
                xmlEventoEspecifico.LoadXml(eSocialEvento.OuterXml);

                if (!AssinaturaDigital.EstaAssinado(xmlEventoEspecifico, "eSocial"))
                {
                    try
                    {
                        AssinaturaDigital.Assinar(xmlEventoEspecifico, "eSocial", tagAtributoID, cert, AlgorithmType.Sha256, false, "Id");
                    }
                    catch (Exception ex)
                    {
                        throw new AssinaturaException($"Ocorreu um erro ao assinar o evento eSocial '{eventoEspecifico}': {ex.Message}");
                    }

                    evento.RemoveChild(eSocialEvento);
                    evento.AppendChild(xml.ImportNode(xmlEventoEspecifico.DocumentElement, true));
                }
            }
        }

        private static XmlNode EncontrarTipoEventoESocial(XmlNode servico, string eventoEspecifico)
        {
            var tipos = servico.SelectNodes("*[local-name()='SchemasEspecificos']/*[local-name()='Tipo']");

            foreach (XmlNode tipo in tipos)
            {
                var evento = tipo.SelectSingleNode("*[local-name()='Evento']")?.InnerText;

                if (evento == eventoEspecifico)
                {
                    return tipo;
                }
            }

            throw new AssinaturaException($"Não existe configuração de assinatura para o evento eSocial '{eventoEspecifico}'.");
        }

        private void Assinar(XmlDocument xml,
            string tagAssinatura,
            string tagID,
            TipoAmbiente? tagNaoAssina,
            bool usaCertificado,
            bool assinaCanonicalizacaoExclusiva,
            AlgorithmType signatureAlgorithmType,
            bool gerarQrCode,
            X509Certificate2 cert,
            TipoAmbiente tipoAmbiente,
            TipoDFe tipoDFe,
            Configuracao configuracao)
        {

            if (string.IsNullOrWhiteSpace(tagAssinatura))
                return;

            if (usaCertificado)
            {
                if (tagNaoAssina is null || tagNaoAssina != tipoAmbiente)
                {
                    try
                    {
                        if (!AssinaturaDigital.EstaAssinado(xml, tagAssinatura))
                        {
                            AssinaturaDigital.Assinar(xml, tagAssinatura, tagID, cert, signatureAlgorithmType, exclusiveC14N: assinaCanonicalizacaoExclusiva);
                        }
                    }
                    catch (Exception ex)
                    {
                        throw new AssinaturaException(
                            $"Ocorreu um erro ao assinar o XML: {ex.Message}");
                    }

                    MontarQRCode(xml, gerarQrCode, tipoDFe, configuracao);
                    PosicionarAssinaturaBPeDepoisDoSuplemento(xml, tipoDFe);
                }
            }
        }

        private static void PosicionarAssinaturaBPeDepoisDoSuplemento(XmlDocument xml, TipoDFe tipoDFe)
        {
            if (tipoDFe != TipoDFe.BPe)
            {
                return;
            }

            var raiz = xml.DocumentElement;
            if (raiz == null)
            {
                return;
            }

            var infBPeSupl = raiz.GetElementsByTagName("infBPeSupl").Count > 0 ? raiz.GetElementsByTagName("infBPeSupl")[0] : null;
            var signature = raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#").Count > 0 ? raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0] : null;

            if (infBPeSupl != null && signature != null && infBPeSupl.NextSibling != signature)
            {
                raiz.RemoveChild(signature);
                raiz.InsertAfter(signature, infBPeSupl);
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

        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig, TipoDFe tipoDFe)
        {
            var servicoValidacao = xmlConfig.SelectSingleNode("ServicosValidacao");
            var nodeDFe = servicoValidacao.SelectSingleNode(tipoDFe.ToString());

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

        /// <summary>
        /// Define o tipo de serviço de NFSe com base no XML, padrão e versão informados.
        /// </summary>
        /// <param name="conteudoXML">Conteúdo XML enviado para identificação</param>
        /// <param name="padraoNFSe">Padrão da NFSe</param>
        /// <param name="versao">Versão do XML</param>
        /// <returns>Tipo de serviço correspondente</returns>
        public static Servico DefinirTipoServicoNFSe(string conteudoXML, PadraoNFSe padraoNFSe, string versao)
        {
            if (string.IsNullOrWhiteSpace(conteudoXML))
            {
                throw new ArgumentNullException(nameof(conteudoXML));
            }

            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);

            return DefinirTipoServicoNFSe(xml, padraoNFSe, versao);
        }

        /// <summary>
        /// Define o tipo de serviço de NFSe com base no XML, padrão e versão informados.
        /// </summary>
        /// <param name="xml">Arquivo XML enviado para identificação</param>
        /// <param name="padraoNFSe">Padrão da NFSe</param>
        /// <param name="versao">Versão do XML</param>
        /// <returns>Tipo de serviço correspondente</returns>
        public static Servico DefinirTipoServicoNFSe(XmlDocument xml, PadraoNFSe padraoNFSe, string versao)
        {
            if (xml is null)
            {
                throw new ArgumentNullException(nameof(xml));
            }

            if (xml.DocumentElement is null)
            {
                throw new Exception("Não foi possível identificar o tipo de serviço NFSe: XML sem tag raiz.");
            }

            var xmlConfig = CarregarConfigValidacao();
            var tagRaiz = xml.DocumentElement.Name;
            var servico = ResolvedorServicoValidacao.ResolverNFSe(xml, versao, tagRaiz, xmlConfig, padraoNFSe);

            if (servico is null)
            {
                throw new Exception($"Não foi possível encontrar a configuração para identificar o tipo de serviço NFSe com padrão: '{padraoNFSe}', tag raiz: '{tagRaiz}' ou versão: '{versao}'.");
            }

            var tipoServico = servico.SelectSingleNode("*[local-name()='TipoServico']")?.InnerText?.Trim();

            if (string.IsNullOrWhiteSpace(tipoServico))
            {
                throw new Exception($"A tag TipoServico não foi configurada para o padrão: '{padraoNFSe}', tag raiz: '{tagRaiz}' e versão: '{versao}'.");
            }

            if (!Enum.TryParse(tipoServico, false, out Servico result))
            {
                throw new Exception($"TipoServico inválido no ValidarConfig.xml: '{tipoServico}'.");
            }

            return result;
        }

        /// <summary>
        /// Define a versão do XML de NFSe com base no conteúdo do XML, padrão de NFSe e código do município.
        /// </summary>
        /// <param name="xml">Arquivo XML enviado para a validação</param>
        /// <param name="padraoNFSe">Padrão da NFSe</param>
        /// <param name="codigoMunicipio">Código do município</param>
        /// <returns></returns>
        public static string DefinirVersaoNFSe(XmlDocument xml, PadraoNFSe padraoNFSe, int codigoMunicipio)
        {
            var raiz = xml.DocumentElement?.LocalName ?? string.Empty;
            var raizQualificada = xml.DocumentElement?.Name ?? string.Empty;
            var namespaceRaiz = xml.DocumentElement?.NamespaceURI ?? string.Empty;
            var conteudoXML = xml.OuterXml;

            bool Contem(string valor) =>
                conteudoXML.IndexOf(valor, StringComparison.OrdinalIgnoreCase) >= 0;

            bool RaizEh(params string[] nomes) =>
                nomes.Any(nome => string.Equals(raiz, nome, StringComparison.OrdinalIgnoreCase));

            string NormalizarVersao(string versao)
            {
                if (string.IsNullOrWhiteSpace(versao))
                {
                    return string.Empty;
                }

                versao = versao.Trim();

                if (!versao.All(x => char.IsDigit(x) || x == '.'))
                {
                    return string.Empty;
                }

                var partes = versao.Split('.');

                return partes.Length == 2 && partes[1].Length == 1
                    ? $"{partes[0]}.{partes[1]}0"
                    : versao;
            }

            string ObterVersaoDeclarada()
            {
                var elementos = xml.SelectNodes("//*");

                foreach (XmlElement elemento in elementos)
                {
                    var versao = elemento.GetAttribute("versao");
                    versao = string.IsNullOrWhiteSpace(versao) ? elemento.GetAttribute("Versao") : versao;
                    versao = NormalizarVersao(versao);

                    if (!string.IsNullOrWhiteSpace(versao))
                    {
                        return versao;
                    }
                }

                var nodeVersao = elementos
                    .Cast<XmlElement>()
                    .FirstOrDefault(x =>
                        string.Equals(x.LocalName, "versao", StringComparison.OrdinalIgnoreCase) &&
                        !string.IsNullOrWhiteSpace(NormalizarVersao(x.InnerText)));

                if (nodeVersao != null)
                {
                    return NormalizarVersao(nodeVersao.InnerText);
                }

                return string.Empty;
            }

            var versaoDeclarada = ObterVersaoDeclarada();

            switch (padraoNFSe)
            {
                case PadraoNFSe.NACIONAL:
                    return string.IsNullOrWhiteSpace(versaoDeclarada) ? "1.01" : versaoDeclarada;

                case PadraoNFSe.CONAM:
                    if (!string.IsNullOrWhiteSpace(versaoDeclarada))
                    {
                        return versaoDeclarada;
                    }

                    return codigoMunicipio == 3506102 ||
                           codigoMunicipio == 3509007 ||
                           codigoMunicipio == 3539806 ||
                           codigoMunicipio == 3552809
                        ? "4.00"
                        : "2.00";

                case PadraoNFSe.DBSELLER:
                    return codigoMunicipio == 4319901 ? "2.04" : "1.00";

                case PadraoNFSe.DSF:
                    if (raizQualificada.StartsWith("ns1:", StringComparison.OrdinalIgnoreCase) ||
                        RaizEh("ConsultaSeqRps") ||
                        codigoMunicipio == 2111300)
                    {
                        return "1.00";
                    }

                    if (RaizEh("CancelarNfseEnvio") &&
                        namespaceRaiz == "http://www.ginfes.com.br/servico_cancelar_nfse_envio")
                    {
                        return "2.00";
                    }

                    if (RaizEh("ConsultarSituacaoLoteRpsEnvio"))
                    {
                        return "3.00";
                    }

                    if (codigoMunicipio == 3170206)
                    {
                        return "2.04";
                    }

                    if ((codigoMunicipio == 3549904 && Contem("ginfes")) ||
                        versaoDeclarada == "3.00")
                    {
                        return "3.00";
                    }
                    if (RaizEh("DPS"))
                    {
                        return "1.01";
                    }

                    return "2.03";

                case PadraoNFSe.EL:
                    return RaizEh("DPS", "NFSe", "pedRegEvento") ||
                           Contem("infDPS") ||
                           Contem("infNFSe") ||
                           Contem("infPedReg")
                        ? "1.01"
                        : "2.04";

                case PadraoNFSe.FIORILLI:
                    if (!string.IsNullOrWhiteSpace(versaoDeclarada))
                    {
                        return versaoDeclarada;
                    }

                    if (raiz.IndexOf("Dps", StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        return raizQualificada.StartsWith("nfse:", StringComparison.OrdinalIgnoreCase)
                            ? "1.00"
                            : "1.01";
                    }

                    return RaizEh("ConsultarLoteRpsEnvio") ? "1.01" : "2.01";

                case PadraoNFSe.GIF:
                    return RaizEh("DPS", "NFSe", "pedRegEvento") ||
                           versaoDeclarada == "1.01"
                        ? "1.01"
                        : "1.00";

                case PadraoNFSe.GINFES:
                    if (versaoDeclarada == "4.00")
                    {
                        return "4.00";
                    }

                    if (RaizEh("CancelarNfseEnvio") && Contem("Prestador"))
                    {
                        return "2.00";
                    }

                    if (codigoMunicipio == 4125506 ||
                        namespaceRaiz.IndexOf("nfe.sjp.pr.gov.br", StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        return "3.00";
                    }

                    return "3.01";

                case PadraoNFSe.GISSONLINE:
                    return versaoDeclarada == "2.05" || Contem("IBSCBS")
                        ? "2.05"
                        : "2.04";

                case PadraoNFSe.IPM:
                    return RaizEh("nfse", "nota") ? "1.20" : "2.04";

                case PadraoNFSe.ISSNET:
                    if (namespaceRaiz.IndexOf("sped.fazenda.gov.br/nfse", StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        return "1.01";
                    }

                    if (namespaceRaiz.IndexOf("abrasf.org.br/nfse.xsd", StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        return "2.04";
                    }

                    if (RaizEh("DPS", "NFSe", "pedRegEvento"))
                    {
                        return "1.01";
                    }

                    return Contem("Pedido") ||
                           Contem("InfDeclaracaoPrestacaoServico") ||
                           RaizEh(
                               "ConsultarLoteRpsEnvio",
                               "ConsultarNfseRpsEnvio",
                               "ConsultarRpsDisponivelEnvio",
                               "EnviarLoteRpsEnvio",
                               "EnviarLoteRpsSincronoEnvio",
                               "SubstituirNfseEnvio")
                        ? "2.04"
                        : "1.01";

                case PadraoNFSe.PAULISTANA:
                    return versaoDeclarada == "2.00" || Contem("Versao=\"2\"") || Contem("IBSCBS")
                        ? "2.00"
                        : "1.00";

                case PadraoNFSe.PRONIM:
                    if (RaizEh("DPS", "NFSe", "pedRegEvento"))
                    {
                        return "1.01";
                    }

                    return RaizEh("ConsultarSituacaoLoteRpsEnvio") ? "1.00" : "2.03";

                case PadraoNFSe.RLZ_INFORMATICA:
                    return RaizEh("DPS", "NFSe", "pedRegEvento") ? "1.01" : "2.03";

                case PadraoNFSe.SIGCORP:
                    if (RaizEh("GerarNota", "CancelarNota"))
                    {
                        return codigoMunicipio == 4113700 ? "1.03" : "3.00";
                    }

                    if (RaizEh("ConsultarNotaPrestador", "ConsultarNotaValida"))
                    {
                        return "3.00";
                    }

                    if (codigoMunicipio == 4113700)
                    {
                        return "1.03";
                    }

                    return codigoMunicipio == 4204202 ||
                           codigoMunicipio == 3131307 ||
                           codigoMunicipio == 3530805 ||
                           codigoMunicipio == 3145208 ||
                           codigoMunicipio == 3300704
                        ? "2.04"
                        : "2.03";

                case PadraoNFSe.SIMPLISS:
                    if (versaoDeclarada == "1.00")
                    {
                        return "1.00";
                    }

                    if (RaizEh("DPS", "NFSe", "pedRegEvento") ||
                        versaoDeclarada == "1.01")
                    {
                        return "1.01";
                    }

                    return codigoMunicipio == 3306305 || codigoMunicipio == 4202404
                        ? "2.03"
                        : "3.00";

                case PadraoNFSe.SMARAPD:
                    if (RaizEh("nfd", "tbnfd"))
                    {
                        return "1.00";
                    }

                    if (RaizEh("DPS", "NFSe", "evento", "consPedRegEvento"))
                    {
                        return "1.01";
                    }

                    if (codigoMunicipio == 3506003)
                    {
                        return "1.01";
                    }

                    if (codigoMunicipio == 3530607 ||
                        codigoMunicipio == 3201308 ||
                        codigoMunicipio == 3523107)
                    {
                        return "2.03";
                    }

                    return "2.04";

                case PadraoNFSe.TINUS:
                    return versaoDeclarada == "2.03" ||
                           namespaceRaiz.IndexOf("abrasf", StringComparison.OrdinalIgnoreCase) >= 0 ||
                           Contem("abrasf")
                        ? "2.03"
                        : "1.00";

                case PadraoNFSe.TIPLAN:
                    if (RaizEh("DPS"))
                    {
                        return "1.01";
                    }

                    if (namespaceRaiz.IndexOf("ABRASF/arquivos/nfse.xsd", StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        return "1.00";
                    }

                    return "2.03";

                case PadraoNFSe.ABASE:
                case PadraoNFSe.PRODATA:
                case PadraoNFSe.PRODEB:
                case PadraoNFSe.SONNER:
                    return "2.01";

                case PadraoNFSe.ADM_SISTEMAS:
                case PadraoNFSe.ELOTECH:
                case PadraoNFSe.FISCO:
                    return "2.03";

                case PadraoNFSe.AGILI:
                case PadraoNFSe.EGOVERNEISS:
                case PadraoNFSe.EQUIPLANO:
                case PadraoNFSe.HM2SOLUCOES:
                case PadraoNFSe.INTERSOL:
                case PadraoNFSe.MEMORY:
                case PadraoNFSe.METROPOLIS:
                case PadraoNFSe.NOBESISTEMAS:
                case PadraoNFSe.PROPRIOBARUERISP:
                case PadraoNFSe.SALVADOR_BA:
                case PadraoNFSe.TECNOSISTEMAS:
                case PadraoNFSe.WEBFISCO:
                    return "1.00";

                case PadraoNFSe.AVMB:
                case PadraoNFSe.FINTEL:
                case PadraoNFSe.FUTURIZE:
                case PadraoNFSe.MODERNIZACAO_PUBLICA:
                case PadraoNFSe.PORTAL_FACIL:
                case PadraoNFSe.WEBISS:
                    return "2.02";

                case PadraoNFSe.BETHA_CLOUD:
                case PadraoNFSe.COPLAN:
                case PadraoNFSe.DIGIFRED:
                case PadraoNFSe.PRIMAX:
                case PadraoNFSe.QUASAR:
                    return "1.01";

                case PadraoNFSe.BSITBR:
                case PadraoNFSe.CENTI:
                case PadraoNFSe.GIAP:
                    return "2.00";

                case PadraoNFSe.MEGASOFT:
                    return "2.02";

                case PadraoNFSe.PROPRIOFORTALEZACE:
                    return "4.00";

                case PadraoNFSe.PUBLICA:
                    return "3.00";

                case PadraoNFSe.TRIBUTUS:
                    return "2.04";

                default:
                    return string.Empty;
            }
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

                #region BPe

                case "consStatServBPe":
                case "consSitBPe":
                case "eventoBPe":
                case "BPe":
                case "BPeTM":
                case "BPeTA":
                    tipoDFe = TipoDFe.BPe;
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
                case "Dare":
                case "DareLote":
                case "Receitas":
                case "DARE":
                    tipoDFe = TipoDFe.DARE;
                    break;
                #endregion

                #region GNRE
                case "TConsultaConfigUf":
                case "TConsLote_GNRE":
                case "TLote_GNRE":
                case "TLote_ConsultaGNRE":
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

