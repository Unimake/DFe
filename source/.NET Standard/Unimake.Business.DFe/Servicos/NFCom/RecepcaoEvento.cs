#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.NFCom;
using System.Xml;
using Unimake.Exceptions;
using Unimake.Business.DFe.Utility;
using System.IO;

namespace Unimake.Business.DFe.Servicos.NFCom
{
    /// <summary>
    /// Enviar o XML de evento da NFCom para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCom.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : ServicoBase, IInteropService<EventoNFCom>
    {
        private EventoNFCom _EventoNFCom;

        /// <summary>
        /// Objeto do XML do evento
        /// </summary>
        public EventoNFCom EventoNFCom
        {
            get => _EventoNFCom ?? (_EventoNFCom = new EventoNFCom().LerXML<EventoNFCom>(ConteudoXML));
            protected set => _EventoNFCom = value;
        }

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, Configuracoes.TipoDFe.ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new EventoNFCom();
            xml = xml.LerXML<EventoNFCom>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.CodigoUF = (int)xml.InfEvento.COrgao;
                Configuracoes.TipoAmbiente = xml.InfEvento.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo();

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                int tpEvento;
                if (ConteudoXML.GetElementsByTagName("tpEvento").Count > 0)
                {
                    tpEvento = Convert.ToInt32(ConteudoXML.GetElementsByTagName("tpEvento")[0].InnerText);
                }
                else
                {
                    throw new Exception("Não foi possível localizar a tag obrigatória <tpEvento> no XML.");
                }

                schemaArquivo = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivo;
                schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivoEspecifico;
            }

            #region Validar o XML geral

            ValidarXMLEvento(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

            #endregion Validar o XML geral

            #region Validar a parte específica de cada evento

            var listEvento = ConteudoXML.GetElementsByTagName("eventoNFCom");

            for (var i = 0; i < listEvento.Count; i++)
            {
                var elementEvento = (XmlElement)listEvento[i];

                if (elementEvento.GetElementsByTagName("infEvento")[0] != null)
                {
                    var elementInfEvento = (XmlElement)elementEvento.GetElementsByTagName("infEvento")[0];

                    if (elementInfEvento.GetElementsByTagName("tpEvento")[0] != null)
                    {
                        var xmlEspecifico = new XmlDocument();
                        xmlEspecifico.LoadXml(elementInfEvento.GetElementsByTagName("detEvento")[0].FirstChild.OuterXml);

                        ValidarXMLEvento(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);
                    }
                }
            }

            #endregion Validar a parte específica de cada evento
        }

        /// <summary>
        /// Validar o conteúdo das tags do XML, alguns validações manuais que o schema não faz.
        /// </summary>
        protected override void XmlValidarConteudo()
        {
            base.XmlValidarConteudo();
        }

        /// <summary>
        /// Propriedade contendo o XML do evento com o protocolo de autorização anexado
        /// </summary>
        public ProcEventoNFCom ProcEventoNFComResult => new ProcEventoNFCom
        {
            Versao = EventoNFCom.Versao,
            EventoNFCom = EventoNFCom,
            RetEventoNFCom = Result
        };

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetEventoNFCom Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEventoNFCom>(RetornoWSXML);
                }

                return new RetEventoNFCom
                {
                    InfEvento = new InfRetEvento
                    {
                        CStat = 0,
                        XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                    }
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="eventoNFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public RecepcaoEvento(EventoNFCom eventoNFCom, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(eventoNFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(eventoNFCom)), configuracao);

            EventoNFCom = EventoNFCom.LerXML<EventoNFCom>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configuraões para conexão e envio do XML para o webservice</param>
        public RecepcaoEvento(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            #region Limpar a assinatura do objeto para recriar e atualizar o ConteudoXML. Isso garante que a propriedade e o objeto tenham assinaturas iguais, evitando discrepâncias. Autor: Wandrey Data: 10/06/2024

            //Remover a assinatura para forçar criar novamente
            EventoNFCom = EventoNFCom.LerXML<EventoNFCom>(ConteudoXML);
            EventoNFCom.Signature = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = EventoNFCom.GerarXML();

            //Forçar assinar novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado
            EventoNFCom = EventoNFCom.LerXML<EventoNFCom>(ConteudoXML);

            #endregion Limpar a assinatura do objeto para recriar e atualizar o ConteudoXML. Isso garante que a propriedade e o objeto tenham assinaturas iguais, evitando discrepâncias. Autor: Wandrey Data: 10/06/2024
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

#if INTEROP
        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="eventoNFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [ComVisible(true)]
        public void Executar(EventoNFCom eventoNFCom, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(eventoNFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(eventoNFCom)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Definir o objeto contendo o XML a ser enviado e configuração de conexão e envio do XML para web-service
        /// </summary>
        /// <param name="eventoNFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(EventoNFCom eventoNFCom, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(eventoNFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(eventoNFCom)), configuracao);
            }
            catch (Exception ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
        }
#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                GravarXmlDistribuicao(pasta, ProcEventoNFComResult.NomeArquivoDistribuicao, ProcEventoNFComResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição no stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o XML de distribuição</param>
        public void GravarXmlDistribuicao(Stream stream)
        {
            try
            {
                GravarXmlDistribuicao(stream, ProcEventoNFComResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}
