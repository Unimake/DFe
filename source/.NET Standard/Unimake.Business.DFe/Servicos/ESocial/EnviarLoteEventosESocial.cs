using System;
using System.Runtime.InteropServices;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o XML o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")]
    [ComVisible(true)]
#endif
    public class EnviarLoteEventosESocial : ServicoBase, IInteropService<ESocialEnvioLoteEventos>
    {

        private ESocialEnvioLoteEventos _ESocialEnvioLoteEventos;

        /// <summary>
        /// Objeto do XML do lote eventos eSocial
        /// </summary>
        public ESocialEnvioLoteEventos ESocialEnvioLoteEventos
        {
            get => _ESocialEnvioLoteEventos ?? (_ESocialEnvioLoteEventos = new ESocialEnvioLoteEventos().LerXML<ESocialEnvioLoteEventos>(ConteudoXML));
            protected set => _ESocialEnvioLoteEventos = value;
        }

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, (Configuracoes.TipoDFe).ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ESocialEnvioLoteEventos();
            xml = xml.LerXML<ESocialEnvioLoteEventos>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialEnviarLoteEventos;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var schemaArquivoEvento = string.Empty;

            ValidarXMLEvento(ConteudoXML, Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (Configuracoes.TiposEventosEspecificos.Count > 0)
            {
                string eventoEspecifico;
                var listEventos = ConteudoXML.GetElementsByTagName("evento");

                foreach (XmlNode nodeEvento in listEventos)
                {
                    var elementEvento = (XmlElement)nodeEvento;
                    var esocialEvento = elementEvento.GetElementsByTagName("eSocial")[0];

                    var xmlEventoEspecifico = new XmlDocument();
                    xmlEventoEspecifico.LoadXml(esocialEvento.OuterXml);

                    eventoEspecifico = esocialEvento.FirstChild.Name;
                    schemaArquivoEvento = Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].SchemaArquivoEvento;

                    //Vamos avaliar depois como será feito essa validação de eventos do eSocial - Wesley: 20/08/2024
                    //ValidarXMLEvento(xmlEventoEspecifico, schemaArquivoEvento, Configuracoes.TiposEventosEspecificos[eventoEspecifico.ToString()].TargetNS);
                }
            }

        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public EnviarLoteEventosESocial() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="eSocialEnviarLoteEventos">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public EnviarLoteEventosESocial(ESocialEnvioLoteEventos eSocialEnviarLoteEventos, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(eSocialEnviarLoteEventos?.GerarXML() ?? throw new ArgumentNullException(nameof(eSocialEnviarLoteEventos)), configuracao);

            #region Limpar a assinatura do objeto para assinarmos novamente evitando problemas - Autor: Wesley Data: 03/04/25 Ticket: #172039

            foreach (var evento in ESocialEnvioLoteEventos.EnvioLoteEventos.Eventos.Evento)
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

            // Atualiza o ConteudoXML sem as assinaturas que vieram no XML
            ConteudoXML = ESocialEnvioLoteEventos.GerarXML();

            // Força a assinatura agora da DLL
            _ = ConteudoXMLAssinado;

            // Atualiza o objeto ESocialEnvioLoteEventos com o novo ConteudoXML
            ESocialEnvioLoteEventos = ESocialEnvioLoteEventos.LerXML<ESocialEnvioLoteEventos>(ConteudoXML);

            #endregion Limpar a assinatura do objeto para assinarmos novamente evitando problemas - Autor: Wesley Data: 03/04/25 Ticket: #172039
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="eSocialEnviarLoteEventos">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ESocialEnvioLoteEventos eSocialEnviarLoteEventos, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(eSocialEnviarLoteEventos?.GerarXML() ?? throw new ArgumentNullException(nameof(eSocialEnviarLoteEventos)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
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
        /// <param name="eSocialEnviarLoteEventos">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao([MarshalAs(UnmanagedType.IUnknown)] ESocialEnvioLoteEventos eSocialEnviarLoteEventos, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(eSocialEnviarLoteEventos?.GerarXML() ?? throw new ArgumentNullException(nameof(eSocialEnviarLoteEventos)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#endif

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            //try
            //{
            //    throw new Exception("Não existe XML de distribuição para consulta status do serviço.");
            //}
            //catch (Exception ex)
            //{
            //    ThrowHelper.Instance.Throw(ex);
            //}
        }

        #endregion Public Methods

        #region Result

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public Xml.ESocial.Retorno.RetornoEnvioLote Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<Xml.ESocial.Retorno.RetornoEnvioLote>(RetornoWSXML);
                }

                return new Xml.ESocial.Retorno.RetornoEnvioLote
                {
                    RetornoEnvioLoteEventos = new Xml.ESocial.Retorno.RetornoEnvioLoteEventos
                    {
                        Status = new Xml.ESocial.Retorno.Status
                        {
                            CdResposta = 0,
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado",
                        }
                    }

                };
            }
        }
        #endregion

    }
}

