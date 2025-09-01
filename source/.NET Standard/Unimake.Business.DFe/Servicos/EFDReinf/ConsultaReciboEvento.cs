#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Exceptions;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML de consulta recibo Evento EFDReinf para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento")]
    [ComVisible(true)]
#endif
    public class ConsultaReciboEvento : ServicoBase, IInteropService<ReinfConsulta>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfConsulta();
            xml = xml.LerXML<ReinfConsulta>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfConsultaReciboEvento;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;
                Configuracoes.TipoEventoEFDReinf = xml.ConsultaReciboEvento.TipoEvento;

                base.DefinirConfiguracao();

                // Montar a URL específica do evento (RequestURI)
                MontarRequestURI(xml, Configuracoes);
            }
        }

        /// <summary>
        /// Método para juntar a URL base com o path do serviço
        /// </summary>
        /// <param name="baseUrl"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        private static string JoinUrl(string baseUrl, string path)
        {
            if (string.IsNullOrWhiteSpace(baseUrl)) baseUrl = string.Empty;
            if (string.IsNullOrWhiteSpace(path)) path = string.Empty;
            baseUrl = baseUrl.TrimEnd('/');
            path = path.TrimStart('/');
            return baseUrl.Length == 0 ? path : $"{baseUrl}/{path}";
        }


        /// <summary>
        /// Substitui as tags do template da URL do web-service pelos valores do XML
        /// </summary>
        /// <param name="xml"></param>
        /// <param name="config"></param>
        /// <exception cref="Exception"></exception>
        private void MontarRequestURI(ReinfConsulta xml, Configuracao config)
        {
            var tipoEvento = xml.ConsultaReciboEvento.TipoEvento;

            if (string.IsNullOrWhiteSpace(tipoEvento) || !config.UrlsRecibosEventos.ContainsKey(tipoEvento))
                throw new Exception($"Não foi encontrada configuração de URL para o evento {tipoEvento}.");

            var cfgURL = config.UrlsRecibosEventos[tipoEvento];

            // Primária por padrão; 4010/4020 podem usar secundária quando faltar CPF/CNPJ
            var urlTemplate = cfgURL.UrlReciboPrimaria ?? string.Empty;
            if (tipoEvento == "4010" && string.IsNullOrWhiteSpace(xml.ConsultaReciboEvento.CpfBenef) && !string.IsNullOrWhiteSpace(cfgURL.UrlReciboSecundaria))
                urlTemplate = cfgURL.UrlReciboSecundaria;
            else if (tipoEvento == "4020" && string.IsNullOrWhiteSpace(xml.ConsultaReciboEvento.CnpjBenef) && !string.IsNullOrWhiteSpace(cfgURL.UrlReciboSecundaria))
                urlTemplate = cfgURL.UrlReciboSecundaria;

            urlTemplate = urlTemplate
                .Replace("{tpInsc}", ((int?)xml.ConsultaReciboEvento.TpInsc)?.ToString() ?? "")
                .Replace("{nrInsc}", xml.ConsultaReciboEvento.NrInsc ?? "")
                .Replace("{perApur}", xml.ConsultaReciboEvento.PerApur.ToString("yyyy-MM"))
                .Replace("{dtApur}", xml.ConsultaReciboEvento.DtApur.ToString("yyyy-MM"))
                .Replace("{dtApuracao}", xml.ConsultaReciboEvento.DtApuracao.ToString("yyyy-MM-dd"))
                .Replace("{tpInscEstab}", ((int?)xml.ConsultaReciboEvento.TpInscEstab)?.ToString() ?? "")
                .Replace("{nrInscEstab}", xml.ConsultaReciboEvento.NrInscEstab ?? "")
                .Replace("{nrInscEstabPrest}", xml.ConsultaReciboEvento.NrInscEstabPrest ?? "")
                .Replace("{tpInscTomador}", ((int?)xml.ConsultaReciboEvento.TpInscTomador)?.ToString() ?? "")
                .Replace("{nrInscTomador}", xml.ConsultaReciboEvento.NrInscTomador ?? "")
                .Replace("{cnpjPrestador}", xml.ConsultaReciboEvento.CnpjPrestador ?? "")
                .Replace("{nrInscAdq}", xml.ConsultaReciboEvento.NrInscAdq ?? "")
                .Replace("{tpInscAdq}", ((int?)xml.ConsultaReciboEvento.TpInscAdq)?.ToString() ?? "")
                .Replace("{tpInscProd}", ((int?)xml.ConsultaReciboEvento.TpInscProd)?.ToString() ?? "")
                .Replace("{nrInscProd}", xml.ConsultaReciboEvento.NrInscProd ?? "")
                .Replace("{nrInscEstabelecimento}", xml.ConsultaReciboEvento.NrInscEstabelecimento ?? "")
                .Replace("{cpfBenef}", xml.ConsultaReciboEvento.CpfBenef ?? "")
                .Replace("{cnpjBenef}", xml.ConsultaReciboEvento.CnpjBenef ?? "");

            // Evento 4080 usa {cnpjFonte}. Seu modelo não tem CnpjFonte.
            urlTemplate = urlTemplate.Replace("{cnpjFonte}", xml.ConsultaReciboEvento.CnpjPrestador ?? "");

            // Base: prioriza RequestURI*, senão WebEndereco*
            var basePrefix = (config.TipoAmbiente == TipoAmbiente.Homologacao)
                ? (!string.IsNullOrWhiteSpace(config.RequestURIHomologacao) ? config.RequestURIHomologacao : config.WebEnderecoHomologacao)
                : (!string.IsNullOrWhiteSpace(config.RequestURIProducao) ? config.RequestURIProducao : config.WebEnderecoProducao);

            config.RequestURI = JoinUrl(basePrefix, urlTemplate);
        }



        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public ReinfRetornoRecibo Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<ReinfRetornoRecibo>(RetornoWSXML);
                }

                return new ReinfRetornoRecibo { };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaReciboEvento() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfConsulta">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaReciboEvento(ReinfConsulta reinfConsulta, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(reinfConsulta?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsulta)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        /// <exception cref="ArgumentNullException"></exception>
        public ConsultaReciboEvento(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
        }

        ///<summary>
        ///Construtor simplificado para API
        /// </summary>
        /// <param name="tipoEvento">Tipo do evento a ser consultado</param>
        /// <param name="tipoInscricao">Tipo de incrição do contribuinte</param>
        /// <param name="numInscricao">Número da inscrição do contribuinte</param>
        /// <param name="tipoAmbiente">Ambiente de Produção ou Homologação</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaReciboEvento(string tipoEvento, TiposInscricao tipoInscricao, string numInscricao, TipoAmbiente tipoAmbiente, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(tipoEvento))
            {
                throw new ArgumentNullException(nameof(tipoEvento));
            }

            if (string.IsNullOrEmpty(tipoInscricao.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoInscricao));
            }

            if (string.IsNullOrEmpty(numInscricao))
            {
                throw new ArgumentNullException(nameof(numInscricao));
            }

            if (string.IsNullOrEmpty(tipoAmbiente.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoAmbiente));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            configuracao.TipoEventoEFDReinf = tipoEvento;

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new Xml.EFDReinf.ConsultaReciboEvento
                {
                    TipoEvento = tipoEvento,
                    TpInsc = tipoInscricao,
                    NrInsc = numInscricao,
                    PerApurField = DateTime.Now.ToString("yyyy-MM")
                }

            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfConsulta">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfConsulta reinfConsulta, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfConsulta?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsulta)), configuracao);
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

#endif

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta status do serviço.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }


        #endregion Public Methods

    }
}