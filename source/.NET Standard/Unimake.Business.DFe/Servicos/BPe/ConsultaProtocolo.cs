#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Exceptions;
#endif

using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Enviar o XML de consulta protocolo do BPe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo")]
    [ComVisible(true)]
#endif
    public class ConsultaProtocolo : ServicoBase, IInteropService<ConsSitBPe>
    {
        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsSitBPe().LerXML<ConsSitBPe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.BPe;
                Configuracoes.Servico = Servico.BPeConsultaProtocolo;
                Configuracoes.CodigoUF = Convert.ToInt32(xml.ChBPe.Substring(0, 2));
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.Modelo = (ModeloDFe)int.Parse(xml.ChBPe.Substring(20, 2));
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetConsSitBPe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsSitBPe>(RetornoWSXML);
                }

                return new RetConsSitBPe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaProtocolo() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consSitBPe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(ConsSitBPe consSitBPe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consSitBPe?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitBPe)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
        }

        /// <summary>
        /// Construtor simplificado
        /// </summary>
        /// <param name="chBPe">Chave de acesso do BPe</param>
        /// <param name="tpAmb">Tipo de ambiente</param>
        /// <param name="configuracao">Configuração para conexão e envio de XML</param>
        public ConsultaProtocolo(string chBPe, TipoAmbiente tpAmb, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(chBPe))
            {
                throw new ArgumentNullException(nameof(chBPe));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsSitBPe
            {
                Versao = "1.00",
                TpAmb = tpAmb,
                ChBPe = chBPe,
                XServ = "CONSULTAR"
            };

            Inicializar(xml.GerarXML(), configuracao);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço
        /// </summary>
        /// <param name="consSitBPe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(ConsSitBPe consSitBPe, Configuracao configuracao)
        {
            try
            {
                Inicializar(consSitBPe?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitBPe)), configuracao);
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

#endif

        /// <summary>
        /// Grava o XML de Distribuição em uma pasta definida.
        /// </summary>
        /// <param name="pasta">Pasta onde é para ser gravado do XML</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta de protocolo.");
            }
            catch (Exception ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}
