#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Exceptions;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;

namespace Unimake.Business.DFe.Servicos.DCe
{
    /// <summary>
    /// Enviar o XML de consulta protocolo da DCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.DCe.ConsultaProtocolo")]
    [ComVisible(true)]
#endif
    public class ConsultaProtocolo : ServicoBase, IInteropService<ConsSitDCe>
    {
        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsSitDCe();
            xml = xml.LerXML<ConsSitDCe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.DCeConsultaProtocolo;
                Configuracoes.TipoDFe = TipoDFe.DCe;
                Configuracoes.CodigoUF = Convert.ToInt32(xml.ChDCe.Substring(0, 2));
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.Modelo = (ModeloDFe)int.Parse(xml.ChDCe.Substring(20, 2));
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetConsSitDCe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsSitDCe>(RetornoWSXML);
                }

                return new RetConsSitDCe
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
        /// <param name="consSitDCe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaProtocolo(ConsSitDCe consSitDCe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consSitDCe?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitDCe)), configuracao);
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

        ///<summary>
        ///Construtor simplificado para API
        /// </summary>
        /// <param name="chDCe">Chave de acesso da DCe</param>
        /// <param name="tpAmb">Tipo de ambiente, Produção ou Homologação</param>
        /// <param name="configuracao">Configuração para conexão e envio de XML</param>
        public ConsultaProtocolo(string chDCe, TipoAmbiente tpAmb, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(chDCe))
            {
                throw new ArgumentNullException(nameof(chDCe));
            }

            if (string.IsNullOrEmpty(tpAmb.ToString()))
            {
                throw new ArgumentNullException(nameof(tpAmb));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsSitDCe
            {
                Versao = "1.00",
                TpAmb = tpAmb,
                ChDCe = chDCe,
                XServ = "CONSULTAR"
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="consSitDCe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(ConsSitDCe consSitDCe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consSitDCe?.GerarXML() ?? throw new ArgumentNullException(nameof(consSitDCe)), configuracao);
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
        /// Grava o XML de Distribuição em uma pasta definida - (Para este serviço não tem XML de distribuição).
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
