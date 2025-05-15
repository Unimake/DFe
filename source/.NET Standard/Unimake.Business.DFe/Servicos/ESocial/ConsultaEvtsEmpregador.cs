#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o xml para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsEmpregador")]
    [ComVisible(true)]
#endif
    public class ConsultarEvtsEmpregador : ServicoBase, IInteropService<ConsultarEvtsEmpregadorESocial>
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsEmpregador() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsEmpregador(ConsultarEvtsEmpregadorESocial consulta, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
        }

        /// <summary>
        /// Construtor simplificado para API
        /// </summary>
        /// <param name="tpInsc">Tipo de inscrição do empregador (ex: 1=CNPJ, 2=CPF)</param>
        /// <param name="nrInsc">Número de inscrição do empregador</param>
        /// <param name="perApur">Período de apuração (formato: AAAA-MM)</param>
        /// <param name="tpEvt">Tipo de evento (ex: S-1020)</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultarEvtsEmpregador(TiposInscricao tpInsc, string nrInsc, string perApur, string tpEvt, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(tpInsc.ToString()))
            {
                throw new ArgumentNullException(nameof(tpInsc));
            }

            if (string.IsNullOrWhiteSpace(nrInsc))
            {
                throw new ArgumentNullException(nameof(nrInsc));
            }

            if (string.IsNullOrWhiteSpace(perApur))
            {
                throw new ArgumentNullException(nameof(perApur));
            }

            if (string.IsNullOrWhiteSpace(tpEvt))
            {
                throw new ArgumentNullException(nameof(tpEvt));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsultarEvtsEmpregadorESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = tpInsc,
                        NrInsc = nrInsc
                    },
                    ConsultaEvtsEmpregador = new ConsultaEvtsEmpregador
                    {
                        PerApurField = perApur,
                        TpEvt = tpEvt
                    }
                }
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço: envia o XML para o web-service
        /// </summary>
        /// <param name="consultarEvtsEmpregadorESocial">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsultarEvtsEmpregadorESocial consultarEvtsEmpregadorESocial, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consultarEvtsEmpregadorESocial?.GerarXML() ?? throw new ArgumentNullException(nameof(consultarEvtsEmpregadorESocial)), configuracao);
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

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pasta"></param>
        /// <param name="nomeArquivo"></param>
        /// <param name="conteudoXML"></param>
        /// <exception cref="NotImplementedException"></exception>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            //throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsultarEvtsEmpregadorESocial();
            xml = xml.LerXML<ConsultarEvtsEmpregadorESocial>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialConsultaEvts;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

    }
}
