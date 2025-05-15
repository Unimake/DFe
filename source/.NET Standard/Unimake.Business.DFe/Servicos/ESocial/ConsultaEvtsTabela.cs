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
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsTabela")]
    [ComVisible(true)]
#endif
    public class ConsultarEvtsTabela : ServicoBase, IInteropService<ConsultarEvtsTabelaESocial>
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsTabela() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsTabela(ConsultarEvtsTabelaESocial consulta, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
        }

        ///<summary>
        ///Construtor simplificado para API
        /// </summary>
        /// <param name="tpInsc">Tipo de inscrição do empregador (ex: 1=CNPJ, 2=CPF)</param>
        /// <param name="nrInsc">Número de inscrição do empregador</param>
        /// <param name="tpEvt">Tipo de evento (ex: S-1200)</param>
        /// <param name="chEvt">Chave do evento</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultarEvtsTabela(TiposInscricao tpInsc, string nrInsc, string tpEvt, string chEvt, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(tpInsc.ToString()))
            {
                throw new ArgumentNullException(nameof(tpInsc));
            }

            if (string.IsNullOrWhiteSpace(nrInsc))
            {
                throw new ArgumentNullException(nameof(nrInsc));
            }

            if (string.IsNullOrWhiteSpace(tpEvt))
            {
                throw new ArgumentNullException(nameof(tpEvt));
            }

            if (string.IsNullOrWhiteSpace(chEvt))
            {
                throw new ArgumentNullException(nameof(chEvt));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsultarEvtsTabelaESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = tpInsc,
                        NrInsc = nrInsc
                    },
                    ConsultaEvtsTabela = new ConsultaEvtsTabela
                    {
                        ChEvt = chEvt,
                        TpEvt = tpEvt,
                        DtIniField = DateTime.Now.ToString(),
                        DtFimField = DateTime.Now.AddDays(15).ToString(),
                    }
                }
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }


#if INTEROP
        /// <summary>
        /// 
        /// </summary>
        /// <param name="consultarEvtsTabelaESocial"></param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
                [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsultarEvtsTabelaESocial consultarEvtsTabelaESocial, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consultarEvtsTabelaESocial?.GerarXML() ?? throw new ArgumentNullException(nameof(consultarEvtsTabelaESocial)), configuracao);
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
            var xml = new ConsultarEvtsTabelaESocial();
            xml = xml.LerXML<ConsultarEvtsTabelaESocial>(ConteudoXML);

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
