#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
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
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsTrabalhador")]
    [ComVisible(true)]
#endif
    public class ConsultarEvtsTrabalhador : ServicoBase, IInteropService<ConsultarEvtsTrabalhadorESocial>
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsTrabalhador() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvtsTrabalhador(ConsultarEvtsTrabalhadorESocial consulta, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
        }

        /// <summary>
        /// Construtor simplificado para consulta de eventos do trabalhador no eSocial
        /// </summary>
        /// <param name="tpInsc">Tipo de inscrição do empregador (ex: 1=CNPJ, 2=CPF)</param>
        /// <param name="nrInsc">Número de inscrição do empregador</param>
        /// <param name="cpfTrab">CPF do trabalhador</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultarEvtsTrabalhador(TiposInscricao tpInsc, string nrInsc, string cpfTrab, Configuracao configuracao) : this()
        {
            if (string.IsNullOrEmpty(tpInsc.ToString()))
            {
                throw new ArgumentNullException(nameof(tpInsc));
            }
            
            if (string.IsNullOrWhiteSpace(nrInsc))
            {
                throw new ArgumentNullException(nameof(nrInsc));
            }

            if (string.IsNullOrWhiteSpace(cpfTrab))
            {
                throw new ArgumentNullException(nameof(cpfTrab));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new ConsultarEvtsTrabalhadorESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = tpInsc,
                        NrInsc = nrInsc
                    },
                    ConsultaEvtsTrabalhador = new Xml.ESocial.ConsultaEvtsTrabalhador
                    {
                        CpfTrab = cpfTrab
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
                /// <param name="consultarEvtsTrabalhadorESocial"></param>
                /// <param name="configuracao"></param>
                /// <exception cref="NotImplementedException"></exception>
                [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsultarEvtsTrabalhadorESocial consultarEvtsTrabalhadorESocial, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consultarEvtsTrabalhadorESocial?.GerarXML() ?? throw new ArgumentNullException(nameof(consultarEvtsTrabalhadorESocial)), configuracao);
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

        #region Result

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public Xml.ESocial.Retorno.RetornoEventoProcessado Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<Xml.ESocial.Retorno.RetornoEventoProcessado>(RetornoWSXML);
                }

                return new Xml.ESocial.Retorno.RetornoEventoProcessado
                {
                    RetornoProcessamentoLoteEventos = new Xml.ESocial.Retorno.RetornoProcessamentoLoteEventos
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
            var xml = new ConsultarEvtsTrabalhadorESocial();
            xml = xml.LerXML<ConsultarEvtsTrabalhadorESocial>(ConteudoXML);

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
