#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.CTeOS;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTeOS
{
    /// <summary>
    /// Envio do XML de CTeOS para webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTeOS.Autorizacao")]
    [ComVisible(true)]
#endif
    public class Autorizacao : ServicoBase, IInteropService<Xml.CTeOS.CTeOS>
    {
        private void MontarQrCode()
        {
            CTeOS = new Xml.CTeOS.CTeOS().LerXML<Xml.CTeOS.CTeOS>(ConteudoXML);

            if (CTeOS.InfCTeSupl == null)
            {
                CTeOS.InfCTeSupl = new Xml.CTeOS.InfCTeSupl();

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);

                var paramLinkQRCode = urlQrCode +
                    "?chCTe=" + CTeOS.InfCTe.Chave +
                    "&tpAmb=" + ((int)CTeOS.InfCTe.Ide.TpAmb).ToString();

                if (CTeOS.InfCTe.Ide.TpEmis == TipoEmissao.ContingenciaEPEC || CTeOS.InfCTe.Ide.TpEmis == TipoEmissao.ContingenciaFSDA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, CTeOS.InfCTe.Chave);
                }

                CTeOS.InfCTeSupl.QrCodCTe = paramLinkQRCode.Trim();
            }

            //Atualizar a propriedade do XML do CTe novamente com o conteúdo atual já a tag de QRCode e link de consulta
            ConteudoXML = CTeOS.GerarXML();
        }

        #region Private Fields

        private Xml.CTeOS.CTeOS _cteOS;
        private readonly Dictionary<string, CteOSProc> CteOSProcs = new Dictionary<string, CteOSProc>();

        #endregion Private Fields

        #region Protected Properties

        /// <summary>
        /// Objeto do XML do CTe-OS
        /// </summary>
        public Xml.CTeOS.CTeOS CTeOS
        {
            get => _cteOS ?? (_cteOS = new Xml.CTeOS.CTeOS().LerXML<Xml.CTeOS.CTeOS>(ConteudoXML));
            protected set => _cteOS = value;
        }

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (CTeOS == null)
            {
                Configuracoes.Definida = false;
                return;
            }

            var xml = CTeOS;

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.CTeAutorizacaoOS;
                Configuracoes.CodigoUF = (int)xml.InfCTe.Ide.CUF;
                Configuracoes.TipoAmbiente = xml.InfCTe.Ide.TpAmb;
                Configuracoes.Modelo = xml.InfCTe.Ide.Mod;
                Configuracoes.TipoEmissao = xml.InfCTe.Ide.TpEmis;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Efetuar um Ajustes no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        #endregion Protected Methods

        #region Public Properties

#if INTEROP

        /// <summary>
        /// Adicionar o retorno da consulta situação do CTeOS na lista dos retornos para elaboração do XML de Distribuição
        /// </summary>
        /// <param name="item">Resultado da consulta situação do CTe</param>
        public void AddRetConsSitCTes(RetConsSitCTe item) => (RetConsSitCTes ?? (RetConsSitCTes = new List<RetConsSitCTe>())).Add(item);

#endif

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta situação do CTe
        /// </summary>
        public List<RetConsSitCTe> RetConsSitCTes = new List<RetConsSitCTe>();

        /// <summary>
        /// Propriedade contendo o XML da CTe com o protocolo de autorização anexado - Envio Assíncrono
        /// </summary>
        public Dictionary<string, CteOSProc> CteOSProcResults
        {
            get
            {
                if (Result.ProtCTe != null)
                {
                    if (CteOSProcs.ContainsKey(CTeOS.InfCTe.Chave))
                    {
                        CteOSProcs[CTeOS.InfCTe.Chave].ProtCTe = Result.ProtCTe;
                    }
                    else
                    {
                        CteOSProcs.Add(CTeOS.InfCTe.Chave,
                            new CteOSProc
                            {
                                Versao = CTeOS.Versao,
                                CTeOS = CTeOS,
                                ProtCTe = Result.ProtCTe
                            });
                    }
                }
                else
                {
                    if (RetConsSitCTes.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitCte, sem a definição dela não é possível obter o conteúdo da CteOSProcResults.");
                    }

                    ProtCTe protCTe = null;

                    #region Resultado do envio do CTeOS através da consulta situação

                    foreach (var item in RetConsSitCTes)
                    {
                        if (item != null && item.ProtCTe != null)
                        {
                            if (item.ProtCTe.InfProt.ChCTe == CTeOS.InfCTe.Chave)
                            {
                                switch (item.ProtCTe.InfProt.CStat)
                                {
                                    case 100: //CTe Autorizado
                                    case 110: //CTe Denegado - Não sei quando ocorre este, mas descobrir ele no manual então estou incluindo. 
                                    case 301: //CTe Denegado - Irregularidade fiscal do emitente
                                    case 302: //CTe Denegado - Irregularidade fiscal do remetente
                                    case 303: //CTe Denegado - Irregularidade fiscal do destinatário
                                    case 304: //CTe Denegado - Irregularidade fiscal do expedidor
                                    case 305: //CTe Denegado - Irregularidade fiscal do recebedor
                                    case 306: //CTe Denegado - Irregularidade fiscal do tomador
                                        protCTe = item.ProtCTe;
                                        break;
                                }
                            }
                        }
                    }

                    if (CteOSProcs.ContainsKey(CTeOS.InfCTe.Chave))
                    {
                        CteOSProcs[CTeOS.InfCTe.Chave].ProtCTe = protCTe;
                    }
                    else
                    {
                        CteOSProcs.Add(CTeOS.InfCTe.Chave,
                            new CteOSProc
                            {
                                Versao = CTeOS.Versao,
                                CTeOS = CTeOS,
                                ProtCTe = protCTe
                            });
                    }

                    #endregion
                }

                return CteOSProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do CTeOS no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do CTeOS que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do CTeOS</returns>
        public string GetCteOSProcResults(string chaveDFe)
        {
            var retornar = "";
            if (CteOSProcResults.Count > 0)
            {
                retornar = CteOSProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif


        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetCTeOS Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetCTeOS>(RetornoWSXML);
                }

                return new RetCTeOS
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public Autorizacao() : base() => CteOSProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="cteOS">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(Xml.CTeOS.CTeOS cteOS, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(cteOS?.GerarXML() ?? throw new ArgumentNullException(nameof(cteOS)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                if (CTeOS == null)
                {
                    throw new NullReferenceException($"{nameof(CTeOS)} não pode ser nulo.");
                }

                DefinirConfiguracao();
            }

            base.Executar();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="cteOS">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(Xml.CTeOS.CTeOS cteOS, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cteOS?.GerarXML() ?? throw new ArgumentNullException(nameof(cteOS)), configuracao);
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
        /// <param name="cteOS">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.CTeOS.CTeOS cteOS, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cteOS?.GerarXML() ?? throw new ArgumentNullException(nameof(cteOS)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
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
                foreach (var item in CteOSProcResults)
                {
                    if (item.Value.ProtCTe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
                    }
                }
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
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                foreach (var item in CteOSProcResults)
                {
                    if (item.Value.ProtCTe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}