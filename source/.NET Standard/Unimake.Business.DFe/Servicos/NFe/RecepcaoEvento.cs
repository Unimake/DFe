using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Security.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de eventos da NFe para o webservice
    /// </summary>
    public class RecepcaoEvento: ServicoBase, IInteropService<EnvEvento>
    {
        #region Private Properties

        private EnvEvento EnvEvento => new EnvEvento().LerXML<EnvEvento>(ConteudoXML);

        #endregion Private Properties

        #region Private Methods

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, (Configuracoes.TipoDFe == TipoDFe.NFCe ? TipoDFe.NFe : Configuracoes.TipoDFe).ToString() + "." + schemaArquivo, targetNS);

            if(!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #endregion Private Methods

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new EnvEvento();
            xml = xml.LerXML<EnvEvento>(ConteudoXML);

            if(!Configuracoes.Definida)
            {
                Configuracoes.CodigoUF = (int)xml.Evento[0].InfEvento.COrgao;
                Configuracoes.TipoAmbiente = xml.Evento[0].InfEvento.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo(); // Efetuar a validação antes de validar schema para evitar alguns erros que não ficam claros para o desenvolvedor.

            var xml = EnvEvento;

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if(Configuracoes.SchemasEspecificos.Count > 0)
            {
                var tpEvento = ((int)xml.Evento[0].InfEvento.TpEvento);

                schemaArquivo = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivo;
                schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[tpEvento.ToString()].SchemaArquivoEspecifico;
            }

            #region Validar o XML geral

            ValidarXMLEvento(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

            #endregion Validar o XML geral

            #region Validar a parte específica de cada evento

            var listEvento = ConteudoXML.GetElementsByTagName("evento");
            for(var i = 0; i < listEvento.Count; i++)
            {
                var elementEvento = (XmlElement)listEvento[i];

                if(elementEvento.GetElementsByTagName("infEvento")[0] != null)
                {
                    var elementInfEvento = (XmlElement)elementEvento.GetElementsByTagName("infEvento")[0];
                    if(elementInfEvento.GetElementsByTagName("tpEvento")[0] != null)
                    {
                        var tpEvento = elementInfEvento.GetElementsByTagName("tpEvento")[0].InnerText;

                        var tipoEventoNFe = (TipoEventoNFe)Enum.Parse(typeof(TipoEventoNFe), tpEvento);

                        var xmlEspecifico = new XmlDocument();
                        switch(tipoEventoNFe)
                        {
                            case TipoEventoNFe.CartaCorrecao:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCCE>((DetEventoCCE)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.Cancelamento:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCanc>((DetEventoCanc)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.CancelamentoPorSubstituicao:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCancSubst>((DetEventoCancSubst)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.EPEC:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoEPEC>((DetEventoEPEC)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.PedidoProrrogacaoPrazo1:
                            case TipoEventoNFe.PedidoProrrogacaoPrazo2:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoPedidoProrrogPrazoICMS>((DetEventoPedidoProrrogPrazoICMS)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo1:
                            case TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo2:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCancPedidoProrrogPrazoICMS>((DetEventoCancPedidoProrrogPrazoICMS)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.ManifestacaoConfirmacaoOperacao:
                            case TipoEventoNFe.ManifestacaoCienciaOperacao:
                            case TipoEventoNFe.ManifestacaoDesconhecimentoOperacao:
                            case TipoEventoNFe.ManifestacaoOperacaoNaoRealizada:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoManif>((DetEventoManif)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.ComprovanteEntregaNFe:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCompEntregaNFe>((DetEventoCompEntregaNFe)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;

                            case TipoEventoNFe.CancelamentoComprovanteEntregaNFe:
                                xmlEspecifico.LoadXml(XMLUtility.Serializar<DetEventoCancCompEntregaNFe>((DetEventoCancCompEntregaNFe)xml.Evento[i].InfEvento.DetEvento).OuterXml);
                                break;


                            default:
                                throw new Exception("Não foi possível identificar o tipo de evento.");
                        }

                        ValidarXMLEvento(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);
                    }
                }
            }

            #endregion Validar a parte específica de cada evento
        }

        /// <summary>
        /// Validar o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected override void XmlValidarConteudo()
        {
            base.XmlValidarConteudo();

            var xml = EnvEvento;

            var tpEvento = xml.Evento[0].InfEvento.TpEvento;

            var msgException = "Conteúdo da tag <descEvento> deve ser igual a \"$\", pois foi este o conteudo informado na tag <tpEvento>.";
            string descEvento;

            switch(tpEvento)
            {
                case TipoEventoNFe.ManifestacaoCienciaOperacao:
                    descEvento = "Ciencia da Operacao";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoConfirmacaoOperacao:
                    descEvento = "Confirmacao da Operacao";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoDesconhecimentoOperacao:
                    descEvento = "Desconhecimento da Operacao";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoOperacaoNaoRealizada:
                    descEvento = "Operacao nao Realizada";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ComprovanteEntregaNFe:
                    descEvento = "Comprovante de Entrega da NF-e";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.CancelamentoComprovanteEntregaNFe:
                    descEvento = "Cancelamento Comprovante de Entrega da NF-e";
                    if(!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.EPEC:
                    if(xml.Evento[0].InfEvento.COrgao != UFBrasil.AN)
                    {
                        throw new Exception("Conteúdo da tag <cOrgao> inválido. Para eventos de manifestação do destinatário o conteúdo da tag <cOrgao> deve igual a 91.");
                    }
                    break;
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML do evento com o protocolo de autorização anexado
        /// </summary>
        public List<ProcEventoNFe> ProcEventoNFeResult
        {
            get
            {
                var retorno = new List<ProcEventoNFe>();

                for(var i = 0; i < EnvEvento.Evento.Count; i++)
                {
                    retorno.Add(new ProcEventoNFe
                    {
                        Versao = EnvEvento.Versao,
                        Evento = EnvEvento.Evento[i],
                        RetEvento = Result.RetEvento[i]
                    });
                };

                return retorno;
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetEnvEvento Result
        {
            get
            {
                if(!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEnvEvento>(RetornoWSXML);
                }

                return new RetEnvEvento
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
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public RecepcaoEvento(EnvEvento envEvento, Configuracao configuracao)
            : base(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento()
        {
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [System.Runtime.InteropServices.ComVisible(true)]
        public void Executar(EnvEvento envEvento, Configuracao configuracao)
        {
            PrepararServico(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
            Executar();
        }


        /// <summary>
        /// Retorna o <see cref="ProcEventoNFe"/> pelo índice ou nulo, se não existir
        /// </summary>
        /// <param name="index">Índice em que deve ser recuperado o evento e convertido para XML</param>
        /// <returns></returns>
        public string GetProcEventoNFeResultXMLByIndex(int index)
        {
            var list = ProcEventoNFeResult;

            if (list.Count == 0 ||
                index >= list.Count)
            {
                return "";
            }

            return list[index].GerarXML().InnerXml;
        }

#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            for(var i = 0; i < Result.RetEvento.Count; i++)
            {
                GravarXmlDistribuicao(pasta, ProcEventoNFeResult[i].NomeArquivoDistribuicao, ProcEventoNFeResult[i].GerarXML().OuterXml);
            }
        }

        /// <summary>
        /// Grava o XML de dsitribuição no stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o XML de distribuição</param>
        public void GravarXmlDistribuicao(Stream stream)
        {
            for(var i = 0; i < Result.RetEvento.Count; i++)
            {
                GravarXmlDistribuicao(stream, ProcEventoNFeResult[i].GerarXML().OuterXml);
            }
        }

        #endregion Public Methods
    }
}