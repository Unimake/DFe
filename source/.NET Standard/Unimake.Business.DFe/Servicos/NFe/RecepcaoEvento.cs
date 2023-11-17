#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de eventos da NFe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : ServicoBase, IInteropService<EnvEvento>
    {
        #region Private Properties

        private EnvEvento EnvEvento => new EnvEvento().LerXML<EnvEvento>(ConteudoXML);

        #endregion Private Properties

        #region Private Methods

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, (Configuracoes.TipoDFe == TipoDFe.NFCe ? TipoDFe.NFe : Configuracoes.TipoDFe).ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
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

            if (!Configuracoes.Definida)
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

            if (Configuracoes.SchemasEspecificos.Count > 0)
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
            for (var i = 0; i < listEvento.Count; i++)
            {
                var elementEvento = (XmlElement)listEvento[i];

                if (elementEvento.GetElementsByTagName("infEvento")[0] != null)
                {
                    var elementInfEvento = (XmlElement)elementEvento.GetElementsByTagName("infEvento")[0];
                    if (elementInfEvento.GetElementsByTagName("tpEvento")[0] != null)
                    {
                        var xmlEspecifico = new XmlDocument();
                        xmlEspecifico.LoadXml(elementInfEvento.GetElementsByTagName("detEvento")[0].OuterXml);

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

            switch (tpEvento)
            {
                case TipoEventoNFe.ManifestacaoCienciaOperacao:
                    descEvento = "Ciencia da Operacao";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoConfirmacaoOperacao:
                    descEvento = "Confirmacao da Operacao";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoDesconhecimentoOperacao:
                    descEvento = "Desconhecimento da Operacao";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ManifestacaoOperacaoNaoRealizada:
                    descEvento = "Operacao nao Realizada";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.ComprovanteEntregaNFe:
                    descEvento = "Comprovante de Entrega da NF-e";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.CancelamentoComprovanteEntregaNFe:
                    descEvento = "Cancelamento Comprovante de Entrega da NF-e";
                    if (!xml.Evento[0].InfEvento.DetEvento.DescEvento.Equals(descEvento))
                    {
                        throw new Exception(msgException.Replace("$", descEvento));
                    }
                    goto case TipoEventoNFe.EPEC;

                case TipoEventoNFe.EPEC:
                    if (xml.Evento[0].InfEvento.ChNFe.Substring(20, 2) == "65" && ((DetEventoEPEC)xml.Evento[0].InfEvento.DetEvento).COrgaoAutor == UFBrasil.SP)
                    {
                        if (xml.Evento[0].InfEvento.COrgao != UFBrasil.SP)
                        {
                            throw new Exception("Conteúdo da tag <cOrgao> inválido. Para o evento de EPEC de NFCe o conteúdo da tag <cOrgao> deve igual a 35.");
                        }
                    }
                    else
                    {
                        if (xml.Evento[0].InfEvento.COrgao != UFBrasil.AN)
                        {
                            throw new Exception("Conteúdo da tag <cOrgao> inválido. Para o evento de EPEC de NFe o conteúdo da tag <cOrgao> deve igual a 91.");
                        }
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

                for (var i = 0; i < EnvEvento.Evento.Count; i++)
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
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetEnvEvento Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
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
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEvento(EnvEvento envEvento, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento() : base() { }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(EnvEvento envEvento, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
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
        /// <param name="envEvento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(EnvEvento envEvento, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(envEvento?.GerarXML() ?? throw new ArgumentNullException(nameof(envEvento)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
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


        /// <summary>
        /// Retorna o <see cref="ProcEventoNFe"/> pelo índice ou nulo, se não existir
        /// </summary>
        /// <param name="index">Índice em que deve ser recuperado o evento e convertido para XML</param>
        /// <returns></returns>
        public ProcEventoNFe GetProcEventoNFeResult(int index)
        {
            var list = ProcEventoNFeResult;

            if (list.Count == 0 ||
                index >= list.Count)
            {
                return null;
            }

            return list[index];
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
                for (var i = 0; i < Result.RetEvento.Count; i++)
                {
                    GravarXmlDistribuicao(pasta, ProcEventoNFeResult[i].NomeArquivoDistribuicao, ProcEventoNFeResult[i].GerarXML().OuterXml);
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
        public void GravarXmlDistribuicao(Stream stream)
        {
            try
            {
                for (var i = 0; i < Result.RetEvento.Count; i++)
                {
                    GravarXmlDistribuicao(stream, ProcEventoNFeResult[i].GerarXML().OuterXml);
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