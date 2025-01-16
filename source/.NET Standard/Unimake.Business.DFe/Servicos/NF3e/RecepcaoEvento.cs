#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NF3e
{
    /// <summary>
    /// Enviar o XML de eventos da NF3e para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NF3e.RecepcaoEvento")]
    [ComVisible(true)]
#endif
    public class RecepcaoEvento : ServicoBase, IInteropService<EventoNF3e>
    {
        private EventoNF3e _EventoNF3e;

        /// <summary>
        /// Objeto do XML do Evento
        /// </summary>
        public EventoNF3e EventoNF3e
        {
            get => _EventoNF3e ?? (_EventoNF3e = new EventoNF3e().LerXML<EventoNF3e>(ConteudoXML));
            protected set => _EventoNF3e = value;
        }

        private void ValidarXMLEvento(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, (Configuracoes.TipoDFe == TipoDFe.NFCe ? TipoDFe.NF3e : Configuracoes.TipoDFe).ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new EventoNF3e();
            xml = xml.LerXML<EventoNF3e>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.CodigoUF = (int)xml.InfEvento.COrgao;
                Configuracoes.TipoAmbiente = xml.InfEvento.TpAmb;
                Configuracoes.SchemaVersao = "1.00";
                Configuracoes.Servico = Servico.NF3eRecepcaoEvento;

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo(); // Efetuar a validação antes de validar schema para evitar alguns erros que não ficam claros para o desenvolvedor.

            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                int tpEvento;
                if (ConteudoXML.GetElementsByTagName("tpEvento").Count > 0)
                {
                    tpEvento = Convert.ToInt32(ConteudoXML.GetElementsByTagName("tpEvento")[0].InnerText);
                }
                else
                {
                    throw new Exception("Não foi possível localizar a tag obrigatória <tpEvento> no XML.");
                }

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
        }

        /// <summary>
        /// Propriedade contendo o xml do evento com o protocolo de autorização
        /// </summary>
        public ProcEventoNF3e ProcEventoNF3eResult
        {
            get
            {
                var retorno = new ProcEventoNF3e();
                retorno.Versao = EventoNF3e.Versao;
                retorno.EventoNF3e = EventoNF3e;
                retorno.RetEventoNF3e = Result;

                return retorno;
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetEventoNF3e Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEventoNF3e>(RetornoWSXML);
                }

                return new RetEventoNF3e
                {
                    InfEvento = new InfRetEvento
                    {
                        CStat = 0,
                        XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                    }
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="eventoNF3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEvento(EventoNF3e eventoNF3e, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(eventoNF3e?.GerarXML() ?? throw new ArgumentNullException(nameof(eventoNF3e)), configuracao);

            EventoNF3e = eventoNF3e.LerXML<EventoNF3e>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RecepcaoEvento(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            #region Limpar a assinatura do objeto para recriar e atualizar o ConteudoXML. Isso garante que a propriedade e o objeto tenham assinaturas iguais, evitando discrepâncias. Autor: Wandrey Data: 10/06/2024

            //Remover a assinatura para forçar criar novamente
            EventoNF3e = EventoNF3e.LerXML<EventoNF3e>(ConteudoXML);
            EventoNF3e.Signature = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = EventoNF3e.GerarXML();

            //Forçar assinar novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado
            EventoNF3e = EventoNF3e.LerXML<EventoNF3e>(ConteudoXML);

            #endregion
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcaoEvento() : base() { }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML , valida e envia para o web-service
        /// </summary>
        /// <param name="EventoNF3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(EventoNF3e EventoNF3e, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(EventoNF3e?.GerarXML() ?? throw new ArgumentNullException(nameof(EventoNF3e)), configuracao);
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
        /// <param name="EventoNF3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(EventoNF3e EventoNF3e, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(EventoNF3e?.GerarXML() ?? throw new ArgumentNullException(nameof(EventoNF3e)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Retorna o <see cref="EventoNF3e"/> pelo índice ou nulo, se não existir
        /// </summary>
        /// <returns></returns>
        public string GetProcEventoNF3eResultXML() => Result.GerarXML().InnerXml;
#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                GravarXmlDistribuicao(pasta, ProcEventoNF3eResult.NomeArquivoDistribuicao, ProcEventoNF3eResult.GerarXML().OuterXml);
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
                GravarXmlDistribuicao(stream, ProcEventoNF3eResult.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}
