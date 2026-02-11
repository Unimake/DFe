using System;
using System.Text;

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta de Eventos da NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarEvento")]
    [ComVisible(true)]
#endif
    public class ConsultarEvento : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarEvento() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarEvento(string conteudoXML, Configuracao configuracao) : this()
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(conteudoXML);

            Inicializar(xmlDoc, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarEvento(XmlDocument conteudoXML, Configuracao configuracao) : this()
            => Inicializar(conteudoXML, configuracao);

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Resultado da consulta de eventos (sucesso ou erro)
        /// </summary>
#if INTEROP
        [ComVisible(true)]
#endif
        public RetornoConsultaEventoNfse Result
        {
            get
            {
                if (string.IsNullOrWhiteSpace(RetornoWSString))
                    return null;

                try
                {
                    return XMLUtility.Deserializar<RetornoConsultaEventoNfse>(RetornoWSXML);
                }
                catch
                {
                    return null;
                }
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        [ComVisible(true)]
        public override void Executar(string conteudoXML, Configuracao configuracao)
            => base.Executar(conteudoXML, configuracao);

#endif

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
                DefinirConfiguracao();
            }

            base.Executar();

            ProcessarRetorno();
        }

        /// <summary>
        /// Processa o retorno do webservice.
        /// Descomprime: Base64 → Base64 → GZIP → XML
        /// Ou trata erros HTTP (404, etc.) em formato JSON
        /// </summary>
        private void ProcessarRetorno()
        {
            if (string.IsNullOrWhiteSpace(RetornoWSString) || RetornoWSXML == null)
                return;

            try
            {
                if (TratarErroJson())
                    return;

                var arquivoXmlNode = RetornoWSXML.GetElementsByTagName("arquivoXml");
                if (arquivoXmlNode?.Count == 0)
                    return;

                var base64Content = arquivoXmlNode[0]?.InnerText;
                if (string.IsNullOrEmpty(base64Content))
                    return;

                var xmlDescomprimido = DescomprimirBase64Duplo(base64Content);

                var arquivoXmlElement = (XmlElement)arquivoXmlNode[0];
                arquivoXmlElement.InnerXml = "";

                var xmlDocEvento = new XmlDocument();
                xmlDocEvento.LoadXml(xmlDescomprimido);

                var eventoImportado = RetornoWSXML.ImportNode(xmlDocEvento.DocumentElement, true);
                arquivoXmlElement.AppendChild(eventoImportado);

                RetornoWSString = RetornoWSXML.OuterXml;
            }
            catch
            {
                // Mantém retorno original
            }
        }

        /// <summary>
        /// Descomprime Base64 duplo + GZIP
        /// </summary>
        private string DescomprimirBase64Duplo(string base64Content)
        {
            var primeiroNivel = Convert.FromBase64String(base64Content);

            var asciiText = Encoding.ASCII.GetString(primeiroNivel);
            var bytes = Convert.FromBase64String(asciiText.Trim());

            return Compress.GZIPDecompress(Convert.ToBase64String(bytes));
        }

        /// <summary>
        /// Trata erros HTTP que retornam JSON
        /// </summary>
        private bool TratarErroJson()
        {
            try
            {
                var stringNode = RetornoWSXML.GetElementsByTagName("string");
                if (stringNode?.Count == 0)
                    return false;

                var conteudo = stringNode[0]?.InnerText;
                if (string.IsNullOrEmpty(conteudo) || !conteudo.Contains("{"))
                    return false;

                var indexJson = conteudo.IndexOf('{');
                var mensagemErro = conteudo.Substring(0, indexJson).Trim();
                var jsonParte = conteudo.Substring(indexJson);

                var xmlTemp = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(jsonParte, "temp");

                var erroNode = xmlTemp.CreateElement("erro");

                var codigoNode = xmlTemp.CreateElement("codigo");
                codigoNode.InnerText = ((int)HttpStatusCode).ToString();
                erroNode.AppendChild(codigoNode);

                var descNode = xmlTemp.CreateElement("descricao");
                descNode.InnerText = mensagemErro;
                erroNode.AppendChild(descNode);

                xmlTemp.DocumentElement.AppendChild(erroNode);

                RetornoWSString = xmlTemp.OuterXml;
                RetornoWSXML = xmlTemp;

                return true;
            }
            catch
            {
                return false;
            }
        }
    }
}

