#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de consulta documentos fiscais eletrônicos destinados para o web-service (NFe)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe")]
    [ComVisible(true)]
#endif
    public class DistribuicaoDFe : ServicoBase, IInteropService<DistDFeInt>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new DistDFeInt();
            xml = xml.LerXML<DistDFeInt>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeDistribuicaoDFe;
                Configuracoes.CodigoUF = (int)xml.COrgao;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetDistDFeInt Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetDistDFeInt>(RetornoWSXML);
                }

                return new RetDistDFeInt
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
        /// <param name="distDFeInt">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public DistribuicaoDFe(DistDFeInt distDFeInt, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(distDFeInt?.GerarXML() ?? throw new ArgumentNullException(nameof(distDFeInt)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public DistribuicaoDFe() : base() { }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();

            //Adicionar os XMLs retornados em suas respectivas listas para que possam ser resgatados em formato de objeto
            if (Result != null && Result.LoteDistDFeInt != null)
            {
                ResNFes = new List<ResNFe>();
                ResEventos = new List<ResEvento>();

                foreach (var item in Result.LoteDistDFeInt.DocZip)
                {
                    var conteudoXML = Compress.GZIPDecompress(Convert.ToBase64String(item.Value));

                    var docXML = new XmlDocument();
                    docXML.Load(Converter.StringToStreamUTF8(conteudoXML));

                    if (item.Schema.StartsWith("resEvento"))
                    {
                        ResEventos.Add(XMLUtility.Deserializar<ResEvento>(conteudoXML));
                    }
                    else if (item.Schema.StartsWith("resNFe"))
                    {
                        ResNFes.Add(XMLUtility.Deserializar<ResNFe>(conteudoXML));
                    }
                }
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="distDFeInt">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(DistDFeInt distDFeInt, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(distDFeInt?.GerarXML() ?? throw new ArgumentNullException(nameof(distDFeInt)), configuracao);
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
                throw new Exception("Este método não está disponível para consulta de documentos fiscais eletrônicos destinados. Utilize o GravarXMLDocZIP(string folder, bool saveXMLResumo).");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Gravar os XML contidos no DocZIP da consulta em uma pasta no HD
        /// </summary>
        /// <param name="folder">Nome da pasta onde é para salvar os XML</param>
        /// <param name="saveXMLResumo">Salvar os arquivos de resumo da NFe e Eventos da NFe?</param>
        public void GravarXMLDocZIP(string folder, bool saveXMLResumo)
        {
            try
            {
                foreach (var item in Result.LoteDistDFeInt.DocZip)
                {
                    var save = true;
                    var conteudoXML = Compress.GZIPDecompress(Convert.ToBase64String(item.Value));
                    var nomeArquivo = string.Empty;

                    var docXML = new XmlDocument();
                    docXML.Load(Converter.StringToStreamUTF8(conteudoXML));

                    if (item.Schema.StartsWith("resEvento"))
                    {
                        nomeArquivo = item.NSU + "-resEvento.xml";
                        save = saveXMLResumo;
                    }
                    else if (item.Schema.StartsWith("procEventoNFe"))
                    {
                        var chNFe = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("evento")[0]).GetElementsByTagName("infEvento")[0]), "chNFe");
                        var tpEvento = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("evento")[0]).GetElementsByTagName("infEvento")[0]), "tpEvento");
                        var nSeqEvento = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("evento")[0]).GetElementsByTagName("infEvento")[0]), "nSeqEvento");
                        nomeArquivo = chNFe + "_" + tpEvento + "_" + nSeqEvento.PadLeft(2, '0') + "-procEventoNFe.xml";
                    }
                    else if (item.Schema.StartsWith("procNFe"))
                    {
                        var chave = ((XmlElement)docXML.GetElementsByTagName("infNFe")[0]).GetAttribute("Id").Substring(3, 44);
                        nomeArquivo = chave + "-procNFe.xml";
                    }
                    else if (item.Schema.StartsWith("resNFe"))
                    {
                        nomeArquivo = item.NSU + "-resNFe.xml";
                        save = saveXMLResumo;
                    }

                    if (save && !string.IsNullOrEmpty(nomeArquivo))
                    {
                        base.GravarXmlDistribuicao(folder, nomeArquivo, conteudoXML);
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Resgatar a lista dos resumos de eventos retornados pelo serviço de distribuição de DFe
        /// </summary>
        public List<ResEvento> ResEventos { get; private set; }

        /// <summary>
        /// Resgatar a lista dos resumos das notas fiscais retornadas pelo serviço de distribuição de DFe
        /// </summary>
        public List<ResNFe> ResNFes { get; private set; }

#if INTEROP

        /// <summary>
        /// Retorna o quantidade de elementos na lista com os Resumos de Eventos
        /// </summary>
        public int GetResEventosCount() => ResEventos.Count;

        /// <summary>
        /// Retorna o resumo do evento (ResEvento) do elemento informado por parâmetro
        /// </summary>
        /// <param name="elemento">Elemento a ser retornado</param>
        /// <returns>Resumo do evento (ResEvento)</returns>
        public ResEvento GetResEvento(int elemento) => ResEventos[elemento];

        /// <summary>
        /// Retorna o quantidade de elementos na lista com os Resumos de NF-e
        /// </summary>
        public int GetResNFeCount() => ResNFes.Count;

        /// <summary>
        /// Retorna o resumo de NFe (ResNFe) do elemento informado por parâmetro
        /// </summary>
        /// <param name="elemento">Elemento a ser retornado</param>
        /// <returns>Resumo de NF-e (ResNFe)</returns>
        public ResNFe GetResNFe(int elemento) => ResNFes[elemento];

#endif

        #endregion Public Methods
    }
}