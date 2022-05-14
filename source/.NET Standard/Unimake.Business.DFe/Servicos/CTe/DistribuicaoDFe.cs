#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTe
{
    /// <summary>
    /// Envio do XML de consulta dos CTe´s destinados para o WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTe.DistribuicaoDFe")]
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
                Configuracoes.Servico = Servico.CTeDistribuicaoDFe;
                Configuracoes.CodigoUF = (int)xml.COrgao;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
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
        public DistribuicaoDFe(DistDFeInt distDFeInt, Configuracao configuracao) : this() => Inicializar(distDFeInt?.GerarXML() ?? throw new NotImplementedException(nameof(distDFeInt)), configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public DistribuicaoDFe() : base() { }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="distDFeInt">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
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
                throw new Exception("Não existe XML de distribuição para consulta de documentos fiscais eletrônicos destinados.");
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
        public void GravarXMLDocZIP(string folder)
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

                    if (item.Schema.StartsWith("procEventoCTe"))
                    {
                        var chCTe = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("eventoCTe")[0]).GetElementsByTagName("infEvento")[0]), "chCTe");
                        var tpEvento = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("eventoCTe")[0]).GetElementsByTagName("infEvento")[0]), "tpEvento");
                        var nSeqEvento = XMLUtility.TagRead(((XmlElement)((XmlElement)docXML.GetElementsByTagName("eventoCTe")[0]).GetElementsByTagName("infEvento")[0]), "nSeqEvento");
                        nomeArquivo = chCTe + "_" + tpEvento + "_" + nSeqEvento.PadLeft(2, '0') + "-procEventoCTe.xml";
                    }
                    else if (item.Schema.StartsWith("procCTe"))
                    {
                        var chave = ((XmlElement)docXML.GetElementsByTagName("infCte")[0]).GetAttribute("Id").Substring(3, 44);
                        nomeArquivo = chave + "-procCTe.xml";
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

        #endregion Public Methods
    }
}