#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using Unimake.Business.DFe.Xml.BPeTA;
using Unimake.Exceptions;
using BPeTAXml = Unimake.Business.DFe.Xml.BPeTA.BPeTA;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Enviar o XML do BPe TA para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA")]
    [ComVisible(true)]
#endif
    public class AutorizacaoBPeTA : ServicoBase, IInteropService<BPeTAXml>
    {
        private BPeTAXml _BPeTA;
        private readonly Dictionary<string, BPeTAProc> BPeTAProcs = new Dictionary<string, BPeTAProc>();

        /// <summary>
        /// Objeto XML do BPe TA
        /// </summary>
        public BPeTAXml BPeTA
        {
            get => _BPeTA ?? (_BPeTA = new BPeTAXml().LerXML<BPeTAXml>(ConteudoXML));
            protected set => _BPeTA = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.BPe;
                Configuracoes.Servico = Servico.BPeTAAutorizacao;
                DefinirConfiguracaoPorInfBPe("BPeTA");

                base.DefinirConfiguracao();
            }
        }

        private void DefinirConfiguracaoPorInfBPe(string tagDocumento)
        {
            if (ConteudoXML.GetElementsByTagName(tagDocumento).Count <= 0)
            {
                throw new Exception("A tag obrigatória <" + tagDocumento + "> não foi localizada no XML.");
            }

            var tagInfBPe = ConteudoXML.GetElementsByTagName("infBPe").Count > 0 ? (XmlElement)ConteudoXML.GetElementsByTagName("infBPe")[0] : null;

            if (tagInfBPe == null)
            {
                throw new Exception("A tag obrigatória <infBPe>, do grupo de tag <" + tagDocumento + ">, não foi localizada no XML.");
            }

            if (tagInfBPe.GetAttribute("versao").Length > 0)
            {
                Configuracoes.SchemaVersao = tagInfBPe.GetAttribute("versao");
            }
            else
            {
                throw new Exception("O atributo obrigatório \"versao\" da tag <infBPe>, do grupo de tag <" + tagDocumento + ">, não foi localizado no XML.");
            }

            if (tagInfBPe.GetElementsByTagName("ide").Count <= 0)
            {
                throw new Exception("A tag obrigatória <ide>, do grupo de tag <" + tagDocumento + "><infBPe>, não foi localizada no XML.");
            }

            var tagIde = (XmlElement)tagInfBPe.GetElementsByTagName("ide")[0];

            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
            {
                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
            {
                throw new Exception("A tag obrigatória <mod>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
            {
                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }
            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
            {
                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <" + tagDocumento + "><infBPe><ide>, não foi localizada no XML.");
            }

            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
        }

        /// <summary>
        /// Propriedade contendo o XML do BPe TA com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, BPeTAProc> BPeTAProcResults
        {
            get
            {
                if (BPeTAProcs.ContainsKey(BPeTA.InfBPe.Chave))
                {
                    BPeTAProcs[BPeTA.InfBPe.Chave].ProtBPe = Result.ProtBPe;
                }
                else
                {
                    BPeTAProcs.Add(BPeTA.InfBPe.Chave, new BPeTAProc
                    {
                        Versao = BPeTA.InfBPe.Versao,
                        BPeTA = BPeTA,
                        ProtBPe = Result.ProtBPe
                    });
                }

                return BPeTAProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do BPe TA no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do BPe TA que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do BPe TA</returns>
        public string GetBPeTAProcResults(string chaveDFe)
        {
            var retornar = "";
            if (BPeTAProcResults.Count > 0)
            {
                retornar = BPeTAProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetBPe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetBPe>(RetornoWSString);
                }

                return new RetBPe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoBPeTA() : base() => BPeTAProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="bpeTA">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTA(BPeTAXml bpeTA, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(bpeTA?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTA)), configuracao);

            BPeTA = BPeTA.LerXML<BPeTAXml>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTA(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            BPeTA = BPeTA.LerXML<BPeTAXml>(ConteudoXML);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço
        /// </summary>
        /// <param name="bpeTA">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(BPeTAXml bpeTA, Configuracao configuracao)
        {
            try
            {
                Inicializar(bpeTA?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTA)), configuracao);
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
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
#if INTEROP
        [ComVisible(true)]
#endif
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in BPeTAProcResults)
                {
                    if (item.Value.ProtBPe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno o protocolo da chave para a elaboração do arquivo de distribuição: " + item.Key);
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
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                foreach (var item in BPeTAProcResults)
                {
                    if (item.Value.ProtBPe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno o protocolo da chave para a elaboração do arquivo de distribuição: " + item.Key);
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}
