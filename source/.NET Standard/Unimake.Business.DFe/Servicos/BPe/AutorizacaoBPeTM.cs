#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using Unimake.Business.DFe.Xml.BPeTM;
using Unimake.Exceptions;
using BPeTMXml = Unimake.Business.DFe.Xml.BPeTM.BPeTM;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Enviar o XML do BPe TM para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM")]
    [ComVisible(true)]
#endif
    public class AutorizacaoBPeTM : ServicoBase, IInteropService<BPeTMXml>
    {
        private BPeTMXml _BPeTM;
        private readonly Dictionary<string, BPeTMProc> BPeTMProcs = new Dictionary<string, BPeTMProc>();

        /// <summary>
        /// Objeto XML do BPe TM
        /// </summary>
        public BPeTMXml BPeTM
        {
            get => _BPeTM ?? (_BPeTM = new BPeTMXml().LerXML<BPeTMXml>(ConteudoXML));
            protected set => _BPeTM = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.BPe;
                Configuracoes.Servico = Servico.BPeTMAutorizacao;
                DefinirConfiguracaoPorInfBPe("BPeTM");

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
        /// Propriedade contendo o XML do BPe TM com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, BPeTMProc> BPeTMProcResults
        {
            get
            {
                if (BPeTMProcs.ContainsKey(BPeTM.InfBPe.Chave))
                {
                    BPeTMProcs[BPeTM.InfBPe.Chave].ProtBPe = Result.ProtBPe;
                }
                else
                {
                    BPeTMProcs.Add(BPeTM.InfBPe.Chave, new BPeTMProc
                    {
                        Versao = BPeTM.InfBPe.Versao,
                        BPeTM = BPeTM,
                        ProtBPe = Result.ProtBPe
                    });
                }

                return BPeTMProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do BPe TM no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do BPe TM que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do BPe TM</returns>
        public string GetBPeTMProcResults(string chaveDFe)
        {
            var retornar = "";
            if (BPeTMProcResults.Count > 0)
            {
                retornar = BPeTMProcResults[chaveDFe].GerarXML().OuterXml;
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
        public AutorizacaoBPeTM() : base() => BPeTMProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="bpeTM">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTM(BPeTMXml bpeTM, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(bpeTM?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTM)), configuracao);

            BPeTM = BPeTM.LerXML<BPeTMXml>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPeTM(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            BPeTM = BPeTM.LerXML<BPeTMXml>(ConteudoXML);
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço
        /// </summary>
        /// <param name="bpeTM">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(BPeTMXml bpeTM, Configuracao configuracao)
        {
            try
            {
                Inicializar(bpeTM?.GerarXML() ?? throw new ArgumentNullException(nameof(bpeTM)), configuracao);
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
                foreach (var item in BPeTMProcResults)
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
    }
}
