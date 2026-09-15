#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFeABI;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFeABI
{
    /// <summary>
    /// Envia uma NFeABI para autorização síncrona.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.NFeABI.NFeABI>
    {
        private Xml.NFeABI.NFeABI nfeABI;
        private readonly Dictionary<string, NFeABIProc> nfeABIProcs = new Dictionary<string, NFeABIProc>();

        private void Preparar(Xml.NFeABI.NFeABI documento, Configuracao configuracao)
        {
            if (documento is null)
            {
                throw new ArgumentNullException(nameof(documento));
            }

            Preparar(documento.GerarXML(), configuracao);
        }

        private void Preparar(XmlDocument documento, Configuracao configuracao)
        {
            if (documento is null)
            {
                throw new ArgumentNullException(nameof(documento));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            nfeABI = null;
            nfeABIProcs.Clear();
            RetornoWSString = null;
            RetornoWSXML = null;
            Warnings = new List<ValidatorDFeException>();

            if (ConteudoXMLOriginal != null)
            {
                ConteudoXMLOriginal.LoadXml(documento.OuterXml);
            }

            Inicializar(documento, configuracao);
            NFeABI = new Xml.NFeABI.NFeABI().LerXML<Xml.NFeABI.NFeABI>(ConteudoXML);
        }

        /// <summary>
        /// Objeto XML da NFeABI enviada.
        /// </summary>
        public Xml.NFeABI.NFeABI NFeABI
        {
            get => nfeABI ?? (nfeABI = new Xml.NFeABI.NFeABI().LerXML<Xml.NFeABI.NFeABI>(ConteudoXML));
            protected set => nfeABI = value;
        }

        /// <summary>
        /// Define as configurações da autorização síncrona.
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFeABIAutorizacaoSinc;

                var tagNFeABI = ConteudoXML.GetElementsByTagName("NFeABI");
                if (tagNFeABI.Count == 0)
                {
                    throw new Exception("A tag obrigatória <NFeABI> não foi localizada no XML.");
                }

                var tagInfNFeABI = ((XmlElement)tagNFeABI[0]).GetElementsByTagName("infNFeABI");
                if (tagInfNFeABI.Count == 0)
                {
                    throw new Exception("A tag obrigatória <infNFeABI>, do grupo <NFeABI>, não foi localizada no XML.");
                }

                var infNFeABI = (XmlElement)tagInfNFeABI[0];
                Configuracoes.SchemaVersao = infNFeABI.GetAttribute("versao");
                if (string.IsNullOrWhiteSpace(Configuracoes.SchemaVersao))
                {
                    throw new Exception("O atributo obrigatório \"versao\" da tag <infNFeABI> não foi localizado no XML.");
                }

                var ide = infNFeABI.GetElementsByTagName("ide");
                if (ide.Count == 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo <NFeABI><infNFeABI>, não foi localizada no XML.");
                }

                var tagIde = (XmlElement)ide[0];
                Configuracoes.CodigoUF = LerInteiroObrigatorio(tagIde, "cUF");
                Configuracoes.Modelo = (ModeloDFe)LerInteiroObrigatorio(tagIde, "mod");
                Configuracoes.TipoEmissao = (TipoEmissao)LerInteiroObrigatorio(tagIde, "tpEmis");
                Configuracoes.TipoAmbiente = (TipoAmbiente)LerInteiroObrigatorio(tagIde, "tpAmb");
                base.DefinirConfiguracao();
            }
        }

        private static int LerInteiroObrigatorio(XmlElement parent, string nomeTag)
        {
            var tags = parent.GetElementsByTagName(nomeTag);
            if (tags.Count == 0)
            {
                throw new Exception("A tag obrigatória <" + nomeTag + ">, do grupo <NFeABI><infNFeABI><ide>, não foi localizada no XML.");
            }

            return Convert.ToInt32(tags[0].InnerText);
        }

        /// <summary>
        /// NFeABI processada com o protocolo retornado em uma autorização aceita.
        /// </summary>
        public Dictionary<string, NFeABIProc> NFeABIProcResults
        {
            get
            {
                if (Result.ProtNFeABI != null &&
                    Result.ProtNFeABI.InfProt != null &&
                    StatusProtocoloAutorizacao.NFeABI(Result.ProtNFeABI.InfProt.CStat) &&
                    string.Equals(Result.ProtNFeABI.InfProt.ChNFeABI, NFeABI.InfNFeABI.Chave, StringComparison.Ordinal))
                {
                    var chave = NFeABI.InfNFeABI.Chave;
                    if (!nfeABIProcs.ContainsKey(chave))
                    {
                        nfeABIProcs.Add(chave, new NFeABIProc
                        {
                            Versao = NFeABI.InfNFeABI.Versao,
                            NFeABI = NFeABI,
                            ProtNFeABI = Result.ProtNFeABI
                        });
                    }
                    else
                    {
                        nfeABIProcs[chave].ProtNFeABI = Result.ProtNFeABI;
                    }
                }

                return nfeABIProcs;
            }
        }

#if INTEROP
        /// <summary>
        /// Obtém o XML processado pela chave da NFeABI.
        /// </summary>
        /// <param name="chaveDFe">Chave da NFeABI.</param>
        /// <returns>XML processado ou string vazia.</returns>
        public string GetNFeABIProcResults(string chaveDFe)
        {
            return NFeABIProcResults.Count > 0 ? NFeABIProcResults[chaveDFe].GerarXML().OuterXml : "";
        }
#endif

        /// <summary>
        /// Retorno tipado da autorização.
        /// </summary>
        public RetNFeABI Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetNFeABI>(CriarRetornoCompativel(RetornoWSString));
                }

                return new RetNFeABI
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        public AutorizacaoSinc() : base() => nfeABIProcs.Clear();

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="nfeABI">NFeABI tipada a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public AutorizacaoSinc(Xml.NFeABI.NFeABI nfeABI, Configuracao configuracao) : this()
        {
            Preparar(nfeABI, configuracao);
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="conteudoXML">XML a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public AutorizacaoSinc(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);
            Preparar(doc, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa a autorização síncrona.
        /// </summary>
        /// <param name="nfeABI">NFeABI tipada a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public void Executar(Xml.NFeABI.NFeABI nfeABI, Configuracao configuracao)
        {
            try
            {
                Preparar(nfeABI, configuracao);
                Executar();
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Define a NFeABI e a configuração para execução posterior.
        /// </summary>
        /// <param name="nfeABI">NFeABI tipada a enviar.</param>
        /// <param name="configuracao">Configuração do serviço.</param>
        public void SetXMLConfiguracao(Xml.NFeABI.NFeABI nfeABI, Configuracao configuracao)
        {
            try
            {
                Preparar(nfeABI, configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
#endif

        /// <summary>
        /// Grava os XMLs processados em uma pasta.
        /// </summary>
        /// <param name="pasta">Pasta de destino.</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            foreach (var item in NFeABIProcResults)
            {
                GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
            }
        }

        /// <summary>
        /// Grava o XML processado em um stream.
        /// </summary>
        /// <param name="stream">Stream de destino.</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            foreach (var item in NFeABIProcResults)
            {
                GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
            }
        }
    }
}
