#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPe;
using Unimake.Exceptions;
using BPeXml = Unimake.Business.DFe.Xml.BPe.BPe;

namespace Unimake.Business.DFe.Servicos.BPe
{
    /// <summary>
    /// Enviar o XML do BPe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe")]
    [ComVisible(true)]
#endif
    public class AutorizacaoBPe : ServicoBase, IInteropService<BPeXml>
    {
        private BPeXml _BPe;
        private readonly Dictionary<string, BPeProc> BPeProcs = new Dictionary<string, BPeProc>();

        /// <summary>
        /// Objeto XML do BPe
        /// </summary>
        public BPeXml BPe
        {
            get => _BPe ?? (_BPe = new BPeXml().LerXML<BPeXml>(ConteudoXML));
            protected set => _BPe = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            DefinirConfiguracaoAutorizacao("BPe", Servico.BPeAutorizacao);
        }

        /// <summary>
        /// Efetuar ajustes no XML do BPe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            PosicionarAssinaturaDepoisDoSuplemento();
            base.AjustarXMLAposAssinado();
        }

        private void PosicionarAssinaturaDepoisDoSuplemento()
        {
            var raiz = ConteudoXML.DocumentElement;
            if (raiz == null)
            {
                return;
            }

            var infBPeSupl = raiz.GetElementsByTagName("infBPeSupl").Count > 0 ? raiz.GetElementsByTagName("infBPeSupl")[0] : null;
            var signature = raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#").Count > 0 ? raiz.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0] : null;

            if (infBPeSupl != null && signature != null && infBPeSupl.NextSibling != signature)
            {
                raiz.RemoveChild(signature);
                raiz.InsertAfter(signature, infBPeSupl);
            }
        }

        private void DefinirConfiguracaoAutorizacao(string tagDocumento, Servico servico)
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.TipoDFe = TipoDFe.BPe;
                Configuracoes.Servico = servico;

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

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação dos BPe enviados
        /// </summary>
        public List<RetConsSitBPe> RetConsSitBPe = new List<RetConsSitBPe>();

        /// <summary>
        /// Propriedade contendo o XML do BPe com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, BPeProc> BPeProcResults
        {
            get
            {
                ProtBPe protBPe = Result.ProtBPe;

                if (protBPe == null)
                {
                    if (RetConsSitBPe.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitBPe, sem a definição dela não é possível obter o conteúdo da BPeProcResults.");
                    }

                    foreach (var item in RetConsSitBPe)
                    {
                        if (item?.ProtBPe == null)
                        {
                            continue;
                        }

                        foreach (var protocolo in item.ProtBPe)
                        {
                            if (protocolo?.InfProt != null && protocolo.InfProt.ChBPe == BPe.InfBPe.Chave && protocolo.InfProt.CStat == 100)
                            {
                                protBPe = protocolo;
                                break;
                            }
                        }
                    }
                }

                if (BPeProcs.ContainsKey(BPe.InfBPe.Chave))
                {
                    BPeProcs[BPe.InfBPe.Chave].ProtBPe = protBPe;
                }
                else
                {
                    BPeProcs.Add(BPe.InfBPe.Chave, new BPeProc
                    {
                        Versao = BPe.InfBPe.Versao,
                        BPe = BPe,
                        ProtBPe = protBPe
                    });
                }

                return BPeProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do BPe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do BPe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do BPe</returns>
        public string GetBPeProcResults(string chaveDFe)
        {
            var retornar = "";
            if (BPeProcResults.Count > 0)
            {
                retornar = BPeProcResults[chaveDFe].GerarXML().OuterXml;
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
        public AutorizacaoBPe() : base() => BPeProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="bpe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPe(BPeXml bpe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(bpe?.GerarXML() ?? throw new ArgumentNullException(nameof(bpe)), configuracao);

            BPe = BPe.LerXML<BPeXml>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoBPe(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            BPe = BPe.LerXML<BPeXml>(ConteudoXML);
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

#if INTEROP

        /// <summary>
        /// Executa o serviço
        /// </summary>
        /// <param name="bpe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(BPeXml bpe, Configuracao configuracao)
        {
            try
            {
                Inicializar(bpe?.GerarXML() ?? throw new ArgumentNullException(nameof(bpe)), configuracao);
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
        /// <param name="bpe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(BPeXml bpe, Configuracao configuracao)
        {
            try
            {
                Inicializar(bpe?.GerarXML() ?? throw new ArgumentNullException(nameof(bpe)), configuracao);
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
                foreach (var item in BPeProcResults)
                {
                    if (item.Value.ProtBPe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave para a elaboração do arquivo de distribuição: " + item.Key);
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
                foreach (var item in BPeProcResults)
                {
                    if (item.Value.ProtBPe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave para a elaboração do arquivo de distribuição: " + item.Key);
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
