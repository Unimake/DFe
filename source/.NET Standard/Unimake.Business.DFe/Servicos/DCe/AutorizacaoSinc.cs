#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.DCe
{
    /// <summary>
    /// Enviar o XML da DCe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.DCe.DCe>
    {
        private Xml.DCe.DCe _DCe;
        private readonly Dictionary<string, DCeProc> DCeProcs = new Dictionary<string, DCeProc>();

        /// <summary>
        /// Objeto XML da DCe
        /// </summary>
        public Xml.DCe.DCe DCe
        {
            get => _DCe ?? (_DCe = new Xml.DCe.DCe().LerXML<Xml.DCe.DCe>(ConteudoXML));
            protected set => _DCe = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.DCeAutorizacaoSinc;
                Configuracoes.TipoDFe = TipoDFe.DCe;

                if (ConteudoXML.GetElementsByTagName("DCe").Count > 0)
                {
                    var tagDCe = (XmlElement)ConteudoXML.GetElementsByTagName("DCe")[0];
                    if (tagDCe.GetElementsByTagName("infDCe").Count > 0)
                    {
                        var tagInfDCe = (XmlElement)ConteudoXML.GetElementsByTagName("infDCe")[0];

                        if (tagInfDCe.GetAttribute("versao").Length > 0)
                        {
                            Configuracoes.SchemaVersao = tagInfDCe.GetAttribute("versao");
                        }
                        else
                        {
                            throw new Exception("O atributo obrigatório \"versao\" da tag <infDCe>, do grupo de tag <DCe>, não foi localizado no XML.");
                        }

                        if (tagInfDCe.GetElementsByTagName("ide").Count > 0)
                        {
                            var tagIde = (XmlElement)tagInfDCe.GetElementsByTagName("ide")[0];

                            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <DCe><infDCe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <mod>, do grupo de tag <DCe><infDCe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <DCe><infDCe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <DCe><infDCe><ide>, não foi localizada no XML.");
                            }

                            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
                            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
                            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
                            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <ide>, do grupo de tag <DCe><infDCe>, não foi localizada no XML.");
                        }
                    }
                    else
                    {
                        throw new Exception("A tag obrigatória <infDCe>, do grupo de tag <DCe>, não foi localizada no XML.");
                    }
                }
                else
                {
                    throw new Exception("A tag obrigatória <DCe> não foi localizada no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Efetuar um Ajustse no XML da DCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            QrCodeXmlHelper.MontarQrCodeDCe(ConteudoXML, Configuracoes);
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação do DCes enviadas
        /// </summary>
        public List<RetConsSitDCe> RetConsSitDCe = new List<RetConsSitDCe>();

        /// <summary>
        /// Propriedade contendo o XML da DCe com o protocolo de autorização anexado - Funciona para envio Assíncrono ou Síncrono
        /// </summary>
        public Dictionary<string, DCeProc> DCeProcResults
        {
            get
            {
                if (Result.ProtDCe != null)
                {
                    if (DCeProcs.ContainsKey(DCe.InfDCe.Chave))
                    {
                        DCeProcs[DCe.InfDCe.Chave].ProtDCe = Result.ProtDCe;
                    }
                    else
                    {
                        DCeProcs.Add(DCe.InfDCe.Chave, new DCeProc
                        {
                            Versao = DCe.InfDCe.Versao,
                            DCe = DCe,
                            ProtDCe = Result.ProtDCe
                        });
                    }
                }
                else
                {
                    if (RetConsSitDCe.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitDCe, sem a definição dela não é possível obter o conteúdo da DCeProcResults.");
                    }

                    ProtDCe protDCe = null;

                    foreach (var item in RetConsSitDCe)
                    {
                        if (item != null && item.ProtDCe != null)
                        {
                            if (item.ProtDCe.InfProt.ChDCe == DCe.InfDCe.Chave)
                            {
                                switch (item.ProtDCe.InfProt.CStat)
                                {
                                    case 100:
                                        protDCe = item.ProtDCe;
                                        break;
                                }
                            }
                        }
                    }

                    if (DCeProcs.ContainsKey(DCe.InfDCe.Chave))
                    {
                        DCeProcs[DCe.InfDCe.Chave].ProtDCe = protDCe;
                    }
                    else
                    {
                        DCeProcs.Add(DCe.InfDCe.Chave, new DCeProc
                        {
                            Versao = DCe.InfDCe.Versao,
                            DCe = DCe,
                            ProtDCe = protDCe
                        });
                    }
                }

                return DCeProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do DCe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do DCe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do DCe</returns>
        public string GetDCeProcResults(string chaveDFe)
        {
            var retornar = "";
            if (DCeProcResults.Count > 0)
            {
                retornar = DCeProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetDCe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetDCe>(RetornoWSString);
                }

                return new RetDCe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSinc() : base() => DCeProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="DCe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoSinc(Xml.DCe.DCe DCe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(DCe?.GerarXML() ?? throw new ArgumentNullException(nameof(DCe)), configuracao);

            DCe = DCe.LerXML<Xml.DCe.DCe>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        /// <exception cref="ArgumentNullException"></exception>
        public AutorizacaoSinc(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            DCe = DCe.LerXML<Xml.DCe.DCe>(ConteudoXML);
            DCe.Signature = null;
            DCe.InfDCeSupl = null;

            ConteudoXML = DCe.GerarXML();
            _ = ConteudoXMLAssinado;
            DCe = DCe.LerXML<Xml.DCe.DCe>(ConteudoXML);
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="DCe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(Xml.DCe.DCe DCe, Configuracao configuracao)
        {
            try
            {
                Inicializar(DCe?.GerarXML() ?? throw new ArgumentNullException(nameof(DCe)), configuracao);

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
        /// <param name="DCe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.DCe.DCe DCe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(DCe?.GerarXML() ?? throw new ArgumentNullException(nameof(DCe)), configuracao);
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
                foreach (var item in DCeProcResults)
                {
                    if (item.Value.ProtDCe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + item.Key);
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
                foreach (var item in DCeProcResults)
                {
                    if (item.Value.ProtDCe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + item.Key);
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
