#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Exceptions;
using System.Collections.Generic;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.GNRE
{
    /// <summary>
    /// Envia XML de consulta Processamento de Lote de Recepção GNRE para WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.GNRE.ConsultaLoteRecepcao")]
    [ComVisible(true)]
#endif
    public class ConsultaLoteRecepcao : ServicoBase, IInteropService<TLoteConsultaGNRE>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new TLoteConsultaGNRE();
            xml = xml.LerXML<TLoteConsultaGNRE>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.GNREConsultaResultadoLote;
                Configuracoes.SchemaVersao = "2.00";
                Configuracoes.CodigoUF = (int)UFBrasil.PR; //Tanto faz o estado, qualquer um vai para o mesmo webservice

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Contem o resultado da consulta do lote da recepção
        /// </summary>
        public TRetLoteGNRE Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<TRetLoteGNRE>(RetornoWSXML);
                }

                return new TRetLoteGNRE
                {
                    SituacaoRecepcao = new SituacaoRecepcao
                    {
                        Codigo = "0",
                        Descricao = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado do WebService."
                    }
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaLoteRecepcao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="TLoteConsultaGNRE">Objeto contendo o XML da consulta do lote da recepção da GNRE</param>
        /// <param name="configuracao">Objeto contendo as configurações a serem utilizadas na consulta do lote da recepção da GNRE</param>
        public ConsultaLoteRecepcao(TLoteConsultaGNRE TLoteConsultaGNRE, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(TLoteConsultaGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(TLoteConsultaGNRE)), configuracao);
        }

        /// <summary>
        /// Construtor simplificado para consulta de lote da GNRE por código de barras
        /// </summary>
        /// <param name="ufBrasil">Unidade Federativa</param>
        /// <param name="cnpj">CNPJ do emitente</param>
        /// <param name="codBarras">Código de barras</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaLoteRecepcao(UFBrasil ufBrasil, string cnpj, string codBarras, Configuracao configuracao) : this()
        {
            if (string.IsNullOrWhiteSpace(ufBrasil.ToString()))
            {
                throw new ArgumentNullException(nameof(ufBrasil));
            }
            
            if (string.IsNullOrWhiteSpace(cnpj))
            {
                throw new ArgumentNullException(nameof(cnpj));
            }

            if (string.IsNullOrWhiteSpace(codBarras))
            {
                throw new ArgumentNullException(nameof(codBarras));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new TLoteConsultaGNRE
            {
                Versao = "2.00",
                Consulta = new List<Consulta>
                {
                    new Consulta
                    {
                        Uf = ufBrasil,
                        EmitenteId = new EmitenteId
                        {
                            CNPJ = cnpj,
                        },
                        CodBarras = codBarras,
                        NumControle = string.Empty,
                        DocOrigem = new DocOrigem
                        {
                            Tipo = "10",
                            Value = "12"
                        },
                        IdConsulta = "10",
                        TipoConsulta = TipoConsultaGNRE.ConsultaPorCodigoBarra
                    }
                }
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

        /// <summary>
        /// Construtor simplificado para consulta de lote da GNRE por número de controle
        /// </summary>
        /// <param name="ufBrasil">Unidade Federativa</param>
        /// <param name="cnpj">CNPJ do emitente</param>
        /// <param name="numControle">Número de controle</param>
        /// <param name="tipoConsulta">Tipo de consulta</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaLoteRecepcao(UFBrasil ufBrasil, string cnpj, string numControle, TipoConsultaGNRE tipoConsulta, Configuracao configuracao) : this()
        {
            if(string.IsNullOrEmpty(ufBrasil.ToString()))
            {
                throw new ArgumentNullException(nameof(ufBrasil));
            }

            if (string.IsNullOrWhiteSpace(cnpj))
            {
                throw new ArgumentNullException(nameof(cnpj));
            }

            if (string.IsNullOrWhiteSpace(numControle))
            {
                throw new ArgumentNullException(nameof(numControle));
            }

            if (string.IsNullOrWhiteSpace(tipoConsulta.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoConsulta));
            }

            if (tipoConsulta == TipoConsultaGNRE.ConsultaPorCodigoBarra)
            {
                throw new ArgumentException("Tipo de consulta inválido. Para consulta por número de controle, utilize o tipo TipoConsultaGNRE.ConsultaPorNumeroControle.");
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new TLoteConsultaGNRE
            {
                Versao = "2.00",
                Consulta = new List<Consulta>
                {
                    new Consulta
                    {
                        Uf = ufBrasil,
                        EmitenteId = new EmitenteId
                        {
                            CNPJ = cnpj,
                        },
                        NumControle = numControle,
                        TipoConsulta = tipoConsulta
                    }
                }
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

        /// <summary>
        /// Construtor completo para consulta de lote da GNRE
        /// </summary>
        /// <param name="ufBrasil">Unidade Federativa</param>
        /// <param name="cnpj">CNPJ do emitente</param>
        /// <param name="codBarras">Código de barras</param>
        /// <param name="numControle">Número de controle</param>
        /// <param name="tipoDocOrigem">Tipo do documento de origem</param>
        /// <param name="valorDocOrigem">Valor do documento de origem</param>
        /// <param name="idConsulta">ID da consulta</param>
        /// <param name="tipoConsulta">Tipo de consulta</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        public ConsultaLoteRecepcao(
            UFBrasil ufBrasil,
            string cnpj,
            string codBarras,
            string numControle,
            string tipoDocOrigem,
            string valorDocOrigem,
            string idConsulta,
            TipoConsultaGNRE tipoConsulta,
            Configuracao configuracao) : this()
        {
            if (string.IsNullOrWhiteSpace(ufBrasil.ToString()))
            {
                throw new ArgumentNullException(nameof(ufBrasil));
            }

            if (string.IsNullOrWhiteSpace(cnpj))
            {
                throw new ArgumentNullException(nameof(cnpj));
            }

            if (string.IsNullOrWhiteSpace(codBarras))
            {
                throw new ArgumentNullException(nameof(codBarras));
            }

            if (string.IsNullOrWhiteSpace(numControle))
            {
                throw new ArgumentNullException(nameof(numControle));
            }

            if (string.IsNullOrWhiteSpace(tipoDocOrigem))
            {
                throw new ArgumentNullException(nameof(tipoDocOrigem));
            }

            if (string.IsNullOrWhiteSpace(valorDocOrigem))
            {
                throw new ArgumentNullException(nameof(valorDocOrigem));
            }

            if (string.IsNullOrWhiteSpace(idConsulta))
            {
                throw new ArgumentNullException(nameof(idConsulta));
            }

            if (string.IsNullOrWhiteSpace(tipoConsulta.ToString()))
            {
                throw new ArgumentNullException(nameof(tipoConsulta));
            }

            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var xml = new TLoteConsultaGNRE
            {
                Versao = "2.00",
                Consulta = new List<Consulta>
                {
                    new Consulta
                    {
                        Uf = ufBrasil,
                        EmitenteId = new EmitenteId
                        {
                            CNPJ = cnpj,
                        },
                        CodBarras = codBarras,
                        NumControle = numControle,
                        DocOrigem = new DocOrigem
                        {
                            Tipo = tipoDocOrigem,
                            Value = valorDocOrigem
                        },
                        IdConsulta = idConsulta,
                        TipoConsulta = tipoConsulta
                    }
                }
            };

            var doc = new XmlDocument();
            doc.LoadXml(xml?.GerarXML().OuterXml);

            Inicializar(doc, configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o envio da consulta do lote da GNRE
        /// </summary>
        /// <param name="TLoteConsultaGNRE">Objeto contendo o XML da consulta do lote da recepção da GNRE</param>
        /// <param name="configuracao">Objeto contendo as configurações a serem utilizadas na consulta do lote da recepção da GNRE</param>
        [ComVisible(true)]
        public void Executar(TLoteConsultaGNRE TLoteConsultaGNRE, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(TLoteConsultaGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(TLoteConsultaGNRE)), configuracao);
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
        /// Gravar o XML retornado na consulta da GNRE
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado o XML retornado</param>
        /// <param name="nomeArquivo">Nome do arquivo que será gravado</param>
        public void GravarXmlRetorno(string pasta, string nomeArquivo) => GravarXmlDistribuicao(pasta, nomeArquivo);

        /// <summary>
        /// Grava o XML de Distribuição em uma pasta definida retornado pelo webservice
        /// </summary>
        /// <param name="pasta">Pasta onde é para ser gravado do XML</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        public void GravarXmlDistribuicao(string pasta, string nomeArquivo)
        {
            try
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    GravarXmlDistribuicao(pasta, nomeArquivo, RetornoWSString);
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