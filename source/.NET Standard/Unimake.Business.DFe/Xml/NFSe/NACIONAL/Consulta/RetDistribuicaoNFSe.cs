#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using System.Text;
using NFSeNacional = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta
{
    /// <summary>
    /// Classe que representa a resposta da consulta de distribuição de NFSe por NSU
    /// Desserializa o XML retornado pela API
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.RetDistribuicaoNFSe")]
    [ComVisible(true)]
#endif
    [XmlRoot("temp")]
    public class RetDistribuicaoNFSe : XMLBase
    {
        /// <summary>
        /// Status do processamento (ex: DOCUMENTOS_LOCALIZADOS, NENHUM_DOCUMENTO_LOCALIZADO, REJEICAO)
        /// </summary>
        [XmlElement("StatusProcessamento")]
        public string StatusProcessamento { get; set; }

        /// <summary>
        /// Lista de documentos localizados
        /// </summary>
        [XmlElement("LoteDFe")]
        public List<LoteDFe> LoteDFe { get; set; }

        /// <summary>
        /// Lista de alertas da resposta
        /// </summary>
        [XmlElement("Alerta")]
        public List<Alertas> Alertas { get; set; }

        /// <summary>
        /// Único erro ou lista de erros da resposta (pode vir como elemento único ou múltiplo)
        /// </summary>
        [XmlElement("Erros")]
        public List<Erros> Erros { get; set; }

        /// <summary>
        /// Tipo de ambiente (PRODUCAO, HOMOLOGACAO)
        /// </summary>
        [XmlElement("TipoAmbiente")]
        public string TipoAmbiente { get; set; }

        /// <summary>
        /// Versão do aplicativo
        /// </summary>
        [XmlElement("VersaoAplicativo")]
        public string VersaoAplicativo { get; set; }

        /// <summary>
        /// Data e hora do processamento
        /// </summary>
        [XmlElement("DataHoraProcessamento")]
        public DateTime DataHoraProcessamento { get; set; }

        public RetDistribuicaoNFSe()
        {
            LoteDFe = new List<LoteDFe>();
            Alertas = new List<Alertas>();
            Erros = new List<Erros>();
        }

        /// <summary>
        /// Desserializar o XML RetDistribuicaoNFSe no objeto
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto RetDistribuicaoNFSe</returns>
        public RetDistribuicaoNFSe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetDistribuicaoNFSe>(doc);
        }

        /// <summary>
        /// Desserializar o XML RetDistribuicaoNFSe no objeto
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto RetDistribuicaoNFSe</returns>
        public RetDistribuicaoNFSe LoadFromXML(string xml) => XMLUtility.Deserializar<RetDistribuicaoNFSe>(xml);

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista LoteDFe
        /// </summary>
        /// <param name="item">Elemento LoteDFe</param>
        public void AddLoteDFe(LoteDFe item)
        {
            if (LoteDFe == null)
            {
                LoteDFe = new List<LoteDFe>();
            }

            LoteDFe.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista LoteDFe
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index</returns>
        public LoteDFe GetLoteDFe(int index)
        {
            if ((LoteDFe?.Count ?? 0) == 0)
                return default;
            return LoteDFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista LoteDFe
        /// </summary>
        public int GetLoteDFeCount => (LoteDFe != null ? LoteDFe.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista Alertas
        /// </summary>
        /// <param name="item">Elemento Alertas</param>
        public void AddAlerta(Alertas item)
        {
            if (Alertas == null)
            {
                Alertas = new List<Alertas>();
            }

            Alertas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Alertas
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index</returns>
        public Alertas GetAlerta(int index)
        {
            if ((Alertas?.Count ?? 0) == 0)
                return default;
            return Alertas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista Alertas
        /// </summary>
        public int GetAlertaCount => (Alertas != null ? Alertas.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista Erros
        /// </summary>
        /// <param name="item">Elemento Erros</param>
        public void AddErro(Erros item)
        {
            if (Erros == null)
            {
                Erros = new List<Erros>();
            }

            Erros.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Erros
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index</returns>
        public Erros GetErro(int index)
        {
            if ((Erros?.Count ?? 0) == 0)
                return default;
            return Erros[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista Erros
        /// </summary>
        public int GetErroCount => (Erros != null ? Erros.Count : 0);

#endif
    }

    /// <summary>
    /// Classe que representa um documento fiscal (DFe) retornado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.LoteDFe")]
    [ComVisible(true)]
#endif
    [XmlType("LoteDFe")]
    public class LoteDFe
    {
        /// <summary>
        /// Número de Sequência Único (NSU)
        /// </summary>
        [XmlElement("NSU")]
        public long NSU { get; set; }

        /// <summary>
        /// Chave de acesso do documento
        /// </summary>
        [XmlElement("ChaveAcesso")]
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Tipo de documento (ex: NFSE, NENHUM)
        /// </summary>
        [XmlElement("TipoDocumento")]
        public string TipoDocumento { get; set; }

        /// <summary>
        /// Tipo de evento (ex: CANCELAMENTO)
        /// </summary>
        [XmlElement("TipoEvento")]
        public string TipoEvento { get; set; }

        /// <summary>
        /// Arquivo XML comprimido em Base64 (GZIP)
        /// </summary>
        [XmlElement("ArquivoXml")]
        public NFSeNacional ArquivoXml { get; set; }

        /// <summary>
        /// Data e hora de geração do documento
        /// </summary>
        [XmlElement("DataHoraGeracao")]
        public DateTime DataHoraGeracao { get; set; }

        /// <summary>
        /// Conteúdo do XML descompactado (propriedade derivada)
        /// </summary>
        [XmlIgnore]
        public string ConteudoXML => ArquivoXml?.GerarXML().OuterXml ?? string.Empty;

        /// <summary>
        /// Conteúdo do XML em XmlDocument (propriedade derivada)
        /// </summary>
        [XmlIgnore]
        public XmlDocument DocXML
        {
            get
            {
                if (ArquivoXml == null)
                {
                    return null;
                }

                var doc = ArquivoXml.GerarXML();
                doc.PreserveWhitespace = true;
                return doc;
            }
        }
    }

    /// <summary>
    /// Classe que representa um alerta da resposta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.Alertas")]
    [ComVisible(true)]
#endif
    [XmlType("Alerta")]
    public class Alertas
    {
        /// <summary>
        /// Mensagem do alerta (pode ser um objeto vazio)
        /// </summary>
        [XmlElement("Mensagem")]
        public object Mensagem { get; set; }

        /// <summary>
        /// Lista de parâmetros
        /// </summary>
        [XmlArray("Parametros")]
        [XmlArrayItem("Parametro")]
        public List<string> Parametros { get; set; }

        /// <summary>
        /// Código do alerta
        /// </summary>
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        /// <summary>
        /// Descrição do alerta
        /// </summary>
        [XmlElement("Descricao")]
        public string Descricao { get; set; }

        /// <summary>
        /// Complemento do alerta
        /// </summary>
        [XmlElement("Complemento")]
        public string Complemento { get; set; }

        public Alertas()
        {
            Parametros = new List<string>();
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo parâmetro à lista
        /// </summary>
        /// <param name="item">Parâmetro</param>
        public void AddParametro(string item)
        {
            if (Parametros == null)
            {
                Parametros = new List<string>();
            }

            Parametros.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Parametros
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index</returns>
        public string GetParametro(int index)
        {
            if ((Parametros?.Count ?? 0) == 0)
                return default;
            return Parametros[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista Parametros
        /// </summary>
        public int GetParametroCount => (Parametros != null ? Parametros.Count : 0);

#endif
    }

    /// <summary>
    /// Classe que representa um erro da resposta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.Erros")]
    [ComVisible(true)]
#endif
    [XmlType("Erros")]
    public class Erros
    {
        /// <summary>
        /// Mensagem do erro (pode ser um objeto vazio)
        /// </summary>
        [XmlElement("Mensagem")]
        public object Mensagem { get; set; }

        /// <summary>
        /// Lista de parâmetros
        /// </summary>
        [XmlArray("Parametros")]
        [XmlArrayItem("Parametro")]
        public List<string> Parametros { get; set; }

        /// <summary>
        /// Código do erro
        /// </summary>
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        /// <summary>
        /// Descrição do erro
        /// </summary>
        [XmlElement("Descricao")]
        public string Descricao { get; set; }

        /// <summary>
        /// Complemento do erro
        /// </summary>
        [XmlElement("Complemento")]
        public string Complemento { get; set; }

        public Erros()
        {
            Parametros = new List<string>();
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo parâmetro à lista
        /// </summary>
        /// <param name="item">Parâmetro</param>
        public void AddParametro(string item)
        {
            if (Parametros == null)
            {
                Parametros = new List<string>();
            }

            Parametros.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Parametros
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index</returns>
        public string GetParametro(int index)
        {
            if ((Parametros?.Count ?? 0) == 0)
                return default;
            return Parametros[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista Parametros
        /// </summary>
        public int GetParametroCount => (Parametros != null ? Parametros.Count : 0);

#endif
    }
}