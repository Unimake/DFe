#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Retorno da consulta de eventos NFSe - Padrão NACIONAL
    /// Contém eventos (sucesso) OU erro (falha)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.RetConsPedRegEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("temp")]
    public class RetConsPedRegEvento : XMLBase
    {
        /// <summary>
        /// Data e hora do processamento
        /// </summary>
        [XmlElement("dataHoraProcessamento")]
        public string DataHoraProcessamento { get; set; }

        /// <summary>
        /// Tipo de ambiente (1=Produção, 2=Homologação)
        /// </summary>
        [XmlElement("tipoAmbiente")]
        public int TipoAmbiente { get; set; }

        /// <summary>
        /// Versão do aplicativo
        /// </summary>
        [XmlElement("versaoAplicativo")]
        public string VersaoAplicativo { get; set; }

        /// <summary>
        /// Informações do evento consultado (presente em caso de sucesso)
        /// </summary>
        [XmlElement("eventos")]
        public EventosConsulta Eventos { get; set; }

        /// <summary>
        /// Informações de erro (presente em caso de falha)
        /// </summary>
        [XmlElement("erro")]
        public Erro Erro { get; set; }

    }

    /// <summary>
    /// Informações do evento consultado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.EventosConsulta")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true)]
    public class EventosConsulta
    {
        /// <summary>
        /// Chave de acesso da NFSe
        /// </summary>
        [XmlElement("chaveAcesso")]
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Tipo do evento
        /// </summary>
        [XmlElement("tipoEvento")]
        public string TipoEvento { get; set; }

        /// <summary>
        /// Número do pedido de registro do evento
        /// </summary>
        [XmlElement("numeroPedidoRegistroEvento")]
        public string NumeroPedidoRegistroEvento { get; set; }

        /// <summary>
        /// Data e hora do recebimento
        /// </summary>
        [XmlElement("dataHoraRecebimento")]
        public string DataHoraRecebimento { get; set; }

        /// <summary>
        /// Evento completo (após descompressão do GZIP)
        /// </summary>
        [XmlElement("arquivoXml")]
        public ArquivoXmlEvento ArquivoXml { get; set; }
    }

    /// <summary>
    /// Container do evento descomprimido
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ArquivoXmlEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true)]
    public class ArquivoXmlEvento
    {
        /// <summary>
        /// Evento completo
        /// </summary>
        [XmlElement("evento", Namespace = "http://www.sped.fazenda.gov.br/nfse")]
        public Evento Evento { get; set; }
    }
}