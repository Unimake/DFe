#if INTEROP
using System.Collections.Generic;
using System.Runtime.InteropServices;
#else
using System.Collections.Generic;
#endif
using System;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    internal static class ConteudoAbertoNFeABI
    {
        internal static string Serializar(XmlElement[] elementos)
        {
            if (elementos == null) return null;
            var retorno = string.Empty;
            foreach (var elemento in elementos) retorno += elemento.OuterXml;
            return retorno;
        }

        internal static XmlElement[] Desserializar(string xml)
        {
            if (string.IsNullOrWhiteSpace(xml)) return null;
            var documento = new XmlDocument();
            documento.LoadXml("<raiz>" + xml + "</raiz>");
            var elementos = new List<XmlElement>();
            foreach (XmlNode no in documento.DocumentElement.ChildNodes)
            {
                if (no is XmlElement elemento) elementos.Add(elemento);
            }
            return elementos.ToArray();
        }
    }

/// <summary>Representa o grupo RetConsSitNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.RetConsSitNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("retConsSitNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class RetConsSitNFeABI : XMLBase
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tpAmb da NFeABI.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag verAplic da NFeABI.</summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cStat da NFeABI.</summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xMotivo da NFeABI.</summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlElement("cUF")]
        public int CUFField { get => (int)CUF; set => CUF = (UFBrasil)value; }
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhRecbto = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }
        /// <summary>Obtém ou define o conteúdo da tag chNFeABI da NFeABI.</summary>
        [XmlElement("chNFeABI")]
        public string ChNFeABI { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag protNFeABI da NFeABI.</summary>
        [XmlElement("protNFeABI")]
        public ProtocoloConsultaNFeABI ProtNFeABI { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag procEventoNFeABI da NFeABI.</summary>
        [XmlElement("procEventoNFeABI")]
        public List<ProcessoEventoConsultaNFeABI> ProcEventoNFeABI { get; set; }
#if INTEROP
        /// <summary>Adiciona um item à coleção ProcEventoNFeABI.</summary>
        public void AddProcEventoNFeABI(ProcessoEventoConsultaNFeABI item) { if (ProcEventoNFeABI == null) ProcEventoNFeABI = new List<ProcessoEventoConsultaNFeABI>(); ProcEventoNFeABI.Add(item); }
        /// <summary>Obtém um item da coleção ProcEventoNFeABI pelo índice.</summary>
        public ProcessoEventoConsultaNFeABI GetProcEventoNFeABI(int index) => ProcEventoNFeABI[index];
        /// <summary>Obtém a quantidade de itens da coleção ProcEventoNFeABI.</summary>
        public int GetProcEventoNFeABICount => ProcEventoNFeABI?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo ProtocoloConsultaNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.ProtocoloConsultaNFeABI")]
    [ComVisible(true)]
#endif
    public class ProtocoloConsultaNFeABI
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo XML aberto do protocolo.</summary>
        [XmlAnyElement]
#if INTEROP
        [ComVisible(false)]
#endif
        public XmlElement[] Conteudo { get; set; }

        /// <summary>Conteúdo XML aberto do protocolo para consumidores COM.</summary>
        [XmlIgnore]
        public string ConteudoXML { get => ConteudoAbertoNFeABI.Serializar(Conteudo); set => Conteudo = ConteudoAbertoNFeABI.Desserializar(value); }
    }

/// <summary>Representa o grupo ProcessoEventoConsultaNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.ProcessoEventoConsultaNFeABI")]
    [ComVisible(true)]
#endif
    public class ProcessoEventoConsultaNFeABI
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo XML aberto do evento processado.</summary>
        [XmlAnyElement]
#if INTEROP
        [ComVisible(false)]
#endif
        public XmlElement[] Conteudo { get; set; }

        /// <summary>Conteúdo XML aberto do evento para consumidores COM.</summary>
        [XmlIgnore]
        public string ConteudoXML { get => ConteudoAbertoNFeABI.Serializar(Conteudo); set => Conteudo = ConteudoAbertoNFeABI.Desserializar(value); }
    }
}
