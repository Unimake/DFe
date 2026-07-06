#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Retorno da consulta situacao do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.RetConsSitBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsSitBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class RetConsSitBPe : XMLBase
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificacao do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versao do aplicativo
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Codigo do status
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descricao do status
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Codigo da UF
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Codigo da UF serializado no XML
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Protocolos do BP-e
        /// </summary>
        [XmlElement("protBPe")]
        public List<ProtBPe> ProtBPe { get; set; }

        /// <summary>
        /// Eventos processados do BP-e
        /// </summary>
        [XmlElement("procEventoBPe")]
        public List<ProcEventoBPe> ProcEventoBPe { get; set; }

#if INTEROP
        public void AddProtBPe(ProtBPe item) => (ProtBPe ?? (ProtBPe = new List<ProtBPe>())).Add(item);
        public ProtBPe GetProtBPe(int index) => (ProtBPe?.Count ?? 0) == 0 ? default : ProtBPe[index];
        public int GetProtBPeCount => ProtBPe != null ? ProtBPe.Count : 0;

        public void AddProcEventoBPe(ProcEventoBPe item) => (ProcEventoBPe ?? (ProcEventoBPe = new List<ProcEventoBPe>())).Add(item);
        public ProcEventoBPe GetProcEventoBPe(int index) => (ProcEventoBPe?.Count ?? 0) == 0 ? default : ProcEventoBPe[index];
        public int GetProcEventoBPeCount => ProcEventoBPe != null ? ProcEventoBPe.Count : 0;
#endif

        public bool ShouldSerializeProtBPe() => ProtBPe?.Count > 0;
        public bool ShouldSerializeProcEventoBPe() => ProcEventoBPe?.Count > 0;
    }
}
