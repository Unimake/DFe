#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Retorno da consulta status do servico do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.RetConsStatServBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsStatServBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class RetConsStatServBPe : XMLBase
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
        /// Data e hora do recebimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Data e hora do recebimento serializada no XML
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tempo medio de resposta
        /// </summary>
        [XmlElement("tMed")]
        public int TMed { get; set; }

        /// <summary>
        /// Data e hora prevista para retorno
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRetorno { get; set; }
#else
        public DateTimeOffset DhRetorno { get; set; }
#endif

        /// <summary>
        /// Data e hora prevista para retorno serializada no XML
        /// </summary>
        [XmlElement("dhRetorno")]
        public string DhRetornoField
        {
            get => DhRetorno.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRetorno = DateTime.Parse(value);
#else
            set => DhRetorno = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Observacao
        /// </summary>
        [XmlElement("xObs")]
        public string XObs { get; set; }

        public bool ShouldSerializeTMed() => TMed > 0;

#if INTEROP
        public bool ShouldSerializeDhRetornoField() => DhRetorno > DateTime.MinValue;
#else
        public bool ShouldSerializeDhRetornoField() => DhRetorno > DateTimeOffset.MinValue;
#endif

        public bool ShouldSerializeXObs() => !string.IsNullOrEmpty(XObs);
    }
}
