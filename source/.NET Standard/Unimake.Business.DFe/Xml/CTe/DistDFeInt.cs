#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// Distribuição de DF-e de interesse
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DistDFeInt")]
    [ComVisible(true)]
#endif
    [XmlRoot("distDFeInt", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class DistDFeInt : XMLBase
    {
        /// <summary>
        /// Versão do leiaute.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do Ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Código da UF do Autor.
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUFAutor { get; set; }

        /// <summary>
        /// Código da UF do Autor (campo para serialização XML).
        /// </summary>
        [XmlElement("cUFAutor")]
        public int CUFAutorField
        {
            get => (int)CUFAutor;
            set => CUFAutor = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Código do órgão.
        /// </summary>
        [XmlIgnore]
        public readonly UFBrasil COrgao = UFBrasil.AN;

        /// <summary>
        /// CNPJ do interessado.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do interessado.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Consulta por NSU.
        /// </summary>
        [XmlElement("consNSU")]
        public ConsNSU ConsNSU { get; set; }

        /// <summary>
        /// Consulta por NSU inicial.
        /// </summary>
        [XmlElement("distNSU")]
        public DistNSU DistNSU { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ()
        {
            return !string.IsNullOrWhiteSpace(CNPJ);
        }

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF()
        {
            return !string.IsNullOrWhiteSpace(CPF);
        }

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ConsNSU")]
    [ComVisible(true)]
#endif
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ConsNSU
    {
        /// <summary>
        /// NSU da consulta.
        /// </summary>
        [XmlElement("NSU", DataType = "token")]
        public string NSU { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DistNSU")]
    [ComVisible(true)]
#endif
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DistNSU
    {
        /// <summary>
        /// Último NSU da consulta.
        /// </summary>
        [XmlElement("ultNSU", DataType = "token")]
        public string UltNSU { get; set; }
    }
}
