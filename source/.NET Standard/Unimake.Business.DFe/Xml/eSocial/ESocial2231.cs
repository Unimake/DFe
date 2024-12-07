#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2231 - Cessão/Exercício em Outro Órgão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2231")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCessao/v_S_01_02_00", IsNullable = false)]
    public class ESocial2231 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Cessão/Exercício em Outro Órgão
        /// </summary>
        [XmlElement("evtCessao")]
        public EvtCessao EvtCessao { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Cessão/Exercício em Outro Órgão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCessao")]
    [ComVisible(true)]
#endif
    public class EvtCessao
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2231 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2231 IdeVinculo { get; set; }

        /// <summary>
        /// Informações da cessão/exercício em outro órgão
        /// </summary>
        [XmlElement("infoCessao")]
        public InfoCessao2231 InfoCessao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2231")]
    [ComVisible(true)]
#endif
    public class IdeEvento2231 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2231")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2231 : IdeVinculo2206 { }

    /// <summary>
    /// Informações da cessão/exercício em outro órgão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCessao2231")]
    [ComVisible(true)]
#endif
    public class InfoCessao2231
    {
        /// <summary>
        /// Informações de início da cessão/exercício em outro órgão
        /// </summary>
        [XmlElement("iniCessao")]
        public IniCessao IniCessao { get; set; }

        /// <summary>
        /// Informação de término da cessão/exercício em outro órgão
        /// </summary>
        [XmlElement("fimCessao")]
        public FimCessao FimCessao { get; set; }
    }

    /// <summary>
    /// Informações de início da cessão/exercício em outro órgão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IniCessao")]
    [ComVisible(true)]
#endif
    public class IniCessao
    {
        /// <summary>
        /// Data de início da cessão/exercício em outro órgão
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniCessao { get; set; }
#else
        public DateTimeOffset DtIniCessao { get; set; }
#endif

        [XmlElement("dtIniCessao")]
        public string DtIniCessaoField
        {
            get => DtIniCessao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniCessao = DateTime.Parse(value);
#else
            set => DtIniCessao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o CNPJ do empregador/órgão público cessionário/de destino
        /// </summary>
        [XmlElement("cnpjCess")]
        public string CnpjCess { get; set; }

        /// <summary>
        /// Informar se o empregador/órgão público declarante continuará informando remunerações (S-1200/S-1202) do trabalhador cedido/em exercício em outro órgão
        /// </summary>
        [XmlElement("respRemun")]
        public SimNaoLetra RespRemun { get; set; }
    }

    /// <summary>
    /// Informação de término da cessão/exercício em outro órgão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FimCessao")]
    [ComVisible(true)]
#endif
    public class FimCessao
    {
        /// <summary>
        /// Preencher com a data de término da cessão/exercício em outro órgão
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermCessao { get; set; }
#else
        public DateTimeOffset DtTermCessao { get; set; }
#endif

        [XmlElement("dtTermCessao")]
        public string DtTermCessaoField
        {
            get => DtTermCessao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTermCessao = DateTime.Parse(value);
#else
            set => DtTermCessao = DateTimeOffset.Parse(value);
#endif
        }
    }
}
