#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2231")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCessao/v_S_01_02_00", IsNullable = false)]
    public class ESocial2231 : XMLBase
    {
        [XmlElement("evtCessao")]
        public EvtCessao EvtCessao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCessao")]
    [ComVisible(true)]
#endif
    public class EvtCessao
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculo IdeVinculo { get; set; }

        [XmlElement("infoCessao")]
        public InfoCessaoESocial2231 InfoCessao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCessaoESocial2231")]
    [ComVisible(true)]
#endif
    public class InfoCessaoESocial2231
    {
        [XmlElement("iniCessao")]
        public IniCessao IniCessao { get; set; }

        [XmlElement("fimCessao")]
        public FimCessao FimCessao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IniCessao")]
    [ComVisible(true)]
#endif
    public class IniCessao
    {
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

        [XmlElement("cnpjCess")]
        public string CnpjCess { get; set; }

        [XmlElement("respRemun")]
        public SimNaoLetra RespRemun { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FimCessao")]
    [ComVisible(true)]
#endif
    public class FimCessao
    {
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
