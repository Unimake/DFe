#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2230")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAfastTemp/v_S_01_02_00", IsNullable = false)]
    public class ESocial2230 : XMLBase
    {
        [XmlElement("evtAfastTemp")]
        public EvtAfastTemp EvtAfastTemp { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAfastTemp")]
    [ComVisible(true)]
#endif
    public class EvtAfastTemp
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculoESocial2230 IdeVinculo { get; set; }

        [XmlElement("infoAfastamento")]
        public InfoAfastamento InfoAfastamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculoESocial2230")]
    [ComVisible(true)]
#endif
    public class IdeVinculoESocial2230
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
        public string CodCateg { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrProcTrabField() => !string.IsNullOrEmpty(CodCateg);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAfastamento")]
    [ComVisible(true)]
#endif
    public class InfoAfastamento
    {
        [XmlElement("iniAfastamento")]
        public IniAfastamento IniAfastamento { get; set; }

        [XmlElement("infoRetif")]
        public InfoRetif InfoRetif { get; set; }

        [XmlElement("fimAfastamento")]
        public FimAfastamento FimAfastamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IniAfastamento")]
    [ComVisible(true)]
#endif
    public class IniAfastamento
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniAfast { get; set; }
#else
        public DateTimeOffset DtIniAfast { get; set; }
#endif

        [XmlElement("dtIniAfast")]
        public string DtIniAfastField
        {
            get => DtIniAfast.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniAfast = DateTime.Parse(value);
#else
            set => DtIniAfast = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("codMotAfast")]
        public string CodMotAfast { get; set; }

        [XmlElement("infoMesmoMtv")]
#if INTEROP
        public SimNaoLetra InfoMesmoMtv { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? InfoMesmoMtv { get; set; }
#endif

        [XmlElement("tpAcidTransito")]
#if INTEROP
        public TipoAcidenteTransito TpAcidTransito { get; set; } = (TipoAcidenteTransito)(-1);
#else
        public TipoAcidenteTransito? TpAcidTransito { get; set; }
#endif

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        [XmlElement("perAquis")]
        public PerAquis PerAquis { get; set; }

        [XmlElement("infoCessao")]
        public InfoCessao InfoCessao { get; set; }

        [XmlElement("infoMandSind")]
        public InfoMandSind InfoMandSind { get; set; }

        [XmlElement("infoMandElet")]
        public InfoMandElet InfoMandElet { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoMesmoMtv() => InfoMesmoMtv != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeInfoMesmoMtv() => InfoMesmoMtv != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpAcidTransito() => TpAcidTransito != (TipoAcidenteTransito)(-1);
#else
        public bool ShouldSerializeTpAcidTransito() => TpAcidTransito != null;
#endif

        public bool ShouldSerializeObservacaoField() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PerAquis")]
    [ComVisible(true)]
#endif
    public class PerAquis
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

        [XmlElement("dtInicio")]
        public string DtIniAfastField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtFim { get; set; }
#else
        public DateTimeOffset DtFim { get; set; }
#endif

        [XmlElement("dtFim")]
        public string DtFimField
        {
            get => DtFim.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFim = DateTime.Parse(value);
#else
            set => DtFim = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeDtFimField() => DtFim > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCessao")]
    [ComVisible(true)]
#endif
    public class InfoCessao
    {
        [XmlElement("cnpjCess")]
        public string CnpjCess { get; set; }

        [XmlElement("infOnus")]
        public InfOnus InfOnus { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandSind")]
    [ComVisible(true)]
#endif
    public class InfoMandSind
    {
        [XmlElement("cnpjSind")]
        public string CnpjSind { get; set; }

        [XmlElement("infOnusRemun")]
        public OnusDaRemuneracao InfOnusRemun { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandElet")]
    [ComVisible(true)]
#endif
    public class InfoMandElet
    {
        [XmlElement("cnpjMandElet")]
        public string CnpjMandElet { get; set; }

        [XmlElement("indRemunCargo")]
#if INTEROP
        public SimNaoLetra IndRemunCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRemunCargo { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRetif")]
    [ComVisible(true)]
#endif
    public class InfoRetif
    {
        [XmlElement("origRetif")]
        public OrigemDaRetificacao OrigRetif { get; set; }

        [XmlElement("tpProc")]
        public string TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeTpProcField() => !string.IsNullOrEmpty(TpProc);
       
        public bool ShouldSerializeNrProcField() => !string.IsNullOrEmpty(NrProc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FimAfastamento")]
    [ComVisible(true)]
#endif
    public class FimAfastamento
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermAfast { get; set; }
#else
        public DateTimeOffset DtTermAfast { get; set; }
#endif

        [XmlElement("dtTermAfast")]
        public string DtTermAfastField
        {
            get => DtTermAfast.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTermAfast = DateTime.Parse(value);
#else
            set => DtTermAfast = DateTimeOffset.Parse(value);
#endif
        }
    }

}
