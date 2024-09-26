#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2410")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenIn/v_S_01_02_00", IsNullable = false)]
    public class ESocial2410 : XMLBase
    {
        [XmlElement("evtCdBenIn")]
        public EvtCdBenIn EvtCdBenIn { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenIn")]
    [ComVisible(true)]
#endif
    public class EvtCdBenIn
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2410 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("beneficiario")]
        public BeneficiarioESocial2410 Beneficiario { get; set; }

        [XmlElement("infoBenInicio")]
        public InfoBenInicio InfoBenInicio { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2410")]
    [ComVisible(true)]
#endif
    public class IdeEvento2410 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BeneficiarioESocial2410")]
    [ComVisible(true)]
#endif
    public class BeneficiarioESocial2410
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("cnpjOrigem")]
        public string CnpjOrigem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);
       
        public bool ShouldSerializeCnpjOrigem() => !string.IsNullOrEmpty(CnpjOrigem);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenInicio")]
    [ComVisible(true)]
#endif
    public class InfoBenInicio
    {
        [XmlElement("cadIni")]
        public SimNaoLetra CadIni { get; set; }

        [XmlElement("indSitBenef")]
#if INTEROP
        public IndSitBenef IndSitBenef { get; set; } = (IndSitBenef)(-1);
#else
        public IndSitBenef? IndSitBenef { get; set; }
#endif

        [XmlElement("nrBeneficio")]
        public string NrBeneficio { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIniBeneficio { get; set; }
#else
        public DateTimeOffset DtIniBeneficio { get; set; }
#endif

        [XmlElement("dtIniBeneficio")]
        public string DtIniBeneficioField
        {
            get => DtIniBeneficio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniBeneficio = DateTime.Parse(value);
#else
            set => DtIniBeneficio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtPublic { get; set; }
#else
        public DateTimeOffset DtPublic { get; set; }
#endif

        [XmlElement("dtPublic")]
        public string DtPublicField
        {
            get => DtPublic.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPublic = DateTime.Parse(value);
#else
            set => DtPublic = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("dadosBeneficio")]
        public DadosBeneficio DadosBeneficio { get; set; }

        [XmlElement("sucessaoBenef")]
        public SucessaoBenef SucessaoBenef { get; set; }

        [XmlElement("mudancaCPF")]
        public MudancaCpfESocial2410 MudancaCPF { get; set; }

        [XmlElement("infoBenTermino")]
        public InfoBenTermino InfoBenTermino { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndSitBenef() => IndSitBenef != (IndSitBenef)(-1);
#else
        public bool ShouldSerializeIndSitBenef() => IndSitBenef != null;
#endif

        public bool ShouldSerializeDtPublicField() => DtPublic > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosBeneficio")]
    [ComVisible(true)]
#endif
    public class DadosBeneficio
    {
        [XmlElement("tpBeneficio")]
        public string TpBeneficio { get; set; }

        [XmlElement("tpPlanRP")]
        public PlanoSegregacaoDaMassa TpPlanRP { get; set; }

        [XmlElement("dsc")]
        public string Dsc { get; set; }

        [XmlElement("indDecJud")]
#if INTEROP
        public SimNaoLetra IndDecJud { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndDecJud { get; set; }
#endif

        [XmlElement("infoPenMorte")]
        public InfoPenMorte InfoPenMorte { get; set; }

        #region ShouldSeriaize

        public bool ShouldSerializeDsc() => !string.IsNullOrEmpty(Dsc);

#if INTEROP
        public bool ShouldSerializeIndDecJud() => IndDecJud != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndDecJud() => IndDecJud != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPenMorte")]
    [ComVisible(true)]
#endif
    public class InfoPenMorte
    {
        [XmlElement("tpPenMorte")]
        public TpPenMorte TpPenMorte { get; set; }

        [XmlElement("instPenMorte")]
        public InstPenMorte InstPenMorte { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InstPenMorte")]
    [ComVisible(true)]
#endif
    public class InstPenMorte
    {
        [XmlElement("cpfInst")]
        public string CpfInst { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtInst { get; set; }
#else
        public DateTimeOffset DtInst { get; set; }
#endif

        [XmlElement("dtInst")]
        public string DtInstField
        {
            get => DtInst.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInst = DateTime.Parse(value);
#else
            set => DtInst = DateTimeOffset.Parse(value);
#endif
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoBenef")]
    [ComVisible(true)]
#endif
    public class SucessaoBenef
    {
        [XmlElement("cnpjOrgaoAnt")]
        public string CnpjOrgaoAnt { get; set; }

        [XmlElement("nrBeneficioAnt")]
        public string NrBeneficioAnt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtTransf { get; set; }
#else
        public DateTimeOffset DtTransf { get; set; }
#endif

        [XmlElement("dtTransf")]
        public string DtTransfField
        {
            get => DtTransf.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTransf = DateTime.Parse(value);
#else
            set => DtTransf = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCpfESocial2410")]
    [ComVisible(true)]
#endif
    public class MudancaCpfESocial2410
    {
        [XmlElement("cpfAnt")]
        public string CpfAnt { get; set; }

        [XmlElement("nrBeneficioAnt")]
        public string NrBeneficioAnt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtAltCPF { get; set; }
#else
        public DateTimeOffset DtAltCPF { get; set; }
#endif

        [XmlElement("dtAltCPF")]
        public string DtAltCPFField
        {
            get => DtAltCPF.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAltCPF = DateTime.Parse(value);
#else
            set => DtAltCPF = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBenTermino")]
    [ComVisible(true)]
#endif
    public class InfoBenTermino
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermBeneficio { get; set; }
#else
        public DateTimeOffset DtTermBeneficio { get; set; }
#endif

        [XmlElement("dtTermBeneficio")]
        public string DtTermBeneficioField
        {
            get => DtTermBeneficio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTermBeneficio = DateTime.Parse(value);
#else
            set => DtTermBeneficio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("mtvTermino")]
        public string MtvTermino { get; set; }
    }
}
