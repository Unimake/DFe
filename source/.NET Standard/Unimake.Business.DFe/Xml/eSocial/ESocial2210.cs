#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2210")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCAT/v_S_01_02_00", IsNullable = false)]
    public class ESocial2210 : XMLBase
    {
        [XmlElement("evtCAT")]
        public EvtCAT EvtCAT { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCAT")]
    [ComVisible(true)]
#endif
    public class EvtCAT
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculo IdeVinculo { get; set; }

        [XmlElement("cat")]
        public Cat Cat { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo")]
    [ComVisible(true)]
#endif
    public class IdeVinculo
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
        public string CodCateg { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeMatriculaField() => !string.IsNullOrEmpty(Matricula);

        public bool ShouldSereializeCodCategField() => !string.IsNullOrEmpty(CodCateg);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Cat")]
    [ComVisible(true)]
#endif
    public class Cat
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtAcid { get; set; }
#else
        public DateTimeOffset DtAcid { get; set; }
#endif

        [XmlElement("dtAcid")]
        public string DtAcidField
        {
            get => DtAcid.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAcid = DateTime.Parse(value);
#else
            set => DtAcid = DateTimeOffset.Parse(value);
#endif
        }
        
        [XmlElement("tpAcid")]
        public TipoAcidenteTrabalho TpAcid { get; set; }

        [XmlElement("hrAcid")]
        public string HrAcid { get; set; }

        [XmlElement("hrsTrabAntesAcid")]
        public string HrsTrabAntesAcid { get; set; }

        [XmlElement("tpCat")]
        public TipoDeCAT TpCat { get; set; }

        [XmlElement("indCatObito")]
        public SimNaoLetra IndCatObito { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtObito { get; set; }
#else
        public DateTimeOffset DtObito { get; set; }
#endif

        [XmlElement("dtObito")]
        public string DtAlteracaoField
        {
            get => DtObito.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtObito = DateTime.Parse(value);
#else
            set => DtObito = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indComunPolicia")]
        public SimNaoLetra IndComunPolicia { get; set; }

        [XmlElement("codSitGeradora")]
        public string CodSitGeradora { get; set; }

        [XmlElement("iniciatCAT")]
        public IniciativaDaCAT IniciatCAT { get; set; }

        [XmlElement("obsCAT")]
        public string ObsCAT { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime UltDiaTrab { get; set; }
#else
        public DateTimeOffset UltDiaTrab { get; set; }
#endif

        [XmlElement("UltDiaTrab")]
        public string UltDiaTrabField
        {
            get => UltDiaTrab.ToString("yyyy-MM-dd");
#if INTEROP
            set => UltDiaTrab = DateTime.Parse(value);
#else
            set => UltDiaTrab = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("houveAfast")]
#if INTEROP
        public SimNaoLetra HouveAfast { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra ? HouveAfast { get; set; }
#endif

        [XmlElement("localAcidente")]
        public LocalAcidente LocalAcidente { get; set; }

        [XmlElement("parteAtingida")]
        public ParteAtingida ParteAtingida { get; set; }

        [XmlElement("agenteCausador")]
        public AgenteCausador AgenteCausador { get; set; }

        [XmlElement("atestado")]
        public Atestado Atestado { get; set; }

        [XmlElement("catOrigem")]
        public CatOrigem CatOrigem { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeHrAcidField() => !string.IsNullOrEmpty(HrAcid);

        public bool ShouldSereializeHrsTrabAntesAcidField() => !string.IsNullOrEmpty(HrsTrabAntesAcid);

        public bool ShouldSerializeDtObitoField() => DtObito > DateTime.MinValue;

        public bool ShouldSerializeUltDiaTrabField() => UltDiaTrab > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeHouveAfast() => HouveAfast != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeHouveAfast() => HouveAfast != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalAcidente")]
    [ComVisible(true)]
#endif
    public class LocalAcidente
    {
        [XmlElement("tpLocal")]
        public TipoLocalAcidente TpLocal { get; set; }

        [XmlElement("dscLocal")]
        public string DscLocal { get; set; }

        [XmlElement("tpLograd")]
        public string TpLograd { get; set; }

        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        [XmlElement("complemento")]
        public string Complemento { get; set; }

        [XmlElement("bairro")]
        public string Bairro { get; set; }

        [XmlElement("cep")]
        public string Cep { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("uf")]
#if INTEROP
        public UFBrasil Uf { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? Uf { get; set; }
#endif

        [XmlElement("pais")]
        public string Pais { get; set; }

        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        [XmlElement("ideLocalAcid")]
        public IdeLocalAcid IdeLocalAcid { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeDscLocalField() => !string.IsNullOrEmpty(DscLocal);

        public bool ShouldSereializeTpLogradField() => !string.IsNullOrEmpty(TpLograd);

        public bool ShouldSereializeComplementoField() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSereializeBairroField() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSereializeCepField() => !string.IsNullOrEmpty(Cep);

        public bool ShouldSereializeCodMunicField() => !string.IsNullOrEmpty(CodMunic);

#if INTEROP
        public bool ShouldSerializeUfField() => Uf != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfField() => Uf != null;
#endif

        public bool ShouldSereializePaisField() => !string.IsNullOrEmpty(Pais);

        public bool ShouldSereializeCodPostalField() => !string.IsNullOrEmpty(CodPostal);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLocalAcid")]
    [ComVisible(true)]
#endif
    public class IdeLocalAcid
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ParteAtingida")]
    [ComVisible(true)]
#endif
    public class ParteAtingida
    {
        [XmlElement("codParteAting")]
        public string CodParteAting { get; set; }

        [XmlElement("lateralidade")]
        public Lateralidade Lateralidade { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AgenteCausador")]
    [ComVisible(true)]
#endif
    public class AgenteCausador
    {
        [XmlElement("codAgntCausador")]
        public string CodAgntCausador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Atestado")]
    [ComVisible(true)]
#endif
    public class Atestado
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtAtendimento { get; set; }
#else
        public DateTimeOffset DtAtendimento { get; set; }
#endif

        [XmlElement("dtAtendimento")]
        public string DtAtendimentoField
        {
            get => DtAtendimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAtendimento = DateTime.Parse(value);
#else
            set => DtAtendimento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("hrAtendimento")]
        public string HrAtendimento { get; set; }

        [XmlElement("indInternacao")]
        public SimNaoLetra IndInternacao { get; set; }

        [XmlElement("durTrat")]
        public string DurTrat { get; set; }

        [XmlElement("indAfast")]
        public SimNaoLetra IndAfast { get; set; }

        [XmlElement("dscLesao")]
        public string DscLesao { get; set; }

        [XmlElement("dscCompLesao")]
        public string DscCompLesao { get; set; }

        [XmlElement("diagProvavel")]
        public string DiagProvavel { get; set; }

        [XmlElement("codCID")]
        public string CodCID { get; set; }

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        [XmlElement("emitente")]
        public Emitente Emitente { get; set; }

        public bool ShouldSereializeDscCompLesaoField() => !string.IsNullOrEmpty(DscCompLesao);

        public bool ShouldSereializeDiagProvavelField() => !string.IsNullOrEmpty(DiagProvavel);

        public bool ShouldSereializeObservacaoField() => !string.IsNullOrEmpty(Observacao);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Emitente")]
    [ComVisible(true)]
#endif
    public class Emitente
    {
        [XmlElement("nmEmit")]
        public string NmEmit { get; set; }

        [XmlElement("ideOC")]
        public OrgaoDeClasseMedica IdeOC { get; set; }

        [XmlElement("nrOC")]
        public string NrOC { get; set; }

        [XmlElement("ufOC")]
#if INTEROP
        public UFBrasil UfOC { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfOC { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeUfOCField() => UfOC != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfOCField() => UfOC != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CatOrigem")]
    [ComVisible(true)]
#endif
    public class CatOrigem
    {
        [XmlElement("nrRecCatOrig")]
        public string NrRecCatOrig { get; set; }
    }
}
