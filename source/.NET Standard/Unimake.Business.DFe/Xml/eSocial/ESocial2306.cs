#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2306")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVAltContr/v_S_01_02_00", IsNullable = false)]
    public class ESocial2306 : XMLBase
    {
        [XmlElement("evtTSVAltContr")]
        public EvtTSVAltContr EvtTSVAltContr { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTSVAltContr")]
    [ComVisible(true)]
#endif
    public class EvtTSVAltContr
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2306 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabSemVinculo")]
        public IdeTrabSemVinculo IdeTrabSemVinculo { get; set; }

        [XmlElement("infoTSVAlteracao")]
        public InfoTSVAlteracao InfoTSVAlteracao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2306")]
    [ComVisible(true)]
#endif
    public class IdeEvento2306 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabSemVinculo")]
    [ComVisible(true)]
#endif
    public class IdeTrabSemVinculo
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTSVAlteracao")]
    [ComVisible(true)]
#endif
    public class InfoTSVAlteracao
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtAlteracao { get; set; }
#else
        public DateTimeOffset DtAlteracao { get; set; }
#endif

        [XmlElement("dtAlteracao")]
        public string DtAlteracaoField
        {
            get => DtAlteracao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAlteracao = DateTime.Parse(value);
#else
            set => DtAlteracao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif


        [XmlElement("infoComplementares")]
        public InfoComplementares InfoComplementares { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplementares")]
    [ComVisible(true)]
#endif
    public class InfoComplementares
    {
        [XmlElement("cargoFuncao")]
        public CargoFuncao CargoFuncao { get; set; }

        [XmlElement("remuneracao")]
        public Remuneracao Remuneracao { get; set; }

        [XmlElement("infoDirigenteSindical")]
        public InfoDirigenteSindicalESocial2306 InfoDirigenteSindical { get; set; }

        [XmlElement("infoTrabCedido")]
        public InfoTrabCedidoESocial2306 InfoTrabCedido { get; set; }

        [XmlElement("infoMandElet")]
        public InfoMandEletESocial2306 InfoMandElet { get; set; }

        [XmlElement("infoEstagiario")]
        public InfoEstagiarioESocial2306 InfoEstagiario { get; set; }

        [XmlElement("localTrabGeral")]
        public LocalTrabGeral LocalTrabGeral {  get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDirigenteSindicalESocial2306")]
    [ComVisible(true)]
#endif
    public class InfoDirigenteSindicalESocial2306
    {
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabCedidoESocial2306")]
    [ComVisible(true)]
#endif
    public class InfoTrabCedidoESocial2306
    {
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev {  get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandEletESocial2306")]
    [ComVisible(true)]
#endif
    public class InfoMandEletESocial2306
    {
        [XmlElement("indRemunCargo")]
#if INTEROP
        public SimNaoLetra IndRemunCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRemunCargo { get; set; }
#endif

        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstagiarioESocial2306")]
    [ComVisible(true)]
#endif
    public class InfoEstagiarioESocial2306
    {
        [XmlElement("natEstagio")]
        public NatEstagio NatEstagio { get; set; }

        [XmlElement("nivEstagio")]
#if INTEROP
        public NivEstagio NivEstagio { get; set; } = (NivEstagio)(-1);
#else
        public NivEstagio? NivEstagio { get; set; }
#endif

        [XmlElement("areaAtuacao")]
        public string AreaAtuacao { get; set; }

        [XmlElement("nrApol")]
        public string NrApol { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtPrevTerm { get; set; }
#else
        public DateTimeOffset DtPrevTerm { get; set; }
#endif

        [XmlElement("dtPrevTerm")]
        public string DtPrevTermField
        {
            get => DtPrevTerm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPrevTerm = DateTime.Parse(value);
#else
            set => DtPrevTerm = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("instEnsino")]
        public InstEnsinoESocial2306 InstEnsino { get; set; }

        [XmlElement("ageIntegracao")]
        public AgeIntegracao AgeIntegracao { get; set; }

        [XmlElement("supervisorEstagio")]
        public SupervisorEstagioESocial2306 SupervisorEstagio { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNivEstagio() => NivEstagio != (NivEstagio)(-1);
#else
        public bool ShouldSerializeNivEstagio() => NivEstagio != null;
#endif

        public bool ShouldSerializeAreaAtuacao() => !string.IsNullOrEmpty(AreaAtuacao);
      
        public bool ShouldSerializeNrApol() => !string.IsNullOrEmpty(NrApol);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InstEnsinoESocial2306")]
    [ComVisible(true)]
#endif
    public class InstEnsinoESocial2306
    {
        [XmlElement("cnpjInstEnsino")]
        public string CnpjInstEnsino { get; set; }

        [XmlElement("nmRazao")]
        public string NmRazao { get; set; }

        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeCnpjInstEnsino() => !string.IsNullOrEmpty(CnpjInstEnsino);
      
        public bool ShouldSerializeNmRazao() => !string.IsNullOrEmpty(NmRazao);
     
        public bool ShouldSerializeDscLograd() => !string.IsNullOrEmpty(DscLograd);
     
        public bool ShouldSerializeNrLograd() => !string.IsNullOrEmpty(NrLograd);
   
        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);
        
        public bool ShouldSerializeCep() => !string.IsNullOrEmpty(Cep);
        
        public bool ShouldSerializeCodMunic() => !string.IsNullOrEmpty(CodMunic);

#if INTEROP
        public bool ShouldSerializeUf() => Uf != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUf() => Uf != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SupervisorEstagioESocial2306")]
    [ComVisible(true)]
#endif
    public class SupervisorEstagioESocial2306
    {
        [XmlElement("cpfSupervisor")]
        public string CpfSupervisor { get; set; }
    }
}
