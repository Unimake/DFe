#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.GNRE;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2200")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmissao/v_S_01_02_00", IsNullable = false)]
    public class ESocial2200 : XMLBase
    {
        [XmlElement("evtAdmissao")]
        public EvtAdmissao EvtAdmissao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAdmissao")]
    [ComVisible(true)]
#endif
    public class EvtAdmissao
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("trabalhador")]
        public Trabalhador Trabalhador { get; set; }

        [XmlElement("vinculo")]
        public VinculoESocial2200 Vinculo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Trabalhador")]
    [ComVisible(true)]
#endif
    public class Trabalhador
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        [XmlElement("sexo")]
        public TipoSexo Sexo { get; set; }

        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstCiv { get; set; } = (EstadoCivil)(-1);
#else
        public EstadoCivil? EstCiv { get; set; }
#endif

        [XmlElement("grauInstr")]
        public GrauDeInstrucao GrauInstr { get; set; }

        [XmlElement("nmSoc")]
        public string NmSoc { get; set; }

        [XmlElement("nascimento")]
        public Nascimento Nascimento { get; set; }

        [XmlElement("endereco")]
        public Endereco Endereco { get; set; }

        [XmlElement("trabImig")]
        public TrabImig TrabImig { get; set; }

        [XmlElement("infoDeficiencia")]
        public InfoDeficiencia InfoDeficiencia { get; set; }

        [XmlElement("dependente")]
        public Dependente Dependente { get; set; }

        [XmlElement("contato")]
        public Contato Contato { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoMesmoMtvEstCiv() => EstCiv != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeInfoMesmoMtvEstCiv() => EstCiv != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Nascimento")]
    [ComVisible(true)]
#endif
    public class Nascimento
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("paisNascto")]
        public string PaisNascto { get; set; }

        [XmlElement("paisNac")]
        public string PaisNac { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Contato")]
    [ComVisible(true)]
#endif
    public class Contato
    {
        [XmlElement("fonePrinc")]
        public string FonePrinc { get; set; }

        [XmlElement("emailPrinc")]
        public string EmailPrinc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFonePrincField() => !string.IsNullOrEmpty(FonePrinc);

        public bool ShouldSerializeEmailPrincField() => !string.IsNullOrEmpty(EmailPrinc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.VinculoESocial2200")]
    [ComVisible(true)]
#endif
    public class VinculoESocial2200
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }

        [XmlElement("cadIni")]
        public SimNaoLetra CadIni { get; set; }

        [XmlElement("infoRegimeTrab")]
        public InfoRegimeTrabESocial2200 InfoRegimeTrab { get; set; }

        [XmlElement("infoContrato")]
        public InfoContratoESocial2200 InfoContrato { get; set; }

        [XmlElement("sucessaoVinc")]
        public SucessaoVincESocial2200 SucessaoVinc { get; set; }

        [XmlElement("transfDom")]
        public TransfDom TransfDom { get; set; }

        [XmlElement("mudancaCPF")]
        public MudancaCPF MudancaCPF { get; set; }

        [XmlElement("afastamento")]
        public Afastamento Afastamento { get; set; }

        [XmlElement("desligamento")]
        public Desligamento Desligamento { get; set; }

        [XmlElement("cessao")]
        public Cessao Cessao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrabESocial2200")]
    [ComVisible(true)]
#endif
    public class InfoRegimeTrabESocial2200
    {
        [XmlElement("infoCeletista")]
        public InfoCeletistaESocial2200 InfoCeletista { get; set; }

        [XmlElement("infoEstatutario")]
        public InfoEstatutarioESocial2200 InfoEstatutario { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCeletistaESocial2200")]
    [ComVisible(true)]
#endif
    public class InfoCeletistaESocial2200
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdm { get; set; }
#else
        public DateTimeOffset DtAdm { get; set; }
#endif

        [XmlElement("dtAdm")]
        public string DtAdmField
        {
            get => DtAdm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdm = DateTime.Parse(value);
#else
            set => DtAdm = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpAdmissao")]
        public TipoAdmissaoTrabalhador TpAdmissao { get; set; }

        [XmlElement("indAdmissao")]
        public IndicativoDeAdmissao IndAdmissao { get; set; }

        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        [XmlElement("tpRegJor")]
        public RegimeJornadaEmpregado TpRegJor { get; set; }

        [XmlElement("natAtividade")]
        public NatAtividade NatAtividade { get; set; }

        [XmlElement("dtBase")]
        public int DtBase { get; set; }

        [XmlElement("cnpjSindCategProf")]
        public string CnpjSindCategProf { get; set; }

        [XmlElement("matAnotJud")]
        public string MatAnotJud { get; set; }

        [XmlElement("FGTS")]
        public FGTS FGTS { get; set; }

        [XmlElement("trabTemporario")]
        public TrabTemporarioESocial2200 TrabTemporario { get; set; }

        [XmlElement("aprend")]
        public AprendESocial2200 Aprend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrProcTrabField() => !string.IsNullOrEmpty(NrProcTrab);

        public bool ShouldSerializeDtBaseField() => DtBase > 0;

        public bool ShouldSerializeMatAnotJudField() => !string.IsNullOrEmpty(MatAnotJud);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FGTS")]
    [ComVisible(true)]
#endif
    public class FGTS
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtOpcFGTS { get; set; }
#else
        public DateTimeOffset DtOpcFGTS { get; set; }
#endif

        [XmlElement("dtOpcFGTS")]
        public string DtOpcFGTSField
        {
            get => DtOpcFGTS.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtOpcFGTS = DateTime.Parse(value);
#else
            set => DtOpcFGTS = DateTimeOffset.Parse(value);
#endif
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabTemporarioESocial2200")]
    [ComVisible(true)]
#endif
    public class TrabTemporarioESocial2200
    {
        [XmlElement("hipLeg")]
        public ContratacaoTrabalhadorTemporario HipLeg { get; set; }

        [XmlElement("justContr")]
        public string JustContr { get; set; }

        [XmlElement("ideEstabVinc")]
        public IdeEstabVinc IdeEstabVinc { get; set; }

        [XmlElement("ideTrabSubstituido")]
        public List<IdeTrabSubstituido> IdeTrabSubstituido { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeTrabSubstituido(IdeTrabSubstituido item)
        {
            if (IdeTrabSubstituido == null)
            {
                IdeTrabSubstituido = new List<IdeTrabSubstituido>();
            }

            IdeTrabSubstituido.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeTrabSubstituido (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeTrabSubstituido</returns>
        public IdeTrabSubstituido GetIdeTrabSubstituido(int index)
        {
            if ((IdeTrabSubstituido?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeTrabSubstituido[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeTrabSubstituido
        /// </summary>
        public int GetIdeTrabSubstituidoCount => (IdeTrabSubstituido != null ? IdeTrabSubstituido.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabVinc")]
    [ComVisible(true)]
#endif
    public class IdeEstabVinc
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabVinc")]
    [ComVisible(true)]
#endif
    public class IdeTrabSubstituido
    {
        [XmlElement("cpfTrabSubst")]
        public string CpfTrabSubst { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AprendESocial2200")]
    [ComVisible(true)]
#endif
    public class AprendESocial2200
    {
        [XmlElement("indAprend")]
        public IndicativoContratacaoAprendiz IndAprend { get; set; }

        [XmlElement("cnpjEntQual")]
        public string CnpjEntQual { get; set; }

        [XmlElement("tpInsc")]
#if INTEROP
        public TiposInscricao TpInsc { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("cnpjPrat")]
        public string CnpjPrat { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjEntQualField() => !string.IsNullOrEmpty(CnpjEntQual);

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TiposInscricao)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInscField() => !string.IsNullOrEmpty(NrInsc);

        public bool ShouldSerializeCnpjPratField() => !string.IsNullOrEmpty(CnpjPrat);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstatutarioESocial2200")]
    [ComVisible(true)]
#endif
    public class InfoEstatutarioESocial2200
    {
        [XmlElement("tpProv")]
        public TipoProvimento TpProv { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtExercicio { get; set; }
#else
        public DateTimeOffset DtExercicio { get; set; }
#endif

        [XmlElement("dtExercicio")]
        public string DtExercicioField
        {
            get => DtExercicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExercicio = DateTime.Parse(value);
#else
            set => DtExercicio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpPlanRP")]
#if INTEROP
        public PlanoSegregacaoDaMassa TpPlanRP { get; set; } = (PlanoSegregacaoDaMassa)(-1);
#else
        public PlanoSegregacaoDaMassa? TpPlanRP { get; set; }
#endif

        [XmlElement("indTetoRGPS")]
#if INTEROP
        public SimNaoLetra IndTetoRGPS { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndTetoRGPS { get; set; }
#endif

        [XmlElement("indAbonoPerm")]
#if INTEROP
        public SimNaoLetra IndAbonoPerm { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndAbonoPerm { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DtIniAbono { get; set; }
#else
        public DateTimeOffset DtIniAbono { get; set; }
#endif

        [XmlElement("dtIniAbono")]
        public string DtIniAbonoField
        {
            get => DtIniAbono.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniAbono = DateTime.Parse(value);
#else
            set => DtIniAbono = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpPlanRP() => TpPlanRP != (PlanoSegregacaoDaMassa)(-1);
#else
        public bool ShouldSerializeTpPlanRP() => TpPlanRP != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndTetoRGPS() => IndTetoRGPS != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndTetoRGPS() => IndTetoRGPS != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndAbonoPerm() => IndAbonoPerm != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndAbonoPerm() => IndAbonoPerm != null;
#endif

        public bool ShouldSerializeDtIniAbonoField() => DtIniAbono > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContratoESocial2200")]
    [ComVisible(true)]
#endif
    public class InfoContratoESocial2200
    {
        [XmlElement("nmCargo")]
        public string NmCargo { get; set; }

        [XmlElement("CBOCargo")]
        public string CBOCargo { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIngrCargo { get; set; }
#else
        public DateTimeOffset DtIngrCargo { get; set; }
#endif

        [XmlElement("dtIngrCargo")]
        public string DtIngrCargoField
        {
            get => DtIngrCargo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIngrCargo = DateTime.Parse(value);
#else
            set => DtIngrCargo = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nmFuncao")]
        public string NmFuncao { get; set; }

        [XmlElement("CBOFuncao")]
        public string CBOFuncao { get; set; }

        [XmlElement("acumCargo")]
#if INTEROP
        public SimNaoLetra AcumCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? AcumCargo { get; set; }
#endif

        [XmlElement("codCateg")]
        public string CodCateg { get; set; }

        [XmlElement("remuneracao")]
        public RemuneracaoESocial2200 Remuneracao { get; set; }

        [XmlElement("duracao")]
        public DuracaoESocial2200 Duracao { get; set; }

        [XmlElement("localTrabalho")]
        public LocalTrabalhoESocial2200 LocalTrabalho { get; set; }

        [XmlElement("horContratual")]
        public HorContratualESocial2200 HorContratual { get; set; }

        [XmlElement("alvaraJudicial")]
        public AlvaraJudicialESocial2200 AlvaraJudicial { get; set; }

        [XmlElement("observacoes")]
        public List<ObservacoesESocial2200> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(ObservacoesESocial2200 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<ObservacoesESocial2200>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public ObservacoesESocial2200 GetObservacoes(int index)
        {
            if ((Observacoes?.Count ?? 0) == 0)
            {
                return default;
            };

            return Observacoes[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Observacoes
        /// </summary>
        public int GetObservacoesCount => (Observacoes != null ? Observacoes.Count : 0);
#endif

        [XmlElement("treiCap")]
        public List<TreiCapESocial2200> TreiCap { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTreiCap(TreiCapESocial2200 item)
        {
            if (TreiCap == null)
            {
                TreiCap = new List<TreiCapESocial2200>();
            }

            TreiCap.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TreiCap (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TreiCap</returns>
        public TreiCapESocial2200 GetTreiCap(int index)
        {
            if ((TreiCap?.Count ?? 0) == 0)
            {
                return default;
            };

            return TreiCap[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TreiCap
        /// </summary>
        public int GetTreiCapCount => (TreiCap != null ? TreiCap.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNmCargoField() => !string.IsNullOrEmpty(NmCargo);
       
        public bool ShouldSerializeCBOCargoField() => !string.IsNullOrEmpty(CBOCargo);

        public bool ShouldSerializeDtIngrCargoField() => DtIngrCargo > DateTime.MinValue;

        public bool ShouldSerializeNmFuncaoField() => !string.IsNullOrEmpty(NmFuncao);
       
        public bool ShouldSerializeCBOFuncaoField() => !string.IsNullOrEmpty(CBOFuncao);

#if INTEROP
        public bool ShouldSerializeAcumCargo() => AcumCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeAcumCargo() => AcumCargo != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemuneracaoESocial2200")]
    [ComVisible(true)]
#endif
    public class RemuneracaoESocial2200
    {
        [XmlElement("vrSalFx")]
        public double VrSalFx { get; set; }

        [XmlElement("undSalFixo")]
        public UnidadeSalarioFixo UndSalFixo { get; set; }

        [XmlElement("dscSalVar")]
        public string DscSalVar { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscSalVarField() => !string.IsNullOrEmpty(DscSalVar);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DuracaoESocial2200")]
    [ComVisible(true)]
#endif
    public class DuracaoESocial2200
    {
        [XmlElement("tpContr")]
        public TipoDeContratoDeTrabalho TpContr { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtTerm { get; set; }
#else
        public DateTimeOffset DtTerm { get; set; }
#endif

        [XmlElement("dtTerm")]
        public string DtTermField
        {
            get => DtTerm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTerm = DateTime.Parse(value);
#else
            set => DtTerm = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("clauAssec")]
#if INTEROP
        public SimNaoLetra ClauAssec { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? ClauAssec { get; set; }
#endif

        [XmlElement("objDet")]
        public string ObjDet { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtTermField() => DtTerm > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeClauAssec() => ClauAssec != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeClauAssec() => ClauAssec != null;
#endif

        public bool ShouldSerializeObjDetField() => !string.IsNullOrEmpty(ObjDet);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabalhoESocial2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabalhoESocial2200
    {
        [XmlElement("localTrabGeral")]
        public LocalTrabGeralESocial2200 LocalTrabGeral { get; set; }

        [XmlElement("localTempDom")]
        public LocalTempDomESocial2200 LocalTempDom { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeralESocial2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabGeralESocial2200
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("descComp")]
        public string DescComp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDescCompField() => !string.IsNullOrEmpty(DescComp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTempDomESocial2200")]
    [ComVisible(true)]
#endif
    public class LocalTempDomESocial2200 : Brasil { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.HorContratualESocial2200")]
    [ComVisible(true)]
#endif
    public class HorContratualESocial2200
    {
        [XmlElement("qtdHrsSem")]
        public string QtdHrsSem { get; set; }

        [XmlElement("tpJornada")]
        public TipoDeJornada TpJornada { get; set; }

        [XmlElement("tmpParc")]
        public ContratoTempoParcial TmpParc { get; set; }

        [XmlElement("horNoturno")]
#if INTEROP
        public SimNaoLetra HorNoturno { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? HorNoturno { get; set; }
#endif

        [XmlElement("dscJorn")]
        public string DscJorn { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQtdHrsSemField() => !string.IsNullOrEmpty(QtdHrsSem);

#if INTEROP
        public bool ShouldSerializeHorNoturno() => HorNoturno != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeHorNoturno() => HorNoturno != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AlvaraJudicialESocial2200")]
    [ComVisible(true)]
#endif
    public class AlvaraJudicialESocial2200
    {
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ObservacoesESocial2200")]
    [ComVisible(true)]
#endif
    public class ObservacoesESocial2200
    {
        [XmlElement("observacao")]
        public string Observacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TreiCapESocial2200")]
    [ComVisible(true)]
#endif
    public class TreiCapESocial2200
    {
        [XmlElement("codTreiCap")]
        public string CodTreiCap { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVincESocial2200")]
    [ComVisible(true)]
#endif
    public class SucessaoVincESocial2200
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

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

        public bool ShouldSerializeMatricAntField() => !string.IsNullOrEmpty(MatricAnt);

        public bool ShouldSerializeObservacaoField() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TransfDom")]
    [ComVisible(true)]
#endif
    public class TransfDom
    {
        [XmlElement("cpfSubstituido")]
        public string CpfSubstituido { get; set; }

        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeMatricAntField() => !string.IsNullOrEmpty(MatricAnt);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPF")]
    [ComVisible(true)]
#endif
    public class MudancaCPF
    {
        [XmlElement("cpfAnt")]
        public string CpfAnt { get; set; }

        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

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

        public bool ShouldSerializeObservacaoField() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Afastamento")]
    [ComVisible(true)]
#endif
    public class Afastamento
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Desligamento")]
    [ComVisible(true)]
#endif
    public class Desligamento
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtDeslig { get; set; }
#else
        public DateTimeOffset DtDeslig { get; set; }
#endif

        [XmlElement("dtDeslig")]
        public string DtDesligField
        {
            get => DtDeslig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDeslig = DateTime.Parse(value);
#else
            set => DtDeslig = DateTimeOffset.Parse(value);
#endif
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Cessao")]
    [ComVisible(true)]
#endif
    public class Cessao
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
    }
}
