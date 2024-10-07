#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;
using Unimake.Business.DFe.Utility;

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

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
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
        public IdeEvento2200 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("trabalhador")]
        public Trabalhador Trabalhador { get; set; }

        [XmlElement("vinculo")]
        public Vinculo2200 Vinculo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2200")]
    [ComVisible(true)]
#endif
    public class IdeEvento2200 : IdeEvento2205 { }

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
        public List<Dependente> Dependente { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente GetDependente(int index)
        {
            if ((Dependente?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dependente[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dependente
        /// </summary>
        public int GetDependenteCount => (Dependente != null ? Dependente.Count : 0);
#endif
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

        public bool ShouldSerializeFonePrinc() => FonePrinc.HasOnlyNumbers() && FonePrinc.Length >= 8;

        public bool ShouldSerializeEmailPrinc() => !string.IsNullOrEmpty(EmailPrinc) &&
                                                         EmailPrinc.Contains("@")   &&
                                                         EmailPrinc.Contains(".") &&
                                                        !EmailPrinc.StartsWith("@") &&
                                                        !EmailPrinc.EndsWith("@") &&
                                                        !EmailPrinc.StartsWith(".") &&
                                                        !EmailPrinc.EndsWith(".");

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Vinculo2200")]
    [ComVisible(true)]
#endif
    public class Vinculo2200
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
        public InfoRegimeTrab2200 InfoRegimeTrab { get; set; }

        [XmlElement("infoContrato")]
        public InfoContrato2200 InfoContrato { get; set; }

        [XmlElement("sucessaoVinc")]
        public SucessaoVinc2200 SucessaoVinc { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrab2200")]
    [ComVisible(true)]
#endif
    public class InfoRegimeTrab2200
    {
        [XmlElement("infoCeletista")]
        public InfoCeletista2200 InfoCeletista { get; set; }

        [XmlElement("infoEstatutario")]
        public InfoEstatutario2200 InfoEstatutario { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCeletista2200")]
    [ComVisible(true)]
#endif
    public class InfoCeletista2200
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
        public TrabTemporario2200 TrabTemporario { get; set; }

        [XmlElement("aprend")]
        public AprendESocial2200 Aprend { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrProcTrab() => !string.IsNullOrEmpty(NrProcTrab);

        public bool ShouldSerializeDtBase() => DtBase > 0;

        public bool ShouldSerializeMatAnotJud() => !string.IsNullOrEmpty(MatAnotJud);

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabTemporario2200")]
    [ComVisible(true)]
#endif
    public class TrabTemporario2200
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabSubstituido")]
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

        public bool ShouldSerializeCnpjEntQual() => !string.IsNullOrEmpty(CnpjEntQual);

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TiposInscricao)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        public bool ShouldSerializeCnpjPrat() => !string.IsNullOrEmpty(CnpjPrat);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstatutario2200")]
    [ComVisible(true)]
#endif
    public class InfoEstatutario2200
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContrato2200")]
    [ComVisible(true)]
#endif
    public class InfoContrato2200
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
        public Remuneracao2200 Remuneracao { get; set; }

        [XmlElement("duracao")]
        public Duracao2200 Duracao { get; set; }

        [XmlElement("localTrabalho")]
        public LocalTrabalho2200 LocalTrabalho { get; set; }

        [XmlElement("horContratual")]
        public HorContratual2200 HorContratual { get; set; }

        [XmlElement("alvaraJudicial")]
        public AlvaraJudicial2200 AlvaraJudicial { get; set; }

        [XmlElement("observacoes")]
        public List<Observacoes2200> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(Observacoes2200 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<Observacoes2200>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public Observacoes2200 GetObservacoes(int index)
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

        public bool ShouldSerializeNmCargo() => !string.IsNullOrEmpty(NmCargo);

        public bool ShouldSerializeCBOCargo() => !string.IsNullOrEmpty(CBOCargo);

        public bool ShouldSerializeDtIngrCargoField() => DtIngrCargo > DateTime.MinValue;

        public bool ShouldSerializeNmFuncao() => !string.IsNullOrEmpty(NmFuncao);

        public bool ShouldSerializeCBOFuncao() => !string.IsNullOrEmpty(CBOFuncao);

#if INTEROP
        public bool ShouldSerializeAcumCargo() => AcumCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeAcumCargo() => AcumCargo != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2200")]
    [ComVisible(true)]
#endif
    public class Remuneracao2200
    {
        /// <summary>
        /// Salário base do trabalhador, correspondente à parte fixa da remuneração.
        /// Validação: Se undSalFixo for igual a[7], preencher com 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrSalFx { get; set; }
        [XmlElement("vrSalFx")]
        public string VrSalFxField
        {
            get => VrSalFx.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSalFx = Converter.ToDouble(value);
        }

        [XmlElement("undSalFixo")]
        public UndSalFixo UndSalFixo { get; set; }

        [XmlElement("dscSalVar")]
        public string DscSalVar { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscSalVar() => !string.IsNullOrEmpty(DscSalVar);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Duracao2200")]
    [ComVisible(true)]
#endif
    public class Duracao2200
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

        public bool ShouldSerializeObjDet() => !string.IsNullOrEmpty(ObjDet);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabalho2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabalho2200
    {
        [XmlElement("localTrabGeral")]
        public LocalTrabGeral2200 LocalTrabGeral { get; set; }

        [XmlElement("localTempDom")]
        public LocalTempDom2200 LocalTempDom { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2200")]
    [ComVisible(true)]
#endif
    public class LocalTrabGeral2200
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("descComp")]
        public string DescComp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDescComp() => !string.IsNullOrEmpty(DescComp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTempDom2200")]
    [ComVisible(true)]
#endif
    public class LocalTempDom2200 : Brasil { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.HorContratual2200")]
    [ComVisible(true)]
#endif
    public class HorContratual2200
    {
        [XmlElement("qtdHrsSem")]
        public string QtdHrsSem { get; set; }

        [XmlElement("tpJornada")]
        public TpJornada TpJornada { get; set; }

        [XmlElement("tmpParc")]
        public TmpParc TmpParc { get; set; }

        [XmlElement("horNoturno")]
#if INTEROP
        public SimNaoLetra HorNoturno { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? HorNoturno { get; set; }
#endif

        [XmlElement("dscJorn")]
        public string DscJorn { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQtdHrsSem() => !string.IsNullOrEmpty(QtdHrsSem);

#if INTEROP
        public bool ShouldSerializeHorNoturno() => HorNoturno != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeHorNoturno() => HorNoturno != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AlvaraJudicial2200")]
    [ComVisible(true)]
#endif
    public class AlvaraJudicial2200
    {
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes2200")]
    [ComVisible(true)]
#endif
    public class Observacoes2200
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc2200")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc2200
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

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

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

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

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

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

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
