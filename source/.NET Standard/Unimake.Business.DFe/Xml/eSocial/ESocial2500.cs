#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2500")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtProcTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial2500 : XMLBase
    {
        [XmlElement("evtProcTrab")]
        public EvtProcTrab EvtProcTrab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtProcTrab")]
    [ComVisible(true)]
#endif
    public class EvtProcTrab
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2500 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador2500 IdeEmpregador { get; set; }

        [XmlElement("infoProcesso")]
        public InfoProcesso InfoProcesso { get; set; }

        [XmlElement("ideTrab")]
        public IdeTrab IdeTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2500")]
    [ComVisible(true)]
#endif
    public class IdeEvento2500 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador2500")]
    [ComVisible(true)]
#endif
    public class IdeEmpregador2500 : IdeEmpregador
    {
        [XmlElement("ideResp")]
        public IdeResp IdeResp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeResp")]
    [ComVisible(true)]
#endif
    public class IdeResp : IdeEmpregador { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcesso")]
    [ComVisible(true)]
#endif
    public class InfoProcesso
    {
        [XmlElement("origem")]
        public Origem Origem { get; set; }

        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        [XmlElement("obsProcTrab")]
        public string ObsProcTrab { get; set; }

        [XmlElement("dadosCompl")]
        public DadosCompl DadosCompl { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObsProcTrab() => !string.IsNullOrEmpty(ObsProcTrab);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosCompl")]
    [ComVisible(true)]
#endif
    public class DadosCompl
    {

        [XmlElement("infoProcJud")]
        public InfoProcJud InfoProcJud { get; set; }

        [XmlElement("infoCCP")]
        public InfoCCP InfoCCP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJud")]
    [ComVisible(true)]
#endif
    public class InfoProcJud
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtSent { get; set; }
#else
        public DateTimeOffset DtSent { get; set; }
#endif

        [XmlElement("dtSent")]
        public string DtSentField
        {
            get => DtSent.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtSent = DateTime.Parse(value);
#else
            set => DtSent = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("ufVara")]
        public UFBrasil UfVara { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("idVara")]
        public string IdVara { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCCP")]
    [ComVisible(true)]
#endif
    public class InfoCCP
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtCCP { get; set; }
#else
        public DateTimeOffset DtCCP { get; set; }
#endif

        [XmlElement("dtCCP")]
        public string DtCCPField
        {
            get => DtCCP.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtCCP = DateTime.Parse(value);
#else
            set => DtCCP = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpCCP")]
        public TpCCP TpCCP { get; set; }

        [XmlElement("cnpjCCP")]
        public string CnpjCCP { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjCCP() => !string.IsNullOrEmpty(CnpjCCP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrab")]
    [ComVisible(true)]
#endif
    public class IdeTrab
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

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

        [XmlElement("infoContr")]
        public List<InfoContr> InfoContr { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoContr(InfoContr item)
        {
            if (InfoContr == null)
            {
                InfoContr = new List<InfoContr>();
            }

            InfoContr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoContr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoContr</returns>
        public InfoContr GetInfoContr(int index)
        {
            if ((InfoContr?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoContr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoContr
        /// </summary>
        public int GetInfoContrCount => (InfoContr != null ? InfoContr.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNmTrab() => !string.IsNullOrEmpty(NmTrab);

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContr")]
    [ComVisible(true)]
#endif
    public class InfoContr
    {
        [XmlElement("tpContr")]
        public TpContr TpContr { get; set; }

        [XmlElement("indContr")]
        public SimNaoLetra IndContr { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtAdmOrig { get; set; }
#else
        public DateTimeOffset DtAdmOrig { get; set; }
#endif

        [XmlElement("dtAdmOrig")]
        public string DtAdmOrigField
        {
            get => DtAdmOrig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdmOrig = DateTime.Parse(value);
#else
            set => DtAdmOrig = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indReint")]
#if INTEROP
        public SimNaoLetra IndReint { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndReint { get; set; }
#endif

        [XmlElement("indCateg")]
        public SimNaoLetra IndCateg { get; set; }

        [XmlElement("indNatAtiv")]
        public SimNaoLetra IndNatAtiv { get; set; }

        [XmlElement("indMotDeslig")]
        public SimNaoLetra IndMotDeslig { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

        [XmlElement("dtInicio")]
        public string DtInicioField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("infoCompl")]
        public InfoCompl InfoCompl { get; set; }

        [XmlElement("mudCategAtiv")]
        public List<MudCategAtiv> MudCategAtiv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddMudCategAtiv(MudCategAtiv item)
        {
            if (MudCategAtiv == null)
            {
                MudCategAtiv = new List<MudCategAtiv>();
            }

            MudCategAtiv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista MudCategAtiv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da MudCategAtiv</returns>
        public MudCategAtiv GetMudCategAtiv(int index)
        {
            if ((MudCategAtiv?.Count ?? 0) == 0)
            {
                return default;
            };

            return MudCategAtiv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista MudCategAtiv
        /// </summary>
        public int GetMudCategAtivCount => (MudCategAtiv != null ? MudCategAtiv.Count : 0);
#endif

        [XmlElement("unicContr")]
        public List<UnicContr> UnicContr { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddUnicContr(UnicContr item)
        {
            if (UnicContr == null)
            {
                UnicContr = new List<UnicContr>();
            }

            UnicContr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista UnicContr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da UnicContr</returns>
        public UnicContr GetUnicContr(int index)
        {
            if ((UnicContr?.Count ?? 0) == 0)
            {
                return default;
            };

            return UnicContr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista UnicContr
        /// </summary>
        public int GetUnicContrCount => (UnicContr != null ? UnicContr.Count : 0);
#endif

        [XmlElement("ideEstab")]
        public IdeEstab2500 IdeEstab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtAdmOrigField() => DtAdmOrig > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeIndReint() => IndReint != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndReint() => IndReint != null;
#endif

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif
        public bool ShouldSerializeDtInicioField() => DtInicio > DateTime.MinValue;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCompl")]
    [ComVisible(true)]
#endif
    public class InfoCompl
    {
        [XmlElement("codCBO")]
        public string CodCBO { get; set; }

        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        [XmlElement("remuneracao")]
        public List<Remuneracao2500> Remuneracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemuneracao(Remuneracao2500 item)
        {
            if (Remuneracao == null)
            {
                Remuneracao = new List<Remuneracao2500>();
            }

            Remuneracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Remuneracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Remuneracao</returns>
        public Remuneracao2500 GetRemuneracao(int index)
        {
            if ((Remuneracao?.Count ?? 0) == 0)
            {
                return default;
            };

            return Remuneracao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Remuneracao
        /// </summary>
        public int GetRemuneracaoCount => (Remuneracao != null ? Remuneracao.Count : 0);
#endif

        [XmlElement("infoVinc")]
        public InfoVinc InfoVinc { get; set; }

        [XmlElement("infoTerm")]
        public InfoTerm InfoTerm { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodCBO() => !string.IsNullOrEmpty(CodCBO);

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2500")]
    [ComVisible(true)]
#endif
    public class Remuneracao2500
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtRemun { get; set; }
#else
        public DateTimeOffset DtRemun { get; set; }
#endif

        [XmlElement("dtRemun")]
        public string DtRemunField
        {
            get => DtRemun.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtRemun = DateTime.Parse(value);
#else
            set => DtRemun = DateTimeOffset.Parse(value);
#endif
        }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoVinc")]
    [ComVisible(true)]
#endif
    public class InfoVinc
    {
        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; }

        /// <summary>
        /// 1 - Regime Geral de Previdência Social - RGPS
        /// 2 - Regime Próprio de Previdência Social - RPPS
        /// 3 - Regime de Previdência Social no exterior
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }

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

        [XmlElement("tmpParc")]
#if INTEROP
        public TmpParc TmpParc { get; set; } = (TmpParc)(-1);
#else
        public TmpParc? TmpParc { get; set; }
#endif

        [XmlElement("duracao")]
        public Duracao2500 Duracao { get; set; }

        [XmlElement("observacoes")]
        public List<Observacoes2500> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(Observacoes2500 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<Observacoes2500>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes2500 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public Observacoes2500 GetObservacoes(int index)
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

        [XmlElement("sucessaoVinc")]
        public SucessaoVinc2500 SucessaoVinc { get; set; }

        [XmlElement("infoDeslig")]
        public InfoDeslig2500 InfoDeslig { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTmpParc() => TmpParc != (TmpParc)(-1);
#else
        public bool ShouldSerializeTmpParc() => TmpParc != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Duracao2500")]
    [ComVisible(true)]
#endif
    public class Duracao2500
    {
        [XmlElement("tpContr")]
        public TipoDeContratoDeTrabalho TipoDeContratoDeTrabalho { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes2500")]
    [ComVisible(true)]
#endif
    public class Observacoes2500 : Observacoes2306 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc2500")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc2500
    {
        /// <summary>
        /// Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// 5 - CGC
        /// 6 - CEI
        /// </summary>
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

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeslig2500")]
    [ComVisible(true)]
#endif
    public class InfoDeslig2500
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

        [XmlElement("mtvDeslig")]
        public string MtvDeslig { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtProjFimAPI { get; set; }
#else
        public DateTimeOffset DtProjFimAPI { get; set; }
#endif

        [XmlElement("dtProjFimAPI")]
        public string DtProjFimAPIField
        {
            get => DtProjFimAPI.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtProjFimAPI = DateTime.Parse(value);
#else
            set => DtProjFimAPI = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("pensAlim")]
#if INTEROP
        public PensAlim PensAlim { get; set; } = (PensAlim)(-1);
#else
        public PensAlim? PensAlim { get; set; }
#endif
        /// <summary>
        /// Percentual a ser destinado a pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero) e menor ou igual a 100 (cem).
        /// Informação obrigatória e exclusiva se pensAlim = [1, 3].
        /// </summary>
        [XmlIgnore]
        public double PercAliment { get; set; }

        [XmlElement("percAliment")]
        public string PercAlimentField
        {
            get => PercAliment.ToString("F2", CultureInfo.InvariantCulture);
            set => PercAliment = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// Informação obrigatória e exclusiva se pensAlim = [2, 3].
        /// </summary>
        [XmlIgnore]
        public double VrAlim { get; set; }

        [XmlElement("vrAlim")]
        public string VrAlimField
        {
            get => VrAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VrAlim = Converter.ToDouble(value);
        }

        #region ShouldSerialize
        public bool ShouldSerializeDtProjFimAPIField() => DtProjFimAPI > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializePensAlim() => PensAlim != (PensAlim)(-1);
#else
        public bool ShouldSerializePensAlim() => PensAlim != null;
#endif

        public bool ShouldSerializePercAlimentField() => PercAliment > 0;

        public bool ShouldSerializeVrAlimField() => VrAlim > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTerm")]
    [ComVisible(true)]
#endif
    public class InfoTerm
    {
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

        [XmlElement("mtvDesligTSV")]
#if INTEROP
        public MtvDesligTSV MtvDesligTSV { get; set; } = (MtvDesligTSV)(-1);
#else
        public MtvDesligTSV? MtvDesligTSV { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeMtvDesligTSV() => MtvDesligTSV != (MtvDesligTSV)(-1);
#else
        public bool ShouldSerializeMtvDesligTSV() => MtvDesligTSV != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudCategAtiv")]
    [ComVisible(true)]
#endif
    public class MudCategAtiv
    {
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DtMudCategAtiv { get; set; }
#else
        public DateTimeOffset DtMudCategAtiv { get; set; }
#endif

        [XmlElement("dtMudCategAtiv")]
        public string DtMudCategAtivField
        {
            get => DtMudCategAtiv.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtMudCategAtiv = DateTime.Parse(value);
#else
            set => DtMudCategAtiv = DateTimeOffset.Parse(value);
#endif
        }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.UnicContr")]
    [ComVisible(true)]
#endif
    public class UnicContr
    {
        [XmlElement("matUnic")]
        public string MatUnic { get; set; }

        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

        [XmlElement("dtInicio")]
        public string DtInicioField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeMatUnic() => !string.IsNullOrEmpty(MatUnic);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        public bool ShouldSerializeDtInicioField() => DtInicio > DateTime.MinValue;

#endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab2500")]
    [ComVisible(true)]
#endif
    public class IdeEstab2500
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("infoVlr")]
        public InfoVlr InfoVlr { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoVlr")]
    [ComVisible(true)]
#endif
    public class InfoVlr
    {
        [XmlIgnore]
#if INTEROP
        public DateTime CompIni { get; set; }
#else
        public DateTimeOffset CompIni { get; set; }
#endif

        [XmlElement("compIni")]
        public string CompIniField
        {
            get => CompIni.ToString("yyyy-MM");
#if INTEROP
            set => CompIni = DateTime.Parse(value);
#else
            set => CompIni = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime CompFim { get; set; }
#else
        public DateTimeOffset CompFim { get; set; }
#endif

        [XmlElement("compFim")]
        public string CompFimField
        {
            get => CompFim.ToString("yyyy-MM");
#if INTEROP
            set => CompFim = DateTime.Parse(value);
#else
            set => CompFim = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indReperc")]
        public IndReperc IndReperc { get; set; }

        [XmlElement("indenSD")]
#if INTEROP
        public SimNaoLetra IndenSD { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndenSD { get; set; }
#endif

        [XmlElement("indenAbono")]
#if INTEROP
        public SimNaoLetra IndenAbono { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndenAbono { get; set; }
#endif

        [XmlElement("abono")]
        public List<Abono> Abono { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddAbono(Abono item)
        {
            if (Abono == null)
            {
                Abono = new List<Abono>();
            }

            Abono.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Abono (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Abono</returns>
        public Abono GetAbono(int index)
        {
            if ((Abono?.Count ?? 0) == 0)
            {
                return default;
            };

            return Abono[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Abono
        /// </summary>
        public int GetAbonoCount => (Abono != null ? Abono.Count : 0);
#endif

        [XmlElement("idePeriodo")]
        public List<IdePeriodo2500> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo2500 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo2500>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo</returns>
        public IdePeriodo2500 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndenSD() => IndenSD != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndenSD() => IndenSD != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndenAbono() => IndenAbono != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndenAbono() => IndenAbono != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Abono")]
    [ComVisible(true)]
#endif
    public class Abono
    {
        [XmlElement("anoBase")]
        public string AnoBase { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo2500")]
    [ComVisible(true)]
#endif
    public class IdePeriodo2500
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlElement("perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("baseCalculo")]
        public BaseCalculo BaseCalculo { get; set; }

        [XmlElement("infoFGTS")]
        public InfoFGTS InfoFGTS { get; set; }

        [XmlElement("baseMudCateg")]
        public BaseMudCateg BaseMudCateg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BaseCalculo")]
    [ComVisible(true)]
#endif
    public class BaseCalculo
    {
        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração mensal do trabalhador.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCpMensal { get; set; }

        [XmlElement("vrBcCpMensal")]
        public string VrBcCpMensalField
        {
            get => VrBcCpMensal.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCpMensal = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração do trabalhador referente ao 13º salário.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCp13 { get; set; }
        [XmlElement("vrBcCp13")]

        public string VrBcCp13Field
        {
            get => VrBcCp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp13 = Converter.ToDouble(value);
        }

        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo2500 InfoAgNocivo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcCp13() => VrBcCp13 > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo2500")]
    [ComVisible(true)]
#endif
    public class InfoAgNocivo2500 : InfoAgNocivo1200 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTS")]
    [ComVisible(true)]
#endif
    public class InfoFGTS
    {
        /// <summary>
        /// Valor da base de cálculo de FGTS ainda não declarada em SEFIP ou no
        /// eSocial, inclusive de verba reconhecida no processo trabalhista.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcFGTSProcTrab { get; set; }

        [XmlElement("vrBcFGTSProcTrab")]
        public string VrBcFGTSProcTrabField
        {
            get => VrBcFGTSProcTrab.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSProcTrab = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo de FGTS declarada apenas em SEFIP
        /// (não informada no eSocial) e ainda não recolhida.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcFGTSSefip { get; set; }

        [XmlElement("vrBcFGTSSefip")]
        public string VrBcFGTSSefipField
        {
            get => VrBcFGTSSefip.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSSefip = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo de FGTS declarada anteriormente no eSocial e ainda não recolhida.
        /// Validação: Somente pode ser informado se perRef for anterior ao início do FGTS Digital.
        /// Deve ser maior que 0 (zero).
        /// </summary>

        [XmlIgnore]
        public double VrBcFGTSDecAnt { get; set; }

        [XmlElement("vrBcFGTSDecAnt")]
        public string VrBcFGTSDeAntField
        {
            get => VrBcFGTSDecAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFGTSDecAnt = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcFGTSSefipField() => VrBcFGTSSefip > 0;

        public bool ShouldSerializeVrBcFGTSDecAntField() => VrBcFGTSDecAnt > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BaseMudCateg")]
    [ComVisible(true)]
#endif
    public class BaseMudCateg
    {
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Valor da remuneração do trabalhador a ser considerada para fins previdenciários
        /// declarada em GFIP ou em S-1200 de trabalhador sem cadastro no S-2300.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCPrev { get; set; }

        [XmlElement("vrBcCPrev")]
        public string VrBcCPrevField
        {
            get => VrBcCPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCPrev = Converter.ToDouble(value);
        }
    }

}
