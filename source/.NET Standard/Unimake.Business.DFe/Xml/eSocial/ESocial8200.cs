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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial8200")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAnotJud/v_S_01_02_00", IsNullable = false)]
    public class ESocial8200 : XMLBase
    {
        [XmlElement("evtAnotJud")]
        public EvtAnotJud EvtAnotJud { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAnotJud")]
    [ComVisible(true)]
#endif
    public class EvtAnotJud
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento8200 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoProcesso")]
        public InfoProcesso8200 InfoProcesso { get; set; }

        [XmlElement("infoAnotJud")]
        public InfoAnotJud InfoAnotJud { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento8200")]
    [ComVisible(true)]
#endif
    public class IdeEvento8200 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcesso8200")]
    [ComVisible(true)]
#endif
    public class InfoProcesso8200
    {
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAnotJud")]
    [ComVisible(true)]
#endif
    public class InfoAnotJud
    {
        /// <summary>
        /// Versão do schema XML - Utilizado somente em tempo de serialização/desserialização, mas não é gerado no XML. Somente de uso interno da DLL para fazer tratamentos entre versões de schemas.
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "S_01_02_00";

        /// <summary>
        /// Retorna somente o valor inteiro da versão para facilitar comparações
        /// </summary>
        private int VersaoSchemaInt => Convert.ToInt32(VersaoSchema.Replace("S_", "").Replace("_", ""));

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

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("natAtividade")]
        public NatAtividade NatAtividade { get; set; }

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

        /// <summary>
        /// Valores válidos:
        /// 1 - CNPJ,
        /// 2 - CAEPF,
        /// 4 - CNO
        /// </summary>
        [XmlElement("tpInscTrab")]
#if INTEROP
        public TiposInscricao TpInscTrab { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInscTrab { get; set; }
#endif

        [XmlElement("localTrabalho")]
        public string LocalTrabalho { get; set; }

        [XmlElement("tpRegTrab")]
        public TipoRegimeTrabalhista TpRegTrab { get; set; } = TipoRegimeTrabalhista.CLT; //Só pode ser CLT e é uma tag obrigatória

        [XmlElement("tpRegPrev")]
        public TipoRegimePrevidenciario TpRegPrev { get; set; }

        [XmlElement("cargo")]
        public List<Cargo> Cargo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCargo(Cargo item)
        {
            if (Cargo == null)
            {
                Cargo = new List<Cargo>();
            }

            Cargo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Cargo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Cargo</returns>
        public Cargo GetCargo(int index)
        {
            if ((Cargo?.Count ?? 0) == 0)
            {
                return default;
            };

            return Cargo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Cargo
        /// </summary>
        public int GetCargoCount => (Cargo != null ? Cargo.Count : 0);
#endif

        [XmlElement("remuneracao")]
        public List<Remuneracao8200> Remuneracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemuneracao(Remuneracao8200 item)
        {
            if (Remuneracao == null)
            {
                Remuneracao = new List<Remuneracao8200>();
            }

            Remuneracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Remuneracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Remuneracao</returns>
        public Remuneracao8200 GetRemuneracao(int index)
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

        [XmlElement("incorporacao")]
        public List<Incorporacao> Incorporacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIncorporacao(Incorporacao item)
        {
            if (Incorporacao == null)
            {
                Incorporacao = new List<Incorporacao>();
            }

            Incorporacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Incorporacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Incorporacao</returns>
        public Incorporacao GetIncorporacao(int index)
        {
            if ((Incorporacao?.Count ?? 0) == 0)
            {
                return default;
            };

            return Incorporacao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Incorporacao
        /// </summary>
        public int GetIncorporacaoCount => (Incorporacao != null ? Incorporacao.Count : 0);
#endif

        [XmlElement("afastamento")]
        public Afastamento8200 Afastamento { get; set; }

        [XmlElement("desligamento")]
        public Desligamento8200 Desligamento { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtTermField() => DtTerm > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeTpInscTrab() => TpInscTrab != (TiposInscricao)(-1);
#else
        public bool ShouldSerializeTpInscTrab() => TpInscTrab != null;
#endif

        public bool ShouldSerializeLocalTrabalho() => !string.IsNullOrEmpty(LocalTrabalho);

        public bool ShouldSerializeTpRegTrab() => VersaoSchemaInt >= 10300;
        public bool ShouldSerializeTpRegPrev() => VersaoSchemaInt >= 10300;

        #endregion
    }

    /// <summary>
    /// Informações de afastamento do trabalhador.
    /// Preenchimento exclusivo em caso de trabalhador que permaneça afastado na data de início da 
    /// obrigatoriedade dos eventos não periódicos para o empregador no eSocial ou na data de transferência ou 
    /// alteração de CPF do empregado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Afastamento8200")]
    [ComVisible(true)]
#endif
    public class Afastamento8200 : Afastamento2200 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Cargo")]
    [ComVisible(true)]
#endif
    public class Cargo
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtCargo { get; set; }
#else
        public DateTimeOffset DtCargo { get; set; }
#endif

        [XmlElement("dtCargo")]
        public string DtCargoField
        {
            get => DtCargo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtCargo = DateTime.Parse(value);
#else
            set => DtCargo = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("CBOCargo")]
        public string CBOCargo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao8200")]
    [ComVisible(true)]
#endif
    public class Remuneracao8200
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

        /// <summary>
        /// Salário base do trabalhador, correspondente à
        /// parte fixa da remuneração em dtRemun.
        /// Validação: Se undSalFixo for igual a[7],
        /// preencher com 0 (zero).
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Incorporacao")]
    [ComVisible(true)]
#endif
    public class Incorporacao
    {
        /// <summary>
        /// Versão do schema XML - Utilizado somente em tempo de serialização/desserialização, mas não é gerado no XML. Somente de uso interno da DLL para fazer tratamentos entre versões de schemas.
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "S_01_02_00";

        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("matIncorp")]
        public string MatIncorp { get; set; }

        [XmlElement("sucessaoVinc")]
        public SucessaoVinc8200 SucessaoVinc { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        public bool ShouldSerializeSucessaoVinc() => VersaoSchema == "S_01_02_00" && SucessaoVinc != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc8200")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc8200
    {
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Desligamento8200")]
    [ComVisible(true)]
#endif
    public class Desligamento8200
    {
        [XmlElement("mtvDeslig")]
        public string MtvDeslig { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeDtProjFimAPIField() => DtProjFimAPI > DateTime.MinValue;

        #endregion
    }
}
