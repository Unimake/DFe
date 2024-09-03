#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5003")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesFGTS/v_S_01_02_00", IsNullable = false)]

    public class ESocial5003 : XMLBase
    {
        [XmlElement("evtBasesFGTS")]
        public EvtBasesFGTS EvtBasesFGTS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtBasesFGTS")]
    [ComVisible(true)]
#endif
    public class EvtBasesFGTS
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial5003 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhadorESocial5003 IdeTrabalhador { get; set; }

        [XmlElement("infoFGTS")]
        public InfoFGTSESocial5003 InfoFGTS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5003")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial5003
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif

        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhadorESocial5003")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhadorESocial5003
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTSESocial5003")]
    [ComVisible(true)]
#endif
    public class InfoFGTSESocial5003
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtVenc { get; set; }
#else
        public DateTimeOffset DtVenc { get; set; }
#endif

        [XmlElement("dtVenc")]
        public string DtVencField
        {
            get => DtVenc.ToString("yyyy-MM");
#if INTEROP
            set => DtVenc = DateTime.Parse(value);
#else
            set => DtVenc = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("classTrib")]
        public string ClassTrib { get; set; }

        [XmlElement("ideEstab")]
        public List<IdeEstabESocial5003> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstabESocial5003 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstabESocial5003>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstabESocial5003 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstab
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtVencField() => DtVenc > DateTime.MinValue;

        public bool ShouldSerializeClassTrib() => !string.IsNullOrEmpty(ClassTrib);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial5003")]
    [ComVisible(true)]
#endif
    public class IdeEstabESocial5003
    {
        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("ideLotacao")]
        public List<IdeLotacaoESocial5003> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacaoESocial5003 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacaoESocial5003>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacaoESocial5003 GetIdeLotacao(int index)
        {
            if ((IdeLotacao?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeLotacao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeLotacao
        /// </summary>
        public int GetIdeLotacaoCount => (IdeLotacao != null ? IdeLotacao.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacaoESocial5003")]
    [ComVisible(true)]
#endif
    public class IdeLotacaoESocial5003
    {
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        [XmlElement("tpLotacao")]
#if INTEROP
        public TpLotacao TpLotacao { get; set; } = (TpLotacao)(-1);
#else
        public TpLotacao? TpLotacao { get; set; }
#endif

        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("infoTrabFGTS")]
        public List<InfoTrabFGTS> InfoTrabFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTrabFGTS(InfoTrabFGTS item)
        {
            if (InfoTrabFGTS == null)
            {
                InfoTrabFGTS = new List<InfoTrabFGTS>();
            }

            InfoTrabFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTrabFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTrabFGTS</returns>
        public InfoTrabFGTS GetInfoTrabFGTS(int index)
        {
            if ((InfoTrabFGTS?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoTrabFGTS[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoTrabFGTS
        /// </summary>
        public int GetInfoTrabFGTSCount => (InfoTrabFGTS != null ? InfoTrabFGTS.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCodLotacao() => !string.IsNullOrEmpty(CodLotacao);

#if INTEROP
        public bool ShouldSerializeTpLotacao() => TpLotacao != (TpLotacao)(-1);
#else
        public bool ShouldSerializeTpLotacao() => TpLotacao != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabFGTS")]
    [ComVisible(true)]
#endif
    public class InfoTrabFGTS
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("categOrig")]
        public string CategOrig { get; set; }

        [XmlElement("tpRegTrab")]
#if INTEROP
        public TipoRegimeTrabalhista TpRegTrab { get; set; } = (TipoRegimeTrabalhista)(-1);
#else
        public TipoRegimeTrabalhista? TpRegTrab { get; set; }
#endif

        [XmlElement("remunSuc")]
#if INTEROP
        public SimNaoLetra RemunSuc { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? RemunSuc { get; set; }
#endif

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
        public string MtvDesligTSV { get; set; }

        [XmlElement("sucessaoVinc")]
        public SucessaoVincESocial5003 SucessaoVinc { get; set; }

        [XmlElement("infoBaseFGTS")]
        public InfoBaseFGTS InfoBaseFGTS { get; set; }

        [XmlElement("procCS")]
        public ProcCS ProcCS { get; set; }

        [XmlElement("eConsignado")]
        public List<EConsignado> EConsignado { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEConsignado(EConsignado item)
        {
            if (EConsignado == null)
            {
                EConsignado = new List<EConsignado>();
            }

            EConsignado.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista EConsignado (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da EConsignado</returns>
        public EConsignado GetEConsignado(int index)
        {
            if ((EConsignado?.Count ?? 0) == 0)
            {
                return default;
            };

            return EConsignado[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista EConsignado
        /// </summary>
        public int GetEConsignadoCount => (EConsignado != null ? EConsignado.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

        public bool ShouldSerializeCategOrig() => !string.IsNullOrEmpty(CategOrig);

#if INTEROP
        public bool ShouldSerializeTpRegTrab() => TpRegTrab != (TipoRegimeTrabalhista)(-1);
#else
        public bool ShouldSerializeTpRegTrab() => TpRegTrab != null;
#endif

#if INTEROP
        public bool ShouldSerializeRemunSuc() => RemunSuc != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeRemunSuc() => RemunSuc != null;
#endif

        public bool ShouldSerializeDtDesligField() => DtDeslig > DateTime.MinValue;

        public bool ShouldSerializeMtvDeslig() => !string.IsNullOrEmpty(MtvDeslig);

        public bool ShouldSerializeDtTermField() => DtTerm > DateTime.MinValue;

        public bool ShouldSerializeMtvDesligTSV() => !string.IsNullOrEmpty(MtvDesligTSV);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVincESocial5003")]
    [ComVisible(true)]
#endif
    public class SucessaoVincESocial5003
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseFGTS")]
    [ComVisible(true)]
#endif
    public class InfoBaseFGTS
    {
        [XmlElement("basePerApur")]
        public List<BasePerApur> BasePerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerApur(BasePerApur item)
        {
            if (BasePerApur == null)
            {
                BasePerApur = new List<BasePerApur>();
            }

            BasePerApur.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerApur (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerApur</returns>
        public BasePerApur GetBasePerApur(int index)
        {
            if ((BasePerApur?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasePerApur[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasePerApur
        /// </summary>
        public int GetBasePerApurCount => (BasePerApur != null ? BasePerApur.Count : 0);
#endif

        [XmlElement("infoBasePerAntE")]
        public List<InfoBasePerAntE> InfoBasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBasePerAntE(InfoBasePerAntE item)
        {
            if (InfoBasePerAntE == null)
            {
                InfoBasePerAntE = new List<InfoBasePerAntE>();
            }

            InfoBasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBasePerAntE</returns>
        public InfoBasePerAntE GetInfoBasePerAntE(int index)
        {
            if ((InfoBasePerAntE?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoBasePerAntE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoBasePerAntE
        /// </summary>
        public int GetInfoBasePerAntECount => (InfoBasePerAntE != null ? InfoBasePerAntE.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerApur")]
    [ComVisible(true)]
#endif
    public class BasePerApur
    {
        [XmlElement("tpValor")]
        public TpValor TpValor { get; set; }

        /// <summary>
        /// Valores válidos:
        /// 1 - Normal (incidência de FGTS)
        /// 9 - Incidência de FGTS suspensa em decorrência de decisão judicial
        /// </summary>
        [XmlElement("indIncid")]
        public IndIncid IndIncid { get; set; }

        [XmlElement("remFGTS")]
        public double RemFGTS { get; set; }

        [XmlElement("dpsFGTS")]
        public double DpsFGTS { get; set; }

        [XmlElement("detRubrSusp")]
        public List<DetRubrSusp> DetRubrSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetRubrSusp(DetRubrSusp item)
        {
            if (DetRubrSusp == null)
            {
                DetRubrSusp = new List<DetRubrSusp>();
            }

            DetRubrSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetRubrSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetRubrSusp</returns>
        public DetRubrSusp GetDetRubrSusp(int index)
        {
            if ((DetRubrSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetRubrSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetRubrSusp
        /// </summary>
        public int GetDetRubrSuspCount => (DetRubrSusp != null ? DetRubrSusp.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDpsFGTS() => DpsFGTS > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetRubrSusp")]
    [ComVisible(true)]
#endif
    public class DetRubrSusp
    {
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        [XmlElement("vrRubr")]
        public double VrRubr { get; set; }

        [XmlElement("ideProcessoFGTS")]
        public List<IdeProcessoFGTS> IdeProcessoFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoFGTS(IdeProcessoFGTS item)
        {
            if (IdeProcessoFGTS == null)
            {
                IdeProcessoFGTS = new List<IdeProcessoFGTS>();
            }

            IdeProcessoFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoFGTS</returns>
        public IdeProcessoFGTS GetIdeProcessoFGTS(int index)
        {
            if ((IdeProcessoFGTS?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeProcessoFGTS[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeProcessoFGTS
        /// </summary>
        public int GetIdeProcessoFGTSCount => (IdeProcessoFGTS != null ? IdeProcessoFGTS.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBasePerAntE")]
    [ComVisible(true)]
#endif
    public class InfoBasePerAntE
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

        [XmlElement("tpAcConv")]
        public string TpAcConv { get; set; }

        [XmlElement("basePerAntE")]
        public List<BasePerAntE> BasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerAntE(BasePerAntE item)
        {
            if (BasePerAntE == null)
            {
                BasePerAntE = new List<BasePerAntE>();
            }

            BasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerAntE</returns>
        public BasePerAntE GetBasePerAntE(int index)
        {
            if ((BasePerAntE?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasePerAntE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasePerAntE
        /// </summary>
        public int GetBasePerAntECount => (BasePerAntE != null ? BasePerAntE.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerAntE")]
    [ComVisible(true)]
#endif
    public class BasePerAntE
    {
        [XmlElement("tpValorE")]
        public TpValor TpValorE { get; set; }

        /// <summary>
        /// Valores válidos:
        /// 1 - Normal (incidência de FGTS)
        /// 9 - Incidência de FGTS suspensa em decorrência de decisão judicial
        /// </summary>
        [XmlElement("indIncidE")]
        public IndIncid IndIncidE { get; set; }

        [XmlElement("remFGTSE")]
        public double RemFGTSE { get; set; }

        [XmlElement("dpsFGTSE")]
        public double DpsFGTSE { get; set; }

        [XmlElement("detRubrSusp")]
        public List<DetRubrSusp> DetRubrSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetRubrSusp(DetRubrSusp item)
        {
            if (DetRubrSusp == null)
            {
                DetRubrSusp = new List<DetRubrSusp>();
            }

            DetRubrSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetRubrSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetRubrSusp</returns>
        public DetRubrSusp GetDetRubrSusp(int index)
        {
            if ((DetRubrSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetRubrSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetRubrSusp
        /// </summary>
        public int GetDetRubrSuspCount => (DetRubrSusp != null ? DetRubrSusp.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDpsFGTSE() => DpsFGTSE > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EConsignado")]
    [ComVisible(true)]
#endif
    public class EConsignado
    {
        [XmlElement("instFinanc")]
        public string InstFinanc { get; set; }

        [XmlElement("nrContrato")]
        public string NrContrato { get; set; }

        [XmlElement("vreConsignado")]
        public double VreConsignado { get; set; }
    }
}