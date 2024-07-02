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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5013")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTS/v_S_01_02_00", IsNullable = false)]

    public class ESocial5013 : XMLBase
    {
        [XmlElement("evtFGTS")]
        public EvtFGTS EvtFGTS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtFGTS")]
    [ComVisible(true)]
#endif
    public class EvtFGTS
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial5013 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoFGTS")]
        public InfoFGTSESocial5013 InfoFGTS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5013")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial5013
    {
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTSESocial5013")]
    [ComVisible(true)]
#endif
    public class InfoFGTSESocial5013
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("indExistInfo")]
        public IndExistInfo IndExistInfo { get; set; }

        [XmlElement("ideEstab")]
        public List<IdeEstabESocial5013> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstabESocial5013 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstabESocial5013>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstabESocial5013 GetIdeEstab(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial5013")]
    [ComVisible(true)]
#endif
    public class IdeEstabESocial5013
    {
        /// <summary>
        /// Valores válidos:
        /// 1 - CNPJ,
        /// 2 - CPF,
        /// 3 - CAEPF,
        /// 4 - CNO
        /// </summary>
        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TpInsc? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("ideLotacao")]
        public List<IdeLotacaoESocial5013> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacaoESocial5013 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacaoESocial5013>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacaoESocial5013 GetIdeLotacao(int index)
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

        public bool ShouldSerializeNrInscField() => !string.IsNullOrEmpty(NrInsc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacaoESocial5013")]
    [ComVisible(true)]
#endif
    public class IdeLotacaoESocial5013
    {
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        [XmlElement("tpLotacao")]
        public TpLotacao TpLotacao { get; set; }

        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("infoBaseFGTS")]
        public InfoBaseFGTSESocial5013 InfoBaseFGTS { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInscField() => !string.IsNullOrEmpty(NrInsc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseFGTSESocial5013")]
    [ComVisible(true)]
#endif
    public class InfoBaseFGTSESocial5013
    {
        [XmlElement("basePerApur")]
        public List<BasePerApurESocial5013> BasePerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerApur(BasePerApurESocial5013 item)
        {
            if (BasePerApur == null)
            {
                BasePerApur = new List<BasePerApurESocial5013>();
            }

            BasePerApur.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerApur (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerApur</returns>
        public BasePerApurESocial5013 GetBasePerApur(int index)
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
        public List<InfoBasePerAntEESocial5013> InfoBasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBasePerAntE(InfoBasePerAntEESocial5013 item)
        {
            if (InfoBasePerAntE == null)
            {
                InfoBasePerAntE = new List<InfoBasePerAntEESocial5013>();
            }

            InfoBasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBasePerAntE</returns>
        public InfoBasePerAntEESocial5013 GetInfoBasePerAntE(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerApurESocial5013")]
    [ComVisible(true)]
#endif
    public class BasePerApurESocial5013
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

        [XmlElement("baseFGTS")]
        public double BaseFGTS { get; set; }

        [XmlElement("vrFGTS")]
        public double VrFGTS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBasePerAntEESocial5013")]
    [ComVisible(true)]
#endif
    public class InfoBasePerAntEESocial5013
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
        public List<BasePerAntEESocial5013> BasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerAntE(BasePerAntEESocial5013 item)
        {
            if (BasePerAntE == null)
            {
                BasePerAntE = new List<BasePerAntEESocial5013>();
            }

            BasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerAntE</returns>
        public BasePerAntEESocial5013 GetBasePerAntE(int index)
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

    public class BasePerAntEESocial5013
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

        [XmlElement("baseFGTSE")]
        public double BaseFGTSE { get; set; }

        [XmlElement("vrFGTSE")]
        public double VrFGTSE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrFGTSE() => VrFGTSE > 0;

        #endregion
    }
}
