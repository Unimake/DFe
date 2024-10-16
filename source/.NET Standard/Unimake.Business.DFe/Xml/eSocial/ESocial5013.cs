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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5013")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTS/v_S_01_02_00", IsNullable = false)]

    public class ESocial5013 : XMLBase
    {
        [XmlElement("evtFGTS")]
        public EvtFGTS EvtFGTS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
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
        public IdeEvento5013 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoFGTS")]
        public InfoFGTS5013 InfoFGTS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5013")]
    [ComVisible(true)]
#endif
    public class IdeEvento5013
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTS5013")]
    [ComVisible(true)]
#endif
    public class InfoFGTS5013
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("indExistInfo")]
        public IndExistInfo IndExistInfo { get; set; }

        [XmlElement("ideEstab")]
        public List<IdeEstab5013> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab5013 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab5013>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstab5013 GetIdeEstab(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab5013")]
    [ComVisible(true)]
#endif
    public class IdeEstab5013
    {
        /// <summary>
        /// Valores válidos:
        /// 1 - CNPJ,
        /// 2 - CPF,
        /// 3 - CAEPF,
        /// 4 - CNO
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("ideLotacao")]
        public List<IdeLotacao5013> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacao5013 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacao5013>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacao5013 GetIdeLotacao(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacao5013")]
    [ComVisible(true)]
#endif
    public class IdeLotacao5013
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
        public InfoBaseFGTS5013 InfoBaseFGTS { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseFGTS5013")]
    [ComVisible(true)]
#endif
    public class InfoBaseFGTS5013
    {
        [XmlElement("basePerApur")]
        public List<BasePerApur5013> BasePerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerApur(BasePerApur5013 item)
        {
            if (BasePerApur == null)
            {
                BasePerApur = new List<BasePerApur5013>();
            }

            BasePerApur.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerApur (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerApur</returns>
        public BasePerApur5013 GetBasePerApur(int index)
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
        public List<InfoBasePerAntE5013> InfoBasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBasePerAntE(InfoBasePerAntE5013 item)
        {
            if (InfoBasePerAntE == null)
            {
                InfoBasePerAntE = new List<InfoBasePerAntE5013>();
            }

            InfoBasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBasePerAntE</returns>
        public InfoBasePerAntE5013 GetInfoBasePerAntE(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerApur5013")]
    [ComVisible(true)]
#endif
    public class BasePerApur5013
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

        /// <summary>
        /// Remuneração (valor da base de cálculo) do FGTS.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double BaseFGTS { get; set; }
        [XmlElement("baseFGTS")]
        public string BaseFGTSString
        {
            get => BaseFGTS.ToString("F2", CultureInfo.InvariantCulture);
            set => BaseFGTS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado
        /// na conta vinculada do trabalhador.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrFGTS { get; set; }
        [XmlElement("vrFGTS")]
        public string VrFGTSField
        {
            get => VrFGTS.ToString("F2", CultureInfo.InvariantCulture);
            set => VrFGTS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrFGTSField() => VrFGTS > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBasePerAntE5013")]
    [ComVisible(true)]
#endif
    public class InfoBasePerAntE5013
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
        public List<BasePerAntE5013> BasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerAntE(BasePerAntE5013 item)
        {
            if (BasePerAntE == null)
            {
                BasePerAntE = new List<BasePerAntE5013>();
            }

            BasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerAntE</returns>
        public BasePerAntE5013 GetBasePerAntE(int index)
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

    public class BasePerAntE5013
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

        /// <summary>
        /// Remuneração (valor da base de cálculo) do FGTS.
        /// Validação: Deve ser maior que 0 (zero).
        /// Deve corresponder ao somatório dos valores informados no campo remFGTSE
        /// do evento de origem, agrupados por tpValorE e indIncidE.
        /// </summary>
        [XmlIgnore]
        public double BaseFGTSE { get; set; }
        [XmlElement("baseFGTSE")]
        public string BaseFGTSEField
        {
            get => BaseFGTSE.ToString("F2", CultureInfo.InvariantCulture);
            set => BaseFGTSE = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado na conta vinculada do trabalhador.
        /// Validação: Deve ser maior que 0 (zero).
        /// Deve corresponder ao somatório dos valores informados no campo
        /// dpsFGTSE do evento de origem, agrupados por tpValorE.
        /// </summary>
        [XmlIgnore]
        public double VrFGTSE { get; set; }
        [XmlElement("vrFGTSE")]
        public string VrFGTSEField
        {
            get => VrFGTSE.ToString("F2", CultureInfo.InvariantCulture);
            set => VrFGTSE = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrFGTSEField() => VrFGTSE > 0;

        #endregion ShouldSerialize
    }
}
