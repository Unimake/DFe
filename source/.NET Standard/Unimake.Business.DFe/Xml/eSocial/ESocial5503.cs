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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5503")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTSProcTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial5503 : XMLBase
    {
        [XmlElement("evtFGTSProcTrab")]
        public EvtFGTSProcTrab EvtFGTSProcTrab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtFGTSProcTrab")]
    [ComVisible(true)]
#endif
    public class EvtFGTSProcTrab
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento5503 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideProc")]
        public IdeProc5503 IdeProc { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador5003 IdeTrabalhador { get; set; }

        [XmlElement("infoTrabFGTS")]
        public List<InfoTrabFGTS5503> InfoTrabFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTrabFGTS(InfoTrabFGTS5503 item)
        {
            if (InfoTrabFGTS == null)
            {
                InfoTrabFGTS = new List<InfoTrabFGTS5503>();
            }

            InfoTrabFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTrabFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTrabFGTS</returns>
        public InfoTrabFGTS5503 GetInfoTrabFGTS(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5503")]
    [ComVisible(true)]
#endif
    public class IdeEvento5503
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProc5503")]
    [ComVisible(true)]
#endif
    public class IdeProc5503
    {
        [XmlElement("origem")]
        public Origem Origem { get; set; }

        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabFGTS5503")]
    [ComVisible(true)]
#endif
    public class InfoTrabFGTS5503
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg ? CodCateg { get; set; }
#endif

        [XmlElement("categOrig")]
        public string CategOrig { get; set; }

        [XmlElement("infoFGTSProcTrab")]
        public InfoFGTSProcTrab InfoFGTSProcTrab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        public bool ShouldSerializeCategOrig() => !string.IsNullOrEmpty(CategOrig);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTSProcTrab")]
    [ComVisible(true)]
#endif
    public class InfoFGTSProcTrab
    {
        /// <summary>
        /// Valor total de FGTS a recolher no processo trabalhista.
        /// </summary>
        [XmlIgnore]
        public double TotalFGTS { get; set; }
        [XmlElement("totalFGTS")]
        public string TotalFGTSField
        {
            get => TotalFGTS.ToString("F2", CultureInfo.InvariantCulture);
            set => TotalFGTS = Converter.ToDouble(value);
        }

        [XmlElement("ideEstab")]
        public IdeEstab5503 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab5503")]
    [ComVisible(true)]
#endif
    public class IdeEstab5503
    {
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("basePerRef")]
        public List<BasePerRef> BasePerRef { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerRef(BasePerRef item)
        {
            if (BasePerRef == null)
            {
                BasePerRef = new List<BasePerRef>();
            }

            BasePerRef.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerRef</returns>
        public BasePerRef GetBasePerRef(int index)
        {
            if ((BasePerRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasePerRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasePerRef
        /// </summary>
        public int GetBasePerRefCount => (BasePerRef != null ? BasePerRef.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerRef")]
    [ComVisible(true)]
#endif
    public class BasePerRef
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

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("tpValorProcTrab")]
        public int TpValorProcTrab { get; set; }

        /// <summary>
        /// Valor da base de cálculo de FGTS ainda não declarada,
        /// reconhecida no processo trabalhista.
        /// Origem: campo vrBcFGTSProcTrab de S-2500.
        /// </summary>
        [XmlIgnore]
        public double RemFGTSProcTrab { get; set; }
        [XmlElement("remFGTSProcTrab")]
        public string RemFGTSProcTrabField
        {
            get => RemFGTSProcTrab.ToString("F2", CultureInfo.InvariantCulture);
            set => RemFGTSProcTrab = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado na conta vinculada
        /// do trabalhador sobre base reconhecida no processo trabalhista.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double DpsFGTSProcTrab { get; set; }
        [XmlElement("dpsFGTSProcTrab")]
        public string DpsFGTSProcTrabField
        {
            get => DpsFGTSProcTrab.ToString("F2", CultureInfo.InvariantCulture);
            set => DpsFGTSProcTrab = Converter.ToDouble(value);

        }

        /// <summary>
        /// Valor da base de cálculo declarada anteriormente
        /// em SEFIP e ainda não recolhida.
        /// Origem: campo vrBcFGTSSefip de S-2500.
        /// </summary>
        [XmlIgnore]
        public double RemFGTSSefip { get; set; }
        [XmlElement("remFGTSSefip")]
        public string RemFGTSSefipField
        {
            get => RemFGTSSefip.ToString("F2", CultureInfo.InvariantCulture);
            set => RemFGTSSefip = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado na conta vinculada
        /// do trabalhador sobre base já declarada anteriormente em SEFIP.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double DpsFGTSSefip { get; set; }
        [XmlElement("dpsFGTSSefip")]
        public string DpsFGTSSefipField
        {
            get => DpsFGTSSefip.ToString("F2", CultureInfo.InvariantCulture);
            set => DpsFGTSSefip = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo declarada anteriormente
        /// no eSocial e ainda não recolhida.
        /// Origem: campo vrBcFGTSDecAnt de S-2500.
        /// </summary>
        [XmlIgnore]
        public double RemFGTSDecAnt { get; set; }
        [XmlElement("remFGTSDecAnt")]
        public string RemFGTSDecAntField
        {
            get => RemFGTSDecAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => RemFGTSDecAnt = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado na conta vinculada do
        /// trabalhador sobre base já declarada anteriormente no eSocial.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double DpsFGTSDecAnt { get; set; }
        [XmlElement("dpsFGTSDecAnt")]
        public string DpsFGTSDecAntField
        {
            get => DpsFGTSDecAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => DpsFGTSDecAnt = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeDpsFGTSProcTrabField() => DpsFGTSProcTrab > 0;

        public bool ShouldSerializeRemFGTSSefipField() => RemFGTSSefip > 0;

        public bool ShouldSerializeDpsFGTSSefipField() => DpsFGTSSefip > 0;

        public bool ShouldSerializeRemFGTSDecAntField() => RemFGTSDecAnt > 0;

        public bool ShouldSerializeDpsFGTSDecAntField() => DpsFGTSDecAnt > 0;

        #endregion ShouldSerialize
    }
}
