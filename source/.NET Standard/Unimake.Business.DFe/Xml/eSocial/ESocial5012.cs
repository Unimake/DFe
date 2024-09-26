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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5012")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrf/v_S_01_02_00", IsNullable = false)]
    public class ESocial5012 : XMLBase
    {
        [XmlElement("evtIrrf")]
        public EvtIrrf EvtIrrf { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtIrrf")]
    [ComVisible(true)]
#endif
    public class EvtIrrf
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento5012 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoIRRF")]
        public InfoIRRF InfoIRRF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5012")]
    [ComVisible(true)]
#endif
    public class IdeEvento5012
    {
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRRF")]
    [ComVisible(true)]
#endif
    public class InfoIRRF
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("indExistInfo")]
        public IndExistInfo IndExistInfo { get; set; }

        [XmlElement("infoCRMen")]
        public List<InfoCRMen> InfoCRMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRMen(InfoCRMen item)
        {
            if (InfoCRMen == null)
            {
                InfoCRMen = new List<InfoCRMen>();
            }

            InfoCRMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRMen</returns>
        public InfoCRMen GetInfoCRMen(int index)
        {
            if ((InfoCRMen?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRMen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRMen
        /// </summary>
        public int GetInfoCRMenCount => (InfoCRMen != null ? InfoCRMen.Count : 0);
#endif

        [XmlElement("infoCRDia")]
        public List<InfoCRDia> InfoCRDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRDia(InfoCRDia item)
        {
            if (InfoCRDia == null)
            {
                InfoCRDia = new List<InfoCRDia>();
            }

            InfoCRDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRDia</returns>
        public InfoCRDia GetInfoCRDia(int index)
        {
            if ((InfoCRDia?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRDia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRDia
        /// </summary>
        public int GetInfoCRDiaCount => (InfoCRDia != null ? InfoCRDia.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRMen")]
    [ComVisible(true)]
#endif
    public class InfoCRMen
    {
        [XmlElement("CRMen")]
        public TpCR CRMen { get; set; }

        /// <summary>
        /// Valor correspondente ao Código de Receita - CR indicado em CRMen.
        /// </summary>
        [XmlIgnore]
        public double VrCRMen { get; set; }
        [XmlElement("vrCRMen")]
        public string VrCRMenField
        {
            get => VrCRMen.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCRMen = Converter.ToDouble(value);

        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRDia")]
    [ComVisible(true)]
#endif
    public class InfoCRDia
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerApurDia { get; set; }
#else
        public DateTimeOffset PerApurDia { get; set; }
#endif

        [XmlElement("perApurDia")]
        public string PerApurDiaField
        {
            get => PerApurDia.ToString("yyy-MM-dd");
#if INTEROP
            set => PerApurDia = DateTime.Parse(value);
#else
            set => PerApurDia = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("CRDia")]
        public string CRDia { get; set; }

        /// <summary>
        /// Valor relativo ao Imposto de Renda Retido na Fonte sobre rendimentos
        /// do trabalho pagos a residente, para fins fiscais, no exterior.
        /// </summary>
        [XmlIgnore]
        public double VrCRDia { get; set; }
        [XmlElement("vrCRDia")]
        public string CRDiaField
        {
            get => VrCRDia.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCRDia = Converter.ToDouble(value);
        }
    }
}
