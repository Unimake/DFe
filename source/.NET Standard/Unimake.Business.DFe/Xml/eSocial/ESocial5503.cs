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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5503")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTSProcTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial5503 : XMLBase
    {
        [XmlElement("evtFGTSProcTrab")]
        public EvtFGTSProcTrab EvtFGTSProcTrab { get; set; }
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
        public IdeEventoESocial5503 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideProc")]
        public IdeProcESocial5503 IdeProc { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhadorESocial5003 IdeTrabalhador { get; set; }

        [XmlElement("infoTrabFGTS")]
        public List<InfoTrabFGTSESocial5503> InfoTrabFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTrabFGTS(InfoTrabFGTSESocial5503 item)
        {
            if (InfoTrabFGTS == null)
            {
                InfoTrabFGTS = new List<InfoTrabFGTSESocial5503>();
            }

            InfoTrabFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTrabFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTrabFGTS</returns>
        public InfoTrabFGTSESocial5503 GetInfoTrabFGTS(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5503")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial5503
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcESocial5503")]
    [ComVisible(true)]
#endif
    public class IdeProcESocial5503
    {
        [XmlElement("origem")]
        public Origem Origem { get; set; }

        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabFGTSESocial5503")]
    [ComVisible(true)]
#endif
    public class InfoTrabFGTSESocial5503
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

        public bool ShouldSerializeMatriculaField() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        public bool ShouldSerializeCategOrigField() => !string.IsNullOrEmpty(CategOrig);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTSProcTrab")]
    [ComVisible(true)]
#endif
    public class InfoFGTSProcTrab
    {
        [XmlElement("totalFGTS")]
        public double TotalFGTS { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstabESocial5503 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial5503")]
    [ComVisible(true)]
#endif
    public class IdeEstabESocial5503
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

        [XmlElement("remFGTSProcTrab")]
        public double RemFGTSProcTrab { get; set; }

        [XmlElement("dpsFGTSProcTrab")]
        public double DpsFGTSProcTrab { get; set; }

        [XmlElement("remFGTSSefip")]
        public double RemFGTSSefip { get; set; }

        [XmlElement("dpsFGTSSefip")]
        public double DpsFGTSSefip { get; set; }

        [XmlElement("remFGTSDecAnt")]
        public double RemFGTSDecAnt { get; set; }

        [XmlElement("dpsFGTSDecAnt")]
        public double DpsFGTSDecAnt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDpsFGTSProcTrab() => DpsFGTSProcTrab > 0;
   
        public bool ShouldSerializeRemFGTSSefip() => RemFGTSSefip > 0;
       
        public bool ShouldSerializeDpsFGTSSefip() => DpsFGTSSefip > 0;
       
        public bool ShouldSerializeRemFGTSDecAnt() => RemFGTSDecAnt > 0;
     
        public bool ShouldSerializeDpsFGTSDecAnt() => DpsFGTSDecAnt > 0;

        #endregion
    }
}
