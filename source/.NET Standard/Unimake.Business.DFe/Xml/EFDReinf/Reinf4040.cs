#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4040")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4040PagtoBenefNaoIdentificado/v2_01_02", IsNullable = false)]
    public class Reinf4040 : XMLBase
    {
        [XmlElement("evtBenefNId")]
        public EvtBenefNId EvtBenefNId { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtBenefNId")]
    [ComVisible(true)]
#endif
    public class EvtBenefNId : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf4040 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstabReinf4040 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf4040")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf4040 : IdeEventoReinf4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabReinf4040")]
    [ComVisible(true)]
#endif
    public class IdeEstabReinf4040
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("ideEvtAdic")]
        public string IdeEvtAdic { get; set; }

        [XmlElement("ideNat")]
        public List<IdeNat> IdeNat { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeNat(IdeNat item)
        {
            if (IdeNat == null)
            {
                IdeNat = new List<IdeNat>();
            }

            IdeNat.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeNat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeNat</returns>
        public IdeNat GetIdeNat(int index)
        {
            if ((IdeNat?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeNat[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeNat
        /// </summary>
        public int GetIdeNatCount => (IdeNat != null ? IdeNat.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeIdeEvtAdic() => !string.IsNullOrEmpty(IdeEvtAdic);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeNat")]
    [ComVisible(true)]
#endif
    public class IdeNat
    {
        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("infoPgto")]
        public List<InfoPgtoReinf4040> InfoPgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPgto(InfoPgtoReinf4040 item)
        {
            if (InfoPgto == null)
            {
                InfoPgto = new List<InfoPgtoReinf4040>();
            }

            InfoPgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPgto</returns>
        public InfoPgtoReinf4040 GetInfoPgto(int index)
        {
            if ((InfoPgto?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoPgto[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoPgto
        /// </summary>
        public int GetInfoPgtoCount => (InfoPgto != null ? InfoPgto.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoPgtoReinf4040")]
    [ComVisible(true)]
#endif
    public class InfoPgtoReinf4040
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtFG { get; set; }
#else
        public DateTimeOffset DtFG { get; set; }
#endif

        [XmlElement("dtFG")]
        public string DtFGField
        {
            get => DtFG.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFG = DateTime.Parse(value);
#else
            set => DtFG = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
        public double VlrLiq { get; set; }

        [XmlElement("vlrLiq")]
        public string VlrLiqField
        {
            get => VlrLiq.ToString("F2", CultureInfoReinf.Info);
            set => VlrLiq = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrBaseIR { get; set; }

        [XmlElement("vlrBaseIR")]
        public string VlrBaseIRField
        {
            get => VlrBaseIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrIR { get; set; }

        [XmlElement("vlrIR")]
        public string VlrIRField
        {
            get => VlrIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEscrCont { get; set; }
#else
        public DateTimeOffset DtEscrCont { get; set; }
#endif

        [XmlElement("dtEscrCont")]
        public string DtEscrContField
        {
            get => DtEscrCont.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEscrCont = DateTime.Parse(value);
#else
            set => DtEscrCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("descr")]
        public string Descr { get; set; }

        [XmlElement("infoProcRet")]
        public List<InfoProcRetReinf4040> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRetReinf4040 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRetReinf4040>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRetReinf4040 GetInfoProcRet(int index)
        {
            if ((InfoProcRet?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcRet[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcRet
        /// </summary>
        public int GetInfoProcRetCount => (InfoProcRet != null ? InfoProcRet.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrLiq() => VlrLiq > 0;
       
        public bool ShouldSerializeVlrIR() => VlrIR > 0;

        public bool ShouldSerializeDtEscrContField() => DtEscrCont > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetReinf4040")]
    [ComVisible(true)]
#endif
    public class InfoProcRetReinf4040
    {

        [XmlElement("tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlElement("nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrBaseSuspIR { get; set; }

        [XmlElement("vlrBaseSuspIR")]
        public string VlrBaseSuspIRField
        {
            get => VlrBaseSuspIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseSuspIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNIR { get; set; }

        [XmlElement("vlrNIR")]
        public string VlrNIRField
        {
            get => VlrNIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrNIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrDepIR { get; set; }

        [XmlElement("vlrDepIR")]
        public string VlrDepIRField
        {
            get => VlrDepIR.ToString("F2", CultureInfoReinf.Info);
            set => VlrDepIR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSereializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrBaseSuspIR() => VlrBaseSuspIR > 0;
        
        public bool ShouldSerializeVlrNIR() => VlrNIR > 0;
        
        public bool ShouldSerializeVlrDepIR() => VlrDepIR > 0;

        #endregion
    }
}
