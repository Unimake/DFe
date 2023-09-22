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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2055")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt2055AquisicaoProdRural/v2_01_02", IsNullable = false)]
    public class Reinf2055 : XMLBase
    {
        [XmlElement("evtAqProd")]
        public EvtAqProd EvtAqProd { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtAqProd")]
    [ComVisible(true)]
#endif
    public class EvtAqProd : ReinfEventoBase
    {

        [XmlElement("ideEvento")]
        public IdeEventoReinf2055 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoAquisProd")]
        public InfoAquisProd InfoAquisProd { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf2055")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf2055 : IdeEventoReinf2050 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoAquisProd")]
    [ComVisible(true)]
#endif
    public class InfoAquisProd
    {
        [XmlElement("ideEstabAdquir")]
        public IdeEstabAdquir IdeEstabAdquir { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabAdquir")]
    [ComVisible(true)]
#endif
    public class IdeEstabAdquir
    {
        [XmlElement("tpInscAdq")]
        public TipoInscricaoAdquirente TpInscAdq { get; set; }

        [XmlElement("nrInscAdq")]
        public string NrInscAdq { get; set; }

        [XmlElement("ideProdutor")]
        public IdeProdutor IdeProdutor { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeProdutor")]
    [ComVisible(true)]
#endif
    public class IdeProdutor
    {
        [XmlElement("tpInscProd")]
        public TiposInscricao TpInscProd { get; set; }

        [XmlElement("nrInscProd")]
        public string NrInscProd { get; set; }

        [XmlElement("indOpcCP")]
        public string IndOpcCP { get; set; }

        [XmlElement("detAquis")]
        public List<DetAquis> DetAquis { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetAquis(DetAquis item)
        {
            if (DetAquis == null)
            {
                DetAquis = new List<DetAquis>();
            }

            DetAquis.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetAquis (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetAquis</returns>
        public DetAquis GetDetAquis(int index)
        {
            if ((DetAquis?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetAquis[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetAquis
        /// </summary>
        public int GetDetAquisCount => (DetAquis != null ? DetAquis.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeIndOpcCP() => !string.IsNullOrEmpty(IndOpcCP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DetAquis")]
    [ComVisible(true)]
#endif
    public class DetAquis
    {
        [XmlElement("indAquis")]
        public IndicativoDaAquisicao IndAquis { get; set; }

        [XmlIgnore]
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCPDescPR { get; set; }

        [XmlElement("vlrCPDescPR")]
        public string VlrCPDescPRField
        {
            get => VlrCPDescPR.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPDescPR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRatDescPR { get; set; }

        [XmlElement("vlrRatDescPR")]
        public string VlrRatDescPRField
        {
            get => VlrRatDescPR.ToString("F2", CultureInfoReinf.Info);
            set => VlrRatDescPR = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrSenarDesc { get; set; }

        [XmlElement("vlrSenarDesc")]
        public string VlrSenarDescField
        {
            get => VlrSenarDesc.ToString("F2", CultureInfoReinf.Info);
            set => VlrSenarDesc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoProcJud")]
        public List<InfoProcJud> InfoProcJud { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcJud(InfoProcJud item)
        {
            if (InfoProcJud == null)
            {
                InfoProcJud = new List<InfoProcJud>();
            }

            InfoProcJud.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcJud (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcJud</returns>
        public InfoProcJud GetInfoProcJud(int index)
        {
            if ((InfoProcJud?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcJud[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcJud
        /// </summary>
        public int GetInfoProcJudCount => (InfoProcJud != null ? InfoProcJud.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcJud")]
    [ComVisible(true)]
#endif
    public class InfoProcJud
    {
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrCPNRet { get; set; }

        [XmlElement("vlrCPNRet")]
        public string VlrCPNRetField
        {
            get => VlrCPNRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrCPNRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRatNRet { get; set; }

        [XmlElement("vlrRatNRet")]
        public string VlrRatNRetField
        {
            get => VlrRatNRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrRatNRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrSenarNRet { get; set; }

        [XmlElement("vlrSenarNRet")]
        public string VlrSenarNRetField
        {
            get => VlrSenarNRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrSenarNRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSereializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        public bool ShouldSerializeVlrTotalNRetPrinc() => VlrCPNRet > 0;
        
        public bool ShouldSerializeVlrRatNRet() => VlrRatNRet > 0;
        
        public bool ShouldSerializeVlrSenarNRet() => VlrSenarNRet > 0;

        #endregion

    }

}
