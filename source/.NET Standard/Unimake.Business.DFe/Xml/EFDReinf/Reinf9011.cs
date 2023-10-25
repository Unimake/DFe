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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf9011")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02", IsNullable = false)]
    public class Reinf9011 : XMLBase
    {
        [XmlElement("evtTotalContrib")]
        public EvtTotalContrib EvtTotalContrib { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTotalContrib")]
    [ComVisible(true)]
#endif
    public class EvtTotalContrib : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf9001 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRecRetorno")]
        public IdeRecRetorno IdeRecRetorno { get; set; }

        [XmlElement("infoRecEv")]
        public InfoRecEvReinf9011 InfoRecEv { get; set; }

        [XmlElement("infoTotalContrib")]
        public InfoTotalContrib InfoTotalContrib { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecEvReinf9011")]
    [ComVisible(true)]
#endif
    public class InfoRecEvReinf9011
    {
        [XmlElement("nrProtEntr")]
        public string NrProtEntr { get; set; }

        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhProcess { get; set; }
#else
        public DateTimeOffset DhProcess { get; set; }
#endif

        [XmlElement("dhProcess")]
        public string DhProcessField
        {
            get => DhProcess.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProcess = DateTime.Parse(value);
#else
            set => DhProcess = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpEv")]
        public string TpEv { get; set; }

        [XmlElement("idEv")]
        public string IdEv { get; set; }

        [XmlElement("hash")]
        public string Hash { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDhProcessField() => DhProcess > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTotalContrib")]
    [ComVisible(true)]
#endif
    public class InfoTotalContrib
    {
        [XmlElement("indExistInfo")]
        public IndicativoExistenciaTributos IndExistInfo { get; set; }

        [XmlElement("identEscritDCTF")]
        public string IdentEscritDCTF { get; set; }

        [XmlElement("RTom")]
        public List<RTom> RTom { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRTom(RTom item)
        {
            if (RTom == null)
            {
                RTom = new List<RTom>();
            }

            RTom.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RTom (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RTom</returns>
        public RTom GetRTom(int index)
        {
            if ((RTom?.Count ?? 0) == 0)
            {
                return default;
            };

            return RTom[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RTom
        /// </summary>
        public int GetRTomCount => (RTom != null ? RTom.Count : 0);
#endif

        [XmlElement("RPrest")]
        public List<RPrest> RPrest { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRPrest(RPrest item)
        {
            if (RPrest == null)
            {
                RPrest = new List<RPrest>();
            }

            RPrest.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RPrest (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RPrest</returns>
        public RPrest GetRPrest(int index)
        {
            if ((RPrest?.Count ?? 0) == 0)
            {
                return default;
            };

            return RPrest[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RPrest
        /// </summary>
        public int GetRPrestCount => (RPrest != null ? RPrest.Count : 0);
#endif

        [XmlElement("RRecRepAD")]
        public List<RRecRepADReinf9011> RRecRepAD { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRRecRepAD(RRecRepADReinf9011 item)
        {
            if (RRecRepAD == null)
            {
                RRecRepAD = new List<RRecRepADReinf9011>();
            }

            RRecRepAD.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RRecRepAD (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RRecRepAD</returns>
        public RRecRepADReinf9011 GetRRecRepAD(int index)
        {
            if ((RRecRepAD?.Count ?? 0) == 0)
            {
                return default;
            };

            return RRecRepAD[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RRecRepAD
        /// </summary>
        public int GetRRecRepADCount => (RRecRepAD != null ? RRecRepAD.Count : 0);
#endif

        [XmlElement("RComl")]
        public List<RComl> RComl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRComl(RComl item)
        {
            if (RComl == null)
            {
                RComl = new List<RComl>();
            }

            RComl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RComl (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RComl</returns>
        public RComl GetRComl(int index)
        {
            if ((RComl?.Count ?? 0) == 0)
            {
                return default;
            };

            return RComl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RComl
        /// </summary>
        public int GetRComlCount => (RComl != null ? RComl.Count : 0);
#endif

        [XmlElement("RAquis")]
        public List<RAquis> RAquis { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRAquis(RAquis item)
        {
            if (RAquis == null)
            {
                RAquis = new List<RAquis>();
            }

            RAquis.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RAquis (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RAquis</returns>
        public RAquis GetRAquis(int index)
        {
            if ((RAquis?.Count ?? 0) == 0)
            {
                return default;
            };

            return RAquis[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RAquis
        /// </summary>
        public int GetRAquisCount => (RAquis != null ? RAquis.Count : 0);
#endif

        [XmlElement("RCPRB")]
        public List<RCPRB> RCPRB { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRCPRB(RCPRB item)
        {
            if (RCPRB == null)
            {
                RCPRB = new List<RCPRB>();
            }

            RCPRB.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RCPRB (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RCPRB</returns>
        public RCPRB GetRCPRB(int index)
        {
            if ((RCPRB?.Count ?? 0) == 0)
            {
                return default;
            };

            return RCPRB[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RCPRB
        /// </summary>
        public int GetRCPRBCount => (RCPRB != null ? RCPRB.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RRecRepADReinf9011")]
    [ComVisible(true)]
#endif
    public class RRecRepADReinf9011
    {
        [XmlElement("CRRecRepAD")]
        public string CRRecRepAD { get; set; }

        [XmlIgnore]
        public double VlrCRRecRepAD { get; set; }

        [XmlElement("vlrCRRecRepAD")]
        public string VlrCRRecRepADField
        {
            get => VlrCRRecRepAD.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecRepAD = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrCRRecRepADSusp { get; set; }

        [XmlElement("vlrCRRecRepADSusp")]
        public string VlrCRRecRepADSuspField
        {
            get => VlrCRRecRepADSusp.ToString("F2", CultureInfoReinf.Info);
            set => VlrCRRecRepADSusp = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrCRRecRepADSusp() => VlrCRRecRepADSusp > 0;

        #endregion
    }
}
