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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4080")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4080RetencaoRecebimento/v2_01_02", IsNullable = false)]
    public class Reinf4080 : XMLBase
    {
        [XmlElement("evtRetRec")]
        public EvtRetRec EvtRetRec { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtRetRec")]
    [ComVisible(true)]
#endif
    public class EvtRetRec : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf4080 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstabReinf4080 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf4080")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf4080 : IdeEventoReinf4010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstabReinf4080")]
    [ComVisible(true)]
#endif
    public class IdeEstabReinf4080
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("ideFont")]
        public IdeFont IdeFont { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeFont")]
    [ComVisible(true)]
#endif
    public class IdeFont
    {
        [XmlElement("cnpjFont")]
        public string CnpjFont { get; set; }

        [XmlElement("ideRend")]
        public List<IdeRend> IdeRend { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeRend(IdeRend item)
        {
            if (IdeRend == null)
            {
                IdeRend = new List<IdeRend>();
            }

            IdeRend.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeRend (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeRend</returns>
        public IdeRend GetIdeRend(int index)
        {
            if ((IdeRend?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeRend[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeRend
        /// </summary>
        public int GetIdeRendCount => (IdeRend != null ? IdeRend.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRend")]
    [ComVisible(true)]
#endif
    public class IdeRend
    {
        [XmlElement("natRend")]
        public string NatRend { get; set; }

        [XmlElement("observ")]
        public string Observ { get; set; }

        [XmlElement("infoRec")]
        public List<InfoRec> InfoRec { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoRec(InfoRec item)
        {
            if (InfoRec == null)
            {
                InfoRec = new List<InfoRec>();
            }

            InfoRec.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoRec (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoRec</returns>
        public InfoRec GetInfoRec(int index)
        {
            if ((InfoRec?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoRec[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoRec
        /// </summary>
        public int GetInfoRecCount => (InfoRec != null ? InfoRec.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSereializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRec")]
    [ComVisible(true)]
#endif
    public class InfoRec
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
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
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

        [XmlElement("observ")]
        public string Observ {  get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeObserv() => !string.IsNullOrEmpty(Observ);

        #endregion

        [XmlElement("infoProcRet")]
        public List<InfoProcRetReinf4080> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRetReinf4080 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRetReinf4080>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRetReinf4080 GetInfoProcRet(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetReinf4080")]
    [ComVisible(true)]
#endif
    public class InfoProcRetReinf4080 : InfoProcRetReinf4040 { }
}
