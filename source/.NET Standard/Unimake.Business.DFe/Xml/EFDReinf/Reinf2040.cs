#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// R-2040 - Recursos repassados para associação desportiva
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2040")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRepassadoAssociacao/v2_01_02", IsNullable = false)]
    public class Reinf2040 : XMLBase
    {
        /// <summary>
        /// Evento recursos repassados para associações desportivas
        /// </summary>
        [XmlElement("evtAssocDespRep")]
        public EvtAssocDespRep EvtAssocDespRep { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtAssocDespRep")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtAssocDespRep : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento2040 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri2040 IdeContri { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2040")]
    [ComVisible(true)]
#endif
    public class IdeEvento2040 : IdeEvento2010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri2040")]
    [ComVisible(true)]
#endif
    public class IdeContri2040
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("ideEstab")]
        public IdeEstab2040 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab2040")]
    [ComVisible(true)]
#endif
    public class IdeEstab2040
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("recursosRep")]
        public List<RecursosRep> RecursosRep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRecursosRep(RecursosRep item)
        {
            if (RecursosRep == null)
            {
                RecursosRep = new List<RecursosRep>();
            }

            RecursosRep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RecursosRep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RecursosRep</returns>
        public RecursosRep GetRecursosRep(int index)
        {
            if ((RecursosRep?.Count ?? 0) == 0)
            {
                return default;
            };

            return RecursosRep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetRecursosRepCount => (RecursosRep != null ? RecursosRep.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RecursosRep")]
    [ComVisible(true)]
#endif
    public class RecursosRep
    {

        [XmlElement("cnpjAssocDesp")]
        public string CnpjAssocDesp { get; set; }

        [XmlIgnore]
        public double VlrTotalRep { get; set; }

        [XmlElement("vlrTotalRep")]
        public string VlrTotalRepField
        {
            get => VlrTotalRep.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRep = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalRet { get; set; }

        [XmlElement("vlrTotalRet")]
        public string VlrTotalRetField
        {
            get => VlrTotalRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrTotalNRet { get; set; }

        [XmlElement("vlrTotalNRet")]
        public string VlrTotalNRetField
        {
            get => VlrTotalNRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalNRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlElement("infoRecurso")]
        public List<InfoRecurso2040> InfoRecurso { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoRecurso(InfoRecurso2040 item)
        {
            if (InfoRecurso == null)
            {
                InfoRecurso = new List<InfoRecurso2040>();
            }

            InfoRecurso.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoRecurso2040 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoRecurso</returns>
        public InfoRecurso2040 GetInfoRecurso(int index)
        {
            if ((InfoRecurso?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoRecurso[index];
        }
        
        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetInfoRecursoCount => (InfoRecurso != null ? InfoRecurso.Count : 0);
#endif

        [XmlElement("infoProc")]
        public List<InfoProc2040> InfoProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProc(InfoProc2040 item)
        {
            if (InfoProc == null)
            {
                InfoProc = new List<InfoProc2040>();
            }

            InfoProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProc2040 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProc</returns>
        public InfoProc2040 GetInfoProc(int index)
        {
            if ((InfoProc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetInfoProcCount => (InfoProc != null ? InfoProc.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecurso2040")]
    [ComVisible(true)]
#endif
    public class InfoRecurso2040 : InfoRecurso2030 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProc2040")]
    [ComVisible(true)]
#endif
    public class InfoProc2040 : InfoProc2030 { }
}