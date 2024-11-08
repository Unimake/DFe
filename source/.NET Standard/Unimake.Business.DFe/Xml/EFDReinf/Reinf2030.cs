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
    /// <summary>
    /// R-2030 - Recursos recebidos por associação desportiva
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRecebidoAssociacao/v2_01_02", IsNullable = false)]
    public class Reinf2030 : XMLBase
    {
        /// <summary>
        /// Evento recursos recebidos por associação desportiva
        /// </summary>
        [XmlElement("evtAssocDespRec")]
        public EvtAssocDespRec EvtAssocDespRec { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtAssocDespRec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtAssocDespRec : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento2030 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri2030 IdeContri { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEvento2030 : IdeEvento2010 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeContri2030 : IdeContri
    {
        [XmlElement("ideEstab")]
        public IdeEstab2030 IdeEstab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEstab2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEstab2030
    {
        [XmlElement("tpInscEstab")]
        public TipoInscricaoEstabelecimento TpInscEstab { get; set; }

        [XmlElement("nrInscEstab")]
        public string NrInscEstab { get; set; }

        [XmlElement("recursosRec")]
        public List<RecursosRec> RecursosRec { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRecursosRec(RecursosRec item)
        {
            if (RecursosRec == null)
            {
                RecursosRec = new List<RecursosRec>();
            }

            RecursosRec.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RecursosRec (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RecursosRec</returns>
        public RecursosRec GetRecursosRec(int index)
        {
            if ((RecursosRec?.Count ?? 0) == 0)
            {
                return default;
            };

            return RecursosRec[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Nfs
        /// </summary>
        public int GetRecursosRecCount => (RecursosRec != null ? RecursosRec.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RecursosRec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class RecursosRec
    {

        [XmlElement("cnpjOrigRecurso")]
        public string CnpjOrigRecurso { get; set; }

        [XmlElement("recEmprExt")]
        public string RecEmprExt { get; set; }

        [XmlElement("nmEmprExt")]
        public string NmEmprExt { get; set; }

        [XmlIgnore]
        public double VlrTotalRec { get; set; }

        [XmlElement("vlrTotalRec")]
        public string VlrTotalRecField
        {
            get => VlrTotalRec.ToString("F2", CultureInfoReinf.Info);
            set => VlrTotalRec = double.Parse(value.ToString(), CultureInfoReinf.Info);
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
        public List<InfoRecurso2030> InfoRecurso { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoRecurso(InfoRecurso2030 item)
        {
            if (InfoRecurso == null)
            {
                InfoRecurso = new List<InfoRecurso2030>();
            }

            InfoRecurso.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoRecurso2030 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoRecurso</returns>
        public InfoRecurso2030 GetInfoRecurso(int index)
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
        public List<InfoProc2030> InfoProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProc(InfoProc2030 item)
        {
            if (InfoProc == null)
            {
                InfoProc = new List<InfoProc2030>();
            }

            InfoProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProc2030 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProc</returns>
        public InfoProc2030 GetInfoProc(int index)
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

        #region ShouldSerialize

        public bool ShouldSerializeCnpjOrigRecurso() => !string.IsNullOrEmpty(CnpjOrigRecurso);
        
        public bool ShouldSerializeRecEmprExt() => !string.IsNullOrEmpty(RecEmprExt);
        
        public bool ShouldSerializeNmEmprExt() => !string.IsNullOrEmpty(NmEmprExt);

        public bool ShouldSerializeVlrTotalNRetField() => VlrTotalNRet > 0;

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoRecurso2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoRecurso2030
    {
        [XmlElement("tpRepasse")]
        public TipoRepasse TpRepasse { get; set; }

        [XmlElement("descRecurso")]
        public string DescRecurso { get; set; }

        [XmlIgnore]
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfoReinf.Info);
            set => VlrBruto = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetApur { get; set; }

        [XmlElement("vlrRetApur")]
        public string VlrRetApurField
        {
            get => VlrRetApur.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetApur = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProc2030")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProc2030
    {

        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlIgnore]
        public double VlrNRet { get; set; }

        [XmlElement("vlrNRet")]
        public string VlrNRetField
        {
            get => VlrNRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion ShouldSerialize
    }
}
