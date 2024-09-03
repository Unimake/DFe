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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2501")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContProc/v_S_01_02_00", IsNullable = false)]
    public class ESocial2501 : XMLBase
    {
        [XmlElement("evtContProc")]
        public EvtContProc EvtContProc { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtContProc")]
    [ComVisible(true)]
#endif
    public class EvtContProc
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideProc")]
        public IdeProc IdeProc { get; set; }

        [XmlElement("ideTrab")]
        public List<IdeTrabESocial2501> IdeTrab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeTrab(IdeTrabESocial2501 item)
        {
            if (IdeTrab == null)
            {
                IdeTrab = new List<IdeTrabESocial2501>();
            }

            IdeTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeTrab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeTrab</returns>
        public IdeTrabESocial2501 GetIdeTrab(int index)
        {
            if ((IdeTrab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeTrab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeTrab
        /// </summary>
        public int GetIdeTrabCount => (IdeTrab != null ? IdeTrab.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProc")]
    [ComVisible(true)]
#endif
    public class IdeProc
    {
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerApurPgto { get; set; }
#else
        public DateTimeOffset PerApurPgto { get; set; }
#endif

        [XmlElement("perApurPgto")]
        public string PerApurPgtoField
        {
            get => PerApurPgto.ToString("yyyy-MM");
#if INTEROP
            set => PerApurPgto = DateTime.Parse(value);
#else
            set => PerApurPgto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("obs")]
        public string Obs { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObs() => !string.IsNullOrEmpty(Obs);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabESocial2501")]
    [ComVisible(true)]
#endif
    public class IdeTrabESocial2501
    {
        [XmlAttribute(AttributeName = "cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("calcTrib")]
        public List<CalcTrib> CalcTrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCalcTrib(CalcTrib item)
        {
            if (CalcTrib == null)
            {
                CalcTrib = new List<CalcTrib>();
            }

            CalcTrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CalcTrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CalcTrib</returns>
        public CalcTrib GetCalcTrib(int index)
        {
            if ((CalcTrib?.Count ?? 0) == 0)
            {
                return default;
            };

            return CalcTrib[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CalcTrib
        /// </summary>
        public int GetCalcTribCount => (CalcTrib != null ? CalcTrib.Count : 0);
#endif

        [XmlElement("infoCRIRRF")]
        public List<InfoCRIRRF> InfoCRIRRF { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRIRRF(InfoCRIRRF item)
        {
            if (InfoCRIRRF == null)
            {
                InfoCRIRRF = new List<InfoCRIRRF>();
            }

            InfoCRIRRF.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRIRRF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRIRRF</returns>
        public InfoCRIRRF GetInfoCRIRRF(int index)
        {
            if ((InfoCRIRRF?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRIRRF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRIRRF
        /// </summary>
        public int GetInfoCRIRRFCount => (InfoCRIRRF != null ? InfoCRIRRF.Count : 0);
#endif

        [XmlElement("infoIRComplem")]
        public InfoIRComplemESocial2501 InfoIRComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CalcTrib")]
    [ComVisible(true)]
#endif
    public class CalcTrib
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlAttribute(AttributeName = "perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        [XmlAttribute(AttributeName = "vrBcCpMensal")]
        public double VrBcCpMensal { get; set; }

        [XmlAttribute(AttributeName = "vrBcCp13")]
        public double VrBcCp13 { get; set; }

        [XmlElement("infoCRContrib")]
        public List<InfoCRContrib> InfoCRContrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRContrib(InfoCRContrib item)
        {
            if (InfoCRContrib == null)
            {
                InfoCRContrib = new List<InfoCRContrib>();
            }

            InfoCRContrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRContrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRContrib</returns>
        public InfoCRContrib GetInfoCRContrib(int index)
        {
            if ((InfoCRContrib?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRContrib[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRContrib
        /// </summary>
        public int GetInfoCRContribCount => (InfoCRContrib != null ? InfoCRContrib.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRContrib")]
    [ComVisible(true)]
#endif
    public class InfoCRContrib
    {
        [XmlAttribute(AttributeName = "tpCR")]
        public string TpCR { get; set; }

        [XmlAttribute(AttributeName = "vrCR")]
        public double VrCR { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRIRRF")]
    [ComVisible(true)]
#endif
    public class InfoCRIRRF
    {
        [XmlAttribute(AttributeName = "tpCR")]
        public string TpCR { get; set; }

        [XmlAttribute(AttributeName = "vrCR")]
        public double VrCR { get; set; }

        [XmlElement("infoIR")]
        public InfoIR InfoIR { get; set; }

        [XmlElement("infoRRA")]
        public InfoRRAESocial2501 InfoRRA { get; set; }

        [XmlElement("dedDepen")]
        public List<DedDepenESocial2501> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepenESocial2501 item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepenESocial2501>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepenESocial2501 GetDedDepen(int index)
        {
            if ((DedDepen?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedDepen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedDepen
        /// </summary>
        public int GetDedDepenCount => (DedDepen != null ? DedDepen.Count : 0);
#endif

        [XmlElement("penAlim")]
        public List<PenAlimESocial2501> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlimESocial2501 item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlimESocial2501>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlimESocial2501 GetPenAlim(int index)
        {
            if ((PenAlim?.Count ?? 0) == 0)
            {
                return default;
            };

            return PenAlim[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PenAlim
        /// </summary>
        public int GetPenAlimCount => (PenAlim != null ? PenAlim.Count : 0);
#endif

        [XmlElement("infoProcRet")]
        public List<InfoProcRetESocial2501> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRetESocial2501 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRetESocial2501>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRetESocial2501 GetInfoProcRet(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIR")]
    [ComVisible(true)]
#endif
    public class InfoIR
    {
        [XmlAttribute(AttributeName = "vrRendTrib")]
        public double VrRendTrib { get; set; }

        [XmlAttribute(AttributeName = "vrRendTrib13")]
        public double VrRendTrib13 { get; set; }

        [XmlAttribute(AttributeName = "vrRendMoleGrave")]
        public double VrRendMoleGrave { get; set; }

        [XmlAttribute(AttributeName = "vrRendIsen65")]
        public double VrRendIsen65 { get; set; }

        [XmlAttribute(AttributeName = "vrJurosMora")]
        public double VrJurosMora { get; set; }

        [XmlAttribute(AttributeName = "vrRendIsenNTrib")]
        public double VrRendIsenNTrib { get; set; }

        [XmlAttribute(AttributeName = "descIsenNTrib")]
        public string DescIsenNTrib { get; set; }

        [XmlAttribute(AttributeName = "vrPrevOficial")]
        public double VrPrevOficial { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrRendTrib() => VrRendTrib > 0;
      
        public bool ShouldSerializeVrRendTrib13() => VrRendTrib13 > 0;
       
        public bool ShouldSerializeVrRendMoleGrave() => VrRendMoleGrave > 0;
       
        public bool ShouldSerializeVrRendIsen65() => VrRendIsen65 > 0;
      
        public bool ShouldSerializeVrJurosMora() => VrJurosMora > 0;
      
        public bool ShouldSerializeVrRendIsenNTrib() => VrRendIsenNTrib > 0;

        public bool ShouldSerializeDescIsenNTrib() => !string.IsNullOrEmpty(DescIsenNTrib);

        public bool ShouldSerializeVrPrevOficial() => VrPrevOficial > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRAESocial2501")]
    [ComVisible(true)]
#endif
    public class InfoRRAESocial2501
    {
        [XmlAttribute(AttributeName = "descRRA")]
        public string DescRRA { get; set; }

        [XmlAttribute(AttributeName = "qtdMesesRRA")]
        public string QtdMesesRRA { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJudESocial2501 DespProcJud { get; set; }

        [XmlElement("ideAdv")]
        public List<IdeAdvESocial2501> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdvESocial2501 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdvESocial2501>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdvESocial2501 GetIdeAdv(int index)
        {
            if ((IdeAdv?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeAdv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeAdv
        /// </summary>
        public int GetIdeAdvCount => (IdeAdv != null ? IdeAdv.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJudESocial2501")]
    [ComVisible(true)]
#endif
    public class DespProcJudESocial2501
    {
        [XmlAttribute(AttributeName = "vlrDespCustas")]
        public double VlrDespCustas { get; set; }

        [XmlAttribute(AttributeName = "vlrDespAdvogados")]
        public double VlrDespAdvogados { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdvESocial2501")]
    [ComVisible(true)]
#endif
    public class IdeAdvESocial2501
    {
        [XmlAttribute(AttributeName = "tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlAttribute(AttributeName = "nrInsc")]
        public string NrInsc { get; set; }

        [XmlAttribute(AttributeName = "vlrAdv")]
        public double VlrAdv { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepenESocial2501")]
    [ComVisible(true)]
#endif
    public class DedDepenESocial2501
    {
        [XmlAttribute(AttributeName = "tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        [XmlAttribute(AttributeName = "vlrDeducao")]
        public double VlrDeducao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PenAlimESocial2501")]
    [ComVisible(true)]
#endif
    public class PenAlimESocial2501
    {
        [XmlAttribute(AttributeName = "tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        [XmlAttribute(AttributeName = "vlrPensao")]
        public double VlrPensao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcRetESocial2501")]
    [ComVisible(true)]
#endif
    public class InfoProcRetESocial2501
    {
        [XmlAttribute(AttributeName = "tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlAttribute(AttributeName = "nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlAttribute(AttributeName = "codSusp")]
        public string CodSusp { get; set; }

        [XmlElement("infoValores")]
        public List<InfoValoresESocial2501> InfoValores { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoValores(InfoValoresESocial2501 item)
        {
            if (InfoValores == null)
            {
                InfoValores = new List<InfoValoresESocial2501>();
            }

            InfoValores.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoValores (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoValores</returns>
        public InfoValoresESocial2501 GetInfoValores(int index)
        {
            if ((InfoValores?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoValores[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoValores
        /// </summary>
        public int GetInfoValoresCount => (InfoValores != null ? InfoValores.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoValoresESocial2501")]
    [ComVisible(true)]
#endif
    public class InfoValoresESocial2501
    {
        [XmlAttribute(AttributeName = "indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        [XmlAttribute(AttributeName = "vlrNRetido")]
        public double VlrNRetido { get; set; }

        [XmlAttribute(AttributeName = "vlrDepJud")]
        public double VlrDepJud { get; set; }

        [XmlAttribute(AttributeName = "vlrCmpAnoCal")]
        public double VlrCmpAnoCal { get; set; }

        [XmlAttribute(AttributeName = "vlrCmpAnoAnt")]
        public double VlrCmpAnoAnt { get; set; }

        [XmlAttribute(AttributeName = "vlrRendSusp")]
        public double VlrRendSusp { get; set; }

        [XmlElement("dedSusp")]
        public List<DedSuspESocial2501> DedSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedSusp(DedSuspESocial2501 item)
        {
            if (DedSusp == null)
            {
                DedSusp = new List<DedSuspESocial2501>();
            }

            DedSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedSusp</returns>
        public DedSuspESocial2501 GetDedSusp(int index)
        {
            if ((DedSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedSusp
        /// </summary>
        public int GetDedSuspCount => (DedSusp != null ? DedSusp.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrNRetido() => VlrNRetido > 0;

        public bool ShouldSerializeVlrDepJud() => VlrDepJud > 0;

        public bool ShouldSerializeVlrCmpAnoCal() => VlrCmpAnoCal > 0;

        public bool ShouldSerializeVlrCmpAnoAnt() => VlrCmpAnoAnt > 0;

        public bool ShouldSerializeVlrRendSusp() => VlrRendSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedSuspESocial2501")]
    [ComVisible(true)]
#endif
    public class DedSuspESocial2501
    {
        [XmlAttribute(AttributeName = "indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        [XmlAttribute(AttributeName = "vlrDedSusp")]
        public double VlrDedSusp { get; set; }

        [XmlElement("benefPen")]
        public List<BenefPenESocial2501> BenefPen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBenefPen(BenefPenESocial2501 item)
        {
            if (BenefPen == null)
            {
                BenefPen = new List<BenefPenESocial2501>();
            }

            BenefPen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BenefPen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BenefPen</returns>
        public BenefPenESocial2501 GetBenefPen(int index)
        {
            if ((BenefPen?.Count ?? 0) == 0)
            {
                return default;
            };

            return BenefPen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BenefPen
        /// </summary>
        public int GetBenefPenCount => (BenefPen != null ? BenefPen.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrDedSusp() => VlrDedSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BenefPenESocial2501")]
    [ComVisible(true)]
#endif
    public class BenefPenESocial2501
    {
        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        [XmlAttribute(AttributeName = "vlrDepenSusp")]
        public double VlrDepenSusp { get; set; }
    }

    public class InfoIRComplemESocial2501
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtLaudo { get; set; }
#else
        public DateTimeOffset DtLaudo { get; set; }
#endif

        [XmlAttribute(AttributeName = "dtLaudo")]
        public string DtLaudoField
        {
            get => DtLaudo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtLaudo = DateTime.Parse(value);
#else
            set => DtLaudo = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("infoDep")]
        public List<InfoDepESocial2501> InfoDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDep(InfoDepESocial2501 item)
        {
            if (InfoDep == null)
            {
                InfoDep = new List<InfoDepESocial2501>();
            }

            InfoDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDep</returns>
        public InfoDepESocial2501 GetInfoDep(int index)
        {
            if ((InfoDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDep
        /// </summary>
        public int GetInfoDepCount => (InfoDep != null ? InfoDep.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDepESocial2501")]
    [ComVisible(true)]
#endif
    public class InfoDepESocial2501
    {
        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlAttribute(AttributeName = "dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlAttribute(AttributeName = "nome")]
        public string Nome { get; set; }

        [XmlAttribute(AttributeName = "depIRRF")]
        public string DepIRRF { get; set; }

        [XmlIgnore]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);

        [XmlAttribute(AttributeName = "tpDep")]
        public TiposDeDependente TpDepAux
        {
            get { return TpDep; }
            set { TpDep = value; }
        }

#else
        public TiposDeDependente? TpDep { get; set; }

        [XmlAttribute(AttributeName = "tpDep")]
        public TiposDeDependente TpDepAux
        {
            get { return TpDep.GetValueOrDefault((TiposDeDependente)(-1)); }
            set { TpDep = value; }
        }
#endif

        [XmlAttribute(AttributeName = "descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);

        public bool ShouldSerializeDepIRRF() => !string.IsNullOrEmpty(DepIRRF);

#if INTEROP
        public bool ShouldSerializeTpDepAux()
        {
            return TpDep != (TiposDeDependente)(-1);
        }
#else
        public bool ShouldSerializeTpDepAux()
    {
        return TpDep.HasValue;
    }
#endif

        public bool ShouldSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }
}
