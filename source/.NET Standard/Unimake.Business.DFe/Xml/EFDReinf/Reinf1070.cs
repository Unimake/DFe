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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1070")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02", IsNullable = false)]
    public class Reinf1070 : XMLBase
    {
        [XmlElement("evtTabProcesso")]
        public EvtTabProcesso EvtTabProcesso { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTabProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtTabProcesso : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoContri")]
        public InfoContri InfoContri { get; set; }

        [XmlElement("infoProcesso")]
        public InfoProcesso InfoProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcesso
    {
        [XmlElement("inclusao")]
        public InclusaoReinf1070 Inclusao { get; set; }

        [XmlElement("alteracao")]
        public AlteracaoReinf1070 Alteracao { get; set; }

        [XmlElement("exclusao")]
        public ExclusaoReinf1070 Exclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InclusaoReinf1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InclusaoReinf1070
    {
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeProcesso
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        [XmlElement("indAutoria")]
#if INTEROP
        public IndicativoAutoria IndAutoria { get; set; } = (IndicativoAutoria)(-1);
#else
        public IndicativoAutoria? IndAutoria { get; set; }
#endif

        [XmlElement("infoSusp")]
        public List<InfoSusp> InfoSusp { get; set; }

        [XmlElement("dadosProcJud")]
        public DadosProcJud DadosProcJud { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoSusp(InfoSusp item)
        {
            if (InfoSusp == null)
            {
                InfoSusp = new List<InfoSusp>();
            }

            InfoSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoSusp</returns>
        public InfoSusp GetInfoSusp(int index)
        {
            if ((InfoSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoSusp
        /// </summary>
        public int GetInfoSuspCount => (InfoSusp != null ? InfoSusp.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoSusp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoSusp
    {
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlElement("indSusp")]
        public IndicativoSuspensao IndSusp { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtDecisao { get; set; }
#else
        public DateTimeOffset DtDecisao { get; set; }
#endif

        [XmlElement("dtDecisao")]
        public string DtDecisaoField
        {
            get => DtDecisao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDecisao = DateTime.Parse(value);
#else
            set => DtDecisao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indDeposito")]
        public SimNaoLetra IndDeposito { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DadosProcJud")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class DadosProcJud
    {
        [XmlElement("ufVara")]
        public UFBrasil UfVara { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("idVara")]
        public string IdVara { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.AlteracaoReinf1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class AlteracaoReinf1070
    {
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }

        [XmlElement("infoSusp")]
        public List<InfoSusp> InfoSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoSusp(InfoSusp item)
        {
            if (InfoSusp == null)
            {
                InfoSusp = new List<InfoSusp>();
            }

            InfoSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoSusp</returns>
        public InfoSusp GetInfoSusp(int index)
        {
            if ((InfoSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoSusp
        /// </summary>
        public int GetInfoSuspCount => (InfoSusp != null ? InfoSusp.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ExclusaoReinf1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class ExclusaoReinf1070
    {
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }

    }
}