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
    ///  R-1070 - Tabela de processos administrativos/judiciais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02", IsNullable = false)]
    public class Reinf1070 : XMLBase
    {
        /// <summary>
        /// Evento tabela de processos
        /// </summary>
        [XmlElement("evtTabProcesso")]
        public EvtTabProcesso EvtTabProcesso { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento tabela de processos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTabProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtTabProcesso : ReinfEventoBase
    {
        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do contribuinte
        /// </summary>
        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        /// <summary>
        /// Informações do contribuinte
        /// </summary>
        [XmlElement("infoContri")]
        public InfoContri InfoContri { get; set; }

        /// <summary>
        /// Informações do Processo
        /// </summary>
        [XmlElement("infoProcesso")]
        public InfoProcesso InfoProcesso { get; set; }
    }

    /// <summary>
    /// Informações do Processo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcesso
    {
        /// <summary>
        /// Inclusão de informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1070 Inclusao { get; set; }

        /// <summary>
        /// Alteração de informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1070 Alteracao { get; set; }

        /// <summary>
        /// Exclusão de informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclusao1070 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de Informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Inclusao1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Inclusao1070
    {
        /// <summary>
        /// Identificação do processo
        /// </summary>
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }
    }

    /// <summary>
    /// Identificação do processo
    /// </summary>
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

        /// <summary>
        /// Informações de suspensão de exibilidade de tributos
        /// </summary>
        [XmlElement("infoSusp")]
        public List<InfoSusp> InfoSusp { get; set; }

        /// <summary>
        /// Informações Complementares do Processo Judicial 
        /// </summary>
        [XmlElement("dadosProcJud")]
        public DadosProcJud DadosProcJud { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

#if INTEROP
        public bool ShouldSerializeIndAutoria() => IndAutoria != (IndicativoAutoria)(-1);
#else
        public bool ShouldSerializeIndAutoria() => IndAutoria != null;
#endif

        #endregion ShouldSerialize

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

    /// <summary>
    /// Informações de suspensão de exibilidade de tributos
    /// </summary>
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

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações Complementares do Processo Judicial 
    /// </summary>
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

    /// <summary>
    /// Alteração de informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Alteracao1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Alteracao1070
    {
        /// <summary>
        /// Informações de identificação do processo
        /// </summary>
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }

        [XmlElement("novaValidade")]
        public NovaValidade1070 NovaValidade { get; set; }
    }

    /// <summary>
    /// Novo período de validade das informações que
    /// estão sendo alteradas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NovaValidade1070")]
    [ComVisible(true)]
#endif
    public class NovaValidade1070 : NovaValidade1050 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Exclusao1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Exclusao1070
    {
        [XmlElement("ideProcesso")]
        public IdeProcessoExclusao IdeProcesso { get; set; }
    }

    /// <summary>
    /// Informações de identificação do processo de Exclusão
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeProcessoExclusao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeProcessoExclusao
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion ShouldSerialize
    }
}