#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPeTM;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// Evento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EventoBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EventoBPe : XMLBase
    {
        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informacoes do evento
        /// </summary>
        [XmlElement("infEvento")]
        public InfEventoBPe InfEvento { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Informacoes do evento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfEventoBPe")]
    [ComVisible(true)]
#endif
    public class InfEventoBPe
    {
        /// <summary>
        /// Identificador da tag a ser assinada
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChBPe + NSeqEvento.ToString("00");
            set => _ = value;
        }

        /// <summary>
        /// Codigo do orgao de recepcao do evento
        /// </summary>
        [XmlElement("cOrgao")]
        public int COrgao { get; set; }

        /// <summary>
        /// Identificacao do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// CNPJ do emissor do evento
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Chave de acesso do BP-e vinculado ao evento
        /// </summary>
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }

        /// <summary>
        /// Data e hora do evento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        /// <summary>
        /// Data e hora do evento serializada no XML
        /// </summary>
        [XmlElement("dhEvento")]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEvento = DateTime.Parse(value);
#else
            set => DhEvento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo do evento
        /// </summary>
        [XmlElement("tpEvento")]
        public TipoEventoBPe TpEvento { get; set; }

        /// <summary>
        /// Sequencial do evento
        /// </summary>
        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Detalhamento do evento
        /// </summary>
        [XmlElement("detEvento")]
        public DetEventoBPe DetEvento { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.DetEventoBPe")]
    [ComVisible(true)]
#endif
    public class DetEventoBPe
    {
        /// <summary>
        /// Versao do evento
        /// </summary>
        [XmlAttribute(AttributeName = "versaoEvento", DataType = "token")]
        public string VersaoEvento { get; set; }

        /// <summary>
        /// Evento de cancelamento
        /// </summary>
        [XmlElement("evCancBPe")]
        public EvCancBPe EvCancBPe { get; set; }

        /// <summary>
        /// Evento de nao embarque
        /// </summary>
        [XmlElement("evNaoEmbBPe")]
        public EvNaoEmbBPe EvNaoEmbBPe { get; set; }

        /// <summary>
        /// Evento de alteracao de poltrona
        /// </summary>
        [XmlElement("evAlteracaoPoltrona")]
        public EvAlteracaoPoltrona EvAlteracaoPoltrona { get; set; }

        /// <summary>
        /// Evento de excesso de bagagem
        /// </summary>
        [XmlElement("evExcessoBagagem")]
        public EvExcessoBagagem EvExcessoBagagem { get; set; }

        /// <summary>
        /// Evento de vinculacao do pagamento
        /// </summary>
        [XmlElement("evVincPgto")]
        public EvVincPgto EvVincPgto { get; set; }

        /// <summary>
        /// Evento de cancelamento da vinculacao do pagamento
        /// </summary>
        [XmlElement("evCancVincPgto")]
        public EvCancVincPgto EvCancVincPgto { get; set; }

        /// <summary>Evento de tributação do Simples Nacional.</summary>
        [XmlElement("evTributacaoSN")]
        public EvTributacaoSN EvTributacaoSN { get; set; }

        /// <summary>Evento de cancelamento da tributação do Simples Nacional.</summary>
        [XmlElement("evCancTribSN")]
        public EvCancTribSN EvCancTribSN { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de cancelamento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvCancBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evCancBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvCancBPe : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento";

        /// <summary>
        /// Numero do protocolo de status do BP-e
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Justificativa do cancelamento
        /// </summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de nao embarque do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvNaoEmbBPe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evNaoEmbBPe", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvNaoEmbBPe : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Nao Embarque";

        /// <summary>
        /// Numero do protocolo de status do BP-e
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Justificativa do nao embarque
        /// </summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de alteracao de poltrona do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvAlteracaoPoltrona")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evAlteracaoPoltrona", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvAlteracaoPoltrona : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Alteracao Poltrona";

        /// <summary>
        /// Numero do protocolo de status do BP-e
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Numero da poltrona, assento ou cabine
        /// </summary>
        [XmlElement("poltrona")]
        public string Poltrona { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de excesso de bagagem do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvExcessoBagagem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evExcessoBagagem", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvExcessoBagagem : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Excesso Bagagem";

        /// <summary>
        /// Numero do protocolo de status do BP-e
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Quantidade de volumes de bagagem carregados
        /// </summary>
        [XmlElement("qBagagem")]
        public string QBagagem { get; set; }

        /// <summary>
        /// Valor total do servico
        /// </summary>
        [XmlIgnore]
        public double VTotBag { get; set; }

        /// <summary>
        /// Valor total do servico serializado no XML
        /// </summary>
        [XmlElement("vTotBag")]
        public string VTotBagField
        {
            get => BPeFormat.Format2(VTotBag);
            set => VTotBag = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de informacoes do IBS e CBS
        /// </summary>
        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        /// <summary>
        /// Valor total do documento fiscal
        /// </summary>
        [XmlIgnore]
        public double? VTotDFe { get; set; }

        /// <summary>
        /// Valor total do documento fiscal serializado no XML
        /// </summary>
        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => BPeFormat.Format2(VTotDFe);
            set => VTotDFe = Converter.ToDouble(value);
        }

        public bool ShouldSerializeIBSCBS() => IBSCBS != null;
        public bool ShouldSerializeVTotDFeField() => VTotDFe != null;
    }

    /// <summary>
    /// Detalhamento do evento de vinculacao do pagamento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvVincPgto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evVincPgto", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvVincPgto : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Vinculacao do Pagamento";

        /// <summary>
        /// Numero do protocolo de autorizacao do DFe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Vinculacao com o pagamento
        /// </summary>
        [XmlElement("pgto")]
        public Pgto Pgto { get; set; }
    }

    /// <summary>
    /// Detalhamento do evento de cancelamento da vinculacao do pagamento do BP-e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvCancVincPgto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evCancVincPgto", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvCancVincPgto : XMLBase
    {
        /// <summary>
        /// Descricao do evento
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento da vinculacao do pagamento";

        /// <summary>
        /// Numero do protocolo de autorizacao do DFe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Numero do protocolo de autorizacao do evento a ser cancelado
        /// </summary>
        [XmlElement("nProtVincPgto")]
        public string NProtVincPgto { get; set; }
    }
    /// <summary>Tributacao do Simples Nacional</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvTributacaoSN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evTributacaoSN", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvTributacaoSN : XMLBase
    {
        /// <summary>Descrição do evento.</summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Tributacao do Simples Nacional";

        /// <summary>Número do protocolo de autorização do BP-e.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Tributação do Simples Nacional por item.</summary>
        [XmlElement("gTribItemSN")]
        public List<GTribItemSN> GTribItemSN { get; set; }

        /// <summary>Totais da tributação do Simples Nacional.</summary>
        [XmlElement("gTotalSN")]
        public GTotalSN GTotalSN { get; set; }

#if INTEROP
        /// <summary>Adiciona um item de tributação do Simples Nacional.</summary>
        public void AddGTribItemSN(GTribItemSN item)
        {
            if (GTribItemSN == null)
            {
                GTribItemSN = new List<GTribItemSN>();
            }
            GTribItemSN.Add(item);
        }

        /// <summary>Obtém um item de tributação pelo índice.</summary>
        public GTribItemSN GetGTribItemSN(int index) => GTribItemSN == null || GTribItemSN.Count == 0 ? default : GTribItemSN[index];

        /// <summary>Quantidade de itens de tributação.</summary>
        public int GetGTribItemSNCount => GTribItemSN?.Count ?? 0;
#endif
    }

    /// <summary>Cancelamento da Tributacao do Simples Nacional</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EvCancTribSN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("evCancTribSN", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class EvCancTribSN : XMLBase
    {
        /// <summary>Descrição do evento.</summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento da Tributacao do Simples Nacional";

        /// <summary>Número do protocolo de autorização do BP-e.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Protocolo do evento de tributação do Simples Nacional cancelado.</summary>
        [XmlElement("nProtEvTribSN")]
        public string NProtEvTribSN { get; set; }
    }

    /// <summary>Tributação do Simples Nacional por item.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GTribItemSN")]
    [ComVisible(true)]
#endif
    public class GTribItemSN
    {
        /// <summary>Número do item.</summary>
        [XmlAttribute("nItem")]
        public string NItem { get; set; }

        /// <summary>Valor de vRBSNItem.</summary>
        [XmlIgnore]
        public double VRBSNItem { get; set; }

        /// <summary>Propriedade auxiliar para serialização de VRBSNItem.</summary>
        [XmlElement("vRBSNItem")]
        public string VRBSNItemField
        {
            get => VRBSNItem.ToString("F2", CultureInfo.InvariantCulture);
            set => VRBSNItem = Converter.ToDouble(value);
        }

        /// <summary>Tipo de receita bruta do Simples Nacional.</summary>
        [XmlElement("tpRBSN")]
        public TipoReceitaBrutaSimplesNacional TpRBSN { get; set; }
        /// <summary>Alíquota de pIBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double PIBSSN { get; set; } = -1;
#else
        public double? PIBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de PIBSSN.</summary>
        [XmlElement("pIBSSN")]
        public string PIBSSNField
        {
            get => ((double?)PIBSSN)?.ToString("0.00##", CultureInfo.InvariantCulture);
            set => PIBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se PIBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializePIBSSNField() => PIBSSN >= 0;
#else
        public bool ShouldSerializePIBSSNField() => PIBSSN.HasValue;
#endif

        /// <summary>Valor de vIBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSN { get; set; } = -1;
#else
        public double? VIBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSN.</summary>
        [XmlElement("vIBSSN")]
        public string VIBSSNField
        {
            get => ((double?)VIBSSN)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNField() => VIBSSN >= 0;
#else
        public bool ShouldSerializeVIBSSNField() => VIBSSN.HasValue;
#endif

        /// <summary>Alíquota de pCBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double PCBSSN { get; set; } = -1;
#else
        public double? PCBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de PCBSSN.</summary>
        [XmlElement("pCBSSN")]
        public string PCBSSNField
        {
            get => ((double?)PCBSSN)?.ToString("0.00##", CultureInfo.InvariantCulture);
            set => PCBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se PCBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializePCBSSNField() => PCBSSN >= 0;
#else
        public bool ShouldSerializePCBSSNField() => PCBSSN.HasValue;
#endif

        /// <summary>Valor de vCBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSN { get; set; } = -1;
#else
        public double? VCBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSN.</summary>
        [XmlElement("vCBSSN")]
        public string VCBSSNField
        {
            get => ((double?)VCBSSN)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNField() => VCBSSN >= 0;
#else
        public bool ShouldSerializeVCBSSNField() => VCBSSN.HasValue;
#endif

        /// <summary>Valor de vIBSPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSPendSusp { get; set; } = -1;
#else
        public double? VIBSPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSPendSusp.</summary>
        [XmlElement("vIBSPendSusp")]
        public string VIBSPendSuspField
        {
            get => ((double?)VIBSPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSPendSuspField() => VIBSPendSusp >= 0;
#else
        public bool ShouldSerializeVIBSPendSuspField() => VIBSPendSusp.HasValue;
#endif

        /// <summary>Valor de vCBSPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSPendSusp { get; set; } = -1;
#else
        public double? VCBSPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSPendSusp.</summary>
        [XmlElement("vCBSPendSusp")]
        public string VCBSPendSuspField
        {
            get => ((double?)VCBSPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSPendSuspField() => VCBSPendSusp >= 0;
#else
        public bool ShouldSerializeVCBSPendSuspField() => VCBSPendSusp.HasValue;
#endif


    }

    /// <summary>Totais da tributação do Simples Nacional.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GTotalSN")]
    [ComVisible(true)]
#endif
    public class GTotalSN
    {
        /// <summary>Valor de vRBSNTot.</summary>
        [XmlIgnore]
        public double VRBSNTot { get; set; }

        /// <summary>Propriedade auxiliar para serialização de VRBSNTot.</summary>
        [XmlElement("vRBSNTot")]
        public string VRBSNTotField
        {
            get => VRBSNTot.ToString("F2", CultureInfo.InvariantCulture);
            set => VRBSNTot = Converter.ToDouble(value);
        }

        /// <summary>Valor de vIBSSNTot.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSNTot { get; set; } = -1;
#else
        public double? VIBSSNTot { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSNTot.</summary>
        [XmlElement("vIBSSNTot")]
        public string VIBSSNTotField
        {
            get => ((double?)VIBSSNTot)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSNTot = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSNTot foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNTotField() => VIBSSNTot >= 0;
#else
        public bool ShouldSerializeVIBSSNTotField() => VIBSSNTot.HasValue;
#endif

        /// <summary>Valor de vIBSSNtotPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSNtotPendSusp { get; set; } = -1;
#else
        public double? VIBSSNtotPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSNtotPendSusp.</summary>
        [XmlElement("vIBSSNtotPendSusp")]
        public string VIBSSNtotPendSuspField
        {
            get => ((double?)VIBSSNtotPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSNtotPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSNtotPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNtotPendSuspField() => VIBSSNtotPendSusp >= 0;
#else
        public bool ShouldSerializeVIBSSNtotPendSuspField() => VIBSSNtotPendSusp.HasValue;
#endif

        /// <summary>Valor de vCBSSNTot.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSNTot { get; set; } = -1;
#else
        public double? VCBSSNTot { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSNTot.</summary>
        [XmlElement("vCBSSNTot")]
        public string VCBSSNTotField
        {
            get => ((double?)VCBSSNTot)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSNTot = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSNTot foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNTotField() => VCBSSNTot >= 0;
#else
        public bool ShouldSerializeVCBSSNTotField() => VCBSSNTot.HasValue;
#endif

        /// <summary>Valor de vCBSSNtotPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSNtotPendSusp { get; set; } = -1;
#else
        public double? VCBSSNtotPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSNtotPendSusp.</summary>
        [XmlElement("vCBSSNtotPendSusp")]
        public string VCBSSNtotPendSuspField
        {
            get => ((double?)VCBSSNtotPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSNtotPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSNtotPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNtotPendSuspField() => VCBSSNtotPendSusp >= 0;
#else
        public bool ShouldSerializeVCBSSNtotPendSuspField() => VCBSSNtotPendSusp.HasValue;
#endif


    }
}
