#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
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
            get => "ID" + ((int)TpEvento).ToString() + ChBPe + NSeqEvento.ToString("000");
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
}
