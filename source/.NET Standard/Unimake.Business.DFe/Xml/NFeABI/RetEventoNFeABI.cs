#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Retorno do pedido de evento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.RetEventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("retEventoNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class RetEventoNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute do retorno.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Informações do resultado do evento.</summary>
        [XmlElement("infEvento")]
        public InfRetEventoNFeABI InfEvento { get; set; }

        /// <summary>Assinatura digital opcional do retorno.</summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>Informações do retorno de evento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfRetEventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class InfRetEventoNFeABI
    {
        /// <summary>Identificador opcional da resposta assinada.</summary>
        [XmlElement("Id")]
        public string Id { get; set; }

        /// <summary>Ambiente do processamento.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>Versão da aplicação autorizadora.</summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>Código do órgão de recepção do evento.</summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>Campo auxiliar para serialização de cOrgao.</summary>
        [XmlElement("cOrgao")]
        public int COrgaoField { get => (int)COrgao; set => COrgao = (UFBrasil)value; }

        /// <summary>Código do status da resposta.</summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>Descrição do status da resposta.</summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>Chave de acesso vinculada ao evento.</summary>
        [XmlElement("chNFeABI")]
        public string ChNFeABI { get; set; }

        /// <summary>Tipo do evento processado.</summary>
        [XmlElement("tpEvento")]
        public TipoEventoNFeABI TpEvento { get; set; }

        /// <summary>Descrição do resultado do evento.</summary>
        [XmlElement("xEvento")]
        public string XEvento { get; set; }

        /// <summary>Número sequencial do evento.</summary>
        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        /// <summary>Data e hora de registro do evento.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        /// <summary>Campo auxiliar para serialização de dhRegEvento.</summary>
        [XmlElement("dhRegEvento")]
        public string DhRegEventoField
        {
#if INTEROP
            get => DhRegEvento > DateTime.MinValue ? DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
#else
            get => DhRegEvento != default(DateTimeOffset) ? DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
#endif
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Número do protocolo do evento.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Indica se tpEvento deve ser serializado.</summary>
        public bool ShouldSerializeTpEvento() => (int)TpEvento > 0;

        /// <summary>Indica se nSeqEvento deve ser serializado.</summary>
        public bool ShouldSerializeNSeqEvento() => NSeqEvento > 0;

        /// <summary>Indica se dhRegEvento deve ser serializado.</summary>
#if INTEROP
        public bool ShouldSerializeDhRegEventoField() => DhRegEvento > DateTime.MinValue;
#else
        public bool ShouldSerializeDhRegEventoField() => DhRegEvento != default(DateTimeOffset);
#endif
    }
}
