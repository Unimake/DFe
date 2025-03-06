#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Protocolo MDFe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ProtMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("protMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ProtMDFe
    {
        /// <summary>
        /// Versão do processamento do protocolo do MDFe.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do protocolo.
        /// </summary>
        [XmlElement("infProt")]
        public InfProt InfProt { get; set; }

        /// <summary>
        /// Informações do fisco.
        /// </summary>
        [XmlElement("infFisco")]
        public InfFisco InfFisco { get; set; }

        /// <summary>
        /// Assinatura digital do protocolo.
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfProt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfProt
    {
        /// <summary>
        /// Identificador da TAG a ser assinada.
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do aplicativo que processou o protocolo.
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Chave de acesso do MDFe.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Data e hora do recebimento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhRecbto" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Digest Value da MDFe processada.
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Código do status da resposta.
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta.
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfFisco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfFisco
    {
        /// <summary>
        /// Código da mensagem.
        /// </summary>
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        /// <summary>
        /// Descrição da mensagem.
        /// </summary>
        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }
}
