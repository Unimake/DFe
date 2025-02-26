#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de retorno da inutilização de número da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetInutNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retInutNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetInutNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno da inutilização de número da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados do retorno do pedido de inutilização de numeração da NFe/NFCe
        /// </summary>
        [XmlElement("infInut")]
        public InfInut InfInut { get; set; }

        /// <summary>
        /// Desserializar o arquivo XML no objeto RetInutNFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto do XML</returns>
        public RetInutNFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetInutNFe>(doc);
        }

        /// <summary>
        /// Desserializar a string do XML RetInutNFe no objeto RetInutNFe
        /// </summary>
        /// <param name="xml">string do XML</param>
        /// <returns>Objeto da RetInutNFe</returns>
        public RetInutNFe LoadFromXML(string xml) => XMLUtility.Deserializar<RetInutNFe>(xml);
    }

    /// <summary>
    /// Classe de dados do retorno do pedido de inutilização de numeração da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfInut")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType("infInut", AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfInut
    {
        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do aplicativo que processou a NFe/NFCe
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do status da mensagem enviada
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status do serviço solicitado
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Código da UF que atendeu a solicitação
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Ano de inutilização da numeração
        /// </summary>
        [XmlElement("ano")]
        public string Ano { get; set; }

        /// <summary>
        /// CNPJ do emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Modelo da NFe/NFCe
        /// </summary>
        [XmlIgnore]
        public ModeloDFe Mod { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade Mod para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("mod")]
        public int ModField
        {
            get => (int)Mod;
            set => Mod = (ModeloDFe)Enum.Parse(typeof(ModeloDFe), value.ToString());
        }

        /// <summary>
        /// Série da NFe/NFCe
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número da NFe/NFCe inicial
        /// </summary>
        [XmlElement("nNFIni")]
        public string NNFIni { get; set; }

        /// <summary>
        /// Número da NFe/NFCe final
        /// </summary>
        [XmlElement("nNFFin")]
        public string NNFFin { get; set; }

        /// <summary>
        /// Data e hora de recebimento, no formato AAAA-MM-DDTHH:MM:SS.
        /// Deve ser preenchida com data e hora da gravação no Banco em caso de Confirmação.
        /// Em caso de Rejeição, com data e hora do recebimento do Pedido de Inutilização
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif
        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhRecbto para atribuir ou resgatar o valor)
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
        /// Número do Protocolo de status da NFe/NFCe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// ID
        /// </summary>
        [XmlElement("Id")]
        public string Id { get; set; }
    }
}
