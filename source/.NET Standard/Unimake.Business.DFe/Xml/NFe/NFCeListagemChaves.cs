#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Estrutura XML para Listagem de Chaves de NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFCeListagemChaves")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("nfceListagemChaves", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class NFCeListagemChaves : XMLBase
    {

        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do ambiente serializado
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Data e Hora de início do período que compreende a listagem (AAAA-MM-DDThh:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataHoraInicial { get; set; }
#else
        public DateTimeOffset DataHoraInicial { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DataHoraInicial para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dataHoraInicial")]
        public string DataHoraInicialField
        {
            get => DataHoraInicial.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DataHoraInicial = DateTime.Parse(value);
#else
            set => DataHoraInicial = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data e Hora de final do período que compreende a listagem (AAAA-MM-DDThh:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataHoraFinal { get; set; }
#else
        public DateTimeOffset DataHoraFinal { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DataHoraInicial para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dataHoraFinal")]
        public string DataHoraFinalField
        {
            get => DataHoraFinal.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DataHoraFinal = DateTime.Parse(value);
#else
            set => DataHoraFinal = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Desserializar a string do XML NfceDownloadXML
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto do NfceDownloadXML</returns>
        public NFCeListagemChaves LoadFromXML(string xml) => XMLUtility.Deserializar<NFCeListagemChaves>(xml);
    }
}