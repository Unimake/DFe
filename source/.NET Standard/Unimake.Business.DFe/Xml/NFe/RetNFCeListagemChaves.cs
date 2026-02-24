#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Estrutura XML para Consulta de Chaves de NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetNFCeListagemChaves")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retNfceListagemChaves", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetNFCeListagemChaves : XMLBase
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
        /// Código do status da mensagem enviada
        /// </summary>
        [XmlElement("cStat")]
        public string CStat { get; set; }

        /// <summary>
        /// Descrição do motivo da solicitação
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Chave de acesso da NFCe
        /// </summary>
        [XmlElement("chNFCe")]
        public List<string> ChNFCe { get; set; } = new List<string>();

        /// <summary>
        /// Data e Hora de emissão da última NFCe listada (AAAA-MM-DDThh:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmisUltNfce { get; set; }
#else
        public DateTimeOffset DhEmisUltNfce { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DataHoraInicial para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEmisUltNfce")]
        public string DhEmisUltNfceField
        {
            get => DhEmisUltNfce.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmisUltNfce = DateTime.Parse(value);
#else
            set => DhEmisUltNfce = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        /// <summary>
        /// Método auxiliar para adicionar chaves via INTEROP (List não é amigável em COM)
        /// </summary>
        public void AddChNFCe(string chave) => ChNFCe.Add(chave);
#endif

        /// <summary>
        /// Desserializar a string do XML RetNfceDownloadXML
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto do RetNfceDownloadXML</returns>
        public RetNFCeListagemChaves LoadFromXML(string xml) => XMLUtility.Deserializar<RetNFCeListagemChaves>(xml);
    }
}