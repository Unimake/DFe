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
        /// Data e Hora de emissão da última NFCe listada
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
            get => DhEmisUltNfce.ToString("yyyy-MM-ddTHH:mm");
#if INTEROP
            set => DhEmisUltNfce = DateTime.Parse(value);
#else
            set => DhEmisUltNfce = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddChNFCe(string item)
        {
            if (ChNFCe == null)
            {
                ChNFCe = new List<string>();
            }

            ChNFCe.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ChNFCe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ChNFCe</returns>
        public string GetChNFCe(int index)
        {
            if ((ChNFCe?.Count ?? 0) == 0)
            {
                return default;
            }

            return ChNFCe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AutXML
        /// </summary>
        public int GetChNFCeCount => (ChNFCe != null ? ChNFCe.Count : 0);

#endif

        /// <summary>
        /// Desserializar a string do XML RetNFCeListagemChaves
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto do RetNFCeListagemChaves</returns>
        public RetNFCeListagemChaves LoadFromXML(string xml) => XMLUtility.Deserializar<RetNFCeListagemChaves>(xml);
    }
}