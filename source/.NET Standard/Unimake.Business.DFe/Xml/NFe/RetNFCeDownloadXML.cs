using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Estrutura XML do retorno do download de NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetNfceDownloadXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retNfceDownloadXML", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetNfceDownloadXML : XMLBase
    {
        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão da aplicação que processou a solicitação
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Data e hora da requisição
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhReq { get; set; }
#else
        public DateTimeOffset DhReq { get; set; }
#endif

        /// <summary>
        /// Data e hora da requisição (campo serializado)
        /// </summary>
        [XmlElement("dhReq")]
        public string DhReqField
        {
            get => DhReq.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DhReq = DateTime.Parse(value);
#else
            set => DhReq = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Código do status da resposta
        /// </summary>
        [XmlElement("cStat")]
        public string CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Informações do processamento da NFCe
        /// </summary>
        [XmlElement("proc")]
        public RetNfceDownloadXMLProc Proc { get; set; }

        /// <summary>
        /// Desserializar o arquivo XML no objeto RetNfceDownloadXML
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto do RetNfceDownloadXML</returns>
        public RetNfceDownloadXML LoadFromFile(string filename)
        {
            var conteudoXML = System.IO.File.ReadAllText(filename, System.Text.Encoding.UTF8);
            return XMLUtility.Deserializar<RetNfceDownloadXML>(conteudoXML);
        }

        /// <summary>
        /// Desserializar a string do XML RetNfceDownloadXML
        /// </summary>
        /// <param name="xml">String do XML</param>
        /// <returns>Objeto do RetNfceDownloadXML</returns>
        public RetNfceDownloadXML LoadFromXML(string xml) => XMLUtility.Deserializar<RetNfceDownloadXML>(xml);
    }

    /// <summary>
    /// Informações do processamento da NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetNfceDownloadXMLProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RetNfceDownloadXMLProc
    {
        /// <summary>
        /// NFCe processada (NfeProc completo)
        /// </summary>
        [XmlElement("nfeProc")]
        public NfeProc NfeProc { get; set; }

        /// <summary>
        /// Eventos da NFCe processados
        /// </summary>
        [XmlElement("procEventoNFe")]
        public List<ProcEventoNFe> ProcEventoNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo evento da NFCe
        /// </summary>
        /// <param name="procEventoNFe">Objeto do evento</param>
        public void AddProcEventoNFe(ProcEventoNFe procEventoNFe)
        {
            if (ProcEventoNFe == null)
            {
                ProcEventoNFe = new List<ProcEventoNFe>();
            }

            ProcEventoNFe.Add(procEventoNFe);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcEventoNFe
        /// </summary>
        /// <param name="index">Índice da lista (começa com 0)</param>
        /// <returns>Conteúdo do index da ProcEventoNFe</returns>
        public ProcEventoNFe GetProcEventoNFe(int index)
        {
            if ((ProcEventoNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista ProcEventoNFe
        /// </summary>
        public int GetProcEventoNFeCount => (ProcEventoNFe != null ? ProcEventoNFe.Count : 0);

#endif
    }
}
