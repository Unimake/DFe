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
using System.Text;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetDistDFeInt")]
    [ComVisible(true)]
#endif
    [XmlRoot("retDistDFeInt", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetDistDFeInt : XMLBase
    {
        /// <summary>
        /// Versão do leiaute XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do Ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do Aplicativo
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do status da resposta
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Data e hora da resposta
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhResp { get; set; }
#else
        public DateTimeOffset DhResp { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhResp" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhResp")]
        public string DhRespField
        {
            get => DhResp.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhResp = DateTime.Parse(value);
#else
            set => DhResp = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Último NSU retornado
        /// </summary>
        [XmlElement("ultNSU", DataType = "token")]
        public string UltNSU { get; set; }

        /// <summary>
        /// Maior NSU retornado
        /// </summary>
        [XmlElement("maxNSU", DataType = "token")]
        public string MaxNSU { get; set; }

        /// <summary>
        /// Lote de documentos compactados
        /// </summary>
        [XmlElement("loteDistDFeInt")]
        public LoteDistDFeInt LoteDistDFeInt { get; set; }

        /// <summary>
        /// Desserializar o XML RetDistDFeInt no objeto RetDistDFeInt
        /// </summary>
        /// <param name="filename">Localização do arquivo XML RetDistDFeInt</param>
        /// <returns>Objeto do RetDistDFeInt</returns>
        public RetDistDFeInt LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetDistDFeInt>(doc);
        }

        /// <summary>
        /// Desserializar o XML RetDistDFeInt no objeto RetDistDFeInt
        /// </summary>
        /// <param name="xml">string do XML RetDistDFeInt</param>
        /// <returns>Objeto da RetDistDFeInt</returns>
        public RetDistDFeInt LoadFromXML(string xml) => XMLUtility.Deserializar<RetDistDFeInt>(xml);
    }

    /// <remarks/>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.LoteDistDFeInt")]
    [ComVisible(true)]
#endif
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LoteDistDFeInt
    {
        /// <summary>
        /// Documentos compactados
        /// </summary>
        [XmlElement("docZip")]
        public List<DocZip> DocZip { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista DocZip (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DocZip</returns>
        public DocZip GetDocZip(int index)
        {
            if ((DocZip?.Count ?? 0) == 0)
            {
                return default;
            };

            return DocZip[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DocZip
        /// </summary>
        public int GetDocZipCount() => (DocZip != null ? DocZip.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DocZip")]
    [ComVisible(true)]
#endif
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DocZip
    {
        /// <summary>
        /// Número Sequencial Único
        /// </summary>
        [XmlAttribute("NSU", DataType = "token")]
        public string NSU { get; set; }

        /// <summary>
        /// Schema do documento
        /// </summary>
        [XmlAttribute("schema")]
        public string Schema { get; set; }

        /// <summary>
        /// Conteúdo do documento compactado em base64
        /// </summary>
        [XmlText(DataType = "base64Binary")]
        public byte[] Value { get; set; }

        /// <summary>
        /// Conteúdo do XML retornado no formato string
        /// </summary>
        [XmlIgnore]
        public string ConteudoXML => Compress.GZIPDecompress(Convert.ToBase64String(Value));

        /// <summary>
        /// Conteúdo do XML retornado no formato XmlDocument
        /// </summary>
        [XmlIgnore]
        public XmlDocument DocXML
        {
            get
            {
                var docXML = new XmlDocument();
                docXML.Load(Converter.StringToStreamUTF8(ConteudoXML));

                return docXML;
            }
        }

        /// <summary>
        /// Tipo dos XML retornados no DocZip
        /// </summary>
        [XmlIgnore]
        public TipoXMLDocZip TipoXML
        {
            get
            {
                var tipoXML = TipoXMLDocZip.Desconhecido;

                if (Schema.StartsWith("procEventoCTe"))
                {
                    tipoXML = TipoXMLDocZip.ProcEventoCTe;
                }
                else if (Schema.StartsWith("procCTe"))
                {
                    tipoXML = TipoXMLDocZip.ProcCTe;
                }

                return tipoXML;
            }
        }
    }
}
