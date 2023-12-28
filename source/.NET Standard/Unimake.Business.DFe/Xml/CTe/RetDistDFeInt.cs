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
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhResp { get; set; }
#else
        public DateTimeOffset DhResp { get; set; }
#endif

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

        [XmlElement("ultNSU", DataType = "token")]
        public string UltNSU { get; set; }

        [XmlElement("maxNSU", DataType = "token")]
        public string MaxNSU { get; set; }

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
        [XmlAttribute("NSU", DataType = "token")]
        public string NSU { get; set; }

        [XmlAttribute("schema")]
        public string Schema { get; set; }

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
