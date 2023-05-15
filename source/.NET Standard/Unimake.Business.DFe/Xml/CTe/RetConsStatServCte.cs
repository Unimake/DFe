#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetConsStatServCte")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsStatServCte", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetConsStatServCte : XMLBase
    {
        private const string FormatDate = "yyyy-MM-ddTHH:mm:sszzz";

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
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlIgnore]
#if INTEROP 
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString(FormatDate);
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tMed")]
        public int TMed { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRetorno { get; set; }
#else
        public DateTimeOffset DhRetorno { get; set; }
#endif

        [XmlElement("dhRetorno")]
        public string DhRetornoField
        {
            get => DhRetorno.ToString(FormatDate);
#if INTEROP
            set => DhRetorno = DateTime.Parse(value);
#else
            set => DhRetorno = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xObs")]
        public string XObs { get; set; }

        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public override XmlDocument GerarXML()
        {
            XmlDocument xml = null;

            if (Convert.ToDecimal(Versao) >= 400)
            {
                // criar uma instância de XmlRootAttribute com o novo nome do elemento
                var newRootAttribute = new XmlRootAttribute("retConsStatServCTe")
                {
                    Namespace = "http://www.portalfiscal.inf.br/cte",
                    IsNullable = false
                };

                xml = XMLUtility.Serializar(this, newRootAttribute, NameSpaces);
            }
            else
            {
                xml = XMLUtility.Serializar(this, NameSpaces);
            }

            return xml;
        }

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="doc">Conteúdo do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
        public override T LerXML<T>(XmlDocument doc) => XMLUtility.Deserializar<T>(doc.OuterXml.Replace("retConsStatServCTe", "retConsStatServCte"));
    }
}
