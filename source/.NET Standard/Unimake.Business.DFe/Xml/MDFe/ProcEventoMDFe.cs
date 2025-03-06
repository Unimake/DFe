#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ProcEventoMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ProcEventoMDFe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Evento do MDFe.
        /// </summary>
        [XmlElement("eventoMDFe")]
        public EventoMDFe EventoMDFe { get; set; }

        /// <summary>
        /// Retorno do evento do MDFe.
        /// </summary>
        [XmlElement("retEventoMDFe")]
        public RetEventoMDFe RetEventoMDFe { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição.
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => EventoMDFe.InfEvento.ChMDFe + "_" + ((int)EventoMDFe.InfEvento.TpEvento).ToString("000000") + "_" + EventoMDFe.InfEvento.NSeqEvento.ToString("00") + "-proceventomdfe.xml";

        /// <summary>
        /// Gera o XML do objeto ProcEventoMDFe.
        /// </summary>
        /// <returns>XmlDocument contendo o XML gerado.</returns>
        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            XmlRootAttribute attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            XmlElement xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("eventoMDFe")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            XmlElement xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEventoMDFe")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            XmlElement xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        /// <summary>
        /// Lê o XML e deserializa para o objeto ProcEventoMDFe.
        /// </summary>
        /// <param name="document">XmlDocument contendo o XML a ser lido.</param>
        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("eventoMDFe");

            if (nodeListEvento != null)
            {
                EventoMDFe = XMLUtility.Deserializar<EventoMDFe>(((XmlElement)nodeListEvento[0]).OuterXml);
                var nodeListEventoSignature = ((XmlElement)nodeListEvento[0]).GetElementsByTagName("Signature");
                if (nodeListEventoSignature != null)
                {
                    EventoMDFe.Signature = XMLUtility.Deserializar<Signature>(((XmlElement)nodeListEventoSignature[0]).OuterXml);
                }
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEventoMDFe");
            if (nodeListRetEvento != null)
            {
                RetEventoMDFe = XMLUtility.Deserializar<RetEventoMDFe>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }
    }
}
