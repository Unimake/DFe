#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcEventoNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ProcEventoNFe : XMLBase
    {
        #region Public Properties

        [XmlElement("evento", Order = 0, Namespace = "http://www.portalfiscal.inf.br/nfe")]
        public Evento Evento { get; set; }

        /// <summary>
        /// Nome do arquivo de distribuição
        /// </summary>
        [XmlIgnore]
        public string NomeArquivoDistribuicao => Evento.InfEvento.ChNFe + "_" + ((int)Evento.InfEvento.TpEvento).ToString("000000") + "_" + Evento.InfEvento.NSeqEvento.ToString("00") + "-proceventonfe.xml";

        [XmlElement("retEvento", Order = 1, Namespace = "http://www.portalfiscal.inf.br/nfe")]
        public RetEvento RetEvento { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            var xmlElementEvento = (XmlElement)xmlDocument.GetElementsByTagName("evento")[0];
            xmlElementEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEvento = (XmlElement)xmlDocument.GetElementsByTagName("retEvento")[0];
            xmlElementRetEvento.SetAttribute("xmlns", attribute.Namespace);

            var xmlElementRetEventoInfEvento = (XmlElement)xmlElementRetEvento.GetElementsByTagName("infEvento")[0];
            xmlElementRetEventoInfEvento.SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            var nodeListEvento = document.GetElementsByTagName("evento");

            if (nodeListEvento != null)
            {
                Evento = XMLUtility.Deserializar<Evento>(((XmlElement)nodeListEvento[0]).OuterXml);
                var nodeListEventoSignature = ((XmlElement)nodeListEvento[0]).GetElementsByTagName("Signature");
                if (nodeListEventoSignature != null)
                {
                    //TODO: Wandrey - Pelo que eu vi a SEFAZ MG corrigiu esta falha, testar consultando a chave 31230507400075000109550090000001901987052951 em homologação, se sim, remover esta parte específica.

                    //SEFAZ MG está retornando o nome da tag signature da seguinte forma <Signature:Signature> e o correto é somente <Signature>
                    //Até que eles façam a correção, já solicitamos abertura de chamado na SEFAZ MG, vamos manter este código para evitar erro de objeto não reconhecido.
                    if (Evento.InfEvento.ChNFe.Substring(0, 2) == "31")
                    {
                        var nodeListSignature = ((XmlElement)nodeListEvento[0]).GetElementsByTagName("Signature:Signature");

                        if (nodeListSignature.Count > 0)
                        {
                            nodeListEventoSignature = ((XmlElement)nodeListEvento[0]).GetElementsByTagName("Signature:Signature");

                            if (nodeListEventoSignature.Count > 0 && (((XmlElement)nodeListEventoSignature[0]).OuterXml.Contains("Signature:Signature")))
                            {
                                Evento.Signature = XMLUtility.Deserializar<Signature>(((XmlElement)nodeListEventoSignature[0]).OuterXml.Replace("Signature:Signature", "Signature").Replace("xmlns:Signature=\"http://www.w3.org/2000/09/xmldsig#\"", ""));
                            }
                        }
                        else
                        {
                            var signature = ((XmlElement)nodeListEventoSignature[0]).OuterXml;

                            signature = signature.Replace("<Signature xmlns=\"http://www.portalfiscal.inf.br/nfe\">", "<Signature xmlns=\"http://www.w3.org/2000/09/xmldsig#\">");

                            Evento.Signature = XMLUtility.Deserializar<Signature>(signature);
                        }
                    }
                    else
                    {
                        Evento.Signature = XMLUtility.Deserializar<Signature>(((XmlElement)nodeListEventoSignature[0]).OuterXml);
                    }
                }
            }

            var nodeListRetEvento = document.GetElementsByTagName("retEvento");
            if (nodeListRetEvento.Count > 0)
            {
                RetEvento = XMLUtility.Deserializar<RetEvento>(((XmlElement)nodeListRetEvento[0]).OuterXml);
            }
        }

        #endregion Public Methods
    }
}