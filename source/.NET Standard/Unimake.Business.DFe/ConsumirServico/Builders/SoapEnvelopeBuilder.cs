using System;
using System.Text;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Builders
{
    internal sealed class SoapEnvelopeBuilder
    {
        public string Build(WSSoap soap, string xmlBody, X509Certificate2 certificado, SoapEnvelopeContext context)
        {
            if (soap.GZIPCompress)
            {
                xmlBody = Compress.GZIPCompress(xmlBody);
            }

            if (xmlBody.IndexOf("?>") >= 0)
            {
                xmlBody = xmlBody.Substring(xmlBody.IndexOf("?>") + 2);
            }

            var retorna = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

            if (soap.TemCDATA)
            {
                soap.SoapString = soap.SoapString.Replace("{cCDATA}", "]]>").Replace("{oCDATA}", "<![CDATA[");
            }

            xmlBody = ApplyPadraoSpecificRequestRules(soap, xmlBody, certificado, ref retorna);
            xmlBody = ApplyServicoSpecificRequestRules(soap, xmlBody);

            if (context.TratarScapeEnvio)
            {
                xmlBody = xmlBody.Replace("<", "&lt;").Replace(">", "&gt;");

                if (soap.SoapString.IndexOf("{xmlBodyScape}") > 0)
                {
                    retorna += soap.SoapString.Replace("{xmlBodyScape}", xmlBody);
                }
                else if (soap.SoapString.IndexOf("{xmlBodyScapeEnvio}") > 0)
                {
                    retorna += soap.SoapString.Replace("{xmlBodyScapeEnvio}", xmlBody);
                }
            }
            else if (context.TratarScapeRetorno)
            {
                retorna += soap.SoapString.Replace("{xmlBodyScapeRetorno}", xmlBody);
            }
            else if (soap.PadraoNFSe != PadraoNFSe.ELOTECH)
            {
                retorna += soap.SoapString.Replace("{xmlBody}", xmlBody);
            }

            return retorna;
        }

        private string ApplyPadraoSpecificRequestRules(WSSoap soap, string xmlBody, X509Certificate2 certificado, ref string retorna)
        {
            if (soap.PadraoNFSe == PadraoNFSe.TINUS)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                xmlBody = string.Empty;

                foreach (XmlNode item in doc.GetElementsByTagName(doc.ChildNodes[0].Name)[0].ChildNodes)
                {
                    xmlBody += item.OuterXml.Replace(" xmlns=\"http://www.tinus.com.br\"", "");
                }

                return xmlBody;
            }

            if (soap.PadraoNFSe == PadraoNFSe.PROPRIOBARUERISP)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                var xmlNode = doc.GetElementsByTagName("ArquivoRPSBase64");
                if (xmlNode.Count > 0)
                {
                    var tagNode = xmlNode[0];
                    tagNode.InnerText = Convert.ToBase64String(Encoding.UTF8.GetBytes(tagNode.InnerText));
                    doc.GetElementsByTagName("ArquivoRPSBase64")[0].InnerText = tagNode.InnerText;
                }

                return doc.OuterXml;
            }

            if (soap.PadraoNFSe == PadraoNFSe.IIBRASIL)
            {
                soap.SoapString = soap.SoapString.Replace("{cCDATA}", "]]>").Replace("{oCDATA}", "<![CDATA[");

                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);

                if (!xmlBody.Contains("Integridade"))
                {
                    var integridade = IIBRASIL.GerarIntegridade(xmlBody, soap.Token);
                    var noIntegridade = doc.CreateNode(XmlNodeType.Element, "Integridade", null);
                    noIntegridade.InnerText = integridade;
                    doc.FirstChild.FirstChild.AppendChild(noIntegridade);
                }

                return doc.OuterXml;
            }

            if (soap.PadraoNFSe == PadraoNFSe.ELOTECH)
            {
                if (xmlBody.Contains("SOAP-ENV:Envelope"))
                {
                    retorna = xmlBody;
                }
                else
                {
                    xmlBody = xmlBody.Replace("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "");
                    var soapAssinado = ELOTECH.AssinaSoapElotech(soap, xmlBody, certificado);
                    retorna = soapAssinado.OuterXml;
                }
            }

            return xmlBody;
        }

        private string ApplyServicoSpecificRequestRules(WSSoap soap, string xmlBody)
        {
            if (soap.Servico == Servico.EFDReinfConsultaReciboEvento)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                var tpEvento = doc.GetElementsByTagName("tipoEvento")[0].InnerText;
                xmlBody = doc.GetElementsByTagName("ConsultaReciboEvento")[0].OuterXml;
                return xmlBody.Replace("ConsultaReciboEvento", "ConsultaReciboEvento" + tpEvento);
            }

            if (soap.Servico == Servico.EFDReinfConsultaFechamento2099)
            {
                var doc = new XmlDocument();
                doc.LoadXml(xmlBody);
                return doc.GetElementsByTagName("ConsultaResultadoFechamento2099")[0].OuterXml;
            }

            return xmlBody;
        }
    }
}
