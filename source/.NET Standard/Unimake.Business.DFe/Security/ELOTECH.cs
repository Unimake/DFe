using System;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;

namespace Unimake.Business.DFe.Security
{
    internal static class ELOTECH
    {
        #region Public Methods

        /// <summary>
        /// Assina o SOAP para o padrão ELOTECH
        /// </summary>
        /// <param name="soap">Objeto WSSoap com as configurações do SOAP</param>
        /// <param name="xmlBody">String do XML a ser trabalhado e incorporado ao SOAP</param>
        /// <param name="certificado">Objeto do certificado utilizado</param>
        /// <returns></returns>
        public static XmlDocument AssinaSoapElotech(WSSoap soap, string xmlBody, X509Certificate2 certificado)
        {
            string STR_SOAPENV = "http://schemas.xmlsoap.org/soap/envelope/";
            string STR_WSSE = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd";
            string STR_WSU = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd";

            var soapEnvelope = new XmlDocument();
            soapEnvelope.PreserveWhitespace = true;
            soapEnvelope.LoadXml(soap.SoapString);

            var ns = new XmlNamespaceManager(soapEnvelope.NameTable);
            ns.AddNamespace("SOAP-ENV", STR_SOAPENV);
            ns.AddNamespace("wsse", STR_WSSE);
            ns.AddNamespace("wsu", STR_WSU);
            ns.AddNamespace("ds", STR_WSU);
            ns.AddNamespace("ec", STR_WSU);

            // *** Grab the body element - this is what we create the signature from
            var body = soapEnvelope.DocumentElement.SelectSingleNode(@"//SOAP-ENV:Body", ns) as XmlElement;

            if (body == null)
            {
                throw new ApplicationException("No body tag found");
            }

            // *** Fill the body
            body.InnerXml = xmlBody;

            // *** Signed XML will create Xml Signature - Xml fragment
            var signedXml = new SignedXmlWithId(soapEnvelope);

            // *** Create a KeyInfo structure
            var keyInfo = new KeyInfo
            {
                Id = "KI-EC95425802FB9F663F15021186132692"
            };

            var keyInfoNode = new KeyInfoNode();
            var wsseSec = soapEnvelope.CreateElement("wsse", "SecurityTokenReference", STR_WSSE);
            wsseSec.SetAttribute("xmlns:wsu", STR_WSU);
            wsseSec.SetAttribute("Id", STR_WSU, "STR-EC95425802FB9F663F15021186132713");
            var wsseRef = soapEnvelope.CreateElement("wsse", "Reference", STR_WSSE);
            wsseRef.SetAttribute("URI", "#X509-EC95425802FB9F663F15021186132611");
            wsseRef.SetAttribute("ValueType", "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-x509-token-profile-1.0#X509v3");
            wsseSec.AppendChild(wsseRef);
            keyInfoNode.Value = wsseSec;
            keyInfo.AddClause(keyInfoNode);

            // *** The actual key for signing - MAKE SURE THIS ISN'T NULL!
            signedXml.SigningKey = certificado.GetRSAPrivateKey();

            // *** provide the certficate info that gets embedded - note this is only
            // *** for specific formatting of the message to provide the cert info
            signedXml.KeyInfo = keyInfo;

            // *** Again unusual - meant to make the document match template
            signedXml.SignedInfo.CanonicalizationMethod = SignedXml.XmlDsigExcC14NTransformUrl;

            // Set the InclusiveNamespacesPrefixList property.
            var canMethod = (XmlDsigExcC14NTransform)signedXml.SignedInfo.CanonicalizationMethodObject;
            canMethod.InclusiveNamespacesPrefixList = "SOAP-ENV";

            // *** Now create reference to sign: Point at the Body element
            var reference = new Reference
            {
                Uri = "#id-1"
            };

            // Add an enveloped transformation to the reference.
            var env = new XmlDsigExcC14NTransform
            {
                InclusiveNamespacesPrefixList = ""
            };
            reference.AddTransform(env); // required to match doc

            signedXml.AddReference(reference);

            // *** Finally create the signature
            signedXml.ComputeSignature();

            // *** wsse:Security element
            var soapSignature = soapEnvelope.DocumentElement.SelectSingleNode(@"//wsse:Security", ns) as XmlElement;
            if (soapSignature == null)
            {
                throw new ApplicationException("No wsse:Security tag found");
            }

            // *** wsse:BinarySecurityToken element
            var soapToken = soapEnvelope.DocumentElement.SelectSingleNode(@"//wsse:BinarySecurityToken", ns) as XmlElement;
            if (soapToken == null)
            {
                throw new ApplicationException("No wsse:BinarySecurityToken tag found");
            }

            var export = certificado.Export(X509ContentType.Cert);
            var base64 = Convert.ToBase64String(export);
            soapToken.InnerText = base64;

            // *** Result is an XML node with the signature detail below it
            // *** Now let's add the sucker into the SOAP-HEADER
            var signedElement = signedXml.GetXml();
            var sId = soapEnvelope.CreateAttribute("Id");
            sId.Value = "SIG-2";
            signedElement.Attributes.Append(sId);

            // *** And add our signature as content
            soapSignature.AppendChild(signedElement);

            // *** Now add the signature header into the master header
            var soapHeader = soapEnvelope.DocumentElement.SelectSingleNode("//SOAP-ENV:Header", ns) as XmlElement;
            if (soapHeader == null)
            {
                throw new ApplicationException("No SOAP-ENV:Header tag found");
            }

            return soapEnvelope;
        }

        #endregion Public Methods

        internal sealed class SignedXmlWithId : SignedXml
        {
            #region Public Constructors

            public SignedXmlWithId(XmlDocument xml) : base(xml) { }

            public SignedXmlWithId(XmlElement xmlElement) : base(xmlElement) { }

            #endregion Public Constructors

            #region Public Methods

            public override XmlElement GetIdElement(XmlDocument doc, string id)
            {
                // check to see if it's a standard ID reference
                var idElem = base.GetIdElement(doc, id);

                if (idElem == null)
                {
                    var nsManager = new XmlNamespaceManager(doc.NameTable);
                    nsManager.AddNamespace("wsu", "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd");

                    idElem = doc.SelectSingleNode("//*[@wsu:Id=\"" + id + "\"]", nsManager) as XmlElement;
                }

                return idElem;
            }

            #endregion Public Methods
        }
    }
}
