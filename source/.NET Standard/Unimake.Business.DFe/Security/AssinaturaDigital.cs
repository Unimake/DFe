using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Security
{
    /// <summary>
    /// Classe para realizar assinatura digital de XMLs
    /// </summary>
    public static class AssinaturaDigital
    {
        #region Public Methods

        /// <summary>
        /// Assinar digitalmente o XML
        /// </summary>
        /// <param name="conteudoXML">XML a ser assinado</param>
        /// <param name="tagAssinatura">Nome da tag a ser assinada</param>
        /// <param name="tagAtributoId">Nome da tag que possui o ID para referencia na URI da assinatura</param>
        /// <param name="x509Cert">Certificado digital a ser utilizado na assinatura</param>
        /// <param name="algorithmType">Tipo de algorítimo a ser utilizado na assinatura</param>
        /// <param name="definirURI">Define o Reference.URI na assinatura</param>
        /// <param name="idAttributeName">Nome do atributo que tem o ID para assinatura. Se nada for passado o sistema vai tentar buscar o nome Id ou id, se não encontrar, não vai criar a URI Reference na assinatura com ID.</param>
        /// <param name="verificaAssinatura">Verificar se já existe assinatura no XML, se sim e existir o método não vai assinar o XML.</param>
        public static void Assinar(XmlDocument conteudoXML,
            string tagAssinatura,
            string tagAtributoId,
            X509Certificate2 x509Cert,
            AlgorithmType algorithmType = AlgorithmType.Sha1,
            bool definirURI = true,
            string idAttributeName = "",
            bool verificaAssinatura = false)
        {
            if (!string.IsNullOrEmpty(tagAssinatura))
            {
                AppDomain.CurrentDomain.AssemblyResolve += Xml.AssemblyResolver.AssemblyResolve;

                if (!verificaAssinatura || !EstaAssinado(conteudoXML, tagAssinatura))
                {
                    try
                    {
                        if (x509Cert == null)
                        {
                            throw new CertificadoDigitalException();
                        }

                        if (conteudoXML.GetElementsByTagName(tagAssinatura).Count == 0)
                        {
                            throw new Exception("A tag para assinatura " + tagAssinatura.Trim() + " não existe no XML. (Código do Erro: 5)");
                        }
                        else if (conteudoXML.GetElementsByTagName(tagAtributoId).Count == 0)
                        {
                            throw new Exception("A tag contendo o ID para assinatura " + tagAtributoId.Trim() + " não existe no XML. (Código do Erro: 4)");
                        }
                        else
                        {
                            var lists = conteudoXML.GetElementsByTagName(tagAssinatura);

                            var tagEhAMesma = false;

                            if (!string.IsNullOrWhiteSpace(tagAssinatura) && !string.IsNullOrWhiteSpace(tagAtributoId))
                            {
                                tagEhAMesma = tagAssinatura.ToLower().Trim() == tagAtributoId.ToLower().Trim();
                            }

                            foreach (XmlNode nodes in lists)
                            {
                                foreach (XmlNode childNodes in nodes.ChildNodes)
                                {
                                    if (!tagEhAMesma)
                                    {
                                        if (!childNodes.Name.Equals(tagAtributoId))
                                        {
                                            continue;
                                        }
                                    }

                                    // Create a reference to be signed
                                    var reference = new Reference
                                    {
                                        Uri = ""
                                    };

                                    // pega o uri que deve ser assinada
                                    var childElemen = (XmlElement)childNodes;

                                    if (definirURI)
                                    {
                                        if (string.IsNullOrEmpty(idAttributeName))
                                        {
                                            if (childElemen.GetAttributeNode("Id") != null)
                                            {
                                                idAttributeName = "Id";
                                            }
                                            else if (childElemen.GetAttributeNode("id") != null)
                                            {
                                                idAttributeName = "id";
                                            }
                                        }
                                        else if (childElemen.GetAttributeNode(idAttributeName) != null)
                                        {
                                            reference.Uri = "#" + childElemen.GetAttributeNode(idAttributeName).Value;
                                        }
                                    }

                                    var signedXml = new SignedXml(conteudoXML);

                                    reference.AddTransform(new XmlDsigEnvelopedSignatureTransform());
                                    reference.AddTransform(new XmlDsigC14NTransform());

                                    switch (algorithmType)
                                    {
                                        case AlgorithmType.Sha256:
                                            signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256";
                                            signedXml.SigningKey = x509Cert.GetRSAPrivateKey();
                                            signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256";
                                            reference.DigestMethod = "http://www.w3.org/2001/04/xmlenc#sha256";
                                            break;

                                        default:
                                            signedXml.SigningKey = x509Cert.PrivateKey;
                                            signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2000/09/xmldsig#rsa-sha1";
                                            reference.DigestMethod = "http://www.w3.org/2000/09/xmldsig#sha1";
                                            break;
                                    }

                                    signedXml.AddReference(reference);

                                    var keyInfo = new KeyInfo();
                                    keyInfo.AddClause(new KeyInfoX509Data(x509Cert));
                                    signedXml.KeyInfo = keyInfo;
                                    signedXml.ComputeSignature();

                                    var xmlDigitalSignature = signedXml.GetXml();

                                    nodes.AppendChild(conteudoXML.ImportNode(xmlDigitalSignature, true));

                                    if (tagEhAMesma)
                                    {
                                        break;
                                    }
                                }

                                if (tagEhAMesma)
                                {
                                    break;
                                }
                            }
                        }
                    }
                    catch (CryptographicException ex)
                    {
                        if (x509Cert.IsA3())
                        {
                            throw new Exception("O certificado deverá ser reiniciado.\r\n Retire o certificado.\r\nAguarde o LED terminar de piscar.\r\n Recoloque o certificado e informe o PIN novamente.\r\n" + ex.ToString());
                        }
                        else
                        {
                            throw;
                        }
                    }
                    catch
                    {
                        throw;
                    }
                }
            }
        }

        /// <summary>
        /// Assinar digitalmente o XML
        /// </summary>
        /// <param name="conteudoXML">XML a ser assinado</param>
        /// <param name="tagAssinatura">Nome da tag a ser assinada</param>
        /// <param name="x509Cert">Certificado digital a ser utilizado na assinatura</param>
        /// <param name="algorithmType">Tipo de algorítimo a ser utilizado na assinatura</param>
        /// <param name="verificaAssinatura">Verificar se já existe assinatura no XML, se sim e existir o método não vai assinar o XML.</param>
        public static void Assinar(XmlDocument conteudoXML,
            string tagAssinatura,
            X509Certificate2 x509Cert,
            AlgorithmType algorithmType = AlgorithmType.Sha1,
            bool verificaAssinatura = false)
        {
            if (!string.IsNullOrEmpty(tagAssinatura))
            {
                AppDomain.CurrentDomain.AssemblyResolve += Xml.AssemblyResolver.AssemblyResolve;

                if (!verificaAssinatura || !EstaAssinado(conteudoXML, tagAssinatura))
                {
                    try
                    {
                        if (x509Cert == null)
                        {
                            throw new CertificadoDigitalException();
                        }

                        if (conteudoXML.GetElementsByTagName(tagAssinatura).Count == 0)
                        {
                            throw new Exception("A tag para assinatura " + tagAssinatura.Trim() + " não existe no XML. (Código do Erro: 5)");
                        }
                        else
                        {
                            var nodes = conteudoXML.GetElementsByTagName(tagAssinatura)[0];
                            var childNodes = nodes;

                            // Create a reference to be signed
                            var reference = new Reference
                            {
                                Uri = ""
                            };

                            // pega o uri que deve ser assinada
                            var childElemen = (XmlElement)childNodes;

                            var signedXml = new SignedXml(conteudoXML);

                            reference.AddTransform(new XmlDsigEnvelopedSignatureTransform());
                            reference.AddTransform(new XmlDsigC14NTransform());

                            switch (algorithmType)
                            {
                                case AlgorithmType.Sha256:
                                    signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256";
                                    signedXml.SigningKey = x509Cert.GetRSAPrivateKey();
                                    signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256";
                                    reference.DigestMethod = "http://www.w3.org/2001/04/xmlenc#sha256";
                                    break;

                                default:
                                    signedXml.SigningKey = x509Cert.PrivateKey;
                                    signedXml.SignedInfo.SignatureMethod = "http://www.w3.org/2000/09/xmldsig#rsa-sha1";
                                    reference.DigestMethod = "http://www.w3.org/2000/09/xmldsig#sha1";
                                    break;
                            }

                            signedXml.AddReference(reference);

                            var keyInfo = new KeyInfo();
                            keyInfo.AddClause(new KeyInfoX509Data(x509Cert));
                            signedXml.KeyInfo = keyInfo;
                            signedXml.ComputeSignature();

                            var xmlDigitalSignature = signedXml.GetXml();

                            nodes.AppendChild(conteudoXML.ImportNode(xmlDigitalSignature, true));
                        }
                    }
                    catch (CryptographicException ex)
                    {
                        if (x509Cert.IsA3())
                        {
                            throw new Exception("O certificado deverá ser reiniciado.\r\n Retire o certificado.\r\nAguarde o LED terminar de piscar.\r\n Recoloque o certificado e informe o PIN novamente.\r\n" + ex.ToString());
                        }
                        else
                        {
                            throw;
                        }
                    }
                    catch
                    {
                        throw;
                    }
                }
            }
        }


        /// <summary>
        /// Verificar se o XML já tem assinatura
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        /// <param name="tagAssinatura">Tag de assinatura onde vamos pesquisar</param>
        /// <returns>true = Já está assinado</returns>
        public static bool EstaAssinado(XmlDocument conteudoXML, string tagAssinatura)
        {
            if (tagAssinatura is null)
            {
                throw new ArgumentNullException(nameof(tagAssinatura));
            }

            if (conteudoXML is null)
            {
                return false;
            }

            if (conteudoXML.GetElementsByTagName(tagAssinatura)[0]?.LastChild?.Name == "Signature")
            {
                return true;
            }

            return false;
        }

        #endregion Public Methods
    }

    /// <summary>
    /// Tipo de algoritimo para assinatura digital
    /// </summary>
    public enum AlgorithmType
    {
        /// <summary>
        /// Tipo de Algorítimo Sha1
        /// </summary>
        Sha1,
        /// <summary>
        /// Tipo de Algorítimo Sha256
        /// </summary>
        Sha256
    }

    /// <summary>
    /// Delegate
    /// </summary>
    /// <param name="sender">Sender</param>
    /// <param name="args">Argumentos</param>
    public delegate void CryptographicExceptionHandler(object sender, EventArgs args);
}