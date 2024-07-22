using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml
{
    /// <summary>
    /// Grupo de Informação do Provedor de Assinatura e Autorização
    /// </summary>
    [Serializable()]
    [XmlRoot("infPAA")]
    public class InfPAA
    {
        /// <summary>
        /// CNPJ do provedor de assinatura e autorização
        /// </summary>
        [XmlElement("CNPJPAA")]
        public string CNPJPAA { get; set; }

        /// <summary>
        /// Assinatura RSA do Emitente para DFe gerado por PAA
        /// </summary>
        [XmlElement("PAASignature")]
        public PAASignature PAASignature { get; set; }
    }

    /// <summary>
    /// Assinatura RSA do Emitente para DFe gerado por PAA
    /// </summary>
    [Serializable()]
    [XmlRoot("PAASignature")]
    public class PAASignature
    {
        /// <summary>
        /// Assinatura digital padrão RSA
        /// </summary>
        [XmlElement("SignatureValue")]
        public string SignatureValue { get; set; }

        /// <summary>
        /// Chave Pública no padrão XML RSA Key
        /// </summary>
        [XmlElement("RSAKeyValue")]
        public string RSAKeyValue { get; set; }
    }

    /// <summary>
    /// Chave Pública no padrão XML RSA Key
    /// </summary>
    [Serializable()]
    [XmlRoot("RSAKeyValue")]
    public class RSAKeyValue
    {
        /// <summary>
        /// Modulus
        /// </summary>
        [XmlElement("Modulus")]
        public string Modulus { get; set; }

        /// <summary>
        /// Exponent
        /// </summary>
        [XmlElement("Exponent")]
        public string Exponent { get; set; }
    }
}
