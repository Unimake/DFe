using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml
{
    /// <summary>
    /// Grupo de Informação do Provedor de Assinatura e Autorização
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.InfPAA")]
    [ComVisible(true)]
#endif
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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PAASignature")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PAASignature")]
    public class PAASignature
    {
        /// <summary>
        /// Assinatura digital padrão RSA
        /// </summary>
        [XmlElement("SignatureValue", DataType = "base64Binary")]
        public byte[] SignatureValue { get; set; }

        /// <summary>
        /// Chave Pública no padrão XML RSA Key
        /// </summary>
        [XmlElement("RSAKeyValue")]
        public RSAKeyValue RSAKeyValue { get; set; }
    }

    /// <summary>
    /// Chave Pública no padrão XML RSA Key
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.RSAKeyValue")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("RSAKeyValue")]
    public class RSAKeyValue
    {
        /// <summary>
        /// Modulus
        /// </summary>
        [XmlElement("Modulus", DataType = "base64Binary")]
        public byte[] Modulus { get; set; }

        /// <summary>
        /// Exponent
        /// </summary>
        [XmlElement("Exponent", DataType = "base64Binary")]
        public byte[] Exponent { get; set; }
    }
}
