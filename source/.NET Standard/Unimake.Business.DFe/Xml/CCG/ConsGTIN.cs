#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Text;
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Xml.CCG
{
    /// <summary>
    /// Classe para serialização e deserialização do XML de consulta centralizada do código GTIN
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CCG.ConsGTIN")]
    [ComVisible(true)]
#endif
    [XmlRoot("consGTIN", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsGTIN : XMLBase
    {
        /// <summary>
        /// Versão do schema XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        private string GTINField;

        /// <summary>
        /// Informar o código GTIN a ser consultado
        /// </summary>
        [XmlElement("GTIN")]
        public string GTIN
        {
            get => GTINField;
            set
            {
                if (string.IsNullOrWhiteSpace(value))
                {
#if INTEROP
                    ThrowHelper.Instance.Throw(new Exception("Código GTIN inválido! Não pode ser nulo ou espaço em branco."));
#else
                    throw new Exception("Código GTIN inválido! Não pode ser nulo ou espaço em branco.");
#endif
                }

                if (value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                {
#if INTEROP
                    ThrowHelper.Instance.Throw(new Exception("Código GTIN informado (" + value + ") inválido. GTIN deve ter, como tamanho, 8,12,13 ou 14 números sem conter letras."));
#else
                    throw new Exception("Código GTIN informado (" + value + ") inválido. GTIN deve ter, como tamanho, 8,12,13 ou 14 números sem conter letras.");
#endif
                }

                for (var i = 0; i < value.Length; i++)
                {
                    if (!"0123456789".Contains(value.Substring(i, 1)))
                    {
#if INTEROP
                        ThrowHelper.Instance.Throw(new Exception("Código GTIN informado (" + value + ") inválido. Não pode conter letras, somente números."));
#else
                        throw new Exception("Código GTIN informado (" + value + ") inválido. Não pode conter letras, somente números.");
#endif
                    }
                }

                GTINField = value;
            }
        }

        /// <summary>
        /// Deserializar o XML consGTIN no objeto ConsGTIN.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML consGTIN</param>
        /// <returns>Objeto do ConsGTIN</returns>
        public ConsGTIN LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<ConsGTIN>(doc);
        }

        /// <summary>
        /// Deserializar o XML consGTIN no objeto ConsGTIN.
        /// </summary>
        /// <param name="xml">string do XML consGTIN</param>
        /// <returns>Objeto do ConsGTIN</returns>
        public ConsGTIN LoadFromXML(string xml) => XMLUtility.Deserializar<ConsGTIN>(xml);
    }
}