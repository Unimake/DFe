#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml
{
    /// <summary>
    /// Classe Base para criação de classes de serialização de XML
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.XMLBase")]
    [ComVisible(true)]
#endif
    public abstract class XMLBase : Contract.Serialization.IXmlSerializable
    {
        #region Protected Properties

        /// <summary>
        /// Lista de NameSpaces
        /// </summary>
        protected List<XMLUtility.TNameSpace> NameSpaces { get; }

        #endregion Protected Properties

        #region Public Constructors

        /// <summary>
        /// Construtor base
        /// </summary>
        public XMLBase()
        {
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            NameSpaces = new List<XMLUtility.TNameSpace>
            {
                new XMLUtility.TNameSpace() { Prefix = "", NS = attribute.Namespace }
            };
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP
        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML e resgata no formato string)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public virtual string GerarXMLString() => GerarXML().OuterXml;
#endif

        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public virtual XmlDocument GerarXML() => XMLUtility.Serializar(this, NameSpaces);

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="doc">Conteúdo do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual T LerXML<T>(XmlDocument doc)
            where T : new() => XMLUtility.Deserializar<T>(doc);

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização
        /// </summary>
        ///<param name="document">XML recebido durante o processo de desserialização</param>
        public virtual void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização
        /// </summary>
        ///<param name="writer">string XML recebido durante o processo de serialização</param>
        public virtual void WriteXml(System.IO.StringWriter writer)
        {
        }

        #endregion Public Methods
    }
}