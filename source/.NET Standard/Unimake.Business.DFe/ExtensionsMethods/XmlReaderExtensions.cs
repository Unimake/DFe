using System.Reflection;
using System.Xml.Linq;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using static Unimake.Business.DFe.Utility.Converter;

namespace System.Xml
{
    /// <summary>
    ///
    /// </summary>
    public static class XmlReaderExtensions
    {
        #region Private Methods

        private static PropertyInfo GetProperty(object value, XElement el)
        {
            var result = value.GetType().GetProperty(el.Name.ToString(), BindingFlags.Instance |
                                                              BindingFlags.Public |
                                                              BindingFlags.FlattenHierarchy |
                                                              BindingFlags.IgnoreCase);

            return !(result?.CanWrite ?? false) ? null : result;
        }

        #endregion Private Methods

        #region Public Methods

        /// <summary>
        /// Deserializa um objeto, em primeiro nó de XML, para o tipo esperado em T
        /// </summary>
        /// <typeparam name="T">Tipo esperado para deserialização</typeparam>
        /// <param name="reader">Reader para ler os dados do XML</param>
        /// <returns></returns>
        public static T DeserializeTo<T>(this XmlReader reader)
            where T : class, new()
        {
            var result = new T();

            var elements = DynamicXml.Parse(reader.ReadInnerXml());

            foreach(XElement el in elements)
            {
                var pi = GetProperty(result, el);

                if(pi == null)
                {
                    continue;
                }

                pi.SetValue(result, ToAny(el.Value, pi.PropertyType));
            }

            return result;
        }

        /// <summary>
        ///
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="reader"></param>
        /// <param name="name"></param>
        /// <param name="propertyInfo"></param>
        /// <returns></returns>
        public static T GetValue<T>(this XmlReader reader, string name, PropertyInfo propertyInfo = null)
        {
            T result = default;

            if(reader.NodeType == XmlNodeType.EndElement)
            {
                _ = reader.Read();
            }

            do
            {
                if((reader.NodeType == XmlNodeType.Element &&
                   !reader.Name.Equals(name, System.StringComparison.InvariantCultureIgnoreCase)) ||
                   reader.NodeType == XmlNodeType.EndElement)
                {
                    return result;
                }

                if(reader.HasValue)
                {
                    result = propertyInfo != null ? (T)ToAny(reader.Value, propertyInfo.PropertyType) : ToAny<T>(reader.Value);

                    break;
                }
            } while(reader.Read());

            _ = reader.Read();
            return result;
        }

        /// <summary>
        /// Converte o reader no tipo signature.
        /// </summary>
        /// <param name="reader">Reader para conversão no tipo signature</param>
        /// <returns></returns>
        public static Signature ToSignature(this XmlReader reader)
        {
            var tag = reader.Name;
            var ns = reader.NamespaceURI;
            var xml = $@"<?xml version=""1.0"" encoding=""UTF-8""?>
                        <{tag} xmlns=""{ns}"">
                        {reader.ReadInnerXml()}
                        </{tag}>";

            var result = XMLUtility.Deserializar<Signature>(xml);
            result.Xmlns = ns;
            return result;
        }

        #endregion Public Methods
    }
}