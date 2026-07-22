using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Resolve os nomes das tags XML diretamente do modelo oficial da NFe.
    /// </summary>
    internal static class NFeXmlTagNameResolver
    {
        private static readonly Dictionary<Type, Dictionary<string, string>> Cache = new Dictionary<Type, Dictionary<string, string>>();
        private static readonly object SyncRoot = new object();

        /// <summary>
        /// Obtém o nome serializado da propriedade informada.
        /// </summary>
        internal static string Get<T>(string propertyName)
        {
            var type = typeof(T);

            lock (SyncRoot)
            {
                Dictionary<string, string> typeCache;
                if (!Cache.TryGetValue(type, out typeCache))
                {
                    typeCache = new Dictionary<string, string>();
                    Cache.Add(type, typeCache);
                }

                string tagName;
                if (typeCache.TryGetValue(propertyName, out tagName))
                {
                    return tagName;
                }

                tagName = Resolve(type, propertyName);
                typeCache.Add(propertyName, tagName);
                return tagName;
            }
        }

        private static string Resolve(Type type, string propertyName)
        {
            var property = type.GetProperty(propertyName, BindingFlags.Instance | BindingFlags.Public);
            if (property == null)
            {
                throw new InvalidOperationException("A propriedade '" + propertyName + "' não foi encontrada em '" + type.FullName + "'.");
            }

            var tagName = GetSerializedName(property);
            if (!string.IsNullOrEmpty(tagName))
            {
                return tagName;
            }

            foreach (var candidate in type.GetProperties(BindingFlags.Instance | BindingFlags.Public))
            {
                tagName = GetSerializedName(candidate);
                if (string.Equals(tagName, propertyName, StringComparison.OrdinalIgnoreCase))
                {
                    return tagName;
                }
            }

            throw new InvalidOperationException("A propriedade '" + type.FullName + "." + propertyName + "' não possui um nome de serialização XML correspondente.");
        }

        private static string GetSerializedName(PropertyInfo property)
        {
            var elementAttributes = property.GetCustomAttributes(typeof(XmlElementAttribute), false);
            if (elementAttributes.Length > 0)
            {
                var attribute = (XmlElementAttribute)elementAttributes[0];
                return string.IsNullOrEmpty(attribute.ElementName) ? property.Name : attribute.ElementName;
            }

            var attributeAttributes = property.GetCustomAttributes(typeof(XmlAttributeAttribute), false);
            if (attributeAttributes.Length > 0)
            {
                var attribute = (XmlAttributeAttribute)attributeAttributes[0];
                return string.IsNullOrEmpty(attribute.AttributeName) ? property.Name : attribute.AttributeName;
            }

            return null;
        }
    }

    /// <summary>
    /// Identificadores que pertencem somente ao contrato do layout TXT.
    /// </summary>
    internal static class NFeTxtFieldNames
    {
        internal const string CnpjCpf = "CNPJ_CPF";
        internal const string SituacaoTributaria = "cSitTrib";
        internal const string Id = "ID";
        internal const string NumeroItem = "NItem";
        internal const string Referencia = "Ref";
        internal const string UfEmbarque = "UFEmbarq";
        internal const string LocalEmbarque = "xLocEmbarq";
    }
}
