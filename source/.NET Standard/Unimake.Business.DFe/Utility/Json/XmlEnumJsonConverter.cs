using Newtonsoft.Json;
using System;
using System.Reflection;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Utility.Json
{
    /// <summary>
    /// Converte valores de enum para JSON usando o valor definido no atributo <see cref="XmlEnumAttribute"/>.
    /// </summary>
    public sealed class XmlEnumJsonConverter : JsonConverter
    {
        /// <summary>
        /// Indica se o conversor suporta leitura.
        /// </summary>
        public override bool CanRead => false;

        /// <summary>
        /// Verifica se o tipo informado pode ser convertido.
        /// </summary>
        /// <param name="objectType">Tipo a ser convertido.</param>
        /// <returns>Retorna verdadeiro quando o tipo for enum.</returns>
        public override bool CanConvert(Type objectType) => objectType.IsEnum;

        /// <summary>
        /// Escreve o valor do enum no JSON.
        /// </summary>
        /// <param name="writer">Writer JSON.</param>
        /// <param name="value">Valor a ser escrito.</param>
        /// <param name="serializer">Serializador JSON.</param>
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            if (value == null)
            {
                writer.WriteNull();
                return;
            }

            var field = value.GetType().GetField(value.ToString());
            var xmlEnum = field?.GetCustomAttribute<XmlEnumAttribute>();
            writer.WriteValue(xmlEnum?.Name ?? value.ToString());
        }

        /// <summary>
        /// Leitura de JSON não suportada por este conversor.
        /// </summary>
        /// <param name="reader">Reader JSON.</param>
        /// <param name="objectType">Tipo do objeto.</param>
        /// <param name="existingValue">Valor existente.</param>
        /// <param name="serializer">Serializador JSON.</param>
        /// <returns>Não retorna valor, pois a leitura não é suportada.</returns>
        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            throw new NotSupportedException();
        }
    }
}
