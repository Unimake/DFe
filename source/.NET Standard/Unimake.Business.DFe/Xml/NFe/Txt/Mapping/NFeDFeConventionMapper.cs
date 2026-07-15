using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal static class NFeDFeConventionMapper
    {
        public static TDestino Mapear<TDestino>(object origem) where TDestino : new()
        {
            var destino = new TDestino();
            var valores = ObterValores(origem);

            foreach (var propriedade in typeof(TDestino).GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .Where(x => x.CanWrite && !x.Name.EndsWith("Field", StringComparison.Ordinal)))
            {
                if (!valores.TryGetValue(propriedade.Name, out var valor))
                {
                    continue;
                }

                propriedade.SetValue(destino, Converter(valor, propriedade.PropertyType), null);
            }

            return destino;
        }

        private static Dictionary<string, object> ObterValores(object origem)
        {
            var tipo = origem.GetType();
            var valores = tipo.GetFields(BindingFlags.Instance | BindingFlags.Public)
                .ToDictionary(x => x.Name, x => x.GetValue(origem), StringComparer.OrdinalIgnoreCase);

            foreach (var propriedade in tipo.GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(x => x.CanRead))
            {
                valores[propriedade.Name] = propriedade.GetValue(origem, null);
            }

            return valores;
        }

        private static object Converter(object valor, Type tipoDestino)
        {
            if (valor == null)
            {
                return null;
            }

            var tipoReal = Nullable.GetUnderlyingType(tipoDestino) ?? tipoDestino;
            if (tipoReal.IsInstanceOfType(valor))
            {
                return valor;
            }

            if (tipoReal.IsEnum)
            {
                var numero = valor is Enum
                    ? Convert.ToInt64(valor, CultureInfo.InvariantCulture)
                    : long.Parse(valor.ToString(), CultureInfo.InvariantCulture);
                return Enum.ToObject(tipoReal, numero);
            }

            if (tipoReal == typeof(string))
            {
                return valor.ToString();
            }

            return Convert.ChangeType(valor, tipoReal, CultureInfo.InvariantCulture);
        }
    }
}
