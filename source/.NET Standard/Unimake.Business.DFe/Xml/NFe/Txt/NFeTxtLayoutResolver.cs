using System;
using System.Collections.Generic;
using System.Globalization;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal static class NFeTxtLayoutResolver
    {
        internal static string Resolver(Dictionary<string, string> layouts, string segmento, decimal versao, int quantidadePipes)
        {
            var segmentoNormalizado = segmento.ToUpperInvariant();
            var versaoNormalizada = versao.ToString("0.00", CultureInfo.InvariantCulture).Replace(".", string.Empty);
            string layout;

            if (layouts.TryGetValue(segmentoNormalizado + "_" + versaoNormalizada, out layout) ||
                layouts.TryGetValue(segmentoNormalizado + "_" + versaoNormalizada + "_" + quantidadePipes, out layout) ||
                layouts.TryGetValue(segmentoNormalizado + "_" + quantidadePipes, out layout) ||
                layouts.TryGetValue(segmentoNormalizado, out layout))
            {
                return layout;
            }

            foreach (var item in layouts)
            {
                if (!item.Value.Substring(1, item.Value.IndexOf('|')).Equals(segmento + "|", StringComparison.InvariantCultureIgnoreCase))
                {
                    continue;
                }

                if (item.Key.Contains("_400_") && item.Key != segmentoNormalizado + "_400_" + quantidadePipes) continue;

                return item.Value;
            }

            return null;
        }
    }
}
