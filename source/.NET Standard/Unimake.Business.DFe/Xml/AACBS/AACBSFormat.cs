using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Xml;

namespace Unimake.Business.DFe.Xml.AACBS
{
    internal static class AACBSFormat
    {
        internal const string Namespace = "http://www.unimake.com.br/dfe/aacbs";
        private static readonly Regex Instant = new Regex(@"\A[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})\z", RegexOptions.CultureInvariant);
        private static readonly HashSet<string> Roots = new HashSet<string>(StringComparer.Ordinal)
        {
            "autenticar", "retAutenticar", "consultarDebitos", "retConsultarDebitos",
            "consultarCreditos", "retConsultarCreditos", "consultarPagamentos", "retConsultarPagamentos",
            "consultarRecolhimentos", "retConsultarRecolhimentos", "consultarSituacaoSolicitacao", "retConsultarSituacaoSolicitacao",
            "consultarLinkDownload", "retConsultarLinkDownload", "baixarArquivoRetorno", "retBaixarArquivoRetorno",
            "retArquivoDebitos", "retArquivoCreditos", "retArquivoPagamentos", "retArquivoRecolhimentos",
            "gerarDarfAdquirente", "retGerarDarfAdquirente", "gerarDarfContribuinte", "retGerarDarfContribuinte"
        };

        internal static bool IsDocument(XmlElement root) => root != null && root.NamespaceURI == Namespace && Roots.Contains(root.LocalName);

        internal static bool IsRoot(string name) => Roots.Contains(name);

        internal static DateTimeOffset ParseInstant(string value)
        {
            if (value == null || !Instant.IsMatch(value))
            {
                throw new FormatException("O instante AACBS deve conter data, hora e fuso explícito.");
            }

            // A projeção CLR trunca somente a fração sub-tick; o membro Field guarda o texto integral.
            var projected = Regex.Replace(value, @"(\.[0-9]{7})[0-9]+", "$1", RegexOptions.CultureInvariant);
            if (!DateTimeOffset.TryParse(projected, CultureInfo.InvariantCulture, DateTimeStyles.None, out var result))
            {
                throw new FormatException("O instante AACBS não possui data, hora ou fuso representável.");
            }
            return result;
        }

        internal static string FormatInstant(DateTimeOffset value) => value.ToString("yyyy-MM-dd'T'HH:mm:ss.fffffffzzz", CultureInfo.InvariantCulture);

        internal static string FormatDecimal(decimal value) => value.ToString("0.############################", CultureInfo.InvariantCulture);

        internal static decimal ParseDecimal(string value)
        {
            value = value?.Trim(' ', '\t', '\r', '\n');
            if (value == null || !Regex.IsMatch(value, @"\A[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)\z", RegexOptions.CultureInvariant))
            {
                throw new FormatException("O valor decimal AACBS deve usar ponto, sem expoente ou separador de milhar.");
            }
            var parts = value.TrimStart('+', '-').Split('.');
            var integerDigits = parts[0].TrimStart('0').Length;
            var fractionDigits = parts.Length == 2 ? parts[1].TrimEnd('0').Length : 0;
            if (integerDigits > 16 || fractionDigits > 2 ||
                !decimal.TryParse(value, NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture, out var result))
            {
                throw new FormatException("O valor monetário AACBS deve respeitar a precisão 18,2, sem arredondamento.");
            }
            return result;
        }
    }
}
