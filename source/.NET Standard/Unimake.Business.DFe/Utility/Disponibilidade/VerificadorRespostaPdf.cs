using System;
using System.IO;
using System.Text;

namespace Unimake.Business.DFe.Utility
{
    internal static class VerificadorRespostaPdf
    {
        // Verifica o envelope de arquivo já recebido. Não valida assinatura ou conteúdo fiscal do PDF.
        internal static bool Completo(Stream stream)
        {
            var memoria = stream as MemoryStream;
            ArraySegment<byte> bytes;
            return memoria != null && memoria.TryGetBuffer(out bytes) && Completo(bytes.Array, bytes.Offset, bytes.Count);
        }

        private static bool Completo(byte[] bytes, int inicio, int tamanho)
        {
            if (bytes == null || tamanho < 16 || !Cabecalho(bytes, inicio)) return false;
            var fim = inicio + tamanho - 1;
            while (fim >= inicio && (bytes[fim] == 10 || bytes[fim] == 13 || bytes[fim] == 32 || bytes[fim] == 9)) fim--;
            return fim - inicio >= 4 && bytes[fim - 4] == '%' && bytes[fim - 3] == '%' && bytes[fim - 2] == 'E' && bytes[fim - 1] == 'O' && bytes[fim] == 'F';
        }

        private static bool Cabecalho(byte[] bytes, int inicio)
            => bytes[inicio] == '%' && bytes[inicio + 1] == 'P' && bytes[inicio + 2] == 'D' && bytes[inicio + 3] == 'F' &&
                bytes[inicio + 4] == '-' && (bytes[inicio + 5] == '1' || bytes[inicio + 5] == '2') &&
                bytes[inicio + 6] == '.' && bytes[inicio + 7] >= '0' && bytes[inicio + 7] <= '9';

        internal static bool Base64Completo(string valor)
        {
            if (string.IsNullOrWhiteSpace(valor)) return false;
            // Sem segunda alocação proporcional ao arquivo. Conferir o alfabeto sem decodificar
            // e decodificar somente as bordas; não interpretar conteúdo fiscal ou assinatura.
            var primeiro = new char[12];
            var ultimo = new char[40];
            var quantidade = 0;
            var preenchimento = 0;
            for (var i = 0; i < valor.Length; i++)
            {
                var c = valor[i];
                if (c == ' ' || c == '\r' || c == '\n' || c == '\t') continue;
                if (c == '=') preenchimento++;
                else
                {
                    if (preenchimento != 0 || !(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '+' || c == '/')) return false;
                }
                if (quantidade < primeiro.Length) primeiro[quantidade] = c;
                ultimo[quantidade % ultimo.Length] = c;
                quantidade++;
            }
            if (quantidade < 24 || quantidade % 4 != 0 || preenchimento > 2) return false;
            try
            {
                var tamanho = Math.Min(quantidade, ultimo.Length);
                var ordenado = new char[tamanho];
                var inicio = quantidade > ultimo.Length ? quantidade % ultimo.Length : 0;
                for (var i = 0; i < tamanho; i++) ordenado[i] = ultimo[(inicio + i) % ultimo.Length];
                var prefixo = Convert.FromBase64CharArray(primeiro, 0, primeiro.Length);
                var sufixo = Convert.FromBase64CharArray(ordenado, 0, ordenado.Length);
                return Cabecalho(prefixo, 0) &&
                    Encoding.ASCII.GetString(sufixo).TrimEnd(' ', '\r', '\n', '\t').EndsWith("%%EOF", StringComparison.Ordinal);
            }
            catch (FormatException) { return false; }
        }
    }
}
