using System;
using System.IO;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal static class TxtLeituraArquivo
    {
        private static readonly Encoding Utf8Estrito = new UTF8Encoding(false, true);

        internal static StreamReader Abrir(string arquivo, Encoding codificacaoWindows1252)
        {
            if (codificacaoWindows1252 == null)
                return new StreamReader(arquivo, Encoding.Default, true);
            if (codificacaoWindows1252.CodePage != 1252)
                throw new ArgumentException("A codificação de fallback deve ser Windows-1252.", nameof(codificacaoWindows1252));

            var stream = File.OpenRead(arquivo);
            try
            {
                var codificacao = LerBom(stream, out var tamanhoBom);
                stream.Position = tamanhoBom;
                if (codificacao == null)
                    codificacao = EscolherSemBom(stream, codificacaoWindows1252);
                else
                    ValidarCodificacao(stream, codificacao);

                return new StreamReader(stream, codificacao, false);
            }
            catch
            {
                stream.Dispose();
                throw;
            }
        }

        private static Encoding EscolherSemBom(Stream stream, Encoding codificacaoWindows1252)
        {
            try
            {
                ValidarCodificacao(stream, Utf8Estrito);

                return Utf8Estrito;
            }
            catch (DecoderFallbackException)
            {
                return codificacaoWindows1252;
            }
        }

        private static void ValidarCodificacao(Stream stream, Encoding codificacao)
        {
            // Valida a sequência completa, inclusive caracteres truncados no final do arquivo.
            var inicio = stream.Position;
            var decoder = codificacao.GetDecoder();
            var bytes = new byte[4096];
            var caracteres = new char[codificacao.GetMaxCharCount(bytes.Length)];
            try
            {
                int lidos;
                do
                {
                    lidos = stream.Read(bytes, 0, bytes.Length);
                    decoder.GetChars(bytes, 0, lidos, caracteres, 0, flush: lidos == 0);
                }
                while (lidos != 0);
            }
            finally
            {
                stream.Position = inicio;
            }
        }

        private static Encoding LerBom(Stream stream, out int tamanho)
        {
            var primeiro = stream.ReadByte();
            var segundo = stream.ReadByte();
            var terceiro = stream.ReadByte();
            var quarto = stream.ReadByte();

            tamanho = 4;
            if (primeiro == 0xFF && segundo == 0xFE && terceiro == 0 && quarto == 0)
                return new UTF32Encoding(false, false, true);
            if (primeiro == 0 && segundo == 0 && terceiro == 0xFE && quarto == 0xFF)
                return new UTF32Encoding(true, false, true);

            tamanho = 3;
            if (primeiro == 0xEF && segundo == 0xBB && terceiro == 0xBF)
                return Utf8Estrito;

            tamanho = 2;
            if (primeiro == 0xFF && segundo == 0xFE)
                return new UnicodeEncoding(false, false, true);
            if (primeiro == 0xFE && segundo == 0xFF)
                return new UnicodeEncoding(true, false, true);

            tamanho = 0;
            return null;
        }
    }
}
