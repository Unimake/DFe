#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Business.Security;
using Unimake.Exceptions;
#endif
using System;
using System.IO;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Text.RegularExpressions;
using Unimake.Cryptography;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe para conversão de objetos
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public static class Converter
    {
        /// <summary>
        /// Converter tipo de um objeto
        /// </summary>
        /// <param name="value">Para qual tipo converter o conteúdo do objeto</param>
        /// <param name="expectedType">Para qual tipo converter o conteúdo do objeto</param>
        /// <returns>Conteúdo do objeto convertido para o tipo informado</returns>
        private static object ChangeType(object value, Type expectedType) => UConvert.ChangeType(value, expectedType);

        /// <summary>
        /// Converter string para MemoryStream com UTF8 Encoding
        /// </summary>
        /// <param name="contentConvert">Conteúdo a ser convertido</param>
        /// <returns>Conteúdo convertido para MemoryStrem com UTF8 Encoding</returns>
        public static MemoryStream StringToStreamUTF8(string contentConvert) => UConvert.ToMemoryStream(contentConvert, System.Text.Encoding.UTF8);

        /// <summary>
        /// Tenta converter qualquer objeto passado em value para o tipo esperado em T
        /// </summary>
        /// <typeparam name="T">Tipo esperado para conversão</typeparam>
        /// <param name="value">Valor que deverá ser convertido</param>
        /// <returns>Value convertido em T</returns>
        public static T ToAny<T>(object value) => (T)ToAny(value, typeof(T));

        /// <summary>
        /// Converter tipo de um objeto
        /// </summary>
        /// <param name="expectedType">Para qual tipo converter o conteúdo do objeto</param>
        /// <param name="value">Conteúdo do objeto a ser convertido</param>
        /// <returns>Conteúdo do objeto convertido para o tipo informado</returns>
        public static object ToAny(object value, Type expectedType) => ChangeType(value, expectedType);

        /// <summary>
        /// Converte um valor do objeto em double
        /// </summary>
        /// <param name="value">valor a ser convertido</param>
        /// <returns>Valor convertido para double</returns>
        public static double ToDouble(object value) => UConvert.ToDouble(value, true);

        /// <summary>
        /// Converter STRING para ENUM
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="value">String a ser convertida</param>
        /// <returns>Retorna o Enum da string passada como parâmetro</returns>
        public static T ToEnum<T>(this string value) => (T)Enum.Parse(typeof(T), value, true);

        /// <summary>
        /// Calcular o valor hexadecimal de uma string
        /// </summary>
        /// <param name="input">Valor a ser convertido</param>
        /// <returns>Valor convertido em hexadecimal</returns>
        public static string ToHexadecimal(string input) => UConvert.ToHexadecimal(input);

        /// <summary>
        /// Criptografa uma string com RSA-SHA1 e retorna o conteúdo convertido para Base64String
        /// </summary>
        /// <param name="certificado">certificado utilizado na criptografia</param>
        /// <param name="value">Conteúdo a ser criptografado</param>
        /// <returns>Retorna a string assinada com RSA SHA1 e convertida para Base64String</returns>
        public static string ToRSASHA1(X509Certificate2 certificado, string value) => SHA1Helper.ToRSASHA1(certificado, value);

        /// <summary>
        /// Converte conteúdo para HSA1HashData
        /// </summary>
        /// <param name="data">Conteúdo a ser convertido</param>
        /// <returns>Conteúdo convertido para SH1HashData</returns>
        public static string ToSHA1HashData(string data) => ToSHA1HashData(data, false);

        /// <summary>
        /// Converte conteúdo para HSA1HashData
        /// </summary>
        /// <param name="data">Conteúdo a ser convertido</param>
        /// <param name="toUpper">Resultado todo em maiúsculo?</param>
        /// <returns>Conteúdo convertido para SH1HashData</returns>
        public static string ToSHA1HashData(string data, bool toUpper) => SHA1Helper.ToSHA1HashData(data, toUpper);

        /// <summary>
        /// Escreve uma string base64 em um arquivo PDF.
        /// <para>A string já deve ser um PDF válido. Este método apenas escreve o arquivo</para>
        /// </summary>
        /// <param name="content">Conteúdo que será escrito no arquivo</param>
        /// <param name="path">Pasta e nome do arquivo onde deve ser gravado o PDF</param>
        /// <exception cref="ArgumentNullException">Se o <paramref name="content"/> for nulo</exception>
        /// <exception cref="ArgumentException">Se o <paramref name="path"/> for nulo, vazio ou espaços</exception>
        public static void Base64ToPDF(string content, string path) => PDFHelper.WriteBase64ToPDFFile(content, path);

        /// <summary>
        /// Calcula o hash SHA-1 de uma entrada e retorna o resultado em formato Base64.
        /// </summary>
        /// <param name="input">A string de entrada para a qual o hash SHA-1 será calculado.</param>
        /// <returns>O hash SHA-1 calculado em formato Base64.</returns>
        public static string CalculateSHA1Hash(string input)
        {
            using (var sha1 = SHA1.Create())
            {
                var inputBytes = Encoding.UTF8.GetBytes(input);
                var hashBytes = sha1.ComputeHash(inputBytes);

                return Convert.ToBase64String(hashBytes);
            }
        }

        /// <summary>
        /// Verifica se uma string está no formato hexadecimal de um hash SHA-1.
        /// </summary>
        /// <param name="input">A string a ser verificada.</param>
        /// <returns>True se a string estiver no formato de hash SHA-1, False caso contrário.</returns>
        public static bool IsSHA1Hash(string input)
        {
            // Definir uma expressão regular para verificar o formato de hash SHA-1
            var pattern = "^[0-9a-fA-F]{40}$";

            // Verificar se a string corresponde ao padrão
            return Regex.IsMatch(input, pattern);
        }

        /// <summary>
        /// Verifica se uma string está no formato Base64 de um hash SHA-1.
        /// </summary>
        /// <param name="input">A string a ser verificada.</param>
        /// <returns>True se a string estiver no formato de hash SHA-1 em Base64, False caso contrário.</returns>
        public static bool IsSHA1Base64(string input)
        {
            return input.Length == 28; // SHA-1 em Base64 tem que ter 28 caracteres
        }
    }

#if INTEROP

    /// <summary>
    /// Classe para conversão de objetos - Específico INTEROP
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ConverterInterop")]
    [ComVisible(true)]
    public class ConverterInterop
    {
        /// <summary>
        /// Criptografa uma string com RSA-SHA1 e retorna o conteúdo convertido para Base64String
        /// </summary>
        /// <param name="certificado">certificado utilizado na criptografia</param>
        /// <param name="value">Conteúdo a ser criptografado</param>
        /// <returns>Retorna a string assinada com RSA SHA1 e convertida para Base64String</returns>
        public string ToRSASHA1(X509Certificate2 certificado, string value) => Converter.ToRSASHA1(certificado, value);

        /// <summary>
        /// Criptografa uma string com RSA-SHA1 e retorna o conteúdo convertido para Base64String
        /// </summary>
        /// <param name="serialNumberOrThumbPrint">Serial Number ou Thumbprint do certificado a ser utilizado na criptografia</param>
        /// <param name="value">Conteúdo a ser criptografado</param>
        /// <returns>Retorna a string assinada com RSA SHA1 e convertida para Base64String</returns>
        public string ToRSASHA1SerialNumber(string serialNumberOrThumbPrint, string value)
        {
            var retorno = string.Empty;
            try
            {
                var certificado = new CertificadoDigital().BuscarCertificadoDigital(serialNumberOrThumbPrint);

                retorno = ToRSASHA1(certificado, value);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }

            return retorno;
        }

        /// <summary>
        /// Escreve uma string base64 em um arquivo PDF.
        /// <para>A string já deve ser um PDF válido. Este método apenas escreve o arquivo</para>
        /// </summary>
        /// <param name="content">Conteúdo que será escrito no arquivo</param>
        /// <param name="path">Pasta e nome do arquivo onde deve ser gravado o PDF</param>
        /// <exception cref="ArgumentNullException">Se o <paramref name="content"/> for nulo</exception>
        /// <exception cref="ArgumentException">Se o <paramref name="path"/> for nulo, vazio ou espaços</exception>
        public void Base64ToPDF(string content, string path) => Converter.Base64ToPDF(content, path);

        /// <summary>
        /// Converte conteúdo para HSA1HashData
        /// </summary>
        /// <param name="data">Conteúdo a ser convertido</param>
        /// <param name="toUpper">Resultado todo em maiúsculo?</param>
        /// <returns>Conteúdo convertido para SH1HashData</returns>
        public string ToSHA1HashData(string data, bool toUpper) => Converter.ToSHA1HashData(data, toUpper);

        /// <summary>
        /// Verifica se uma string está no formato hexadecimal de um hash SHA-1.
        /// </summary>
        /// <param name="input">A string a ser verificada.</param>
        /// <returns>True se a string estiver no formato de hash SHA-1, False caso contrário.</returns>
        public bool IsSHA1Hash(string input) => Converter.IsSHA1Hash(input);

        /// <summary>
        /// Calcula o hash SHA-1 de uma entrada e retorna o resultado em formato Base64.
        /// </summary>
        /// <param name="input">A string de entrada para a qual o hash SHA-1 será calculado.</param>
        /// <returns>O hash SHA-1 calculado em formato Base64.</returns>
        public string CalculateSHA1Hash(string input) => Converter.CalculateSHA1Hash(input);

        /// <summary>
        /// Verifica se uma string está no formato Base64 de um hash SHA-1.
        /// </summary>
        /// <param name="input">A string a ser verificada.</param>
        /// <returns>True se a string estiver no formato de hash SHA-1 em Base64, False caso contrário.</returns>
        public bool IsSHA1Base64(string input) => Converter.IsSHA1Base64(input);
    }

#endif
}