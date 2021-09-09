using System;
using System.ComponentModel;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe para conversão de objetos
    /// </summary>
    public static class Converter
    {
        #region Private Methods

        /// <summary>
        /// Converter tipo de um objeto
        /// </summary>
        /// <param name="value">Para qual tipo converter o conteúdo do objeto</param>
        /// <param name="expectedType">Para qual tipo converter o conteúdo do objeto</param>
        /// <returns>Conteúdo do objeto convertido para o tipo informado</returns>
        private static object ChangeType(object value, Type expectedType)
        {
            if(value == null)
            {
                return null;
            }

            object result = null;

            try
            {
                if(expectedType.IsEnum)
                {
                    if(int.TryParse(value.ToString(), out var i))
                    {
                        result = (from enun in Enum.GetValues(expectedType).Cast<int>()
                                  where enun == i
                                  select enun).First();
                    }
                }
                else if(expectedType == typeof(TimeSpan) ||
                        expectedType == typeof(TimeSpan?))
                {
                    var timeDate = new DateTime();

                    if(DateTime.TryParse(value.ToString(), out timeDate))
                    {
                        result = new TimeSpan(timeDate.Ticks);
                    }
                }
                else if(expectedType == typeof(bool) ||
                         expectedType == typeof(bool?))
                {
                    bool.TryParse(value?.ToString(), out var b);
                    result = b;
                }
                else
                {
                    if(!ConvertFromTypeDescriptor(value, expectedType, ref result))
                    {
                        result = Convert.ChangeType(value, expectedType);
                    }
                }
            }
            catch
            {
                ConvertFromTypeDescriptor(value, expectedType, ref result);
            }

            return result;
        }

        private static bool ConvertFromTypeDescriptor(object value, Type expectedType, ref object result)
        {
            var conv = TypeDescriptor.GetConverter(expectedType);

            if(!(conv?.CanConvertFrom(value.GetType()) ?? false))
            {
                return false;
            }

            try
            {
                result = conv.ConvertFrom(value);
                return true;
            }
            catch
            {
                return false;
            }
        }

        #endregion Private Methods

        #region Public Methods

        /// <summary>
        /// Converter string para MemoryStream com  UTF8 Encoding
        /// </summary>
        /// <param name="contentConvert">Conteúdo a ser convertido</param>
        /// <returns>Conteúdo convertido para MemoryStrem com UTF8 Encoding</returns>
        public static MemoryStream StringToStreamUTF8(string contentConvert)
        {
            var encoding = new System.Text.UTF8Encoding();
            var byteArray = encoding.GetBytes(contentConvert);
            var memoryStream = new MemoryStream(byteArray);
            memoryStream.Seek(0, SeekOrigin.Begin);

            return memoryStream;
        }

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
        public static object ToAny(object value, Type expectedType)
        {
            var result = default(object);

            expectedType = Nullable.GetUnderlyingType(expectedType) ?? expectedType;

            if(value != null && value != DBNull.Value)
            {
                try
                {
                    if(expectedType.IsEnum)
                    {
                        result = Enum.Parse(expectedType, value.ToString());
                    }
                    else if(expectedType.FullName.Equals(typeof(string).FullName))
                    {
                        result = value.ToString();
                    }
                    else if(expectedType.FullName.Equals(typeof(double).FullName))
                    {
                        result = double.Parse(value.ToString(), CultureInfo.InvariantCulture);
                    }
                    else
                    {
                        result = ChangeType(value, expectedType);
                    }
                }
                catch
                {
                    result = default;
                }
            }

            return result;
        }

        /// <summary>
        /// Converte um valor do objeto em double
        /// </summary>
        /// <param name="value">valor a ser convertido</param>
        /// <returns>Valor convertido para double</returns>
        public static double ToDouble(object value)
        {
            if(value == null)
            {
                //TODO: Marcelo >>> Vai retornar zero por padrão mesmo?
                return 0;
            }

            double.TryParse(value.ToString(),
                            NumberStyles.Number,
                            CultureInfo.InvariantCulture,
                            out var result);
            return result;
        }

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
        public static string ToHexadecimal(string input)
        {
            var hexOutput = "";
            var values = input.ToCharArray();
            foreach(var letter in values)
            {
                // Get the integral value of the character.
                var value = Convert.ToInt32(letter);

                // Convert the decimal value to a hexadecimal value in string form.
                hexOutput += string.Format("{0:x}", value);
            }

            return hexOutput;
        }

        /// <summary>
        /// Criptografa uma string com RSA-SHA1 e retorna o conteúdo convertido para Base64String
        /// </summary>
        /// <param name="certificado">certificado utilizado na criptografia</param>
        /// <param name="value">Conteúdo a ser criptografado</param>
        /// <returns>Retorna a string assinada com RSA SHA1 e convertida para Base64String</returns>
        public static string ToRSASHA1(X509Certificate2 certificado, string value)
        {
            // Converter a cadeia de caracteres ASCII para bytes.
            var asciiEncoding = new ASCIIEncoding();
            var asciiBytes = asciiEncoding.GetBytes(value);

            // Gerar o HASH (array de bytes) utilizando SHA1
            var sha1 = new SHA1CryptoServiceProvider();
            var sha1Hash = sha1.ComputeHash(asciiBytes);

            // Assinar o HASH (array de bytes) utilizando RSA-SHA1.
            var rsa = certificado.PrivateKey as RSACryptoServiceProvider;
            asciiBytes = rsa.SignHash(sha1Hash, "SHA1");
            var result = Convert.ToBase64String(asciiBytes);

            return result;
        }

        /// <summary>
        /// Converte conteúdo para HSA1HashData
        /// </summary>
        /// <param name="data">Conteudo a ser convertido</param>
        /// <returns>Conteúdo convertido para SH1HashData</returns>
        public static string ToSHA1HashData(string data) => ToSHA1HashData(data, false);

        /// <summary>
        /// Converte conteúdo para HSA1HashData
        /// </summary>
        /// <param name="data">Conteudo a ser convertido</param>
        /// <param name="toUpper">Resultado todo em maiúsculo?</param>
        /// <returns>Conteúdo convertido para SH1HashData</returns>
        public static string ToSHA1HashData(string data, bool toUpper)
        {
            using(HashAlgorithm algorithm = new SHA1CryptoServiceProvider())
            {
                var buffer = algorithm.ComputeHash(Encoding.ASCII.GetBytes(data));
                var builder = new StringBuilder(buffer.Length);
                foreach(var num in buffer)
                {
                    if(toUpper)
                    {
                        builder.Append(num.ToString("X2"));
                    }
                    else
                    {
                        builder.Append(num.ToString("x2"));
                    }
                }

                return builder.ToString();
            }
        }

        /// <summary>
        /// Converte uma string Base64 para um arquivo PDF
        /// </summary>
        /// <param name="contentBase64">Conteúdo Base64 a ser convertido para PDF</param>
        /// <param name="arqPDF">Pasta e nome do arquivo onde deve ser gravado o PDF</param>
        public static void Base64ToPDF(string contentBase64, string arqPDF)
        {
            BinaryWriter writer = null;

            try
            {
                var sPDFDecoded = Convert.FromBase64String(contentBase64);

                if (File.Exists(arqPDF))
                {
                    File.Delete(arqPDF);
                }

                writer = new BinaryWriter(File.Open(arqPDF, FileMode.CreateNew));
                writer.Write(sPDFDecoded);
            }
            catch
            {
                throw;
            }
            finally
            {
                if(writer != null)
                {
                    writer.Close();
                }
            }
        }

        #endregion Public Methods
    }
}