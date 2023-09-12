using System;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography;
using System.Text;

namespace Unimake.Business.DFe.Security
{
    internal static class Criptografia
    {
        private static string _chave = "unimake_uninfe";

        public static string criptografaSenha(string senhaCripto)
        {
            try
            {
                if (String.IsNullOrEmpty(senhaCripto))
                    return "";
                else
                    return criptografaSenha(senhaCripto, _chave);
            }
            catch (Exception ex)
            {
                return "String errada. " + ex.Message;
            }

        }

        public static string descriptografaSenha(string senhaDescripto)
        {
            try
            {
                if (String.IsNullOrEmpty(senhaDescripto))
                    return "";
                else
                    if (IsCriptografadaSenha(senhaDescripto))
                    return descriptografaSenha(senhaDescripto, _chave);
                else
                    return senhaDescripto;
            }
            catch (Exception ex)
            {
                return "Wrong Input. " + ex.Message;
            }
        }

        public static string criptografaSenha(string senhaCripto, string chave)
        {
            try
            {
                TripleDESCryptoServiceProvider objcriptografaSenha = new TripleDESCryptoServiceProvider();
                MD5CryptoServiceProvider objcriptoMd5 = new MD5CryptoServiceProvider();

                byte[] byteHash, byteBuff;
                string strTempKey = chave;

                byteHash = objcriptoMd5.ComputeHash(ASCIIEncoding.ASCII.GetBytes(strTempKey));
                objcriptoMd5 = null;
                objcriptografaSenha.Key = byteHash;
                objcriptografaSenha.Mode = CipherMode.ECB;

                byteBuff = ASCIIEncoding.ASCII.GetBytes(senhaCripto);
                return Convert.ToBase64String(objcriptografaSenha.CreateEncryptor().TransformFinalBlock(byteBuff, 0, byteBuff.Length));
            }
            catch (Exception ex)
            {
                return "Digite os valores Corretamente." + ex.Message;
            }
        }

        public static string descriptografaSenha(string strCriptografada, string chave)
        {
            try
            {
                TripleDESCryptoServiceProvider objdescriptografaSenha = new TripleDESCryptoServiceProvider();
                MD5CryptoServiceProvider objcriptoMd5 = new MD5CryptoServiceProvider();

                byte[] byteHash, byteBuff;
                string strTempKey = chave;

                byteHash = objcriptoMd5.ComputeHash(ASCIIEncoding.ASCII.GetBytes(strTempKey));
                objcriptoMd5 = null;
                objdescriptografaSenha.Key = byteHash;
                objdescriptografaSenha.Mode = CipherMode.ECB;

                byteBuff = Convert.FromBase64String(strCriptografada);
                string strDecrypted = ASCIIEncoding.ASCII.GetString(objdescriptografaSenha.CreateDecryptor().TransformFinalBlock(byteBuff, 0, byteBuff.Length));
                objdescriptografaSenha = null;

                return strDecrypted;
            }
            catch (Exception ex)
            {
                return "Digite os valores Corretamente." + ex.Message;
            }
        }

        /// <summary>
        /// Metodo que verifica se a string encontra-se criptografada, pode ser utilizada
        /// antes de se tentar descriptografar uma senha evitando exceções na aplicação.
        /// </summary>
        /// <param name="senhaCripto">string com a senha</param>
        /// <returns>booleano que se a senha esta criptografda</returns>
        /// <author>Renan Borges</author>
        public static bool IsCriptografadaSenha(string senhaCripto)
        {
            try
            {
                if (String.IsNullOrEmpty(senhaCripto))
                    return false;
                else
                    return IsCriptografadaSenha(senhaCripto, _chave);
            }
            catch
            {
                return false;
            }

        }

        /// <summary>
        /// Metodo que verifica se a string encontra-se criptografada, pode ser utilizada
        /// antes de se tentar descriptografar uma senha evitando exceções na aplicação.
        /// </summary>
        /// <param name="senhaCripto">string com a senha</param>
        /// <returns>booleano que se a senha esta criptografda</returns>
        /// <author>Renan Borges</author>
        public static bool IsCriptografadaSenha(string strCriptografada, string chave)
        {
            try
            {
                TripleDESCryptoServiceProvider objdescriptografaSenha = new TripleDESCryptoServiceProvider();
                MD5CryptoServiceProvider objcriptoMd5 = new MD5CryptoServiceProvider();

                byte[] byteHash, byteBuff;
                string strTempKey = chave;

                byteHash = objcriptoMd5.ComputeHash(ASCIIEncoding.ASCII.GetBytes(strTempKey));
                objcriptoMd5 = null;
                objdescriptografaSenha.Key = byteHash;
                objdescriptografaSenha.Mode = CipherMode.ECB;

                byteBuff = Convert.FromBase64String(strCriptografada);
                string strDecrypted = ASCIIEncoding.ASCII.GetString(objdescriptografaSenha.CreateDecryptor().TransformFinalBlock(byteBuff, 0, byteBuff.Length));
                objdescriptografaSenha = null;

                return true;
            }
            catch
            {
                return false;
            }
        }

        public static bool compararStrings(string num01, string num02)
        {
            bool stringValor;
            if (num01.Equals(num02))
            {
                stringValor = true;
            }
            else
            {
                stringValor = false;
            }
            return stringValor;
        }

        /// <summary>
        /// Assina a string utilizando RSA-SHA1
        /// </summary>
        /// <param name="cert">certificado utilizado para assinar a string</param>
        /// <param name="value">Valor a ser assinado</param>
        /// <returns></returns>
        public static string SignWithRSASHA1(X509Certificate2 cert, String value)
        {
            //Regras retiradas da página 39 do manual da Prefeitura Municipal de Blumenau
            // Converta a cadeia de caracteres ASCII para bytes. 
            ASCIIEncoding asciiEncoding = new ASCIIEncoding();
            byte[] asciiBytes = asciiEncoding.GetBytes(value);

            // Gere o HASH (array de bytes) utilizando SHA1
            SHA1CryptoServiceProvider sha1 = new SHA1CryptoServiceProvider();
            byte[] sha1Hash = sha1.ComputeHash(asciiBytes);

            //- Assine o HASH (array de bytes) utilizando RSA-SHA1.
            RSACryptoServiceProvider rsa = new RSACryptoServiceProvider();
            rsa = cert.PrivateKey as RSACryptoServiceProvider;
            asciiBytes = rsa.SignHash(sha1Hash, "SHA1");
            string result = Convert.ToBase64String(asciiBytes);
            return result;
        }

        public static string GerarRSASHA512(string value, bool lower = false)
        {
            var sha512 = SHA512.Create();
            var bytes = Encoding.UTF8.GetBytes(value);
            var hash = sha512.ComputeHash(bytes);

            var result = new StringBuilder();
            for (var i = 0; i < hash.Length; i++)
            {
                result.Append(hash[i].ToString($"{(lower ? "x" : "X")}2"));
            }

            return result.ToString();
        }


        public static string GetSHA1HashData(string data)
        {
            return GetSHA1HashData(data, false);
        }

        public static string GetSHA1HashData(string data, bool toUpper)
        {
            HashAlgorithm algorithm = new SHA1CryptoServiceProvider();
            byte[] buffer = algorithm.ComputeHash(System.Text.Encoding.ASCII.GetBytes(data));
            System.Text.StringBuilder builder = new System.Text.StringBuilder(buffer.Length);
            foreach (byte num in buffer)
            {
                if (toUpper)
                    builder.Append(num.ToString("X2"));
                else
                    builder.Append(num.ToString("x2"));
            }

            return builder.ToString();
        }

        public static bool ValidateSHA1HashData(string inputData, string storedHashData)
        {
            //hash input text and save it string variable
            string getHashInputData = GetSHA1HashData(inputData);

            if (string.Compare(getHashInputData, storedHashData) == 0)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
}
