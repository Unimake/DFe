using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;

namespace Unimake.Business.DFe.Security
{
    /// <summary>
    /// Extensão da classe X509Certificate2
    /// </summary>
    public static class ClsX509Certificate2Extension
    {
        /// <summary>
        /// Carregar o PIN do certificado A3 para que não apresente a tela para o usuário digitar.
        /// </summary>
        /// <param name="certificado">Certificado Digital</param>
        /// <param name="pinPassword">O Pin Code / Senha / Password do certificado digital</param>
        public static void SetPinPrivateKey(this X509Certificate2 certificado, string pinPassword)
        {
            if (certificado == null)
            {
                throw new ArgumentNullException("certificado == null!");
            }

            var key = (RSACryptoServiceProvider)certificado.PrivateKey;

            var ProviderHandle = IntPtr.Zero;
            var PinBuffer = Encoding.ASCII.GetBytes(pinPassword);

            //Não é necessário descarregar o handle
            SafeNativeMethods.Execute(() => SafeNativeMethods.CryptAcquireContext(
                ref ProviderHandle,
                key.CspKeyContainerInfo.KeyContainerName,
                key.CspKeyContainerInfo.ProviderName,
                key.CspKeyContainerInfo.ProviderType,
                SafeNativeMethods.CryptContextFlags.Silent));

            SafeNativeMethods.Execute(() => SafeNativeMethods.CryptSetProvParam(
                ProviderHandle,
                SafeNativeMethods.CryptParameter.KeyExchangePin,
                PinBuffer,
                0));

            SafeNativeMethods.Execute(() => SafeNativeMethods.CertSetCertificateContextProperty(
                certificado.Handle,
                SafeNativeMethods.CertificateProperty.CryptoProviderHandle,
                0,
                ProviderHandle));

            CarregarPINA3(certificado);
        }

        /// <summary>
        /// É necessário assinar um XML para carregar o PIN para não solicitar ao usuário.
        /// </summary>
        /// <param name="certificado">Certificado digital que é para carregar o PIN</param>
        private static void CarregarPINA3(X509Certificate2 certificado)
        {
            var docTemp = new XmlDocument();
            docTemp.LoadXml("<pinA3><xServ Id=\"ID1\">PINA3</xServ></pinA3>");

            var tagAssinatura = "pinA3";
            var tagAtributoID = "xServ";

            AssinaturaDigital.Assinar(docTemp, tagAssinatura, tagAtributoID, certificado, AlgorithmType.Sha1, true, "Id");
        }

        /// <summary>
        /// Retorna true se o certificado for do tipo A3.
        /// </summary>
        /// <param name="x509cert">Certificado que deverá ser validado se é A3 ou não.</param>
        /// <returns>true = É um certificado A3</returns>
        public static bool IsA3(this X509Certificate2 x509cert)
        {
            if (x509cert == null)
            {
                return false;
            }

            var result = false;

            try
            {
                if (x509cert.PrivateKey is RSACryptoServiceProvider service)
                {
                    if (service.CspKeyContainerInfo.Removable &&
                    service.CspKeyContainerInfo.HardwareDevice)
                    {
                        result = true;
                    }
                }
            }
            catch
            {
                result = false;
            }

            return result;
        }
    }

    /// <summary>
    /// Funções da API do Windows que realmente executam a passagem do PIN
    /// </summary>
    internal static class SafeNativeMethods
    {
        /// <summary>
        /// Sinalizadores/Flags de contexto para a criptografia
        /// </summary>
        internal enum CryptContextFlags
        {
            None = 0,
            Silent = 0x40
        }

        /// <summary>
        /// Propriedades do certificado
        /// </summary>
        internal enum CertificateProperty
        {
            None = 0,
            CryptoProviderHandle = 0x1
        }

        /// <summary>
        /// Parâmetros para criptografia
        /// </summary>
        internal enum CryptParameter
        {
            None = 0,
            KeyExchangePin = 0x20
        }

        [DllImport("advapi32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern bool CryptAcquireContext(
            ref IntPtr hProv,
            string containerName,
            string providerName,
            int providerType,
            CryptContextFlags flags
            );

        [DllImport("advapi32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        public static extern bool CryptSetProvParam(
            IntPtr hProv,
            CryptParameter dwParam,
            [In] byte[] pbData,
            uint dwFlags);

        [DllImport("CRYPT32.DLL", SetLastError = true)]
        internal static extern bool CertSetCertificateContextProperty(
            IntPtr pCertContext,
            CertificateProperty propertyId,
            uint dwFlags,
            IntPtr pvData
            );

        /// <summary>
        /// Executar funções para criptografia/descriptografia do PIN do certificado
        /// </summary>
        /// <param name="action">Ação/função a ser executada</param>
        public static void Execute(Func<bool> action)
        {
            if (!action())
            {
                throw new Win32Exception(Marshal.GetLastWin32Error());
            }
        }
    }

#if INTEROP

    /// <summary>
    /// Extensão da classe X509Certificate2 específica para INTEROP (Outras linguagens diferente do C#)
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")]
    [ComVisible(true)]
    public class ClsX509Certificate2ExtensionInterop
    {
        /// <summary>
        /// Carregar o PIN do certificado A3 para que não apresente a tela para o usuário digitar.
        /// </summary>
        /// <param name="certificado">Certificado Digital</param>
        /// <param name="pinPassword">O Pin Code / Senha / Password do certificado digital</param>
        public void SetPinPrivateKey(X509Certificate2 certificado, string pinPassword)
        {
            if (certificado == null)
            {
                throw new ArgumentNullException("certificado == null!");
            }

            var key = (RSACryptoServiceProvider)certificado.PrivateKey;

            var ProviderHandle = IntPtr.Zero;
            var PinBuffer = Encoding.ASCII.GetBytes(pinPassword);

            //Não é necessário descarregar o handle
            SafeNativeMethods.Execute(() => SafeNativeMethods.CryptAcquireContext(
                ref ProviderHandle,
                key.CspKeyContainerInfo.KeyContainerName,
                key.CspKeyContainerInfo.ProviderName,
                key.CspKeyContainerInfo.ProviderType,
                SafeNativeMethods.CryptContextFlags.Silent));

            SafeNativeMethods.Execute(() => SafeNativeMethods.CryptSetProvParam(
                ProviderHandle,
                SafeNativeMethods.CryptParameter.KeyExchangePin,
                PinBuffer,
                0));

            SafeNativeMethods.Execute(() => SafeNativeMethods.CertSetCertificateContextProperty(
                certificado.Handle,
                SafeNativeMethods.CertificateProperty.CryptoProviderHandle,
                0,
                ProviderHandle));

            CarregarPINA3(certificado);
        }

        /// <summary>
        /// É necessário assinar um XML para carregar o PIN para não solicitar ao usuário.
        /// </summary>
        /// <param name="certificado">Certificado digital que é para carregar o PIN</param>
        private void CarregarPINA3(X509Certificate2 certificado)
        {
            var docTemp = new XmlDocument();
            docTemp.LoadXml("<pinA3><xServ Id=\"ID1\">PINA3</xServ></pinA3>");

            var tagAssinatura = "pinA3";
            var tagAtributoID = "xServ";

            AssinaturaDigital.Assinar(docTemp, tagAssinatura, tagAtributoID, certificado, AlgorithmType.Sha1, true, "Id");
        }

        /// <summary>
        /// Retorna true se o certificado for do tipo A3.
        /// </summary>
        /// <param name="x509cert">Certificado que deverá ser validado se é A3 ou não.</param>
        /// <returns>true = É um certificado A3</returns>
        public bool IsA3(X509Certificate2 x509cert)
        {
            if (x509cert == null)
            {
                return false;
            }

            var result = false;

            try
            {
                if (x509cert.PrivateKey is RSACryptoServiceProvider service)
                {
                    if (service.CspKeyContainerInfo.Removable &&
                    service.CspKeyContainerInfo.HardwareDevice)
                    {
                        result = true;
                    }
                }
            }
            catch
            {
                result = false;
            }

            return result;
        }
    }

#endif
}