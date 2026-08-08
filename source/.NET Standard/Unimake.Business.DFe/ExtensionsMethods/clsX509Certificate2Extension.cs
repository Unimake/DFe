using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;

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
            X509Certificate2A3Service.Instance.SetPinPrivateKey(certificado, pinPassword);
        }

        /// <summary>
        /// Retorna true se o certificado for do tipo A3.
        /// </summary>
        /// <param name="x509cert">Certificado que deverá ser validado se é A3 ou não.</param>
        /// <returns>true = É um certificado A3</returns>
        public static bool IsA3(this X509Certificate2 x509cert)
        {
            return X509Certificate2A3Service.Instance.IsA3(x509cert);
        }
    }

    internal sealed class X509Certificate2A3Service
    {
        private const int NteBadAlgorithmId = unchecked((int)0x80090008);
        private static readonly byte[] Sha256Hash = new byte[32];
        private static readonly byte[] Sha1Hash = new byte[20];
        private readonly IX509Certificate2NativeApi nativeApi;
        private readonly IPrivateKeyWarmUp privateKeyWarmUp;

        internal static readonly X509Certificate2A3Service Instance =
            new X509Certificate2A3Service(new X509Certificate2NativeApi(), new PrivateKeyWarmUp());

        internal X509Certificate2A3Service(
            IX509Certificate2NativeApi nativeApi,
            IPrivateKeyWarmUp privateKeyWarmUp)
        {
            this.nativeApi = nativeApi ?? throw new ArgumentNullException(nameof(nativeApi));
            this.privateKeyWarmUp = privateKeyWarmUp ?? throw new ArgumentNullException(nameof(privateKeyWarmUp));
        }

        internal void SetPinPrivateKey(X509Certificate2 certificado, string pinPassword)
        {
            ValidarCertificado(certificado);

            if (pinPassword == null)
            {
                throw new ArgumentNullException(nameof(pinPassword));
            }

            if (pinPassword.Length == 0)
            {
                throw new ArgumentException("O PIN do certificado digital não pode ser vazio.", nameof(pinPassword));
            }

            if (!nativeApi.IsWindows)
            {
                throw new PlatformNotSupportedException("A configuração programática do PIN de certificados A3 é suportada somente no Windows.");
            }

            var privateKey = nativeApi.AcquirePrivateKey(certificado, true);

            try
            {
                if (privateKey.KeySpec == CertificateKeySpec.Cng)
                {
                    ConfigurarPinCng(privateKey.Handle, pinPassword);
                }
                else
                {
                    ConfigurarPinCsp(privateKey, pinPassword);
                }

                privateKeyWarmUp.WarmUp(certificado);
            }
            finally
            {
                nativeApi.ReleasePrivateKey(privateKey);
            }
        }

        internal bool IsA3(X509Certificate2 certificado)
        {
            if (certificado == null || !nativeApi.IsWindows)
            {
                return false;
            }

            try
            {
                ValidarCertificado(certificado);

                var privateKey = nativeApi.AcquirePrivateKey(certificado, false);

                try
                {
                    NativeImplementationType implementationType;
                    var identified = privateKey.KeySpec == CertificateKeySpec.Cng
                        ? nativeApi.TryGetCngImplementationType(privateKey.Handle, out implementationType)
                        : nativeApi.TryGetCspImplementationType(privateKey.Handle, out implementationType);

                    return identified &&
                        (implementationType & NativeImplementationType.Hardware) != 0 &&
                        (implementationType & NativeImplementationType.Removable) != 0;
                }
                finally
                {
                    nativeApi.ReleasePrivateKey(privateKey);
                }
            }
            catch
            {
                // IsA3 historicamente não propaga falhas de acesso ao middleware/certificado.
                return false;
            }
        }

        private void ConfigurarPinCsp(PrivateKeyHandle privateKey, string pinPassword)
        {
            CryptProviderParameter parameter;

            switch (privateKey.KeySpec)
            {
                case CertificateKeySpec.KeyExchange:
                    parameter = CryptProviderParameter.KeyExchangePin;
                    break;

                case CertificateKeySpec.Signature:
                    parameter = CryptProviderParameter.SignaturePin;
                    break;

                default:
                    throw new CryptographicException($"Especificação de chave CSP não suportada: {(int)privateKey.KeySpec}.");
            }

            var pinBuffer = Encoding.ASCII.GetBytes(pinPassword + '\0');

            try
            {
                nativeApi.SetCspPin(privateKey.Handle, parameter, pinBuffer);
            }
            finally
            {
                Array.Clear(pinBuffer, 0, pinBuffer.Length);
            }
        }

        private void ConfigurarPinCng(IntPtr providerHandle, string pinPassword)
        {
            var pinBuffer = Encoding.Unicode.GetBytes(pinPassword + '\0');

            try
            {
                nativeApi.SetCngPin(providerHandle, pinBuffer);
            }
            finally
            {
                Array.Clear(pinBuffer, 0, pinBuffer.Length);
            }
        }

        private static void ValidarCertificado(X509Certificate2 certificado)
        {
            if (certificado == null)
            {
                throw new ArgumentNullException(nameof(certificado));
            }

            if (!certificado.HasPrivateKey)
            {
                throw new CryptographicException("O certificado digital não possui chave privada.");
            }

            using (var rsa = certificado.GetRSAPublicKey())
            {
                if (rsa == null)
                {
                    throw new CryptographicException("A chave privada do certificado digital não é do tipo RSA.");
                }
            }
        }

        private sealed class PrivateKeyWarmUp : IPrivateKeyWarmUp
        {
            public void WarmUp(X509Certificate2 certificado)
            {
                using (var rsa = certificado.GetRSAPrivateKey())
                {
                    if (rsa == null)
                    {
                        throw new CryptographicException("Não foi possível acessar a chave privada RSA do certificado digital.");
                    }

                    try
                    {
                        rsa.SignHash(Sha256Hash, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                    }
                    catch (CryptographicException ex) when (ShouldFallbackToSha1(ex))
                    {
                        rsa.SignHash(Sha1Hash, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1);
                    }
                }
            }
        }

        internal static bool ShouldFallbackToSha1(CryptographicException exception)
        {
            return exception != null && exception.HResult == NteBadAlgorithmId;
        }
    }

    internal interface IPrivateKeyWarmUp
    {
        void WarmUp(X509Certificate2 certificado);
    }

    internal interface IX509Certificate2NativeApi
    {
        bool IsWindows { get; }

        PrivateKeyHandle AcquirePrivateKey(X509Certificate2 certificado, bool cache);

        void ReleasePrivateKey(PrivateKeyHandle privateKey);

        void SetCngPin(IntPtr providerHandle, byte[] pinBuffer);

        void SetCspPin(IntPtr providerHandle, CryptProviderParameter parameter, byte[] pinBuffer);

        bool TryGetCngImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType);

        bool TryGetCspImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType);
    }

    internal struct PrivateKeyHandle
    {
        internal IntPtr Handle;
        internal CertificateKeySpec KeySpec;
        internal bool CallerMustFree;
    }

    internal enum CertificateKeySpec
    {
        KeyExchange = 1,
        Signature = 2,
        Cng = -1
    }

    [Flags]
    internal enum NativeImplementationType
    {
        None = 0,
        Hardware = 0x1,
        Software = 0x2,
        Mixed = Hardware | Software,
        Unknown = 0x4,
        Removable = 0x8,
        HardwareRandomNumberGenerator = 0x10
    }

    internal enum CryptProviderParameter
    {
        KeyExchangePin = 0x20,
        SignaturePin = 0x21
    }

    internal sealed class X509Certificate2NativeApi : IX509Certificate2NativeApi
    {
        private const string ImplementationTypeProperty = "Impl Type";
        private const string PinProperty = "SmartCardPin";
        private const string ProviderHandleProperty = "Provider Handle";

        public bool IsWindows => RuntimeInformation.IsOSPlatform(OSPlatform.Windows);

        public PrivateKeyHandle AcquirePrivateKey(X509Certificate2 certificado, bool cache)
        {
            var flags = CryptAcquireFlags.PreferCng | CryptAcquireFlags.Silent;

            if (cache)
            {
                flags |= CryptAcquireFlags.Cache;
            }

            if (!SafeNativeMethods.CryptAcquireCertificatePrivateKey(
                certificado.Handle,
                flags,
                IntPtr.Zero,
                out var providerHandle,
                out var keySpec,
                out var callerMustFree))
            {
                throw CreateWin32Exception("CryptAcquireCertificatePrivateKey");
            }

            return new PrivateKeyHandle
            {
                Handle = providerHandle,
                KeySpec = keySpec,
                CallerMustFree = callerMustFree
            };
        }

        public void ReleasePrivateKey(PrivateKeyHandle privateKey)
        {
            if (!privateKey.CallerMustFree || privateKey.Handle == IntPtr.Zero)
            {
                return;
            }

            if (privateKey.KeySpec == CertificateKeySpec.Cng)
            {
                SafeNativeMethods.NCryptFreeObject(privateKey.Handle);
            }
            else
            {
                SafeNativeMethods.CryptReleaseContext(privateKey.Handle, 0);
            }
        }

        public void SetCngPin(IntPtr providerHandle, byte[] pinBuffer)
        {
            var result = SafeNativeMethods.NCryptSetProperty(
                providerHandle,
                PinProperty,
                pinBuffer,
                pinBuffer.Length,
                0);

            if (result != 0)
            {
                throw new Win32Exception(result, $"NCryptSetProperty({PinProperty}) falhou com o código 0x{result:X8}.");
            }
        }

        public void SetCspPin(IntPtr providerHandle, CryptProviderParameter parameter, byte[] pinBuffer)
        {
            if (!SafeNativeMethods.CryptSetProvParam(providerHandle, parameter, pinBuffer, 0))
            {
                throw CreateWin32Exception($"CryptSetProvParam({parameter})");
            }
        }

        public bool TryGetCngImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType)
        {
            implementationType = NativeImplementationType.None;

            var result = SafeNativeMethods.NCryptGetPropertyHandle(
                providerHandle,
                ProviderHandleProperty,
                out var storageProviderHandle,
                IntPtr.Size,
                out var resultSize,
                0);

            if (result != 0 || resultSize < IntPtr.Size || storageProviderHandle == IntPtr.Zero)
            {
                return false;
            }

            try
            {
                result = SafeNativeMethods.NCryptGetPropertyInt32(
                    storageProviderHandle,
                    ImplementationTypeProperty,
                    out var value,
                    sizeof(int),
                    out resultSize,
                    0);

                implementationType = result == 0 && resultSize >= sizeof(int)
                    ? (NativeImplementationType)value
                    : NativeImplementationType.None;

                return result == 0 && resultSize >= sizeof(int);
            }
            finally
            {
                SafeNativeMethods.NCryptFreeObject(storageProviderHandle);
            }
        }

        public bool TryGetCspImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType)
        {
            var dataSize = sizeof(int);
            var success = SafeNativeMethods.CryptGetProvParam(
                providerHandle,
                CryptProviderParameterQuery.ImplementationType,
                out var value,
                ref dataSize,
                0);

            implementationType = success && dataSize >= sizeof(int)
                ? (NativeImplementationType)value
                : NativeImplementationType.None;

            return success && dataSize >= sizeof(int);
        }

        private static Win32Exception CreateWin32Exception(string operation)
        {
            var errorCode = Marshal.GetLastWin32Error();
            return new Win32Exception(errorCode, $"{operation} falhou: {new Win32Exception(errorCode).Message}");
        }
    }

    [Flags]
    internal enum CryptAcquireFlags
    {
        Cache = 0x1,
        Silent = 0x40,
        PreferCng = 0x20000
    }

    internal enum CryptProviderParameterQuery
    {
        ImplementationType = 0x3
    }

    /// <summary>
    /// Funções das APIs de criptografia do Windows utilizadas para certificados A3.
    /// </summary>
    internal static class SafeNativeMethods
    {
        [DllImport("crypt32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CryptAcquireCertificatePrivateKey(
            IntPtr certificateContext,
            CryptAcquireFlags flags,
            IntPtr parameters,
            out IntPtr providerOrKeyHandle,
            out CertificateKeySpec keySpec,
            [MarshalAs(UnmanagedType.Bool)] out bool callerMustFree);

        [DllImport("advapi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CryptSetProvParam(
            IntPtr providerHandle,
            CryptProviderParameter parameter,
            [In] byte[] data,
            uint flags);

        [DllImport("advapi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CryptGetProvParam(
            IntPtr providerHandle,
            CryptProviderParameterQuery parameter,
            out int data,
            ref int dataSize,
            uint flags);

        [DllImport("advapi32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CryptReleaseContext(IntPtr providerHandle, uint flags);

        [DllImport("ncrypt.dll", CharSet = CharSet.Unicode)]
        internal static extern int NCryptSetProperty(
            IntPtr objectHandle,
            string propertyName,
            [In] byte[] input,
            int inputSize,
            int flags);

        [DllImport("ncrypt.dll", CharSet = CharSet.Unicode, EntryPoint = "NCryptGetProperty")]
        internal static extern int NCryptGetPropertyInt32(
            IntPtr objectHandle,
            string propertyName,
            out int output,
            int outputSize,
            out int resultSize,
            int flags);

        [DllImport("ncrypt.dll", CharSet = CharSet.Unicode, EntryPoint = "NCryptGetProperty")]
        internal static extern int NCryptGetPropertyHandle(
            IntPtr objectHandle,
            string propertyName,
            out IntPtr output,
            int outputSize,
            out int resultSize,
            int flags);

        [DllImport("ncrypt.dll")]
        internal static extern int NCryptFreeObject(IntPtr objectHandle);
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
            X509Certificate2A3Service.Instance.SetPinPrivateKey(certificado, pinPassword);
        }

        /// <summary>
        /// Retorna true se o certificado for do tipo A3.
        /// </summary>
        /// <param name="x509cert">Certificado que deverá ser validado se é A3 ou não.</param>
        /// <returns>true = É um certificado A3</returns>
        public bool IsA3(X509Certificate2 x509cert)
        {
            return X509Certificate2A3Service.Instance.IsA3(x509cert);
        }
    }

#endif
}
