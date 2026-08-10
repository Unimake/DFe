using System;
using System.ComponentModel;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Security;
using Xunit;

namespace Unimake.DFe.Test.Utility.Certificados
{
    public class X509Certificate2A3ServiceTest
    {
        [Fact]
        [Trait("Utility", "Certificados")]
        public void SetPinPrivateKeyDeveConfigurarPinUnicodeNoCngELimparBuffer()
        {
            using (var certificate = CreateRsaCertificate())
            {
                var nativeApi = new FakeNativeApi(CertificateKeySpec.Cng);
                var warmUp = new FakeWarmUp();
                var service = new X509Certificate2A3Service(nativeApi, warmUp);

                service.SetPinPrivateKey(certificate, "1234");

                Assert.Equal(new byte[] { 49, 0, 50, 0, 51, 0, 52, 0, 0, 0 }, nativeApi.PinDuringCall);
                Assert.All(nativeApi.PinBuffer, value => Assert.Equal(0, value));
                Assert.True(nativeApi.CacheRequested);
                Assert.True(nativeApi.Released);
                Assert.True(warmUp.Executed);
            }
        }

        [Theory]
        [InlineData((int)CertificateKeySpec.KeyExchange, (int)CryptProviderParameter.KeyExchangePin)]
        [InlineData((int)CertificateKeySpec.Signature, (int)CryptProviderParameter.SignaturePin)]
        [Trait("Utility", "Certificados")]
        public void SetPinPrivateKeyDeveSelecionarPinCspPeloKeySpec(
            int keySpecValue,
            int expectedParameterValue)
        {
            using (var certificate = CreateRsaCertificate())
            {
                var keySpec = (CertificateKeySpec)keySpecValue;
                var expectedParameter = (CryptProviderParameter)expectedParameterValue;
                var nativeApi = new FakeNativeApi(keySpec);
                var service = new X509Certificate2A3Service(nativeApi, new FakeWarmUp());

                service.SetPinPrivateKey(certificate, "9876");

                Assert.Equal(expectedParameter, nativeApi.CspParameter);
                Assert.Equal(new byte[] { 57, 56, 55, 54, 0 }, nativeApi.PinDuringCall);
                Assert.All(nativeApi.PinBuffer, value => Assert.Equal(0, value));
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void SetPinPrivateKeyDeveLimparBufferELiberarHandleQuandoConfiguracaoFalhar()
        {
            using (var certificate = CreateRsaCertificate())
            {
                var nativeApi = new FakeNativeApi(CertificateKeySpec.Cng)
                {
                    SetPinException = new Win32Exception(5)
                };
                var warmUp = new FakeWarmUp();
                var service = new X509Certificate2A3Service(nativeApi, warmUp);

                Assert.Throws<Win32Exception>(() => service.SetPinPrivateKey(certificate, "1234"));

                Assert.All(nativeApi.PinBuffer, value => Assert.Equal(0, value));
                Assert.True(nativeApi.Released);
                Assert.False(warmUp.Executed);
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void SetPinPrivateKeyDeveValidarEntradas()
        {
            var service = new X509Certificate2A3Service(
                new FakeNativeApi(CertificateKeySpec.Cng),
                new FakeWarmUp());

            Assert.Throws<ArgumentNullException>(() => service.SetPinPrivateKey(null, "1234"));

            using (var certificate = CreateRsaCertificate())
            {
                Assert.Throws<ArgumentNullException>(() => service.SetPinPrivateKey(certificate, null));
                Assert.Throws<ArgumentException>(() => service.SetPinPrivateKey(certificate, string.Empty));
            }

            using (var certificateWithoutPrivateKey = CreateRsaCertificateWithoutPrivateKey())
            {
                Assert.Throws<CryptographicException>(() => service.SetPinPrivateKey(certificateWithoutPrivateKey, "1234"));
            }

            using (var ecdsaCertificate = CreateEcdsaCertificate())
            {
                Assert.Throws<CryptographicException>(() => service.SetPinPrivateKey(ecdsaCertificate, "1234"));
            }
        }

        [Theory]
        [InlineData((int)CertificateKeySpec.Cng, (int)(NativeImplementationType.Hardware | NativeImplementationType.Removable), true)]
        [InlineData((int)CertificateKeySpec.Signature, (int)(NativeImplementationType.Hardware | NativeImplementationType.Removable), true)]
        [InlineData((int)CertificateKeySpec.Cng, (int)NativeImplementationType.Hardware, false)]
        [InlineData((int)CertificateKeySpec.Cng, (int)(NativeImplementationType.Software | NativeImplementationType.Removable), false)]
        [InlineData((int)CertificateKeySpec.Cng, (int)NativeImplementationType.Software, false)]
        [Trait("Utility", "Certificados")]
        public void IsA3DeveExigirImplementacaoDeHardwareERemovivel(
            int keySpecValue,
            int implementationTypeValue,
            bool expected)
        {
            using (var certificate = CreateRsaCertificate())
            {
                var keySpec = (CertificateKeySpec)keySpecValue;
                var implementationType = (NativeImplementationType)implementationTypeValue;
                var nativeApi = new FakeNativeApi(keySpec)
                {
                    ImplementationType = implementationType
                };
                var warmUp = new FakeWarmUp();
                var service = new X509Certificate2A3Service(nativeApi, warmUp);

                Assert.Equal(expected, service.IsA3(certificate));
                Assert.False(nativeApi.CacheRequested);
                Assert.True(nativeApi.Released);
                Assert.False(warmUp.Executed);
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void IsA3DeveRetornarFalseQuandoIdentificacaoFalhar()
        {
            using (var certificate = CreateRsaCertificate())
            {
                var nativeApi = new FakeNativeApi(CertificateKeySpec.Cng)
                {
                    IdentificationSucceeded = false
                };
                var service = new X509Certificate2A3Service(nativeApi, new FakeWarmUp());

                Assert.False(service.IsA3(certificate));

                nativeApi.AcquireException = new Win32Exception(5);
                Assert.False(service.IsA3(certificate));
                Assert.False(service.IsA3(null));
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void IsA3PublicoDeveRejeitarCertificadoRsaEmSoftware()
        {
            using (var certificate = CreateRsaCertificate())
            {
                Assert.False(certificate.IsA3());
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void SetPinPrivateKeyDeveFalharClaramenteForaDoWindows()
        {
            using (var certificate = CreateRsaCertificate())
            {
                var nativeApi = new FakeNativeApi(CertificateKeySpec.Cng)
                {
                    IsWindows = false
                };
                var service = new X509Certificate2A3Service(nativeApi, new FakeWarmUp());

                Assert.Throws<PlatformNotSupportedException>(() => service.SetPinPrivateKey(certificate, "1234"));
                Assert.False(service.IsA3(certificate));
                Assert.False(nativeApi.Acquired);
            }
        }

        [Fact]
        [Trait("Utility", "Certificados")]
        public void FallbackSha1DeveOcorrerSomenteParaAlgoritmoNaoSuportado()
        {
            Assert.True(X509Certificate2A3Service.ShouldFallbackToSha1(new BadAlgorithmCryptographicException()));
            Assert.False(X509Certificate2A3Service.ShouldFallbackToSha1(new CryptographicException("Falha genérica.")));
            Assert.False(X509Certificate2A3Service.ShouldFallbackToSha1(null));
        }

        private static X509Certificate2 CreateRsaCertificate()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest(
                    "CN=Teste A3 RSA",
                    rsa,
                    HashAlgorithmName.SHA256,
                    RSASignaturePadding.Pkcs1);

                return request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private static X509Certificate2 CreateRsaCertificateWithoutPrivateKey()
        {
            using (var certificate = CreateRsaCertificate())
            {
                return new X509Certificate2(certificate.Export(X509ContentType.Cert));
            }
        }

        private static X509Certificate2 CreateEcdsaCertificate()
        {
            using (var ecdsa = ECDsa.Create())
            {
                var request = new CertificateRequest(
                    "CN=Teste A3 ECDSA",
                    ecdsa,
                    HashAlgorithmName.SHA256);

                return request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private sealed class FakeNativeApi : IX509Certificate2NativeApi
        {
            private readonly CertificateKeySpec keySpec;

            internal FakeNativeApi(CertificateKeySpec keySpec)
            {
                this.keySpec = keySpec;
            }

            public bool IsWindows { get; set; } = true;

            internal bool Acquired { get; private set; }

            internal Exception AcquireException { get; set; }

            internal bool CacheRequested { get; private set; }

            internal CryptProviderParameter CspParameter { get; private set; }

            internal bool IdentificationSucceeded { get; set; } = true;

            internal NativeImplementationType ImplementationType { get; set; }

            internal byte[] PinBuffer { get; private set; }

            internal byte[] PinDuringCall { get; private set; }

            internal bool Released { get; private set; }

            internal Exception SetPinException { get; set; }

            public PrivateKeyHandle AcquirePrivateKey(X509Certificate2 certificado, bool cache)
            {
                Acquired = true;
                CacheRequested = cache;

                if (AcquireException != null)
                {
                    throw AcquireException;
                }

                return new PrivateKeyHandle
                {
                    Handle = new IntPtr(123),
                    KeySpec = keySpec,
                    CallerMustFree = !cache
                };
            }

            public void ReleasePrivateKey(PrivateKeyHandle privateKey)
            {
                Released = true;
            }

            public void SetCngPin(IntPtr providerHandle, byte[] pinBuffer)
            {
                CapturePin(pinBuffer);
            }

            public void SetCspPin(IntPtr providerHandle, CryptProviderParameter parameter, byte[] pinBuffer)
            {
                CspParameter = parameter;
                CapturePin(pinBuffer);
            }

            public bool TryGetCngImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType)
            {
                implementationType = ImplementationType;
                return IdentificationSucceeded;
            }

            public bool TryGetCspImplementationType(IntPtr providerHandle, out NativeImplementationType implementationType)
            {
                implementationType = ImplementationType;
                return IdentificationSucceeded;
            }

            private void CapturePin(byte[] pinBuffer)
            {
                PinBuffer = pinBuffer;
                PinDuringCall = pinBuffer.ToArray();

                if (SetPinException != null)
                {
                    throw SetPinException;
                }
            }
        }

        private sealed class FakeWarmUp : IPrivateKeyWarmUp
        {
            internal bool Executed { get; private set; }

            public void WarmUp(X509Certificate2 certificado)
            {
                Executed = true;
            }
        }

        private sealed class BadAlgorithmCryptographicException : CryptographicException
        {
            internal BadAlgorithmCryptographicException()
            {
                HResult = unchecked((int)0x80090008);
            }
        }
    }
}
