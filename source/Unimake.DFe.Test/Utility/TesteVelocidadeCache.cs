using System;
using System.Diagnostics;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.Security;
using Unimake.DFe.Test.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility
{
    public class TesteVelocidadeCache
    {
        [Fact]
        public void TesteCertificadoVelocidade()
        {
            var caminho = @"C:\Projetos\Unimake_PV.pfx";
            var senha = "12345678";

            // JEITO LENTO - carrega certificado toda vez
            var sw1 = Stopwatch.StartNew();
            for (int i = 0; i < 50; i++)
            {
                var cert = new CertificadoDigital().CarregarCertificadoDigitalA1(caminho, senha);
            }
            sw1.Stop();
            Console.WriteLine($"Tempo do método original: {sw1.ElapsedMilliseconds} ms");

            // JEITO RÁPIDO - usa cache
            var helper = new CertificadoDigitalHelper(caminho, senha);

            // Pré-carregar certificado para cache antes do loop
            helper.ObterCertificado();

            var sw2 = Stopwatch.StartNew();
            for (int i = 0; i < 50; i++)
            {
                var cert = helper.ObterCertificado();
            }
            sw2.Stop();
            Console.WriteLine($"Tempo com cache: {sw2.ElapsedMilliseconds} ms");
        }


        public class CertificadoDigitalHelper
        {
            private readonly string _caminho;
            private readonly string _senha;
            private X509Certificate2 _certificadoCache;

            public CertificadoDigitalHelper(string caminho, string senha)
            {
                _caminho = caminho;
                _senha = senha;
            }

            public X509Certificate2 ObterCertificado()
            {
                if (_certificadoCache == null)
                {
                    _certificadoCache = new CertificadoDigital().CarregarCertificadoDigitalA1(_caminho, _senha);
                }
                return _certificadoCache;
            }
        }
    }
}
