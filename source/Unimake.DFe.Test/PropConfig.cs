using System;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;
using Unimake.Security.Platform;

namespace Unimake.DFe.Test
{
    /// <summary>
    /// Propriedades com configurações diversas para serem utilizados nos testes
    /// </summary>
    public static class PropConfig
    {
        /// <summary>
        /// Caminho do certificado digital A1
        /// </summary>
        private static string PathCertificadoDigital { get; set; } = @"C:\Projetos\Unimake_PV.pfx";

        /// <summary>
        /// Senha de uso do certificado digital A1
        /// </summary>
        private static string SenhaCertificadoDigital { get; set; } = "12345678";

        private static X509Certificate2 CertificadoDigitalField;

        /// <summary>
        /// Certificado digital
        /// </summary>
        public static X509Certificate2 CertificadoDigital
        {
            get
            {
                if (CertificadoDigitalField == null)
                {
                    CertificadoDigitalField = new CertificadoDigital().CarregarCertificadoDigitalA1(PathCertificadoDigital, SenhaCertificadoDigital);
                }

                return CertificadoDigitalField;
            }

            private set => ThrowHelper.Instance.Throw(new Exception("Não é possível atribuir um certificado digital! Somente resgate o valor da propriedade que o certificado é definido automaticamente."));
        }

        public static string CNPJEmpresaCertificado { get; set; } = "06117473000150";
        public static UFBrasil UFEmpresaCertificado { get; set; } = UFBrasil.PR;
    }
}
