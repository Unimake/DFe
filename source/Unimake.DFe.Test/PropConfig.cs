using System;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.Security;
using Unimake.Exceptions;

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

        /// <summary>AppId do uMessenger</summary>
        public static string UMessengerAppId { get; set; } = "Seu_AppId";

        /// <summary>Secret do uMessenger</summary>
        public static string UMessengerSecret { get; set; } = "Seu_Secret";

        /// <summary>Nome da instância do uMessenger</summary>
        public static string UMessengerInstanceName { get; set; } = "Seu_InstanceName";

        /// <summary>Número de destino para testes de envio via uMessenger (formato: 55DDNNNNNNNNN)</summary>
        public static string UMessengerDestinoTeste { get; set; } = "55DDNNNNNNNNN";

        /// <summary>AppId do eBoleto</summary>
        public static string EBoletoAppId { get; set; } = "Seu_AppId";

        /// <summary>Secret do eBoleto</summary>
        public static string EBoletoSecret { get; set; } = "Seu_Secret";

        /// <summary>Identificador da configuração de integração do eBoleto</summary>
        public static string EBoletoConfigurationId { get; set; } = "Seu_ConfigurationId";

        /// <summary>Número no banco para testes de consulta, cancelamento, alteração de vencimento e instrução</summary>
        public static string EBoletoNumeroNoBancoTeste { get; set; } = "Seu_NumeroNoBanco";

        /// <summary>CPF/CNPJ do pagador usado nos testes do eBoleto</summary>
        public static string EBoletoPagadorInscricao { get; set; } = "99999999999";

        /// <summary>Nome do pagador usado nos testes do eBoleto</summary>
        public static string EBoletoPagadorNome { get; set; } = "Pagador Teste";
    }
}
