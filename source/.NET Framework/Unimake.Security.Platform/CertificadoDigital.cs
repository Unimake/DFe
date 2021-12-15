using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Cryptography.X509Certificates;
using Unimake.Security.Platform.Exceptions;

namespace Unimake.Security.Platform
{
    /// <summary>
    /// Trabalhar com certificado digital
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Security.Platform.CertificadoDigital")]
    [ComVisible(true)]
    public class CertificadoDigital
    {
        #region Public Constructors

        /// <summary>
        /// Trabalhar com certificado digital
        /// </summary>
        public CertificadoDigital()
        {
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Abre a tela de dialogo do windows para seleção do certificado digital
        /// </summary>
        /// <returns>Retorna a coleção de certificados digitais</returns>
        public X509Certificate2Collection AbrirTelaSelecao()
        {
            var store = new X509Store("MY", StoreLocation.CurrentUser);
            store.Open(OpenFlags.ReadOnly | OpenFlags.OpenExistingOnly);
            var collection = store.Certificates;
            _ = collection.Find(X509FindType.FindByTimeValid, DateTime.Now, false);
            var collection2 = collection.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, false);
            var scollection = X509Certificate2UI.SelectFromCollection(collection2, "Certificado(s) digital(is) disponível(is)", "Selecione o certificado digital para uso no aplicativo", X509SelectionFlag.SingleSelection);

            return scollection;
        }

        /// <summary>
        /// Busca o certificado digital pelo Serial Number ou Thumb Print no repositório do windows
        /// </summary>
        /// <param name="serialNumberOrThumbPrint">Serial number ou Thumb print do certificado digital a ser utilizado na localização</param>
        /// <returns>Certificado digital</returns>
        public X509Certificate2 BuscarCertificadoDigital(string serialNumberOrThumbPrint)
        {
            var x509Cert = new X509Certificate2();
            var store = new X509Store("MY", StoreLocation.CurrentUser);
            store.Open(OpenFlags.ReadOnly | OpenFlags.OpenExistingOnly);
            var collection = store.Certificates;
            var collection1 = collection.Find(X509FindType.FindByTimeValid, DateTime.Now, false);
            var collection2 = collection1.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, false);

            //Primeiro tento encontrar pelo thumbprint
            var collection3 = collection2.Find(X509FindType.FindByThumbprint, serialNumberOrThumbPrint, false);
            if(collection3.Count <= 0)
            {
                //Se não encontrou pelo thumbprint tento pelo SerialNumber pegando o mesmo thumbprint que veio no arquivo de configurações para ver se não encontro.
                collection3 = collection2.Find(X509FindType.FindBySerialNumber, serialNumberOrThumbPrint, false);

                if(collection3.Count <= 0)
                {
                    throw new Exception("Certificado digital informado não foi localizado no repositório do windows.");
                }
            }

            x509Cert = collection3[0];

            return x509Cert;
        }

        /// <summary>
        /// Carrega o certificado digital pelos bytes do certificado
        /// </summary>
        /// <param name="bytes">Bytes do certificado para carga do mesmo</param>
        /// <param name="senha">Senha utilizada para instalar o certificado, será usada para carga do mesmo</param>
        /// <returns>Certificado Digital</returns>
        [ComVisible(false)] // *** ATENÇÃO ***
        public X509Certificate2 CarregarCertificadoDigitalA1(byte[] bytes, string senha) => new X509Certificate2(bytes, senha);

        /// <summary>
        /// Carrega o certificado digital A1 direto do arquivo .PFX
        /// </summary>
        /// <param name="caminho">Caminho do certificado digital. Ex. c:\certificados\certificado.pfx</param>
        /// <param name="senha">Senha utilizada para instalar o arquivo .pfx</param>
        /// <returns>Certificado Digital</returns>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 CarregarCertificadoDigitalA1(string caminho, string senha)
        {
            if(string.IsNullOrWhiteSpace(caminho))
            {
                throw new CarregarCertificadoException("O caminho do arquivo é requerido");
            }

            var fi = new FileInfo(caminho);

            if(!fi.Exists)
            {
                throw new CarregarCertificadoException($"O arquivo '{caminho}' não pode ser acessado ou não existe.");
            }

            if(string.IsNullOrWhiteSpace(senha))
            {
                throw new CarregarCertificadoException("A senha é requerida");
            }

            var x509Cert = new X509Certificate2();

            using(var fs = fi.OpenRead())
            {
                var buffer = new byte[fs.Length];
                fs.Read(buffer, 0, buffer.Length);
                x509Cert = new X509Certificate2(buffer, senha);
            }

            return x509Cert;
        }

        /// <summary>
        /// Converte a string Base64 no certificado
        /// </summary>
        /// <param name="base64">String base64 convertida pelo método <see cref="ToBase64(string)"/></param>
        /// <param name="password">Senha do certificado</param>
        /// <returns>Base64</returns>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 FromBase64(string base64, string password)
        {
            var buffer = Convert.FromBase64String(base64);
            return new X509Certificate2(buffer, password);
        }

        /// <summary>
        /// Executa tela com os certificados digitais instalados para seleção do usuário
        /// </summary>
        /// <returns>Retorna o certificado digital (null se nenhum certificado foi selecionado ou se o certificado selecionado está com alguma falha)</returns>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 Selecionar()
        {
            var scollection = AbrirTelaSelecao();

            if(scollection.Count > 0)
            {
                return scollection[0];
            }

            return null;
        }

        /// <summary>
        /// Converte o arquivo do certificado em base664 e retorna
        /// </summary>
        /// <param name="arquivo">Nome do arquivo</param>
        /// <returns>Base64</returns>
        public string ToBase64(string arquivo)
        {
            byte[] result = null;

            using(Stream responseStream = new FileStream(arquivo, FileMode.Open))
            {
                using(var memoryStream = new MemoryStream())
                {
                    responseStream.CopyTo(memoryStream);
                    result = memoryStream.ToArray();
                }
            }

            return Convert.ToBase64String(result);
        }

        /// <summary>
        /// Converte o arquivo do certificado em um array de bytes
        /// </summary>
        /// <param name="arquivo">Nome do arquivo</param>
        /// <returns>Array de bytes do arquivo do certificado</returns>
        public byte[] ToByteArray(string arquivo)
        {
            byte[] result = null;

            using(Stream responseStream = new FileStream(arquivo, FileMode.Open))
            {
                using(var memoryStream = new MemoryStream())
                {
                    responseStream.CopyTo(memoryStream);
                    result = memoryStream.ToArray();
                }
            }

            return result;
        }

        /// <summary>
        /// Verifica se o certificado digital está vencido
        /// </summary>
        /// <param name="certificado">Certificado digital</param>
        /// <returns>true = Certificado vencido</returns>
#if INTEROP
        public bool Vencido([MarshalAs(UnmanagedType.IDispatch)] X509Certificate2 certificado)
#else

        public bool Vencido(X509Certificate2 certificado)
#endif
        {
            var retorna = false;

            if(certificado == null)
            {
                throw new CertificadoDigitalException();
            }

            if(DateTime.Compare(DateTime.Now, certificado.NotAfter) > 0)
            {
                retorna = true;
            }

            return retorna;
        }

        #endregion Public Methods
    }
}