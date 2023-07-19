using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
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
        /// <returns>Retorna o certificado digital selecionado</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.AbrirTelaSelecao();
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        public X509Certificate2 AbrirTelaSelecao()
        {
            var store = new X509Store("MY", StoreLocation.CurrentUser);
            store.Open(OpenFlags.ReadOnly | OpenFlags.OpenExistingOnly);
            var collection = store.Certificates;
            var collection1 = collection.Find(X509FindType.FindByTimeValid, DateTime.Now, false);
            var collection2 = collection1.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, false);
            var scollection = X509Certificate2UI.SelectFromCollection(collection2, "Certificado(s) digital(is) disponível(is)", "Selecione o certificado digital para uso no aplicativo", X509SelectionFlag.SingleSelection);

            if (scollection.Count == 0)
            {
                return null;
            }

            return scollection[0]; //Apesar de ser uma coleção, a tela de seleção sempre retorna somente 1 certificado, que foi o selecionado pelo usuário, por isso pegamos sempre o index 0
        }

        /// <summary>
        /// Busca o certificado digital pelo Serial Number ou Thumb Print no repositório do windows
        /// </summary>
        /// <param name="serialNumberOrThumbPrint">Serial number ou Thumb print do certificado digital a ser utilizado na localização</param>
        /// <returns>Certificado digital</returns>
        /// <example>
        /// Buscar o certificado digital pelo serialNumber:
        /// <code>
        /// var serialNumber = "1234567890"
        /// var certificado = new CertificadoDigital();        
        /// var certificadoSelecionado = certificado.BuscarCertificadoDigital(serialNumber);
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// Buscar o certificado digital pelo ThumbPrint:
        /// <code>
        /// var thumbPrint = "1234567890154878787978987987987"
        /// var certificado = new CertificadoDigital();        
        /// var certificadoSelecionado = certificado.BuscarCertificadoDigital(thumbPrint);
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        public X509Certificate2 BuscarCertificadoDigital(string serialNumberOrThumbPrint)
        {
            var store = new X509Store("MY", StoreLocation.CurrentUser);
            store.Open(OpenFlags.ReadOnly | OpenFlags.OpenExistingOnly);
            var collection = store.Certificates;
            var collection1 = collection.Find(X509FindType.FindByTimeValid, DateTime.Now, false);
            var collection2 = collection1.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, false);

            //Primeiro tento encontrar pelo thumbprint
            var collection3 = collection2.Find(X509FindType.FindByThumbprint, serialNumberOrThumbPrint, false);
            if (collection3.Count <= 0)
            {
                //Se não encontrou pelo thumbprint tento pelo SerialNumber pegando o mesmo thumbprint que veio no arquivo de configurações para ver se não encontro.
                collection3 = collection2.Find(X509FindType.FindBySerialNumber, serialNumberOrThumbPrint, false);

                if (collection3.Count <= 0)
                {
                    throw new Exception("Certificado digital informado não foi localizado no repositório do windows.");
                }
            }

            return collection3[0];
        }

        /// <summary>
        /// Carrega o certificado digital pelos bytes do certificado
        /// </summary>
        /// <param name="bytes">Bytes do certificado para carga do mesmo</param>
        /// <param name="senha">Senha utilizada para instalar o certificado, será usada para carga do mesmo</param>
        /// <returns>Certificado Digital</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();        
        /// </code>
        /// Criando uma array bytes do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificadoByte = certificado.ToByteArray(@"d:\projetos\UnimakePV.pfx");
        /// </code>
        /// Recuperar o certificado para uso a partir de uma array byte
        /// <code>
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(certificadoByte, "12345678");
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [ComVisible(false)] // *** ATENÇÃO ***
        public X509Certificate2 CarregarCertificadoDigitalA1(byte[] bytes, string senha) => new X509Certificate2(bytes, senha);

        /// <summary>
        /// Carrega o certificado digital pelos bytes do certificado
        /// </summary>
        /// <param name="bytes">Bytes do certificado para carga do mesmo</param>
        /// <param name="senha">Senha utilizada para instalar o certificado, será usada para carga do mesmo</param>
        /// <param name="keyStorageFlags">Define onde e como importar a chave privada de um certificado X.509. (Uma combinação bit a bit dos valores de enumeração que controlam onde e como importar o certificado.)</param>
        /// <returns>Certificado Digital</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();        
        /// </code>
        /// Criando uma array bytes do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificadoByte = certificado.ToByteArray(@"d:\projetos\UnimakePV.pfx");
        /// </code>
        /// Recuperar o certificado para uso a partir de uma array byte com definição de onde e como importar a chave privada, muitas vezes necessário definir em aplicações web.
        /// <code>
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(certificadoByte, "12345678", X509KeyStorageFlags.MachineKeySet);
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [ComVisible(false)] // *** ATENÇÃO ***
        public X509Certificate2 CarregarCertificadoDigitalA1(byte[] bytes, string senha, X509KeyStorageFlags keyStorageFlags) => new X509Certificate2(bytes, senha, keyStorageFlags);

        /// <summary>
        /// Carrega o certificado digital A1 direto do arquivo .PFX
        /// </summary>
        /// <param name="caminho">Caminho do certificado digital. Ex. c:\certificados\certificado.pfx</param>
        /// <param name="senha">Senha utilizada para instalar o arquivo .pfx</param>
        /// <returns>Certificado Digital</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 CarregarCertificadoDigitalA1(string caminho, string senha)
        {
            if (string.IsNullOrWhiteSpace(caminho))
            {
                throw new CarregarCertificadoException("O caminho do arquivo é requerido");
            }

            var fi = new FileInfo(caminho);

            if (!fi.Exists)
            {
                throw new CarregarCertificadoException($"O arquivo '{caminho}' não pode ser acessado ou não existe.");
            }

            if (string.IsNullOrWhiteSpace(senha))
            {
                throw new CarregarCertificadoException("A senha é requerida");
            }

            var x509Cert = new X509Certificate2();

            try
            {
                using (var fs = fi.OpenRead())
                {
                    var buffer = new byte[fs.Length];
                    fs.Read(buffer, 0, buffer.Length);
                    x509Cert = new X509Certificate2(buffer, senha);
                }
            }
            catch (CryptographicException)
            {
                throw new CertificadoDigitalException("Senha do certificado digital está incorreta.", ErrorCodes.SenhaCertificadoIncorreta);
            }
            catch (Exception)
            {
                throw;
            }

            return x509Cert;
        }

        /// <summary>
        /// Carrega o certificado digital A1 direto do arquivo .PFX
        /// </summary>
        /// <param name="caminho">Caminho do certificado digital. Ex. c:\certificados\certificado.pfx</param>
        /// <param name="senha">Senha utilizada para instalar o arquivo .pfx</param>
        /// <param name="keyStorageFlags">Define onde e como importar a chave privada de um certificado X.509. (Uma combinação bit a bit dos valores de enumeração que controlam onde e como importar o certificado.)</param>
        /// <returns>Certificado Digital</returns>
        /// Recupera o certificado digital direto do .PFX com definição de onde e como importar a chave privada, muitas vezes necessário definir em aplicações web.
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678", X509KeyStorageFlags.MachineKeySet);
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 CarregarCertificadoDigitalA1(string caminho, string senha, X509KeyStorageFlags keyStorageFlags)
        {
            if (string.IsNullOrWhiteSpace(caminho))
            {
                throw new CarregarCertificadoException("O caminho do arquivo é requerido");
            }

            var fi = new FileInfo(caminho);

            if (!fi.Exists)
            {
                throw new CarregarCertificadoException($"O arquivo '{caminho}' não pode ser acessado ou não existe.");
            }

            if (string.IsNullOrWhiteSpace(senha))
            {
                throw new CarregarCertificadoException("A senha é requerida");
            }

            var x509Cert = new X509Certificate2();

            try
            {
                using (var fs = fi.OpenRead())
                {
                    var buffer = new byte[fs.Length];
                    fs.Read(buffer, 0, buffer.Length);
                    x509Cert = new X509Certificate2(buffer, senha, keyStorageFlags);
                }
            }
            catch (CryptographicException)
            {
                throw new CertificadoDigitalException("Senha do certificado digital está incorreta.", ErrorCodes.SenhaCertificadoIncorreta);
            }
            catch (Exception)
            {
                throw;
            }

            return x509Cert;
        }

        /// <summary>
        /// Converte a string Base64 no certificado
        /// </summary>
        /// <param name="base64">String base64 convertida pelo método <see cref="ToBase64(string)"/></param>
        /// <param name="password">Senha do certificado</param>
        /// <returns>Certificado digital</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// </code>
        /// Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificadoBase64 = certificado.ToBase64(@"d:\projetos\UnimakePV.pfx");
        /// </code>   
        /// Recuperar o certificado para uso a partir de um Base64
        /// <code>
        /// var certificadoSelecionado = certificado.FromBase64(certificadoBase64, "12345678");
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 FromBase64(string base64, string password)
        {
            var buffer = Convert.FromBase64String(base64);
            return new X509Certificate2(buffer, password);
        }

        /// <summary>
        /// Converte a string Base64 no certificado
        /// </summary>
        /// <param name="base64">String base64 convertida pelo método <see cref="ToBase64(string)"/></param>
        /// <param name="password">Senha do certificado</param>
        /// <param name="keyStorageFlags">Define onde e como importar a chave privada de um certificado X.509. (Uma combinação bit a bit dos valores de enumeração que controlam onde e como importar o certificado.)</param>
        /// <returns>Certificado digital</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// </code>
        /// Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificadoBase64 = certificado.ToBase64(@"d:\projetos\UnimakePV.pfx");
        /// </code>   
        /// Recupera o certificado digital de uma string Base64 com definição de onde e como importar a chave privada, muitas vezes necessário definir em aplicações web.
        /// <code>
        /// var certificadoSelecionado = certificado.FromBase64(certificadoBase64, "12345678", X509KeyStorageFlags.MachineKeySet);
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 FromBase64(string base64, string password, X509KeyStorageFlags keyStorageFlags)
        {
            var buffer = Convert.FromBase64String(base64);
            return new X509Certificate2(buffer, password, keyStorageFlags);
        }

        /// <summary>
        /// Abre a tela de dialogo do windows para seleção do certificado digital
        /// </summary>
        /// <returns>Retorna o certificado digital selecionado</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.AbrirTelaSelecao();
        /// MessageBox.Show(certificadoSelecionado.Subject);
        /// </code>
        /// </example>
        [return: MarshalAs(UnmanagedType.IDispatch)]
        public X509Certificate2 Selecionar() => AbrirTelaSelecao();

        /// <summary>
        /// Converte o arquivo .PFX do certificado em base64
        /// </summary>
        /// <param name="arquivo">Nome do arquivo</param>
        /// <returns>Base64 do certificado digital informado</returns>
        /// <example>
        /// Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var base64DoCertificado = certificado.ToBase64(@"d:\projetos\UnimakePV.pfx");
        /// </code>
        /// Grave o conteúdo do variável "base64DoCertificado" para gravar em sua base de dados
        /// </example>
        public string ToBase64(string arquivo)
        {
            byte[] result = null;

            using (Stream responseStream = new FileStream(arquivo, FileMode.Open))
            {
                using (var memoryStream = new MemoryStream())
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
        /// <example>
        /// Criando uma array bytes do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var arrayByteCertificado = certificado.ToByteArray(@"d:\projetos\UnimakePV.pfx");;
        /// </code>
        /// Grave o conteúdo do variável "arrayByteCertificado" para gravar em sua base de dados
        /// </example>
        public byte[] ToByteArray(string arquivo)
        {
            byte[] result = null;

            using (Stream responseStream = new FileStream(arquivo, FileMode.Open))
            {
                using (var memoryStream = new MemoryStream())
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
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// if (certificado.Vencido(certificadoSelecionado))
        /// {
        ///    MessageBox.Show("Certificado digital vencido!!!");
        /// }
        /// </code>
        /// </example>
#if INTEROP
        public bool Vencido([MarshalAs(UnmanagedType.IDispatch)] X509Certificate2 certificado)
#else

        public bool Vencido(X509Certificate2 certificado)
#endif
        {
            var retorna = false;

            if (certificado == null)
            {
                throw new CertificadoDigitalException();
            }

            if (DateTime.Compare(DateTime.Now, certificado.NotAfter) > 0)
            {
                retorna = true;
            }

            return retorna;
        }

        /// <summary>
        /// Retorna o Thumbprint (impressão digital, ID) do certificado digital
        /// </summary>
        /// <param name="certificado">Certificado que é para pegar a informação</param>
        /// <returns>Retorna o Thumbprint</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// var thumbPrint = certificado.GetThumbprint(certificadoSelecionado)
        /// </code>
        /// </example>
        public string GetThumbprint(X509Certificate2 certificado) => certificado.Thumbprint;

        /// <summary>
        /// Retorna Subject (dados do proprietário) do certificado digital
        /// </summary>
        /// <param name="certificado">Certificado que é para pegar a informação</param>
        /// <returns>Retorna o Subject</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// var subject = certificado.GetSubject(certificadoSelecionado)
        /// </code>
        /// </example>
        public string GetSubject(X509Certificate2 certificado) => certificado.Subject;

        /// <summary>
        /// Retorna o SerialNumber (Número de série, ID) do certificado digital
        /// </summary>
        /// <param name="certificado">Certificado que é para pegar a informação</param>
        /// <returns>Retorna o SerialNumber</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// var serialNumber = certificado.GetSerialNumber(certificadoSelecionado)
        /// </code>
        /// </example>
        public string GetSerialNumber(X509Certificate2 certificado) => certificado.SerialNumber;

        /// <summary>
        /// Retorna o Not After (Data de vencimento final do certificado digital) do certificado digital
        /// </summary>
        /// <param name="certificado">Certificado que é para pegar a informação</param>
        /// <returns>Retorna o Not AfterThumbprint</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// var validadeInicial = certificado.GetNotAfter(certificadoSelecionado)
        /// </code>
        /// </example>
        public string GetNotAfter(X509Certificate2 certificado) => certificado.NotAfter.ToString("dd/MM/yyyy HH:mm:ss");

        /// <summary>
        /// Retorna o Not Before (Data de vencimento inicial do certificado digital) do certificado digital
        /// </summary>
        /// <param name="certificado">Certificado que é para pegar a informação</param>
        /// <returns>Retorna o NotBefore</returns>
        /// <example>
        /// <code>
        /// var certificado = new CertificadoDigital();
        /// var certificadoSelecionado = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");
        /// var validadeFinal = certificado.GetNotBefore(certificadoSelecionado)
        /// </code>
        /// </example>
        public string GetNotBefore(X509Certificate2 certificado) => certificado.NotBefore.ToString("dd/MM/yyyy HH:mm:ss");

        #endregion Public Methods
    }

    /// <summary>
    /// Trabalhar com certificado digital - Interop
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Security.Platform.CertificadoDigitalInterop")]
    [ComVisible(true)]
    public class CertificadoDigitalInterop
    {
        private readonly CertificadoDigital Certificado = new CertificadoDigital();

        private X509Certificate2 CertificadoSelecionado { get; set; }

        /// <summary>
        /// Trabalhar com certificado digital
        /// </summary>
        public CertificadoDigitalInterop() { }

        /// <summary>
        /// Abre a tela para selecionar o certificado digital
        /// </summary>
        public void AbrirTelaSelecao() => CertificadoSelecionado = Certificado.AbrirTelaSelecao();

        /// <summary>
        /// Converter o arquivo do certificado A1 (.PFX) para string Base64
        /// </summary>
        /// <param name="arquivo">Caminho do arquivo do certificado A1 (.PFX)</param>
        /// <returns>Retorna o BASE64 do arquivo do certificado digital</returns>
        public string ToBase64(string arquivo) => Certificado.ToBase64(arquivo);

        /// <summary>
        /// Carrega o certificado digital A1 partindo o arquivo .PFX
        /// </summary>
        /// <param name="caminho">Caminho do arquivo .PFX do certificado A1</param>
        /// <param name="senha">Senha de instalação/uso do certificado</param>
        public void CarregarCertificadoDigitalA1(string caminho, string senha) => CertificadoSelecionado = Certificado.CarregarCertificadoDigitalA1(caminho, senha);

        /// <summary>
        /// Converte a string Base64 no certificado
        /// </summary>
        /// <param name="base64">String base64 convertida pelo método <see cref="ToBase64(string)"/></param>
        /// <param name="password">Senha do certificado</param>
        public void FromBase64(string base64, string password) => CertificadoSelecionado = Certificado.FromBase64(base64, password);

        /// <summary>
        /// Verifica se o certificado digital selecionado está vencido
        /// </summary>
        /// <returns>true = vencido</returns>
        public bool Vencido() => Certificado.Vencido(CertificadoSelecionado);

        /// <summary>
        /// Retorna o thumbprint do certificado digital selecionado
        /// </summary>
        /// <returns>Thumbprint do certificado digital selecionado</returns>
        public string GetThumbprint() => CertificadoSelecionado.Thumbprint;

        /// <summary>
        /// Retorna o subject do certificado digital selecionado
        /// </summary>
        /// <returns>Subject do certificado digital selecionado</returns>
        public string GetSubject() => CertificadoSelecionado.Subject;

        /// <summary>
        /// Retorna o SerialNumber do certificado digital selecionado
        /// </summary>
        /// <returns>SerialNumber do certificado digital selecionado</returns>
        public string GetSerialNumber() => CertificadoSelecionado.SerialNumber;

        /// <summary>
        /// Retorna o Not After (Data de vencimento final do certificado digital) do certificado digital
        /// </summary>
        /// <returns>Retorna o Not After</returns>
        public string GetNotAfter() => Certificado.GetNotAfter(CertificadoSelecionado);

        /// <summary>
        /// Retorna o Not Before (Data de vencimento inicial do certificado digital) do certificado digital
        /// </summary>
        /// <returns>Retorna o NotBefore</returns>
        public string GetNotBefore() => Certificado.GetNotBefore(CertificadoSelecionado);
    }
}