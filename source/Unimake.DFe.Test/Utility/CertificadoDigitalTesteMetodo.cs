using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;


namespace Unimake.DFe.Test.Utility
{

    public class CertificadoDigitalTesteMetodo
    {

        // cache estático, valor guardado X509Certificate2 carregado 
        // 'readonly' evitar que fique recriando o certificado 
        private static readonly Dictionary<string, X509Certificate2> _cacheCertificados = new();

        public  X509Certificate2 CarregarCertificadoDigitalA1(string caminho, string senha) 
        {

            if (string.IsNullOrWhiteSpace(caminho))
            {
                throw new Exception("O caminho do arquivo é requerido");      
            }

            var fi = new FileInfo(caminho);

            if (!fi.Exists)
            {
                throw new Exception($"O arquivo '{caminho}' não pode ser acessado ou não existe");
                
            }

            var chave = $"{fi.FullName}_{senha}"; // chave unica 


            if (_cacheCertificados.TryGetValue(chave, out var certificado))
            {
                return certificado;
            
            
            }

            try
            {
                using (var fs = fi.OpenRead())
                {
                    var buffer = new byte[fs.Length];
                    fs.Read(buffer, 0, buffer.Length);
                    certificado = new X509Certificate2(buffer, senha);
                }
                _cacheCertificados[chave] = certificado;
            }

            catch (CryptographicException)
            {
                throw new Exception("Senha do certificado digital incorreta.");
            }

            return certificado;

        }
  
    
    
    }














}