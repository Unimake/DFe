using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFe;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Servicos
{
    /// <summary>
    /// Testes do QR Code da NF-e com DANFE Simplificado Tipo 2.
    /// </summary>
    public class DanfeSimplificadoTipo2QrCodeTest
    {
        /// <summary>
        /// Gerar QR Code versão 3 para a NF-e em emissão normal e em contingência offline.
        /// </summary>
        /// <param name="tipoEmissao">Tipo de emissão da NF-e.</param>
        /// <param name="quantidadeParametros">Quantidade de parâmetros esperados na URL após p=.</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(TipoEmissao.Normal, 3)]
        [InlineData(TipoEmissao.ContingenciaOffLine, 8)]
        public void GerarQrCodeParaDanfeSimplificadoTipo2(TipoEmissao tipoEmissao, int quantidadeParametros)
        {
            var enviNFe = CriarEnviNFe(tipoEmissao, FormatoImpressaoDANFE.SimplificadoTipo2);
            var configuracao = CriarConfiguracao(tipoEmissao);

            using (var autorizacao = new Autorizacao(enviNFe, configuracao))
            {
                var infNFeSupl = autorizacao.ConteudoXMLAssinado.GetElementsByTagName("infNFeSupl");

                Assert.Single(infNFeSupl);
                Assert.Equal(configuracao.UrlChaveHomologacao, infNFeSupl[0]["urlChave"].InnerText);
                var qrCode = infNFeSupl[0]["qrCode"].InnerText;
                Assert.Equal(quantidadeParametros, qrCode.Substring(qrCode.IndexOf("?p=") + 3).Split('|').Length);
                Assert.Contains("|3|2", qrCode);
            }
        }

        /// <summary>
        /// Não gerar QR Code para a NF-e que não utiliza o DANFE Simplificado Tipo 2.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFe")]
        public void NaoGerarQrCodeParaDanfeNormal()
        {
            var enviNFe = CriarEnviNFe(TipoEmissao.Normal, FormatoImpressaoDANFE.NormalRetrato);

            using (var autorizacao = new Autorizacao(enviNFe, CriarConfiguracao(TipoEmissao.Normal)))
            {
                Assert.Empty(autorizacao.ConteudoXMLAssinado.GetElementsByTagName("infNFeSupl"));
            }
        }

        private static Configuracao CriarConfiguracao(TipoEmissao tipoEmissao)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = tipoEmissao,
                TipoAmbiente = TipoAmbiente.Homologacao,
                UrlChaveHomologacao = "https://www.exemplo.gov.br/nfe/consulta",
                UrlQrCodeHomologacao = "https://www.exemplo.gov.br/nfe/qrcode",
                CertificadoDigital = CriarCertificadoDigital()
            };
        }

        private static X509Certificate2 CriarCertificadoDigital()
        {
            using (var rsa = RSA.Create(2048))
            {
                var requisicao = new CertificateRequest("CN=Unimake DFe Teste", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return requisicao.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private static EnviNFe CriarEnviNFe(TipoEmissao tipoEmissao, FormatoImpressaoDANFE formatoImpressao)
        {
            var documento = new XmlDocument();
            documento.Load(@"..\..\..\NFe\Resources\NFe1.xml");

            var nfe = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(documento);
            nfe.InfNFeField.Ide.TpImp = formatoImpressao;
            nfe.InfNFeField.Ide.TpEmis = tipoEmissao;
            nfe.InfNFeField.Ide.TpAmb = TipoAmbiente.Homologacao;

            return new EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<Business.DFe.Xml.NFe.NFe> { nfe }
            };
        }
    }
}
