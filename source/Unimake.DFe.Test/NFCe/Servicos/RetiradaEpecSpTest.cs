using System;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;
using Xunit;
using NFeRecepcaoEvento = Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento;
using NFCeRecepcaoEvento = Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento;

namespace Unimake.DFe.Test.NFCe.Servicos
{
    [Trait("DFe", "NFCe")]
    public class RetiradaEpecSpTest
    {
        [Theory]
        [InlineData(false, false, false)]
        [InlineData(true, true, false)]
        [InlineData(true, false, true)]
        public void RecusaEpecDaNFCeSpEmQualquerPosicaoDoLote(bool misto, bool cancelamentoPrimeiro, bool tipado)
        {
            var epec = Evento(110140, 65, 35, 35);
            var cancelamento = Evento(110111, 65, 35, 35);
            var xml = Lote(!misto ? epec : cancelamentoPrimeiro ? cancelamento + epec : epec + cancelamento);
            var configuracao = new Configuracao { TipoDFe = TipoDFe.NFCe, Definida = true };
            var erro = tipado
                ? Assert.Throws<ValidarXMLException>(() => new NFeRecepcaoEvento(new EnvEvento().LoadFromXML(xml), configuracao))
                : Assert.Throws<ValidarXMLException>(() => new NFCeRecepcaoEvento(xml, configuracao));

            Assert.Contains("Utilize a contingência offline da NFC-e", erro.Message);
        }

        [Fact]
        public void MantemEpecDaNFeECancelamentoDaNFCe()
        {
            using var rsa = RSA.Create(2048);
            var pedido = new CertificateRequest("CN=EVENTOS TESTE OFFLINE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
            using var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            using (var epec = new NFeRecepcaoEvento(Lote(Evento(110140, 55, 91, 41)),
                new Configuracao { TipoDFe = TipoDFe.NFe, TipoEmissao = (TipoEmissao)4, CertificadoDigital = certificado }))
            using (var cancelamento = new NFCeRecepcaoEvento(Lote(Evento(110111, 65, 35, 35)),
                new Configuracao { TipoDFe = TipoDFe.NFCe, TipoEmissao = TipoEmissao.Normal, CertificadoDigital = certificado }))
            {
                Assert.Equal(91, epec.Configuracoes.CodigoUF);
                Assert.Equal(TipoDFe.NFe, epec.Configuracoes.TipoDFe);
                Assert.Equal(35, cancelamento.Configuracoes.CodigoUF);
                Assert.Equal("homologacao.nfce.fazenda.sp.gov.br", new Uri(cancelamento.Configuracoes.WebEnderecoHomologacao).Host);
            }
        }

        [Fact]
        public void RemoveTipoPublicoERotaDaNFCeSp()
        {
            var assembly = typeof(NFCeRecepcaoEvento).Assembly;
            Assert.Null(assembly.GetType("Unimake.Business.DFe.Servicos.NFCe.RecepcaoEventoEPEC"));
            using (var stream = assembly.GetManifestResourceStream("Unimake.Business.DFe.Servicos.Config.NFCe.SP.xml"))
            using (var reader = new System.IO.StreamReader(stream))
                Assert.DoesNotContain("RecepcaoEventoEPEC", reader.ReadToEnd());
        }

        private static string Lote(string eventos) =>
            "<envEvento xmlns=\"http://www.portalfiscal.inf.br/nfe\" versao=\"1.00\"><idLote>1</idLote>" + eventos + "</envEvento>";

        private static string Evento(int tipo, int modelo, int orgao, int uf)
        {
            var emissao = tipo == 110140 ? 4 : 1;
            var baseChave = $"{uf}261112345678000123{modelo}001000000001{emissao}00000001";
            var total = baseChave.Reverse().Select((digito, indice) => (digito - '0') * (2 + indice % 8)).Sum();
            var dv = 11 - total % 11;
            var chave = baseChave + (dv > 9 ? 0 : dv);
            var detalhe = tipo == 110140
                ? $"<descEvento>EPEC</descEvento><cOrgaoAutor>{uf}</cOrgaoAutor><tpAutor>1</tpAutor><verAplic>TESTE</verAplic>" +
                  "<dhEmi>2026-11-24T12:00:00-03:00</dhEmi><tpNF>1</tpNF><IE>0000000000</IE>" +
                  "<dest><UF>SP</UF><CPF>12345678909</CPF><vNF>10.00</vNF><vICMS>0.00</vICMS><vST>0.00</vST></dest>"
                : "<descEvento>Cancelamento</descEvento><nProt>135260000000001</nProt><xJust>Cancelamento sintetico de teste offline</xJust>";

            return $"<evento versao=\"1.00\"><infEvento Id=\"ID{tipo}{chave}01\"><cOrgao>{orgao}</cOrgao><tpAmb>2</tpAmb>" +
                $"<CNPJ>12345678000123</CNPJ><chNFe>{chave}</chNFe><dhEvento>2026-11-24T12:00:01-03:00</dhEvento>" +
                $"<tpEvento>{tipo}</tpEvento><nSeqEvento>1</nSeqEvento><verEvento>1.00</verEvento>" +
                $"<detEvento versao=\"1.00\">{detalhe}</detEvento></infEvento></evento>";
        }
    }
}
