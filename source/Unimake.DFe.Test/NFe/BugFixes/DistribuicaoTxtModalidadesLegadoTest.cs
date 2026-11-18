using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Xml.NFe.Txt;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.NFe.BugFixes
{
    [Trait("DFe", "NFe")]
    public class DistribuicaoTxtModalidadesLegadoTest
    {
        private const string Chave = "41260899999999000191550010000000011000000010";
        private static TxtConversaoContexto Contexto => new TxtConversaoContexto
        {
            TipoAmbiente = TipoAmbiente.Homologacao, CodigoUF = UFBrasil.PR, Modelo = ModeloDFe.NFe
        };

        public static IEnumerable<object[]> Combinacoes()
        {
            foreach (var campos in new[] { "ultNSU", "NSU", "chNFe", "ultNSU,NSU", "ultNSU,chNFe", "NSU,chNFe", "ultNSU,NSU,chNFe" })
                foreach (var inverter in new[] { false, true })
                    yield return new object[] { campos, inverter };
        }

        [Theory]
        [MemberData(nameof(Combinacoes))]
        public void NFePreservaTodosOsGruposEDeixaSchemaRecusarConflitos(string campos, bool inverter)
        {
            var documento = Converter(campos, inverter);
            var legado = new DistDFeInt
            {
                Versao = "1.01", TpAmb = TipoAmbiente.Homologacao, CUFAutor = UFBrasil.PR, CNPJ = "99999999000191",
                ConsChNFe = campos.Split(',').Contains("chNFe") ? new ConsChNFe { ChNFe = Chave } : null,
                ConsNSU = campos.Split(',').Contains("NSU") ? new ConsNSU { NSU = "000000000000101" } : null,
                DistNSU = campos.Split(',').Contains("ultNSU") ? new DistNSU { UltNSU = "000000000000100" } : null
            }.GerarXML();
            // São as atribuições independentes do leitor TaskRecepcaoDFe do legado.
            Assert.Equal(Grupos(legado).Select(x => x.OuterXml), Grupos(documento).Select(x => x.OuterXml));
            Assert.Equal(campos.Split(',').Length, Grupos(documento).Length);
            Assert.Equal("http://www.portalfiscal.inf.br/nfe", documento.DocumentElement.NamespaceURI);
            Assert.Equal("1.01", documento.DocumentElement.GetAttribute("versao"));
            Assert.Equal("99999999000191", documento.GetElementsByTagName("CNPJ")[0].InnerText);
            var schema = new ValidarSchema();
            schema.Validar(documento, "NFe.distDFeInt_v1.01.xsd");
            Assert.Equal(campos.Split(',').Length == 1, schema.Success);
        }

        [Theory]
        [InlineData("ultNSU", "distNSU", "000000000000100")]
        [InlineData("NSU", "consNSU", "000000000000101")]
        [InlineData("chNFe", "distNSU", "000000000000000")]
        [InlineData("ultNSU,NSU", "consNSU", "000000000000101")]
        [InlineData("ultNSU,chNFe", "distNSU", "000000000000100")]
        [InlineData("NSU,chNFe", "consNSU", "000000000000101")]
        [InlineData("ultNSU,NSU,chNFe", "consNSU", "000000000000101")]
        public void CTeMantemSelecaoAnterior(string campos, string grupo, string valor)
        {
            var documento = Converter(campos, false, cte: true);
            var unico = Assert.Single(Grupos(documento));
            Assert.Equal(grupo, unico.LocalName);
            Assert.Equal(valor, unico.InnerText);
            Assert.Equal("http://www.portalfiscal.inf.br/cte", documento.DocumentElement.NamespaceURI);
        }

        [Fact]
        public void AusenciaDeModalidadeMantemDefaultAtual()
        {
            var grupo = Assert.Single(Grupos(Converter("", false)));
            Assert.Equal("distNSU", grupo.LocalName);
            Assert.Equal("000000000000000", grupo.InnerText);
        }

        [Theory]
        [InlineData("ultNSU")]
        [InlineData("NSU")]
        [InlineData("chNFe")]
        [InlineData("ultNSU,NSU")]
        [InlineData("NSU,chNFe")]
        public void CampoExplicitoVazioPermaneceNoGrupoParaValidacao(string campos)
        {
            var documento = Converter(campos, false, vazios: true);
            Assert.Equal(campos.Split(',').Length, Grupos(documento).Length);
            Assert.All(Grupos(documento), grupo => Assert.Equal("", grupo.InnerText));
            var schema = new ValidarSchema();
            schema.Validar(documento, "NFe.distDFeInt_v1.01.xsd");
            Assert.False(schema.Success);
        }

        [Theory]
        [InlineData("ultNSU,NSU")]
        [InlineData("ultNSU,chNFe")]
        [InlineData("NSU,chNFe")]
        [InlineData("ultNSU,NSU,chNFe")]
        public void ValidacaoPublicaRecusaGruposSimultaneosSemRemoverNenhum(string campos)
        {
            var documento = Converter(campos, true);
            var antes = documento.OuterXml;
            using (var certificado = CriarCertificado())
            {
                var resultado = new ValidarEstruturaXML().ValidarServico(documento, Configurar(certificado));
                Assert.False(resultado.Validado);
                Assert.NotEqual("1", resultado.StatusValidacao);
                Assert.Equal(antes, documento.OuterXml);
            }
        }

        [Theory]
        [InlineData("ultNSU,NSU")]
        [InlineData("ultNSU,chNFe")]
        [InlineData("NSU,chNFe")]
        [InlineData("ultNSU,NSU,chNFe")]
        public void ConstrucaoDoServicoRecusaSchemaAntesDeQualquerExecutar(string campos)
        {
            var documento = Converter(campos, false);
            using (var certificado = CriarCertificado())
            {
                // Exercita apenas a construção/validação existente, nunca Executar.
                Assert.Throws<ValidarXMLException>(() => new Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe(documento.OuterXml, Configurar(certificado)));
            }
        }

        [Fact]
        public void RetornoTxtPreservaBytesEOrdemLegados()
        {
            var retorno = new RetDistDFeInt
            {
                Versao = "1.01", TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE-OFFLINE", CStat = 137,
                XMotivo = "Nenhum documento sintético",
#if INTEROP
                DhResp = new DateTime(2026, 9, 24, 12, 5, 0),
#else
                DhResp = new DateTimeOffset(2026, 9, 24, 12, 5, 0, TimeSpan.FromHours(-3)),
#endif
                UltNSU = "000000000000100", MaxNSU = "000000000000100"
            }.GerarXML().OuterXml;
            const string esperado = "2;TESTE-OFFLINE;137;Nenhum documento sintético;2026-09-24T12:05:00-03:00;000000000000100;000000000000100;\r\n";
            Assert.Equal(Encoding.Default.GetBytes(esperado), Encoding.Default.GetBytes(new DistribuicaoDFeTxtConverter().ConverterRetorno(retorno)));
        }

        private static XmlDocument Converter(string campos, bool inverter, bool cte = false, bool vazios = false)
        {
            var nomes = campos.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries).AsEnumerable();
            if (inverter) nomes = nomes.Reverse();
            var linhas = nomes.Select(x => x + "|" + (vazios ? "" : x == "chNFe" ? Chave : x == "NSU" ? "000000000000101" : "000000000000100"));
            var arquivo = Path.GetTempFileName();
            try
            {
                File.WriteAllText(arquivo, "versao|1.01\r\ntpAmb|2\r\ncUFAutor|41\r\nCNPJ|99999999000191\r\n" + string.Join("\r\n", linhas), new UTF8Encoding(false));
                var resultado = cte
                    ? new Unimake.Business.DFe.Xml.CTe.DistribuicaoDFeTxtConverter().Converter(arquivo, Contexto)
                    : new DistribuicaoDFeTxtConverter().Converter(arquivo, Contexto);
                Assert.True(resultado.Sucesso);
                using (var leitor = XmlReader.Create(new StringReader(resultado.Xml), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
                {
                    var documento = new XmlDocument { XmlResolver = null };
                    documento.Load(leitor);
                    return documento;
                }
            }
            finally { File.Delete(arquivo); }
        }
        private static XmlElement[] Grupos(XmlDocument documento) => documento.DocumentElement.ChildNodes.OfType<XmlElement>()
            .Where(x => x.LocalName == "distNSU" || x.LocalName == "consNSU" || x.LocalName == "consChNFe").ToArray();
        private static Configuracao Configurar(X509Certificate2 certificado) => new Configuracao
        {
            TipoDFe = TipoDFe.NFe, TipoAmbiente = TipoAmbiente.Homologacao, CodigoUF = 41,
            TipoEmissao = TipoEmissao.Normal, CertificadoDigital = certificado
        };
        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=N05-R03-C01 SINTETICO", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                    return X509CertificateLoader.LoadPkcs12(certificado.Export(X509ContentType.Pfx), null, X509KeyStorageFlags.EphemeralKeySet);
            }
        }
    }
}
