using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    [Trait("DFe", "NFe")]
    public class ConciliacaoFinanceiraEscapeTest
    {
        private const string Ns = "http://www.portalfiscal.inf.br/nfe";

        [Theory]
        [InlineData("PAGAMENTO SIMPLES", "PAGAMENTO SIMPLES")]
        [InlineData("PAGAMENTO Á Ç", "PAGAMENTO Á Ç")]
        [InlineData("A & B", "A & B")]
        [InlineData("A < B > C", "A < B > C")]
        [InlineData("ASPAS \"DUPLAS\" E 'SIMPLES'", "ASPAS \"DUPLAS\" E 'SIMPLES'")]
        [InlineData("A &amp; B", "A & B")]
        [InlineData("A &lt; B &gt; C", "A < B > C")]
        [InlineData("ASPAS &quot;DUPLAS&quot; E &#39;SIMPLES&#39;", "ASPAS \"DUPLAS\" E 'SIMPLES'")]
        [InlineData("A &amp; B <C> & D", "A & B <C> & D")]
        [InlineData("TEXTO &inexistente;", "TEXTO &inexistente;")]
        public void GeracaoERoundTrip_DevemEscaparSomenteNaEscrita(string entrada, string esperado)
        {
            var evento = CriarEvento(entrada);
            Assert.Equal(esperado, PrimeiroPagamento(evento).XPag);
            var xml = evento.GerarXML();
            ConferirConteudo(xml, esperado);
            foreach (var leitura in new[]
            {
                XMLUtility.Deserializar<EnvEvento>(xml),
                new EnvEvento().LerXML<EnvEvento>(xml),
                new EnvEvento().LoadFromXML(xml.OuterXml)
            })
            {
                Assert.Equal(esperado, PrimeiroPagamento(leitura).XPag);
                ConferirConteudo(leitura.GerarXML(), esperado);
            }
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void TextoVazio_DeveContinuarSendoRecusadoPeloSchema(string texto)
        {
            var xml = CriarEvento(texto).GerarXML();
            var detalhe = new XmlDocument { XmlResolver = null };
            detalhe.AppendChild(detalhe.ImportNode(Selecionar(xml, "/n:envEvento/n:evento/n:infEvento/n:detEvento"), true));
            Assert.Equal("", Selecionar(xml, "/n:envEvento/n:evento/n:infEvento/n:detEvento/n:detPag[1]/n:xPag").InnerText);
            var validar = new ValidarSchema();
            validar.Validar(detalhe, "NFe.e110750_v1.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("xPag", validar.ErrorMessage);
        }
        public static IEnumerable<object[]> Normalizacoes()
        {
            yield return new object[] { null, null };
            yield return new object[] { "", "" };
            yield return new object[] { "   ", "" };
            yield return new object[] { "  A &amp; B  ", "A & B" };
            yield return new object[] { "A &lt;B&gt; &quot;C&quot; &#39;D&#39;", "A <B> \"C\" 'D'" };
            yield return new object[] { new string('A', 70) + " &amp; B", new string('A', 60) };
        }

        [Theory]
        [MemberData(nameof(Normalizacoes))]
        public void Propriedade_DeveManterNormalizacaoIgualAoPagamentoDaNFe(string entrada, string esperado)
        {
            var conciliacao = new DetPagECONF { XPag = entrada };
            var pagamento = new DetPag { XPag = entrada };
            Assert.Equal(esperado, conciliacao.XPag);
            Assert.Equal(pagamento.XPag, conciliacao.XPag);
        }

        [Theory]
        [InlineData("A & B <C>")]
        [InlineData("ASPAS \"DUPLAS\" E 'SIMPLES'")]
        public void XmlErpBemFormado_DevePreservarTextoNoRoundTrip(string esperado)
        {
            var xml = CriarEvento("PAGAMENTO SIMPLES").GerarXML();
            // A fixture representa um ERP que já produziu XML bem formado.
            Selecionar(xml, "/n:envEvento/n:evento/n:infEvento/n:detEvento/n:detPag[1]/n:xPag").InnerText = esperado;
            var entrada = LerSeguro(xml.OuterXml);
            var evento = XMLUtility.Deserializar<EnvEvento>(entrada);
            Assert.Equal(esperado, PrimeiroPagamento(evento).XPag);
            ConferirConteudo(evento.GerarXML(), esperado);
        }

        [Theory]
        [InlineData(1)]
        [InlineData(3)]
        public void MeioDiferenteDeOutros_NaoDevePassarAEmitirXPag(int meio)
        {
            var evento = CriarEvento("A & B <C>");
            PrimeiroPagamento(evento).TPag = (MeioPagamento)meio;
            var xml = evento.GerarXML();
            Assert.Empty(xml.GetElementsByTagName("xPag").Cast<XmlNode>());
            ValidarDetalhe(xml);
        }

        [Theory]
        [InlineData("PAGAMENTO SIMPLES", "PAGAMENTO SIMPLES")]
        [InlineData("A & B <C>", "A & B <C>")]
        [InlineData("A &amp; B &lt;C&gt;", "A & B <C>")]
        [InlineData("ASPAS &quot;DUPLAS&quot; E &#39;SIMPLES&#39;", "ASPAS \"DUPLAS\" E 'SIMPLES'")]
        public void AssinaturaEDistribuicao_DevemPreservarEventoLiteral(string entrada, string esperado)
        {
            using (var certificado = CriarCertificado())
            using (var servico = new Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(CriarEvento(entrada), new Configuracao
            {
                TipoDFe = TipoDFe.NFe, CodigoUF = 41, TipoAmbiente = TipoAmbiente.Homologacao,
                TipoEmissao = TipoEmissao.Normal, CertificadoDigital = certificado
            }))
            {
                var assinado = servico.ConteudoXMLAssinado;
                ConferirConteudo(assinado, esperado);
                ConferirAssinatura(assinado, "/n:envEvento/n:evento");
                var original = Selecionar(assinado, "/n:envEvento/n:evento").OuterXml;
                var inf = servico.EnvEvento.Evento[0].InfEvento;
                var resposta = new RetEnvEvento
                {
                    Versao = "1.00", IdLote = "1", TpAmb = TipoAmbiente.Homologacao, COrgao = UFBrasil.PR,
                    CStat = 128, XMotivo = "LOTE SINTETICO PROCESSADO",
                    RetEvento = new List<RetEvento>
                    {
                        new RetEvento
                        {
                            Versao = "1.00", InfEvento = new InfEventoRetEvento
                            {
                                TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE-OFFLINE", COrgao = UFBrasil.PR, CStat = 135,
                                XMotivo = "EVENTO SINTETICO REGISTRADO", ChNFe = inf.ChNFe,
                                TpEvento = TipoEventoNFe.ConciliacaoFinanceira, NSeqEvento = 1,
                                DhRegEventoField = "2026-11-17T12:40:00-03:00", NProt = "141260000000001"
                            }
                        }
                    }
                };
                // Estado público controlado; não executar transporte nem fabricar procEvento.
                servico.RetornoWSXML = XMLUtility.Serializar(resposta);
                servico.RetornoWSString = servico.RetornoWSXML.OuterXml;
                var distribuicao = Assert.Single(servico.ProcEventoNFeResult).GerarXML();
                Assert.Equal(original, Selecionar(distribuicao, "/n:procEventoNFe/n:evento").OuterXml);
                ConferirAssinatura(distribuicao, "/n:procEventoNFe/n:evento");
                Assert.Equal(esperado, Selecionar(distribuicao, "/n:procEventoNFe/n:evento/n:infEvento/n:detEvento/n:detPag[1]/n:xPag").InnerText);
                var validar = new ValidarSchema();
                validar.Validar(distribuicao, "NFe.procEventoNFe_v1.00.xsd", Ns);
                Assert.True(validar.Success, validar.ErrorMessage);
            }
        }

        private static EnvEvento CriarEvento(string texto) => new EnvEvento
        {
            Versao = "1.00", IdLote = "1", Evento = new List<Evento>
            {
                new Evento
                {
                    Versao = "1.00", InfEvento = new InfEvento(new DetEventoConciliacaoFinanceira
                    {
                        Versao = "1.00", VerAplic = "TESTE-OFFLINE", DetPag = new List<DetPagECONF>
                        {
                            new DetPagECONF
                            {
                                IndPag = (IndicadorPagamento)0, TPag = MeioPagamento.Outros, XPag = texto,
                                VPag = 100.25, DPag = new DateTime(2026, 11, 17),
                                CNPJPag = "99999999000191", UFPag = UFBrasil.PR, CNPJIF = "99999999000191",
                                TBand = (BandeiraOperadoraCartao)1, CAut = "AUT-SINTETICA", CNPJReceb = "99999999000191", UFReceb = UFBrasil.PR
                            },
                            new DetPagECONF { TPag = (MeioPagamento)1, VPag = 0.10, DPag = new DateTime(2026, 11, 18) }
                        }
                    })
                    {
                        COrgao = UFBrasil.PR, TpAmb = TipoAmbiente.Homologacao, CNPJ = "99999999000191",
                        ChNFe = XMLUtility.MontarChaveDFe(UFBrasil.PR, new DateTime(2026, 11, 17), "99999999000191", ModeloDFe.NFe, 59, 50, TipoEmissao.Normal, "10000001"),
                        DhEventoField = "2026-11-17T12:36:00-03:00", TpEvento = TipoEventoNFe.ConciliacaoFinanceira, NSeqEvento = 1, VerEvento = "1.00"
                    }
                }
            }
        };

        private static DetPagECONF PrimeiroPagamento(EnvEvento evento) =>
            Assert.IsType<DetEventoConciliacaoFinanceira>(evento.Evento[0].InfEvento.DetEvento).DetPag[0];

        private static void ConferirConteudo(XmlDocument xml, string esperado)
        {
            const string caminho = "/n:envEvento/n:evento/n:infEvento/n:detEvento";
            var detalhe = Selecionar(xml, caminho);
            Assert.Equal(new[] { "descEvento", "verAplic", "detPag", "detPag" }, detalhe.ChildNodes.OfType<XmlElement>().Select(n => n.LocalName));
            var pagamento = Selecionar(xml, caminho + "/n:detPag[1]");
            Assert.Equal(new[] { "indPag", "tPag", "xPag", "vPag", "dPag", "CNPJPag", "UFPag", "CNPJIF", "tBand", "cAut", "CNPJReceb", "UFReceb" }, pagamento.ChildNodes.OfType<XmlElement>().Select(n => n.LocalName));
            Assert.Equal(Ns, pagamento.NamespaceURI);
            Assert.Equal(esperado, Selecionar(xml, caminho + "/n:detPag[1]/n:xPag").InnerText);
            Assert.Empty(Selecionar(xml, caminho + "/n:detPag[1]/n:xPag").ChildNodes.OfType<XmlElement>());
            Assert.Equal("100.25", Selecionar(xml, caminho + "/n:detPag[1]/n:vPag").InnerText);
            Assert.Equal("2026-11-17", Selecionar(xml, caminho + "/n:detPag[1]/n:dPag").InnerText);
            Assert.Equal("99999999000191", Selecionar(xml, caminho + "/n:detPag[1]/n:CNPJIF").InnerText);
            Assert.Equal("01", Selecionar(xml, caminho + "/n:detPag[1]/n:tBand").InnerText);
            Assert.Equal("AUT-SINTETICA", Selecionar(xml, caminho + "/n:detPag[1]/n:cAut").InnerText);
            Assert.Equal("0.10", Selecionar(xml, caminho + "/n:detPag[2]/n:vPag").InnerText);
            Assert.Equal("2026-11-18", Selecionar(xml, caminho + "/n:detPag[2]/n:dPag").InnerText);
            Assert.Single(xml.GetElementsByTagName("xPag").Cast<XmlNode>());
            ValidarDetalhe(xml);
        }

        private static void ValidarDetalhe(XmlDocument xml)
        {
            var doc = new XmlDocument { XmlResolver = null };
            doc.AppendChild(doc.ImportNode(Selecionar(xml, "/n:envEvento/n:evento/n:infEvento/n:detEvento"), true));
            var validar = new ValidarSchema();
            validar.Validar(doc, "NFe.e110750_v1.00.xsd", Ns);
            Assert.True(validar.Success, validar.ErrorMessage);
        }

        private static XmlElement Selecionar(XmlDocument xml, string caminho)
        {
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("n", Ns);
            return (XmlElement)xml.SelectSingleNode(caminho, ns);
        }

        private static XmlDocument LerSeguro(string texto)
        {
            var xml = new XmlDocument { PreserveWhitespace = true, XmlResolver = null };
            using (var reader = XmlReader.Create(new System.IO.StringReader(texto), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
                xml.Load(reader);
            return xml;
        }

        private static void ConferirAssinatura(XmlDocument xml, string caminho)
        {
            var evento = Selecionar(xml, caminho);
            var assinatura = new SignedXml(evento);
            assinatura.LoadXml((XmlElement)evento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
            Assert.True(assinatura.CheckSignature());
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var chave = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=R03-C01 SINTETICO", chave, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                    return X509CertificateLoader.LoadPkcs12(certificado.Export(X509ContentType.Pfx, "teste"), "teste", X509KeyStorageFlags.Exportable | X509KeyStorageFlags.EphemeralKeySet);
            }
        }
    }
}
