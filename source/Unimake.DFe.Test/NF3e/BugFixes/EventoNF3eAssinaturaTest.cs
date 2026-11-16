using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Xunit;

namespace Unimake.DFe.Test.NF3e.BugFixes
{
    [Trait("DFe", "NF3e")]
    [Trait("Categoria", "Offline")]
    public class EventoNF3eAssinaturaTest
    {
        private const string NamespaceEvento = "http://www.portalfiscal.inf.br/nf3e";

        public static IEnumerable<object[]> Leituras()
        {
            foreach (var tipo in Tipos())
            foreach (var caminho in new[] { "XMLUtility", "LerXML", "LoadFromXML" })
            foreach (var assinado in new[] { false, true })
                yield return new object[] { tipo, caminho, assinado };
        }

        public static IEnumerable<object[]> Envios()
        {
            foreach (var tipo in Tipos())
            foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
            foreach (var tipado in new[] { false, true })
                yield return new object[] { tipo, ambiente, tipado };
        }

        private static TipoEventoNF3e[] Tipos() => new[]
        {
            TipoEventoNF3e.Cancelamento,
            TipoEventoNF3e.VinculacaoPagamento,
            TipoEventoNF3e.CancelamentoVinculacaoPagamento
        };

        [Theory]
        [MemberData(nameof(Leituras))]
        public void DesserializarDevePreservarDetalhamentoEAssinatura(TipoEventoNF3e tipo, string caminho, bool assinado)
        {
            using (var certificado = CriarCertificado())
            {
                var evento = CriarEvento(tipo, TipoAmbiente.Homologacao);
                using (var servico = new RecepcaoEvento(evento, Configuracao(certificado)))
                {
                    var original = assinado ? Ler(servico.ConteudoXMLAssinado.OuterXml) : evento.GerarXML();
                    var leitura = caminho == "XMLUtility" ? XMLUtility.Deserializar<EventoNF3e>(original)
                        : caminho == "LerXML" ? new EventoNF3e().LerXML<EventoNF3e>(original)
                        : new EventoNF3e().LoadFromXML(original.OuterXml);
                    ConferirDetalhe(leitura, tipo);
                    Assert.Equal(assinado, leitura.Signature != null);
                    var serializado = leitura.GerarXML();
                    Assert.Equal(assinado ? 1 : 0, serializado.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
                    if (assinado)
                    {
                        ConferirAssinatura(serializado);
                        Assert.Equal(original.DocumentElement.OuterXml, serializado.DocumentElement.OuterXml);
                    }
                }
            }
        }

        [Theory]
        [InlineData(TipoEventoNF3e.Cancelamento)]
        [InlineData(TipoEventoNF3e.VinculacaoPagamento)]
        [InlineData(TipoEventoNF3e.CancelamentoVinculacaoPagamento)]
        public void ReadXmlNaoDeveConsumirAssinaturaIrma(TipoEventoNF3e tipo)
        {
            using (var certificado = CriarCertificado())
            using (var servico = new RecepcaoEvento(CriarEvento(tipo, TipoAmbiente.Homologacao), Configuracao(certificado)))
            using (var reader = XmlReader.Create(new StringReader(servico.ConteudoXMLAssinado.OuterXml), OpcoesLeitura()))
            {
                Assert.True(reader.ReadToFollowing("detEvento", NamespaceEvento));
                var detalhe = new EventoDetalhe();
                detalhe.ReadXml(reader);
                // Mesmo antes de aplicar o parser específico, a leitura pública deve respeitar o limite do grupo.
                Assert.True(reader.ReadToFollowing("Signature", SignedXml.XmlDsigNamespaceUrl));
                var modelo = new InfEvento { TpEvento = tipo, DetEvento = detalhe };
                ConferirDetalhe(new EventoNF3e { InfEvento = modelo }, tipo);
            }
        }

        [Theory]
        [MemberData(nameof(Envios))]
        public void ExecutarEGravarDistribuicaoDevemPreservarEventoAssinado(TipoEventoNF3e tipo, TipoAmbiente ambiente, bool tipado)
        {
            using (var certificado = CriarCertificado())
            {
                var evento = CriarEvento(tipo, ambiente);
                var retorno = new RetEventoNF3e
                {
                    Versao = "1.00",
                    InfEvento = new InfRetEvento
                    {
                        TpAmb = ambiente, COrgao = UFBrasil.PR, VerAplic = "TESTE OFFLINE",
                        CStat = 135, XMotivo = "Evento registrado", ChNF3e = evento.InfEvento.ChNF3e,
                        TpEvento = tipo, NSeqEvento = "1", NProt = "1999999999999999", DhRegEvento = evento.InfEvento.DhEvento
                    }
                }.GerarXML();
                var transporte = new TransporteSimulado(retorno);
                using (SoapTransportExecutorFactory.Override(() => transporte))
                using (var servico = tipado ? new RecepcaoEvento(evento, Configuracao(certificado))
                    : new RecepcaoEvento(evento.GerarXML().OuterXml, Configuracao(certificado)))
                {
                    var original = servico.ConteudoXMLAssinado.DocumentElement.OuterXml;
                    servico.Executar();
                    Assert.Equal(1, transporte.Chamadas);
                    ConferirDetalhe(servico.EventoNF3e, tipo);
                    Assert.NotNull(servico.EventoNF3e.Signature);
                    var soap = Ler(transporte.Requisicao.Body);
                    var enviado = (XmlElement)soap.SelectSingleNode("//*[local-name()='nf3eDadosMsg']/*");
                    Assert.Equal(original, enviado.OuterXml);
                    Assert.Equal(original, servico.ConteudoXMLAssinado.DocumentElement.OuterXml);
                    using (var stream = new MemoryStream())
                    {
                        servico.GravarXmlDistribuicao(stream);
                        var distribuicao = Ler(System.Text.Encoding.UTF8.GetString(stream.ToArray()).TrimStart('\uFEFF'));
                        var ns = new XmlNamespaceManager(distribuicao.NameTable);
                        ns.AddNamespace("n", NamespaceEvento);
                        var publicado = (XmlElement)distribuicao.SelectSingleNode("/n:procEventoNF3e/n:eventoNF3e", ns);
                        Assert.Equal(original, publicado.OuterXml);
                        ConferirAssinatura(Ler(publicado.OuterXml));
                        Assert.Equal("135", distribuicao.SelectSingleNode("/n:procEventoNF3e/n:retEventoNF3e/n:infEvento/n:cStat", ns).InnerText);
                        Assert.Equal(Canonicalizar(retorno), Canonicalizar(servico.RetornoWSXML));
                        foreach (var campo in new[] { "tpAmb", "verAplic", "cOrgao", "cStat", "xMotivo", "chNF3e", "tpEvento", "nSeqEvento", "dhRegEvento", "nProt" })
                            Assert.Equal(retorno.SelectSingleNode("/n:retEventoNF3e/n:infEvento/n:" + campo, ns).InnerText,
                                distribuicao.SelectSingleNode("/n:procEventoNF3e/n:retEventoNF3e/n:infEvento/n:" + campo, ns).InnerText);
                    }
                }
            }
        }

        private static EventoNF3e CriarEvento(TipoEventoNF3e tipo, TipoAmbiente ambiente)
        {
            EventoDetalhe detalhe;
            switch (tipo)
            {
                case TipoEventoNF3e.Cancelamento:
                    detalhe = new DetEventoCanc { VersaoEvento = "1.00", NProt = "1999999999999999", XJust = "CANCELAMENTO SINTETICO PARA TESTE OFFLINE" };
                    break;
                case TipoEventoNF3e.VinculacaoPagamento:
                    detalhe = new DetEventoVincPgto
                    {
                        VersaoEvento = "1.00", NProt = "1999999999999999",
                        Pgto = new Pgto { NPag = "1", IdTransacao = "AF06-TRANSACAO-SINTETICA", TpMeioPgto = "17", CNPJReceb = "99999999000191", CNPJBasePSP = "99999999" }
                    };
                    break;
                default:
                    detalhe = new DetEventoCancVincPgto { VersaoEvento = "1.00", NProt = "1999999999999999", NProtVincPgto = "1999999999999998" };
                    break;
            }
            return new EventoNF3e
            {
                Versao = "1.00",
                InfEvento = new InfEvento(detalhe)
                {
                    COrgao = UFBrasil.PR, TpAmb = ambiente, CNPJ = "99999999000191",
                    ChNF3e = "41260899999999000191660590000000501000000010",
#if INTEROP
                    DhEvento = new DateTime(2026, 8, 10, 12, 0, 0),
#else
                    DhEvento = new DateTimeOffset(2026, 8, 10, 12, 0, 0, TimeSpan.FromHours(-3)),
#endif
                    TpEvento = tipo, NSeqEvento = 1
                }
            };
        }

        private static string Canonicalizar(XmlDocument xml)
        {
            var transform = new XmlDsigC14NTransform();
            transform.LoadInput(xml);
            using (var stream = (Stream)transform.GetOutput(typeof(Stream)))
            using (var reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
        }

        private static void ConferirDetalhe(EventoNF3e evento, TipoEventoNF3e tipo)
        {
            Assert.Equal(tipo, evento.InfEvento.TpEvento);
            Assert.Equal("1.00", evento.InfEvento.DetEvento.VersaoEvento);
            switch (tipo)
            {
                case TipoEventoNF3e.Cancelamento:
                    var cancelamento = Assert.IsType<DetEventoCanc>(evento.InfEvento.DetEvento);
                    Assert.Equal("Cancelamento", cancelamento.DescEvento);
                    Assert.Equal("1999999999999999", cancelamento.NProt);
                    Assert.Equal("CANCELAMENTO SINTETICO PARA TESTE OFFLINE", cancelamento.XJust);
                    break;
                case TipoEventoNF3e.VinculacaoPagamento:
                    var vinculacao = Assert.IsType<DetEventoVincPgto>(evento.InfEvento.DetEvento);
                    Assert.Equal("Vinculacao do Pagamento", vinculacao.DescEvento);
                    Assert.Equal("1999999999999999", vinculacao.NProt);
                    Assert.Equal("1", vinculacao.Pgto.NPag);
                    Assert.Equal("AF06-TRANSACAO-SINTETICA", vinculacao.Pgto.IdTransacao);
                    Assert.Equal("17", vinculacao.Pgto.TpMeioPgto);
                    Assert.Equal("99999999000191", vinculacao.Pgto.CNPJReceb);
                    Assert.Equal("99999999", vinculacao.Pgto.CNPJBasePSP);
                    break;
                default:
                    var cancelarVinculacao = Assert.IsType<DetEventoCancVincPgto>(evento.InfEvento.DetEvento);
                    Assert.Equal("Cancelamento da vinculacao do pagamento", cancelarVinculacao.DescEvento);
                    Assert.Equal("1999999999999999", cancelarVinculacao.NProt);
                    Assert.Equal("1999999999999998", cancelarVinculacao.NProtVincPgto);
                    break;
            }
        }

        private static Configuracao Configuracao(X509Certificate2 certificado) => new Configuracao
        {
            TipoDFe = TipoDFe.NF3e, CertificadoDigital = certificado, HasProxy = false
        };

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
                return new CertificateRequest("CN=AF06 ASSINATURA OFFLINE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
                    .CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
        }

        private static XmlReaderSettings OpcoesLeitura() => new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null };
        private static XmlDocument Ler(string texto)
        {
            var xml = new XmlDocument { PreserveWhitespace = true, XmlResolver = null };
            using (var reader = XmlReader.Create(new StringReader(texto), OpcoesLeitura())) xml.Load(reader);
            return xml;
        }

        private static void ConferirAssinatura(XmlDocument documento)
        {
            Assert.Equal(1, documento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            var assinatura = new SignedXml(documento);
            assinatura.LoadXml((XmlElement)documento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
            Assert.True(assinatura.CheckSignature());
        }

        private sealed class TransporteSimulado : ISoapTransportExecutor
        {
            private readonly string _resposta;
            internal int Chamadas { get; private set; }
            internal TransportRequest Requisicao { get; private set; }

            internal TransporteSimulado(XmlDocument retorno)
            {
                var soap = new XmlDocument { XmlResolver = null };
                var envelope = soap.CreateElement("s", "Envelope", "http://www.w3.org/2003/05/soap-envelope");
                soap.AppendChild(envelope);
                var body = soap.CreateElement("s", "Body", envelope.NamespaceURI);
                envelope.AppendChild(body);
                var result = soap.CreateElement("nf3eRecepcaoEventoResult", NamespaceEvento + "/wsdl/NF3eRecepcaoEvento");
                body.AppendChild(result);
                result.AppendChild(soap.ImportNode(retorno.DocumentElement, true));
                _resposta = soap.OuterXml;
            }

            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                Requisicao = request;
                return new TransportResponse { StatusCode = HttpStatusCode.OK, Content = _resposta };
            }
        }
    }
}
