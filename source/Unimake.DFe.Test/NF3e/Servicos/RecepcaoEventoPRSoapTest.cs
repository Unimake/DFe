using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Xml.NF3e;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NF3e.Servicos
{
    [Trait("DFe", "NF3e")]
    [Trait("Categoria", "Offline")]
    public class RecepcaoEventoPRSoapTest
    {
        private const string NamespaceEvento = "http://www.portalfiscal.inf.br/nf3e";
        private const string NamespaceServico = NamespaceEvento + "/wsdl/NF3eRecepcaoEvento";
        private const string NamespaceExterno = "urn:unimake:teste:af06:externo";

        public static IEnumerable<object[]> CenariosEnvio()
        {
            foreach (var uf in new[] { UFBrasil.PR, UFBrasil.RS })
            foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
            foreach (var tipado in new[] { false, true })
            foreach (var codigo in new[] { 135, 999 })
                yield return new object[] { uf, ambiente, tipado, codigo };
        }

        [Theory]
        [MemberData(nameof(CenariosEnvio))]
        public void ExecutarDeveSepararNamespaceDaAcaoEPreservarEventoERetorno(
            UFBrasil uf, TipoAmbiente ambiente, bool tipado, int codigo)
        {
            Executar(uf, ambiente, tipado, codigo, NamespaceServico);
        }

        public static IEnumerable<object[]> CenariosExternos()
        {
            foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
            foreach (var cenario in new[] { "nova", "antiga", "mesma", "desabilitada" })
                yield return new object[] { ambiente, cenario };
        }

        [Theory]
        [MemberData(nameof(CenariosExternos))]
        public void ExecutarDeveRespeitarVersaoEOpcaoDaConfiguracaoExterna(TipoAmbiente ambiente, string cenario)
        {
            using (var externa = new ConfiguracaoExterna(cenario))
            {
                var usarExterna = cenario == "nova";
                Executar(UFBrasil.PR, ambiente, true, 999, usarExterna ? NamespaceExterno : NamespaceServico,
                    configuracao =>
                    {
                        configuracao.BuscarConfiguracaoPastaBase = cenario != "desabilitada";
                        configuracao.PastaArquivoConfiguracao = externa.Pasta;
                    }, usarExterna ? NamespaceExterno + "/acao" : null,
                    usarExterna ? "https://af06.example.invalid/eventos" : null);
            }
        }

        [Theory]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConfiguracaoDefinidaPeloConsumidorDevePreservarTemplateEAcao(TipoAmbiente ambiente)
        {
            Executar(UFBrasil.PR, ambiente, true, 999, NamespaceExterno,
                configuracao =>
                {
                    configuracao.CodigoUF = 41;
                    configuracao.TipoAmbiente = ambiente;
                    configuracao.Servico = Servico.NF3eRecepcaoEvento;
                    configuracao.SchemaVersao = "1.00";
                    configuracao.Load("RecepcaoEvento");
                    configuracao.WebSoapString = Template(NamespaceExterno);
                    configuracao.WebActionHomologacao = configuracao.WebActionProducao = NamespaceExterno + "/acao";
                    configuracao.WebEnderecoHomologacao = configuracao.WebEnderecoProducao = "https://af06.example.invalid/eventos";
                    configuracao.Definida = true;
                }, NamespaceExterno + "/acao", "https://af06.example.invalid/eventos");
        }

        [Theory]
        [InlineData("StatusServico", TipoAmbiente.Homologacao)]
        [InlineData("StatusServico", TipoAmbiente.Producao)]
        [InlineData("ConsultaProtocolo", TipoAmbiente.Homologacao)]
        [InlineData("ConsultaProtocolo", TipoAmbiente.Producao)]
        [InlineData("AutorizacaoSinc", TipoAmbiente.Homologacao)]
        [InlineData("AutorizacaoSinc", TipoAmbiente.Producao)]
        public void TemplateEspecificoNaoDeveAlterarOutrosServicosPR(string servico, TipoAmbiente ambiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e, CodigoUF = 41, TipoAmbiente = ambiente,
                TipoEmissao = TipoEmissao.Normal, SchemaVersao = "1.00"
            };
            configuracao.Load(servico);
            var acao = ambiente == TipoAmbiente.Homologacao
                ? configuracao.WebActionHomologacao : configuracao.WebActionProducao;
            var envelope = Ler(configuracao.WebSoapString.Replace("{xmlBody}", ""));
            Assert.Equal(acao, envelope.SelectSingleNode("//*[local-name()='nf3eDadosMsg']").NamespaceURI);
            Assert.Equal(servico == "AutorizacaoSinc", configuracao.GZIPCompress);
        }

        private static void Executar(UFBrasil uf, TipoAmbiente ambiente, bool tipado, int codigo,
            string namespaceEsperado, Action<Configuracao> configurar = null,
            string acaoEsperada = null, string endpointEsperado = null)
        {
            using (var rsa = RSA.Create(2048))
            using (var certificado = new CertificateRequest("CN=NF3E AF06 TESTE OFFLINE", rsa,
                HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
                .CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
            {
                var evento = CriarEvento(uf, ambiente);
                var configuracao = new Configuracao
                {
                    TipoDFe = TipoDFe.NF3e, TipoEmissao = TipoEmissao.Normal,
                    CertificadoDigital = certificado, HasProxy = false
                };
                configurar?.Invoke(configuracao);
                var retorno = new RetEventoNF3e
                {
                    Versao = "1.00",
                    InfEvento = new InfRetEvento
                    {
                        TpAmb = ambiente, COrgao = uf, VerAplic = "TESTE OFFLINE",
                        CStat = codigo, XMotivo = codigo == 135 ? "Evento registrado e vinculado a NF3e" : "Erro nao catalogado",
                        ChNF3e = evento.InfEvento.ChNF3e, TpEvento = TipoEventoNF3e.Cancelamento,
                        NSeqEvento = "1", DhRegEvento = evento.InfEvento.DhEvento,
                        NProt = codigo == 135 ? "1999999999999999" : null
                    }
                }.GerarXML();
                var transporte = new TransporteCaptura(retorno, "nf3eRecepcaoEventoResult");
                // A fábrica é substituída antes da construção; nem produção acessa rede.
                using (SoapTransportExecutorFactory.Override(() => transporte))
                using (var servico = tipado ? new RecepcaoEvento(evento, configuracao)
                    : new RecepcaoEvento(evento.GerarXML().OuterXml, configuracao))
                {
                    var assinado = servico.ConteudoXMLAssinado.DocumentElement.OuterXml;
                    servico.Executar();
                    var requisicao = Assert.Single(transporte.Requisicoes);
                    var envelope = Ler(requisicao.Body);
                    var ns = new XmlNamespaceManager(envelope.NameTable);
                    ns.AddNamespace("s", "http://www.w3.org/2003/05/soap-envelope");
                    ns.AddNamespace("m", namespaceEsperado);
                    ns.AddNamespace("n", NamespaceEvento);
                    Assert.Equal("http://www.w3.org/2003/05/soap-envelope", envelope.DocumentElement.NamespaceURI);
                    var mensagem = Assert.IsType<XmlElement>(envelope.SelectSingleNode("/s:Envelope/s:Body/m:nf3eDadosMsg", ns));
                    Assert.Single(mensagem.ChildNodes.Cast<XmlNode>());
                    var enviado = Assert.IsType<XmlElement>(mensagem.FirstChild);
                    Assert.Equal("eventoNF3e", enviado.LocalName);
                    Assert.Equal(NamespaceEvento, enviado.NamespaceURI);
                    Assert.Equal(assinado, Ler(enviado.OuterXml).OuterXml);
                    Assert.Equal(assinado, servico.ConteudoXMLAssinado.DocumentElement.OuterXml);
                    var xmlEvento = Ler(enviado.OuterXml);
                    var assinatura = new SignedXml(xmlEvento);
                    assinatura.LoadXml((XmlElement)xmlEvento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
                    Assert.True(assinatura.CheckSignature());
                    Assert.Equal("POST", requisicao.Method);
                    Assert.Equal("application/soap+xml; charset=utf-8;", requisicao.ContentType);
                    Assert.Equal(acaoEsperada ?? NamespaceServico + (uf == UFBrasil.PR ? "/NF3eRecepcaoEvento" : "/nf3eRecepcaoEvento"), requisicao.Headers["SOAPAction"]);
                    var endpointPadrao = uf == UFBrasil.PR
                        ? "https://" + (ambiente == TipoAmbiente.Homologacao ? "homologacao." : "") + "nf3e.fazenda.pr.gov.br/nf3e/NF3eRecepcaoEvento?wsdl"
                        : "https://nf3e" + (ambiente == TipoAmbiente.Homologacao ? "-homologacao" : "") + ".svrs.rs.gov.br/ws/nf3eRecepcaoEvento/nf3eRecepcaoEvento.asmx?wsdl";
                    Assert.Equal(endpointEsperado ?? endpointPadrao, requisicao.RequestUri);
                    Assert.True(requisicao.UseCertificate);
                    Assert.Same(certificado, requisicao.Certificate);
                    Assert.False(requisicao.UseProxy);
                    Assert.False(configuracao.GZIPCompress);
                    Assert.Equal(retorno.DocumentElement.OuterXml, servico.RetornoWSXML.DocumentElement.OuterXml);
                    Assert.Equal(codigo, servico.Result.InfEvento.CStat);
                    Assert.Equal(ambiente, servico.Result.InfEvento.TpAmb);
                    Assert.Equal(codigo == 135 ? "1999999999999999" : null, servico.Result.InfEvento.NProt);
                }
            }
        }

        private static EventoNF3e CriarEvento(UFBrasil uf, TipoAmbiente ambiente) => new EventoNF3e
        {
            Versao = "1.00",
            InfEvento = new InfEvento(new DetEventoCanc
            {
                VersaoEvento = "1.00", NProt = "1999999999999999",
                XJust = "CANCELAMENTO SINTETICO EXCLUSIVO PARA TESTE OFFLINE"
            })
            {
                COrgao = uf, TpAmb = ambiente, CNPJ = "99999999000191",
                ChNF3e = ((int)uf).ToString() + "261199999999000191660590000000501000000010",
#if INTEROP
                DhEvento = new DateTimeOffset(2026, 11, 16, 10, 0, 0, TimeSpan.FromHours(-3)).LocalDateTime,
#else
                DhEvento = new DateTimeOffset(2026, 11, 16, 10, 0, 0, TimeSpan.FromHours(-3)),
#endif
                TpEvento = TipoEventoNF3e.Cancelamento, NSeqEvento = 1
            }
        };

        private static XmlDocument Ler(string conteudo)
        {
            var xml = new XmlDocument { PreserveWhitespace = true, XmlResolver = null };
            using (var reader = XmlReader.Create(new StringReader(conteudo),
                new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null })) xml.Load(reader);
            return xml;
        }

        private static string Template(string ns)
        {
            XNamespace soap = "http://www.w3.org/2003/05/soap-envelope";
            return new XElement(soap + "Envelope", new XAttribute(XNamespace.Xmlns + "s", soap),
                new XElement(soap + "Body", new XElement(XName.Get("nf3eDadosMsg", ns), "{xmlBody}")))
                .ToString(SaveOptions.DisableFormatting);
        }

        private sealed class TransporteCaptura : ISoapTransportExecutor
        {
            private readonly string _resposta;
            internal IList<TransportRequest> Requisicoes { get; } = new List<TransportRequest>();

            internal TransporteCaptura(XmlDocument retorno, string tagRetorno)
            {
                XNamespace soap = "http://www.w3.org/2003/05/soap-envelope";
                _resposta = new XElement(soap + "Envelope", new XElement(soap + "Body",
                    new XElement(XName.Get(tagRetorno, NamespaceServico), XElement.Parse(retorno.OuterXml))))
                    .ToString(SaveOptions.DisableFormatting);
            }

            public TransportResponse Execute(TransportRequest request)
            {
                Requisicoes.Add(request);
                return new TransportResponse { StatusCode = HttpStatusCode.OK, Content = _resposta };
            }
        }

        private sealed class ConfiguracaoExterna : IDisposable
        {
            internal string Pasta { get; } = Path.Combine(Path.GetTempPath(), "Unimake-AF06-" + Guid.NewGuid().ToString("N"));

            internal ConfiguracaoExterna(string cenario)
            {
                Directory.CreateDirectory(Pasta);
                var assembly = typeof(Configuracao).Assembly;
                var recurso = assembly.GetManifestResourceNames().Single(x => x.EndsWith(".Servicos.Config.NF3e.PR.xml", StringComparison.Ordinal));
                XDocument xml;
                using (var stream = assembly.GetManifestResourceStream(recurso))
                using (var reader = XmlReader.Create(stream, new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
                    xml = XDocument.Load(reader);
                xml.Root.AddFirst(new XElement("VersaoConfiguracao", cenario == "antiga" ? "1" : cenario == "mesma" ? Info.VersaoDLL : "999999999999999999"));
                var evento = xml.Descendants("RecepcaoEvento").Single();
                evento.Element("WebSoapString")?.Remove();
                evento.Add(new XElement("WebSoapString", new XCData(Template(NamespaceExterno))));
                foreach (var nome in new[] { "WebActionHomologacao", "WebActionProducao" }) evento.Element(nome).Value = NamespaceExterno + "/acao";
                foreach (var nome in new[] { "WebEnderecoHomologacao", "WebEnderecoProducao" }) evento.Element(nome).Value = "https://af06.example.invalid/eventos";
                xml.Save(Path.Combine(Pasta, "PR.xml"));
            }

            public void Dispose()
            {
                var caminho = Path.GetFullPath(Pasta);
                var temporaria = Path.GetFullPath(Path.GetTempPath()).TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar;
                if (!caminho.StartsWith(temporaria, StringComparison.OrdinalIgnoreCase)
                    || !Path.GetFileName(caminho).StartsWith("Unimake-AF06-", StringComparison.Ordinal))
                    throw new InvalidOperationException("Limpeza fora da fixture temporária.");
                Directory.Delete(caminho, true);
            }
        }
    }
}
