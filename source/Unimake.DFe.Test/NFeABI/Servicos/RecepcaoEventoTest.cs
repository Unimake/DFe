using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFeABI;
using Unimake.Exceptions;
using Xunit;
using NFeABIRecepcaoEvento = Unimake.Business.DFe.Servicos.NFeABI.RecepcaoEvento;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    [Trait("DFe", "NFeABI")]
    [Trait("Servico", "RecepcaoEvento")]
    public class RecepcaoEventoTest : NFeABIServicoTestBase
    {
        private const string NamespaceEvento = "http://www.portalfiscal.inf.br/nfeabi";
        private const string NamespaceWsdl = NamespaceEvento + "/wsdl/NFeABIRecepcaoEvento";
        private const string Endpoint = "https://homologacao.nfeabi.fazenda.pr.gov.br/nfeabi/NFeABIRecepcaoEvento";

        [Theory]
        [InlineData("cancelamento", 110111)]
        [InlineData("pagamento-parcela", 112310)]
        [InlineData("pagamento-complementar", 112310)]
        [InlineData("desfazimento-alienacao", 112320)]
        [InlineData("apropriacao-credito", 211310)]
        public void PreparaEAssinaEventosComOContratoPublicado(string nome, int codigoEvento)
        {
            var evento = CarregarEvento(nome);
            evento.Signature = null;
            using var certificado = CriarCertificado();
            var configuracao = new Configuracao { CertificadoDigital = certificado };
            var servico = new NFeABIRecepcaoEvento(evento, configuracao);

            Assert.Equal(Servico.NFeABIRecepcaoEvento, configuracao.Servico);
            Assert.Equal(TipoDFe.NFeABI, configuracao.TipoDFe);
            Assert.Equal(ModeloDFe.NFeABI, configuracao.Modelo);
            Assert.Equal((int)evento.InfEvento.COrgao, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.Equal("1.00", configuracao.SchemaVersao);
            Assert.Equal(Endpoint, configuracao.WebEnderecoHomologacao);
            Assert.Equal(NamespaceWsdl + "/nfeabiRecepcaoEvento", configuracao.WebActionHomologacao);
            Assert.Equal("nfeabiRecepcaoEventoResult", configuracao.WebTagRetorno);

            var envelope = new XmlDocument();
            envelope.LoadXml(configuracao.WebSoapString.Replace("{xmlBody}", servico.ConteudoXMLAssinado.DocumentElement.OuterXml));
            Assert.Equal("http://www.w3.org/2003/05/soap-envelope", envelope.DocumentElement.NamespaceURI);
            var wrapper = Assert.IsType<XmlElement>(envelope.GetElementsByTagName("nfeabiDadosMsg", NamespaceWsdl)[0]);
            Assert.Equal("evento", wrapper.FirstChild.LocalName);
            Assert.Equal(NamespaceEvento, wrapper.FirstChild.NamespaceURI);

            var xml = servico.ConteudoXMLAssinado;
            var assinatura = Assert.IsType<XmlElement>(xml.DocumentElement.LastChild);
            Assert.Equal("Signature", assinatura.LocalName);
            var validadorAssinatura = new SignedXml(xml);
            validadorAssinatura.LoadXml(assinatura);
            var referencia = Assert.IsType<Reference>(Assert.Single(validadorAssinatura.SignedInfo.References));
            Assert.Equal("#" + evento.InfEvento.Id, referencia.Uri);
            Assert.True(validadorAssinatura.CheckSignature(certificado, true));
            Assert.Equal(codigoEvento, (int)servico.EventoNFeABI.InfEvento.TpEvento);
        }

        [Fact]
        public void RejeitaDetalheIncompativelAntesDaTransmissao()
        {
            var evento = CarregarEvento("cancelamento");
            evento.InfEvento.TpEvento = (TipoEventoNFeABI)112320;
            evento.Signature = null;
            using var certificado = CriarCertificado();

            Assert.Throws<ValidatorDFeException>(() => new NFeABIRecepcaoEvento(
                evento, new Configuracao { CertificadoDigital = certificado }));
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void RejeitaDesfazimentoSemDescricaoAntesDaAssinatura(bool receberXml)
        {
            var evento = CarregarEvento("desfazimento-alienacao");
            evento.Signature = null;
            evento.InfEvento.DetEvento.EvDesfazimAlienac.TpDesfaz = "99";
            evento.InfEvento.DetEvento.EvDesfazimAlienac.XDesfaz = null;

            var erro = Assert.Throws<ValidatorDFeException>(() => receberXml
                ? new NFeABIRecepcaoEvento(evento.GerarXML().OuterXml, new Configuracao())
                : new NFeABIRecepcaoEvento(evento, new Configuracao()));
            Assert.Contains("xDesfaz", erro.Message);
        }

        [Fact]
        public void RejeitaDoisIdentificadoresDoAutorAntesDaSerializacao()
        {
            var evento = CarregarEvento("cancelamento");
            evento.InfEvento.CPF = "12345678901";

            var erro = Assert.Throws<ValidatorDFeException>(() => new NFeABIRecepcaoEvento(evento, new Configuracao()));
            Assert.Contains("CNPJ ou CPF", erro.Message);
        }

        [Fact]
        public void RejeitaIdBemFormadoDeOutraChaveAntesDaAssinatura()
        {
            var evento = CarregarEvento("cancelamento");
            evento.Signature = null;
            var xml = evento.GerarXML();
            var infEvento = Assert.IsType<XmlElement>(xml.GetElementsByTagName("infEvento", NamespaceEvento)[0]);
            evento.InfEvento.ChNFeABI = "41260912345678000123770010000000021012345671";
            infEvento.SetAttribute("Id", evento.InfEvento.Id);

            var erro = Assert.Throws<ValidarXMLException>(() => new NFeABIRecepcaoEvento(xml.OuterXml, new Configuracao()));
            Assert.Contains("Id de <infEvento>", erro.Message);
        }

        [Fact]
        public void RoteiaEventoDeOrgaoNacionalPelaUfDaChave()
        {
            var evento = CarregarEvento("cancelamento");
            evento.InfEvento.COrgao = UFBrasil.AN;
            evento.Signature = null;
            using var certificado = CriarCertificado();
            var servico = new NFeABIRecepcaoEvento(evento, new Configuracao { CertificadoDigital = certificado });

            Assert.Equal((int)UFBrasil.PR, servico.Configuracoes.CodigoUF);
            Assert.Equal(Endpoint, servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(UFBrasil.AN, servico.EventoNFeABI.InfEvento.COrgao);
        }

        [Fact]
        public void ProduzRetornoTipadoComProtocoloERetornoAmigavelAntesDoEnvio()
        {
            var servico = new NFeABIRecepcaoEvento(CarregarEvento("cancelamento"), new Configuracao());
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            var retorno = new RetEventoNFeABI
            {
                Versao = "1.00",
                InfEvento = new InfRetEventoNFeABI
                {
                    TpAmb = TipoAmbiente.Homologacao,
                    COrgao = UFBrasil.PR,
                    VerAplic = "TESTE",
                    CStat = 135,
                    XMotivo = "Evento registrado e vinculado",
                    NProt = "1412600000000001",
                    DhRegEventoField = "2026-09-14T12:00:00-03:00"
                }
            };
            servico.RetornoWSXML = retorno.GerarXML();

            Assert.Equal(135, servico.Result.InfEvento.CStat);
            Assert.Equal("1412600000000001", servico.Result.InfEvento.NProt);
        }

        [Fact]
        public void RecepcaoEventoEmHomologacao()
        {
            var caminho = Environment.GetEnvironmentVariable("NFEABI_EVENTO_HOMOLOGACAO_XML");
            if (string.IsNullOrWhiteSpace(caminho))
            {
                Assert.Skip("Informe em NFEABI_EVENTO_HOMOLOGACAO_XML o evento de uma nota de homologação preparado para transmissão.");
            }

            var evento = LerEvento(caminho);
            Assert.Equal(TipoAmbiente.Homologacao, evento.InfEvento.TpAmb);
            evento.Signature = null;
            var servico = new NFeABIRecepcaoEvento(evento, CriarConfiguracao(evento.InfEvento.COrgao));
            servico.Executar();

            Assert.NotNull(servico.EventoNFeABI.Signature);
            Assert.NotEqual(0, servico.Result.InfEvento.CStat);
        }

        [Theory]
        [InlineData(false)]
#if INTEROP
        [InlineData(true)]
#endif
        public async Task LimpaProtocoloAnteriorQuandoAProximaTentativaFalha(bool usarInterop)
        {
            const string protocolo = "1412600000000001";
            using var servidor = new TcpListener(IPAddress.Loopback, 0);
            servidor.Start();
            using var limite = new CancellationTokenSource(TimeSpan.FromSeconds(10));
            using var certificado = CriarCertificado();
            var configuracao = new Configuracao { CertificadoDigital = certificado, TimeOutWebServiceConnect = 5000 };
            var evento = CarregarEvento("cancelamento");
            evento.Signature = null;
            var servico = new NFeABIRecepcaoEvento(evento, configuracao);
            configuracao.WebEnderecoHomologacao = $"http://127.0.0.1:{((IPEndPoint)servidor.LocalEndpoint).Port}/";

            var primeiraResposta = ResponderSoapAsync(servidor, CriarRespostaSoap(protocolo), limite.Token);
            servico.Executar();
            await primeiraResposta;
            Assert.Equal(135, servico.Result.InfEvento.CStat);
            Assert.Equal(protocolo, servico.Result.InfEvento.NProt);
            Assert.Equal(HttpStatusCode.OK, servico.HttpStatusCode);

            if (!usarInterop)
            {
                var segundaResposta = ResponderSoapAsync(servidor, "resposta inválida", limite.Token);
                Assert.ThrowsAny<Exception>(() => servico.Executar());
                await segundaResposta;
            }
#if INTEROP
            else
            {
                Assert.Contains(evento.InfEvento.ChNFeABI, servico.GetConteudoXMLOriginal());
                Assert.Contains(evento.InfEvento.ChNFeABI, servico.GetConteudoXMLAssinado());
                evento.InfEvento.ChNFeABI = "41260912345678000123770010000000021012345678";
                evento.InfEvento.NSeqEvento = 2;
                Assert.Throws<ValidatorDFeException>(() => servico.Executar(evento, configuracao));
                Assert.Empty(servico.GetConteudoXMLOriginal());
                Assert.Empty(servico.GetConteudoXMLAssinado());
                Assert.Null(servico.EventoNFeABI);
                Assert.False(servidor.Pending());
            }
#endif

            Assert.Null(servico.RetornoWSString);
            Assert.Null(servico.RetornoWSXML);
            Assert.Equal(default, servico.HttpStatusCode);
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            Assert.Null(servico.Result.InfEvento.NProt);
        }

        [Fact]
        public void RejeitaAmbienteDivergenteComConfiguracaoDefinida()
        {
            var evento = CarregarEvento("cancelamento");
            evento.InfEvento.TpAmb = TipoAmbiente.Producao;
            evento.Signature = null;
            var configuracao = new Configuracao { Definida = true, TipoAmbiente = TipoAmbiente.Homologacao };

            var erro = Assert.Throws<ValidarXMLException>(() => new NFeABIRecepcaoEvento(evento, configuracao));
            Assert.Contains("tpAmb de <infEvento> (1)", erro.Message);
            Assert.Contains("ambiente configurado para a recepção do evento (2)", erro.Message);
        }

        [Fact]
        public void RejeitaAmbienteAlteradoAposPrepararEvento()
        {
            var configuracao = new Configuracao();
            var servico = new NFeABIRecepcaoEvento(CarregarEvento("cancelamento"), configuracao);
            configuracao.Definida = true;
            configuracao.TipoAmbiente = TipoAmbiente.Producao;

            var erro = Assert.Throws<ValidarXMLException>(() => servico.Executar());
            Assert.Contains("tpAmb de <infEvento> (2)", erro.Message);
            Assert.Equal(0, servico.Result.InfEvento.CStat);
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void RejeitaUrlDeApiAdicionadaAposPrepararEvento(bool urlDeProducao)
        {
            var configuracao = new Configuracao();
            var servico = new NFeABIRecepcaoEvento(CarregarEvento("cancelamento"), configuracao);
            const string urlApi = "http://127.0.0.1:1/api-anterior";
            if (urlDeProducao)
            {
                configuracao.RequestURIProducao = urlApi;
            }
            else
            {
                configuracao.RequestURIHomologacao = urlApi;
            }

            var erro = Assert.Throws<ValidarXMLException>(() => servico.Executar());
            Assert.Contains("exige transporte SOAP", erro.Message);
            Assert.Equal(0, servico.Result.InfEvento.CStat);
        }

#if INTEROP
        [Theory]
        [InlineData(false, "NFeABIConsultaProtocolo")]
        [InlineData(true, "ação SOAP")]
        public void InteropRejeitaConfiguracaoDeConsultaMesmoEmHomologacao(bool alterarServico, string mensagem)
        {
            using var servidor = new TcpListener(IPAddress.Loopback, 0);
            servidor.Start();
            using var certificado = CriarCertificado();
            var evento = CarregarEvento("cancelamento");
            var servico = new NFeABIRecepcaoEvento(evento, new Configuracao());
            Assert.NotEmpty(servico.GetConteudoXMLOriginal());

            var configuracao = new Configuracao { CertificadoDigital = certificado, TimeOutWebServiceConnect = 1000 };
            var consulta = new Business.DFe.Servicos.NFeABI.ConsultaProtocolo(
                evento.InfEvento.ChNFeABI, TipoAmbiente.Homologacao, configuracao);
            consulta.Configuracoes.WebEnderecoHomologacao = $"http://127.0.0.1:{((IPEndPoint)servidor.LocalEndpoint).Port}/";
            configuracao.Definida = true;
            if (alterarServico)
            {
                configuracao.Servico = Servico.NFeABIRecepcaoEvento;
            }

            evento.Signature = null;
            var erro = Assert.Throws<ValidarXMLException>(() => servico.Executar(evento, configuracao));
            Assert.Contains(mensagem, erro.Message);
            Assert.Empty(servico.GetConteudoXMLOriginal());
            Assert.Empty(servico.GetConteudoXMLAssinado());
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            Assert.False(servidor.Pending());
        }

        [Fact]
        public async Task InteropEnviaDoisEventosValidosEAtualizaXmlOriginal()
        {
            using var servidor = new TcpListener(IPAddress.Loopback, 0);
            servidor.Start();
            using var limite = new CancellationTokenSource(TimeSpan.FromSeconds(10));
            using var certificado = CriarCertificado();
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFeABI,
                CodigoUF = (int)UFBrasil.PR,
                TipoAmbiente = TipoAmbiente.Homologacao,
                Servico = Servico.NFeABIRecepcaoEvento,
                SchemaVersao = "1.00",
                CertificadoDigital = certificado,
                TimeOutWebServiceConnect = 5000
            };
            configuracao.Load("RecepcaoEvento");
            configuracao.WebEnderecoHomologacao = $"http://127.0.0.1:{((IPEndPoint)servidor.LocalEndpoint).Port}/";
            configuracao.Definida = true;

            var servico = new NFeABIRecepcaoEvento();
            var primeiro = CarregarEvento("cancelamento");
            var segundo = CarregarEvento("cancelamento");
            segundo.InfEvento.ChNFeABI = "41260912345678000123770010000000021012345678";
            primeiro.Signature = null;
            segundo.Signature = null;

            async Task EnviarEConferirAsync(EventoNFeABI evento, string protocolo)
            {
                var requisicao = ResponderSoapAsync(servidor, CriarRespostaSoap(protocolo), limite.Token);
                servico.Executar(evento, configuracao);
                var chave = evento.InfEvento.ChNFeABI;
                var envelope = new XmlDocument();
                envelope.LoadXml(await requisicao);
                var dados = Assert.IsType<XmlElement>(Assert.Single(envelope.GetElementsByTagName("nfeabiDadosMsg", NamespaceWsdl)));
                Assert.Contains("<chNFeABI>" + chave + "</chNFeABI>", dados.InnerXml);
                Assert.Equal(protocolo, servico.Result.InfEvento.NProt);
                Assert.Empty(servico.Warnings);

                var original = new XmlDocument();
                original.LoadXml(servico.GetConteudoXMLOriginal());
                Assert.Equal(chave, original.GetElementsByTagName("chNFeABI", NamespaceEvento)[0].InnerText);
                Assert.Empty(original.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl));
                Assert.NotNull(servico.EventoNFeABI.Signature);
            }

            await EnviarEConferirAsync(primeiro, "1412600000000001");

            var urlSoapCustomizada = configuracao.WebEnderecoHomologacao;
            var envelopeEvento = configuracao.WebSoapString;
            var consulta = new Business.DFe.Servicos.NFeABI.ConsultaProtocolo(
                segundo.InfEvento.ChNFeABI, TipoAmbiente.Homologacao, new Configuracao());
            configuracao.WebSoapString = consulta.Configuracoes.WebSoapString;

            var erroEnvelope = Assert.Throws<ValidarXMLException>(() => servico.Executar(segundo, configuracao));
            Assert.Contains("envelope SOAP", erroEnvelope.Message);
            Assert.Empty(servico.GetConteudoXMLOriginal());
            Assert.Empty(servico.GetConteudoXMLAssinado());
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            Assert.False(servidor.Pending());
            Assert.Equal(urlSoapCustomizada, configuracao.WebEnderecoHomologacao);

            configuracao.WebSoapString = envelopeEvento;
            var tagRetornoEvento = configuracao.WebTagRetorno;
            var tagRetornoConsulta = consulta.Configuracoes.WebTagRetorno;

            void RejeitarTagDeRetorno()
            {
                var erroRetorno = Assert.Throws<ValidarXMLException>(() => servico.Executar(segundo, configuracao));
                Assert.Contains("tag de retorno SOAP", erroRetorno.Message);
                Assert.Empty(servico.GetConteudoXMLOriginal());
                Assert.Empty(servico.GetConteudoXMLAssinado());
                Assert.Equal(0, servico.Result.InfEvento.CStat);
                Assert.False(servidor.Pending());
            }

            configuracao.WebTagRetorno = tagRetornoConsulta;
            RejeitarTagDeRetorno();

            configuracao.WebTagRetorno = tagRetornoEvento;
            configuracao.WebTagRetornoHomologacao = tagRetornoConsulta;
            RejeitarTagDeRetorno();

            configuracao.WebTagRetorno = tagRetornoConsulta;
            configuracao.WebTagRetornoHomologacao = tagRetornoEvento;
            configuracao.RequestURIHomologacao = urlSoapCustomizada + "api-anterior";
            Assert.True(configuracao.IsAPI);
            var erroApi = Assert.Throws<ValidarXMLException>(() => servico.Executar(segundo, configuracao));
            Assert.Contains("exige transporte SOAP", erroApi.Message);
            Assert.Empty(servico.GetConteudoXMLOriginal());
            Assert.Empty(servico.GetConteudoXMLAssinado());
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            Assert.False(servidor.Pending());

            configuracao.RequestURIHomologacao = null;
            Assert.Equal(urlSoapCustomizada, configuracao.WebEnderecoHomologacao);
            await EnviarEConferirAsync(segundo, "1412600000000002");

            segundo.InfEvento.TpAmb = TipoAmbiente.Producao;
            var erro = Assert.Throws<ValidarXMLException>(() => servico.Executar(segundo, configuracao));
            Assert.Contains("tpAmb de <infEvento> (1)", erro.Message);
            Assert.Empty(servico.GetConteudoXMLOriginal());
            Assert.Empty(servico.GetConteudoXMLAssinado());
            Assert.Equal(0, servico.Result.InfEvento.CStat);
            Assert.False(servidor.Pending());
        }
#endif

        private static string CriarRespostaSoap(string protocolo) => $@"<s:Envelope xmlns:s=""http://www.w3.org/2003/05/soap-envelope"">
  <s:Body>
    <nfeabiRecepcaoEventoResult xmlns=""{NamespaceWsdl}"">
      <retEventoNFeABI xmlns=""{NamespaceEvento}"" versao=""1.00"">
        <infEvento>
          <tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cOrgao>41</cOrgao>
          <cStat>135</cStat><xMotivo>Evento registrado</xMotivo>
          <dhRegEvento>2026-09-14T12:00:00-03:00</dhRegEvento><nProt>{protocolo}</nProt>
        </infEvento>
      </retEventoNFeABI>
    </nfeabiRecepcaoEventoResult>
  </s:Body>
</s:Envelope>";

        private static async Task<string> ResponderSoapAsync(TcpListener servidor, string xml, CancellationToken limite)
        {
            using var cliente = await servidor.AcceptTcpClientAsync(limite);
            using var stream = cliente.GetStream();
            // Latin1 preserva a contagem de bytes indicada pelo Content-Length.
            using var leitor = new StreamReader(stream, Encoding.Latin1, detectEncodingFromByteOrderMarks: false, leaveOpen: true);
            var tamanho = 0;
            string linha;
            while (!string.IsNullOrEmpty(linha = await leitor.ReadLineAsync(limite)))
            {
                const string cabecalho = "Content-Length:";
                if (linha.StartsWith(cabecalho, StringComparison.OrdinalIgnoreCase))
                {
                    tamanho = int.Parse(linha.Substring(cabecalho.Length));
                }
            }

            Assert.True(tamanho > 0);
            var corpo = new char[tamanho];
            Assert.Equal(tamanho, await leitor.ReadBlockAsync(corpo.AsMemory(), limite));

            var resposta = Encoding.UTF8.GetBytes(xml);
            var headers = Encoding.ASCII.GetBytes(
                $"HTTP/1.1 200 OK\r\nContent-Type: application/soap+xml; charset=utf-8\r\nContent-Length: {resposta.Length}\r\nConnection: close\r\n\r\n");
            await stream.WriteAsync(headers, limite);
            await stream.WriteAsync(resposta, limite);
            return Encoding.UTF8.GetString(Encoding.Latin1.GetBytes(corpo));
        }

        private static EventoNFeABI CarregarEvento(string nome) =>
            LerEvento(Path.Combine(@"..\..\..\NFeABI\Resources", $"eventoNFeABI-{nome}-valido.xml"));

        private static EventoNFeABI LerEvento(string caminho)
        {
            var xml = new XmlDocument();
            xml.Load(caminho);
            return XMLUtility.Deserializar<EventoNFeABI>(xml);
        }

        private static X509Certificate2 CriarCertificado()
        {
            using var rsa = RSA.Create(2048);
            var requisicao = new CertificateRequest("CN=NFeABI Evento Teste", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
            return requisicao.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
        }
    }
}
