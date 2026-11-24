using System;
using System.IO;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
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
