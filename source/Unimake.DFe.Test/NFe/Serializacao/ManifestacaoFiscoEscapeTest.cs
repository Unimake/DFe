using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    [Trait("DFe", "NFe")]
    public class ManifestacaoFiscoEscapeTest
    {
        private const string Ns = "http://www.portalfiscal.inf.br/nfe";
        private const string CaminhoDetalhe = "/n:envEvento/n:evento/n:infEvento/n:detEvento";

        public static IEnumerable<object[]> Textos()
        {
            foreach (var codigo in new[] { 412120, 412130 })
                foreach (var texto in new[] { "MOTIVO SIMPLES", "MOTIVO Á Ç", "A & B", "A < B > C", "ASPAS \"DUPLAS\" E 'SIMPLES'", "A &amp; B", "A &lt; B &gt; C", "TEXTO &inexistente;", "A &amp; B <C> & D" })
                    yield return new object[] { codigo, texto };
        }

        [Theory]
        [MemberData(nameof(Textos))]
        public void GeracaoERoundTrip_DevemEscaparSomenteNaEscrita(int codigo, string texto)
        {
            var evento = CriarEvento(codigo, texto);
            Assert.Equal(texto, Detalhe(evento).XMotivo);
            var xml = evento.GerarXML();
            ConferirDetalhe(xml, codigo, texto);
            foreach (var leitura in new[]
            {
                XMLUtility.Deserializar<EnvEvento>(xml),
                new EnvEvento().LerXML<EnvEvento>(xml),
                new EnvEvento().LoadFromXML(xml.OuterXml)
            })
            {
                Assert.Equal(texto, Detalhe(leitura).XMotivo);
                ConferirDetalhe(leitura.GerarXML(), codigo, texto);
            }
        }

        [Theory]
        [InlineData(412120, "A & B <C>")]
        [InlineData(412130, "A & B <C>")]
        [InlineData(412120, "A &amp; B")]
        [InlineData(412130, "A &amp; B")]
        [InlineData(412120, "ASPAS \"DUPLAS\" E 'SIMPLES'")]
        [InlineData(412130, "ASPAS \"DUPLAS\" E 'SIMPLES'")]
        public void XmlRecebido_DevePreservarTextoEscapadoNoRoundTrip(int codigo, string texto)
        {
            var xml = CriarEvento(codigo, "MOTIVO SIMPLES").GerarXML();
            Selecionar(xml, CaminhoDetalhe + "/n:xMotivo").InnerText = texto;
            ValidarDetalhe(xml, codigo);
            var lido = new EnvEvento().LoadFromXML(xml.OuterXml);
            Assert.Equal(texto, Detalhe(lido).XMotivo);
            ConferirDetalhe(lido.GerarXML(), codigo, texto);
        }

        public static IEnumerable<object[]> ValoresPublicos()
        {
            foreach (var codigo in new[] { 412120, 412130 })
                foreach (var texto in new[] { null, "", "  A &amp; B  ", new string('A', 600) })
                    yield return new object[] { codigo, texto };
        }

        [Theory]
        [MemberData(nameof(ValoresPublicos))]
        public void Propriedade_DeveContinuarSemNormalizarDecodificarOuTruncar(int codigo, string texto)
        {
            Assert.Equal(texto, Detalhe(CriarEvento(codigo, texto)).XMotivo);
        }

        public static IEnumerable<object[]> LimitesSchema()
        {
            foreach (var codigo in new[] { 412120, 412130 })
            {
                yield return new object[] { codigo, null, false };
                yield return new object[] { codigo, "", false };
                yield return new object[] { codigo, new string('A', 499) + "&", true };
                yield return new object[] { codigo, new string('A', 500) + "&", false };
            }
        }

        [Theory]
        [MemberData(nameof(LimitesSchema))]
        public void Schema_DeveContinuarValidandoComprimentoDoValor(int codigo, string texto, bool valido)
        {
            var evento = CriarEvento(codigo, texto);
            Assert.Equal(texto, Detalhe(evento).XMotivo);
            var xml = evento.GerarXML();
            Assert.Equal(texto ?? "", Selecionar(xml, CaminhoDetalhe + "/n:xMotivo").InnerText);
            var validar = ValidarDetalhe(xml, codigo, exigirSucesso: false);
            Assert.Equal(valido, validar.Success);
            if (!valido) Assert.Contains("xMotivo", validar.ErrorMessage);
        }

        [Theory]
        [InlineData(412120, "MOTIVO SIMPLES")]
        [InlineData(412130, "MOTIVO SIMPLES")]
        [InlineData(412120, "AÇÃO & <OFFLINE>")]
        [InlineData(412130, "AÇÃO & <OFFLINE>")]
        [InlineData(412120, "LITERAL &amp; E &lt;TEXTO&gt;")]
        [InlineData(412130, "LITERAL &amp; E &lt;TEXTO&gt;")]
        public void DocumentoRecebido_DevePreservarAssinaturaSemReassinar(int codigo, string texto)
        {
            using (var certificado = CriarCertificado())
            {
                var xml = CriarEvento(codigo, texto).GerarXML();
                ConferirDetalhe(xml, codigo, texto);
                AssinaturaDigital.Assinar(xml, "evento", "infEvento", certificado);
                var eventoAssinado = Selecionar(xml, "/n:envEvento/n:evento");
                ConferirAssinatura(eventoAssinado);
                var original = eventoAssinado.OuterXml;
                var modelo = XMLUtility.Deserializar<Evento>(original);
                var recebido = new ProcEventoNFe
                {
                    Versao = "1.00", Evento = modelo, RetEvento = new RetEvento
                    {
                        Versao = "1.00", InfEvento = new InfEventoRetEvento
                        {
                            TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE-OFFLINE", COrgao = UFBrasil.PR,
                            CStat = 135, XMotivo = "RETORNO SINTETICO", ChNFe = modelo.InfEvento.ChNFe,
                            TpEvento = (TipoEventoNFe)codigo, NSeqEvento = 1, NProt = "141260000000001",
                            DhRegEventoField = "2026-11-18T12:40:00-03:00"
                        }
                    }
                }.GerarXML();
                var validar = new ValidarSchema();
                validar.Validar(recebido, "NFe.procEventoNFe_v1.00.xsd", Ns);
                Assert.True(validar.Success, validar.ErrorMessage);
                var lido = new ProcEventoNFe();
                lido.ReadXml(recebido);
                var regenerado = lido.GerarXML();
                var atual = Selecionar(regenerado, "/n:procEventoNFe/n:evento");
                Assert.Equal(original, atual.OuterXml);
                ConferirAssinatura(atual);
                Assert.Equal(texto, Selecionar(regenerado, "/n:procEventoNFe/n:evento/n:infEvento/n:detEvento/n:xMotivo").InnerText);
                Selecionar(regenerado, "/n:procEventoNFe/n:evento/n:infEvento/n:detEvento/n:xMotivo").InnerText = "ADULTERAÇÃO SINTÉTICA";
                var assinatura = new SignedXml(atual);
                assinatura.LoadXml((XmlElement)atual.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
                Assert.False(assinatura.CheckSignature());
            }
        }

        private static EnvEvento CriarEvento(int codigo, string texto)
        {
            DetEventoManifestacaoFiscoPedidoTransferenciaCreditoIBSOperacaoSucessao detalhe = codigo == 412130
                ? new DetEventoManifestacaoFiscoPedidoTransferenciaCreditoCBSOperacaoSucessao()
                : new DetEventoManifestacaoFiscoPedidoTransferenciaCreditoIBSOperacaoSucessao();
            detalhe.Versao = "1.00"; detalhe.COrgaoAutor = UFBrasil.PR; detalhe.TpAutor = TipoAutor.Fisco;
            detalhe.VerAplic = "TESTE-OFFLINE"; detalhe.IndDeferimento = SimNao.Nao;
            detalhe.CMotivo = (MotivoManifestacaoFisco)2; detalhe.XMotivo = texto;
            return new EnvEvento
            {
                Versao = "1.00", IdLote = "1", Evento = new List<Evento>
                {
                    new Evento
                    {
                        Versao = "1.00", InfEvento = new InfEvento(detalhe)
                        {
                            COrgao = UFBrasil.PR, TpAmb = TipoAmbiente.Homologacao, CNPJ = "99999999000191",
                            ChNFe = XMLUtility.MontarChaveDFe(UFBrasil.PR, new DateTime(2026, 11, 18), "99999999000191", ModeloDFe.NFe, 59, 50, TipoEmissao.Normal, "10000001"),
                            DhEventoField = "2026-11-18T12:36:00-03:00", TpEvento = (TipoEventoNFe)codigo, NSeqEvento = 1, VerEvento = "1.00"
                        }
                    }
                }
            };
        }

        private static DetEventoManifestacaoFiscoPedidoTransferenciaCreditoIBSOperacaoSucessao Detalhe(EnvEvento evento) =>
            Assert.IsAssignableFrom<DetEventoManifestacaoFiscoPedidoTransferenciaCreditoIBSOperacaoSucessao>(evento.Evento[0].InfEvento.DetEvento);

        private static void ConferirDetalhe(XmlDocument xml, int codigo, string texto)
        {
            var detalhe = Selecionar(xml, CaminhoDetalhe);
            Assert.Equal(new[] { "descEvento", "cOrgaoAutor", "tpAutor", "verAplic", "indDeferimento", "cMotivo", "xMotivo" }, detalhe.ChildNodes.OfType<XmlElement>().Select(n => n.LocalName));
            Assert.Equal("1.00", detalhe.GetAttribute("versao"));
            Assert.Equal(Ns, detalhe.NamespaceURI);
            Assert.Equal("41", Selecionar(xml, CaminhoDetalhe + "/n:cOrgaoAutor").InnerText);
            Assert.Equal("5", Selecionar(xml, CaminhoDetalhe + "/n:tpAutor").InnerText);
            Assert.Equal("TESTE-OFFLINE", Selecionar(xml, CaminhoDetalhe + "/n:verAplic").InnerText);
            Assert.Equal("0", Selecionar(xml, CaminhoDetalhe + "/n:indDeferimento").InnerText);
            Assert.Equal("2", Selecionar(xml, CaminhoDetalhe + "/n:cMotivo").InnerText);
            var motivo = Selecionar(xml, CaminhoDetalhe + "/n:xMotivo");
            Assert.Equal(texto, motivo.InnerText);
            Assert.Empty(motivo.ChildNodes.OfType<XmlElement>());
            ValidarDetalhe(xml, codigo);
        }

        private static ValidarSchema ValidarDetalhe(XmlDocument xml, int codigo, bool exigirSucesso = true)
        {
            var detalhe = new XmlDocument { XmlResolver = null };
            detalhe.AppendChild(detalhe.ImportNode(Selecionar(xml, CaminhoDetalhe), true));
            var validar = new ValidarSchema();
            validar.Validar(detalhe, "NFe.e" + codigo + "_v1.00.xsd", Ns);
            if (exigirSucesso) Assert.True(validar.Success, validar.ErrorMessage);
            return validar;
        }

        private static XmlElement Selecionar(XmlDocument xml, string caminho)
        {
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("n", Ns);
            return (XmlElement)xml.SelectSingleNode(caminho, ns);
        }

        private static void ConferirAssinatura(XmlElement evento)
        {
            var assinatura = new SignedXml(evento);
            assinatura.LoadXml((XmlElement)evento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
            Assert.True(assinatura.CheckSignature());
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var chave = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=R04-C01 SINTETICO", chave, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                    return X509CertificateLoader.LoadPkcs12(certificado.Export(X509ContentType.Pfx, "teste"), "teste", X509KeyStorageFlags.Exportable | X509KeyStorageFlags.EphemeralKeySet);
            }
        }
    }
}
