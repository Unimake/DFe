using System;
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

namespace Unimake.DFe.Test.NFe.BugFixes
{
    [Trait("DFe", "NFe")]
    public class LoteEventosPreservacaoTest
    {
        [Theory]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        public void DesserializacaoGenerica_DevePreservarCadaEventoEDetalhamento(int quantidade)
        {
            var modelo = CriarLote(quantidade, false);
            var xml = modelo.GerarXML();
            var generico = XMLUtility.Deserializar<EnvEvento>(xml);
            ConferirEventos(generico, quantidade);
            ConferirEventos(new EnvEvento().LerXML<EnvEvento>(xml), quantidade);
            ConferirEventos(new EnvEvento().LoadFromXML(xml.OuterXml), quantidade);
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void ValidacaoDiretaEConstrutor_DevePreservarLoteEAssinaturas(bool nfce)
        {
            using (var certificado = CriarCertificado())
            {
                var modelo = CriarLote(3, nfce);
                var xml = modelo.GerarXML();
                var configuracao = new Configuracao { TipoDFe = nfce ? TipoDFe.NFCe : TipoDFe.NFe, CodigoUF = 41, TipoAmbiente = TipoAmbiente.Homologacao, CertificadoDigital = certificado };
                var validador = new ValidarEstruturaXML();
                var resultado = validador.ValidarServico(xml, configuracao);
                Assert.True(resultado.Validado, resultado.MensagemRetorno);
                ConferirEventos(new EnvEvento().LerXML<EnvEvento>(xml), 3);
                ConferirAssinaturas(xml, 3);
                resultado = validador.ValidarServico(xml, configuracao);
                Assert.True(resultado.Validado, resultado.MensagemRetorno);
                ConferirAssinaturas(xml, 3);

                using (var servico = nfce
                    ? (Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento)new Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento(modelo, configuracao)
                    : new Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(modelo, configuracao))
                {
                    ConferirEventos(servico.EnvEvento, 3);
                    ConferirAssinaturas(servico.ConteudoXMLAssinado, 3);
                }
            }
        }

        [Theory]
        [InlineData(false, false)]
        [InlineData(false, true)]
        [InlineData(true, false)]
        [InlineData(true, true)]
        public void Distribuicao_DeveCorrelacionarRespostasInvertidas(bool nfce, bool comId)
        {
            using (var certificado = CriarCertificado())
            {
                var config = new Configuracao { TipoDFe = nfce ? TipoDFe.NFCe : TipoDFe.NFe, CodigoUF = 41, CertificadoDigital = certificado };
                using (var servico = nfce
                    ? (Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento)new Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento(CriarLote(2, true), config)
                    : new Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(CriarLote(2, false), config))
                {
                    var resposta = new RetEnvEvento
                    {
                        Versao = "1.00", IdLote = "123", TpAmb = TipoAmbiente.Homologacao, COrgao = UFBrasil.PR,
                        CStat = 128, XMotivo = "LOTE SINTETICO PROCESSADO",
                        RetEvento = servico.EnvEvento.Evento.AsEnumerable().Reverse().Select(e => new RetEvento
                        {
                            Versao = "1.00", InfEvento = new InfEventoRetEvento
                            {
                                Id = comId ? e.InfEvento.Id : null, TpAmb = e.InfEvento.TpAmb,
                                COrgao = UFBrasil.PR, CStat = e.InfEvento.NSeqEvento == 1 ? 135 : 573,
                                XMotivo = "RESULTADO SINTETICO", ChNFe = e.InfEvento.ChNFe,
                                TpEvento = e.InfEvento.TpEvento, NSeqEvento = e.InfEvento.NSeqEvento,
                                DhRegEventoField = "2026-09-24T12:05:00-03:00", NProt = "141260000000101"
                            }
                        }).ToList()
                    };
                    servico.RetornoWSXML = XMLUtility.Serializar(resposta);
                    servico.RetornoWSString = servico.RetornoWSXML.OuterXml;
                    var distribuicoes = servico.ProcEventoNFeResult;
                    Assert.Equal(2, distribuicoes.Count);
                    foreach (var item in distribuicoes)
                    {
                        Assert.Equal(item.Evento.InfEvento.NSeqEvento, item.RetEvento.InfEvento.NSeqEvento);
                        Assert.Equal(item.Evento.InfEvento.NSeqEvento == 1 ? 135 : 573, item.RetEvento.InfEvento.CStat);
                    }
                    foreach (var falha in new[] { "duplicado", "id-divergente", "ambiente-divergente", "ausente" })
                    {
                        var invalido = XMLUtility.Deserializar<RetEnvEvento>(XMLUtility.Serializar(resposta));
                        switch (falha)
                        {
                            case "duplicado": invalido.RetEvento[0] = invalido.RetEvento[1]; break;
                            case "id-divergente": invalido.RetEvento[1].InfEvento.Id = "ID-DIVERGENTE"; break;
                            case "ambiente-divergente": invalido.RetEvento[1].InfEvento.TpAmb = TipoAmbiente.Producao; break;
                            case "ausente": invalido.RetEvento.RemoveAt(1); break;
                        }
                        servico.RetornoWSXML = XMLUtility.Serializar(invalido);
                        servico.RetornoWSString = servico.RetornoWSXML.OuterXml;
                        var erro = Assert.Throws<InvalidOperationException>(() => servico.ProcEventoNFeResult);
                        if (falha == "duplicado") Assert.Contains("ambígua", erro.Message);
                        Assert.DoesNotContain(servico.EnvEvento.Evento[0].InfEvento.ChNFe, erro.Message);
                    }
                }
            }
        }

        private static void ConferirEventos(EnvEvento lote, int quantidade)
        {
            Assert.Equal(quantidade, lote.Evento.Count);
            Assert.Equal("123", lote.IdLote);
            for (var i = 0; i < quantidade; i++)
            {
                Assert.Equal(i + 1, lote.Evento[i].InfEvento.NSeqEvento);
                Assert.Equal("CORRECAO SINTETICA DO EVENTO " + (i + 1), ((DetEventoCCE)lote.Evento[i].InfEvento.DetEvento).XCorrecao);
            }
        }

        private static void ConferirAssinaturas(XmlDocument xml, int quantidade)
        {
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("nfe", "http://www.portalfiscal.inf.br/nfe");
            ns.AddNamespace("ds", SignedXml.XmlDsigNamespaceUrl);
            var eventos = xml.SelectNodes("/nfe:envEvento/nfe:evento", ns);
            Assert.Equal(quantidade, eventos.Count);
            foreach (XmlElement evento in eventos)
            {
                var assinatura = new SignedXml(evento);
                assinatura.LoadXml((XmlElement)evento.SelectSingleNode("ds:Signature", ns));
                Assert.True(assinatura.CheckSignature());
                var id = evento.SelectSingleNode("nfe:infEvento/@Id", ns).Value;
                Assert.Equal("#" + id, ((Reference)assinatura.SignedInfo.References[0]).Uri);
            }
        }

        private static EnvEvento CriarLote(int quantidade, bool nfce)
        {
            var chave = nfce ? "41260899999999000191650010000000011000000014" : "41260899999999000191550010000000011000000014";
            return new EnvEvento
            {
                Versao = "1.00", IdLote = "123",
                Evento = Enumerable.Range(1, quantidade).Select(i => new Evento
                {
                    Versao = "1.00", InfEvento = new InfEvento(new DetEventoCCE { Versao = "1.00", XCorrecao = "CORRECAO SINTETICA DO EVENTO " + i })
                    {
                        COrgao = UFBrasil.PR, TpAmb = TipoAmbiente.Homologacao, CNPJ = "99999999000191", ChNFe = chave,
                        DhEventoField = "2026-09-24T12:00:00-03:00", TpEvento = TipoEventoNFe.CartaCorrecao, NSeqEvento = i, VerEvento = "1.00"
                    }
                }).ToList()
            };
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var chave = RSA.Create())
            {
                chave.KeySize = 2048;
                var pedido = new CertificateRequest("CN=TESTE SINTETICO LOTE EVENTOS", chave, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddYears(1)))
                    return new X509Certificate2(certificado.Export(X509ContentType.Pfx, "teste"), "teste", X509KeyStorageFlags.Exportable | X509KeyStorageFlags.EphemeralKeySet);
            }
        }
    }
}
