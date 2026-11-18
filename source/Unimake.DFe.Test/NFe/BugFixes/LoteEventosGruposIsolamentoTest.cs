using System;
using System.Collections.Generic;
using System.Linq;
using System.Collections;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.BugFixes
{
    [Trait("DFe", "NFe")]
    public class LoteEventosGruposIsolamentoTest
    {
        private static readonly int[] Codigos = { 111500, 111501, 110750, 112120, 211124, 112130, 112140, 211110, 211130, 211140, 211150 };
        private const string Ns = "http://www.portalfiscal.inf.br/nfe";

        public static IEnumerable<object[]> Lotes()
        {
            foreach (var codigo in Codigos)
            foreach (var misto in new[] { false, true })
            foreach (var invertido in new[] { false, true })
                yield return new object[] { codigo, misto, invertido };
        }

        [Theory]
        [MemberData(nameof(Lotes))]
        public void Leitores_DevemIsolarGruposEManterOrdem(int codigo, bool misto, bool invertido)
        {
            var modelo = CriarLote(codigo, misto, invertido);
            var xml = modelo.GerarXML();
            var original = xml.OuterXml;
            foreach (var leitura in new[]
            {
                XMLUtility.Deserializar<EnvEvento>(xml),
                new EnvEvento().LoadFromXML(xml.OuterXml),
                new EnvEvento().LerXML<EnvEvento>(xml)
            })
            {
                Conferir(modelo, xml, leitura);
                Conferir(modelo, xml, new EnvEvento().LoadFromXML(leitura.GerarXML().OuterXml));
            }
            Assert.Equal(original, xml.OuterXml);
        }

        public static IEnumerable<object[]> Preparadores() => Codigos.Select(c => new object[] { c });

        [Theory]
        [MemberData(nameof(Preparadores))]
        public void GrupoAusente_NaoDeveImportarItensDoVizinho(int codigo)
        {
            var modelo = CriarLote(codigo, true, false);
            var detalhe = modelo.Evento.Single(e => (int)e.InfEvento.TpEvento == codigo && e.InfEvento.NSeqEvento == 2).InfEvento.DetEvento;
            Lista(detalhe).Clear();
            var xml = modelo.GerarXML();
            Conferir(modelo, xml, new EnvEvento().LoadFromXML(xml.OuterXml));
        }

        [Theory]
        [InlineData(false, false)]
        [InlineData(false, true)]
        [InlineData(true, false)]
        [InlineData(true, true)]
        public void LoteRtcEConciliacao_DevePrepararAssinarEDistribuirSemTransporte(bool cpf, bool invertido)
        {
            var modelo = new EnvEvento
            {
                Versao = "1.00", IdLote = "123", Evento = new List<Evento>
                {
                    CriarEvento(112150, 1, 50), CriarEvento(211128, 1, 51),
                    CriarEvento(110750, 1, 50), CriarEvento(110750, 2, 50), CriarEvento(110751, 1, 51)
                }
            };
            foreach (var evento in modelo.Evento)
            {
                evento.InfEvento.COrgao = UFBrasil.SVRS;
                if (cpf) { evento.InfEvento.CNPJ = null; evento.InfEvento.CPF = "12345678909"; }
            }
            if (invertido) modelo.Evento.Reverse();
            using (var chave = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=LOTE SINTETICO ISOLAMENTO", chave, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var original = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddYears(1)))
                using (var certificado = X509CertificateLoader.LoadPkcs12(original.Export(X509ContentType.Pfx, "teste"), "teste", X509KeyStorageFlags.Exportable | X509KeyStorageFlags.EphemeralKeySet))
                using (var servico = new Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(modelo, new Configuracao
                {
                    TipoDFe = TipoDFe.NFe, CodigoUF = 92, TipoAmbiente = TipoAmbiente.Homologacao,
                    TipoEmissao = TipoEmissao.Normal, CertificadoDigital = certificado
                }))
                {
                    var assinado = servico.ConteudoXMLAssinado;
                    Conferir(modelo, modelo.GerarXML(), new EnvEvento().LoadFromXML(assinado.OuterXml));
                    ConferirAssinaturas(assinado);
                    var literais = assinado.GetElementsByTagName("evento", Ns).Cast<XmlElement>().ToDictionary(e => ((XmlElement)e.GetElementsByTagName("infEvento", Ns)[0]).GetAttribute("Id"), e => e.OuterXml);
                    servico.RetornoWSXML = XMLUtility.Serializar(new RetEnvEvento
                    {
                        Versao = "1.00", IdLote = "123", TpAmb = TipoAmbiente.Homologacao, COrgao = UFBrasil.SVRS,
                        CStat = 128, XMotivo = "RESULTADO SINTETICO",
                        RetEvento = modelo.Evento.AsEnumerable().Reverse().Select(e => new RetEvento
                        {
                            Versao = "1.00", InfEvento = new InfEventoRetEvento
                            {
                                TpAmb = TipoAmbiente.Homologacao, COrgao = UFBrasil.SVRS, CStat = 135,
                                XMotivo = "RESULTADO SINTETICO", ChNFe = e.InfEvento.ChNFe, TpEvento = e.InfEvento.TpEvento,
                                NSeqEvento = e.InfEvento.NSeqEvento, DhRegEventoField = "2026-11-18T12:05:00-03:00", NProt = "141260000000101"
                            }
                        }).ToList()
                    });
                    servico.RetornoWSString = servico.RetornoWSXML.OuterXml;
                    var distribuicoes = servico.ProcEventoNFeResult;
                    Assert.Equal(5, distribuicoes.Count);
                    foreach (var distribuicao in distribuicoes)
                    {
                        var xml = distribuicao.GerarXML();
                        var evento = (XmlElement)xml.GetElementsByTagName("evento", Ns)[0];
                        Assert.Equal(literais[distribuicao.Evento.InfEvento.Id], evento.OuterXml);
                        ConferirAssinaturas(xml);
                    }
                }
            }
        }

        private static void ConferirAssinaturas(XmlDocument xml)
        {
            foreach (XmlElement evento in xml.GetElementsByTagName("evento", Ns))
            {
                var assinatura = new SignedXml(evento);
                assinatura.LoadXml((XmlElement)evento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
                Assert.True(assinatura.CheckSignature());
                Assert.Equal("#" + ((XmlElement)evento.GetElementsByTagName("infEvento", Ns)[0]).GetAttribute("Id"), ((Reference)assinatura.SignedInfo.References[0]).Uri);
            }
        }

        [Theory]
        [MemberData(nameof(Preparadores))]
        public void PreparadorPublico_DeveSerIsoladoEIdempotente(int codigo)
        {
            var modelo = CriarLote(codigo, true, true);
            var xml = modelo.GerarXML();
            var leitura = new EnvEvento().LoadFromXML(xml.OuterXml);
            for (var repeticao = 0; repeticao < 2; repeticao++)
            {
                switch (codigo)
                {
                    case 111500: case 111501: leitura.PreparaItemPedido(xml); break;
                    case 110750: leitura.PreparaDetPag(xml); break;
                    case 112120: leitura.PrepararGConsumo(xml); break;
                    case 211124: leitura.PrepararGPerecimentoAdquirente(xml); break;
                    case 112130: leitura.PrepararGPerecimento(xml); break;
                    case 112140: leitura.PrepararGItemNaoFornecido(xml); break;
                    case 211110: leitura.PrepararGCredPres(xml); break;
                    case 211130: leitura.PrepararGImobilizado(xml); break;
                    case 211140: leitura.PrepararGConsumoComb(xml); break;
                    case 211150: leitura.PrepararGCredito(xml); break;
                }
                Conferir(modelo, xml, leitura);
            }
        }

        [Fact]
        public void TodosOsGruposNoMesmoLote_DevemPermanecerIsoladosNasDuasOrdens()
        {
            var modelo = new EnvEvento { Versao = "1.00", IdLote = "123" };
            foreach (var codigo in Codigos)
            {
                modelo.Evento.Add(CriarEvento(codigo, 1, 50));
                modelo.Evento.Add(CriarEvento(codigo, 2, 50));
            }
            for (var ordem = 0; ordem < 2; ordem++)
            {
                var xml = modelo.GerarXML();
                Conferir(modelo, xml, XMLUtility.Deserializar<EnvEvento>(xml));
                modelo.Evento.Reverse();
            }
        }

        private static EnvEvento CriarLote(int codigo, bool misto, bool invertido)
        {
            var lote = new EnvEvento { Versao = "1.00", IdLote = "123" };
            lote.Evento.Add(CriarEvento(codigo, 1, 50));
            lote.Evento.Add(CriarEvento(codigo, 2, 50));
            lote.Evento.Add(CriarEvento(codigo, 1, 51));
            if (misto)
            {
                lote.Evento.Insert(0, CriarEvento(110110, 1, 52));
                lote.Evento.Insert(2, CriarEvento(codigo == 112130 ? 211124 : 112130, 3, 53));
            }
            if (invertido) lote.Evento.Reverse();
            return lote;
        }

        private static IList Lista(EventoDetalhe detalhe) =>
            (IList)detalhe.GetType().GetProperties().Single(p => p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() == typeof(List<>)).GetValue(detalhe);

        private static Evento CriarEvento(int codigo, int sequencia, int numero, bool incluirSegundoItem = true)
        {
            var marca = numero * 10 + sequencia;
            EventoDetalhe detalhe;
            switch (codigo)
            {
                case 111500: case 111501:
                    detalhe = new DetEventoPedidoProrrogPrazoICMS { NProt = "141260000000101", ItemPedido = new List<ItemPedidoProrrogPrazoICMS> { new ItemPedidoProrrogPrazoICMS { NumItem = marca, QtdeItem = marca + 0.25 } } }; break;
                case 110750:
                    detalhe = new DetEventoConciliacaoFinanceira { VerAplic = "TESTE", DetPag = new List<DetPagECONF> { new DetPagECONF { TPag = MeioPagamento.Outros, XPag = "PAGAMENTO " + marca, VPag = marca + 0.25, DPag = new DateTime(2026, 11, 18) } } }; break;
                case 112120:
                    detalhe = new DetEventoImportacaoALCZFMNaoConvertidaIsencao { GConsumo = new List<GConsumo> { new GConsumo { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoque { Qtde = marca, Unidade = "UN" } } } }; break;
                case 211124:
                    detalhe = new DetEventoPerecimentoDuranteTransporteContratadoAdquirente { GPerecimento = new List<GPerecimentoAdquirente> { new GPerecimentoAdquirente { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoquePerecimentoAdquirente { QPerecimento = marca, UPerecimento = "UN" } } } }; break;
                case 112130:
                    detalhe = new DetEventoPerecimentoDuranteTransporteContratadoFornecedor { GPerecimento = new List<GPerecimento> { new GPerecimento { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoquePerecimento { QPerecimento = marca, UPerecimento = "UN", VIBS = marca + 1, VCBS = marca + 2 } } } }; break;
                case 112140:
                    detalhe = new DetEventoFornecimentoNaoRealizadoComPagamentoAntecipado { GItemNaoFornecido = new List<GItemNaoFornecido> { new GItemNaoFornecido { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoqueFornecimento { QNaoFornecida = marca, UNaoFornecida = "UN" } } } }; break;
                case 211110:
                    detalhe = new DetEventoSolicitacaoApropriacaoCreditoPresumido { GCredPresOper = new List<GCredPresOperEvento> { new GCredPresOperEvento { NItem = marca, VBCCredPres = marca + 0.25, CCredPres = "01", GIBSCredPres = new GIBSCredPresEvento { PCredPres = 1, VCredPres = marca }, GCBSCredPres = new GCBSCredPresEvento { PCredPres = 2, VCredPres = marca + 2 } } } }; break;
                case 211130:
                    detalhe = new DetEventoImobilizacaoItem { GImobilizacao = new List<GImobilizacao> { new GImobilizacao { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoqueImobilizacao { QImobilizado = marca, UImobilizado = "UN" } } } }; break;
                case 211140:
                    detalhe = new DetEventoSolicitacaoApropriacaoCreditoCombustivel { GConsumoComb = new List<GConsumoComb> { new GConsumoComb { NItem = marca, VIBS = marca, VCBS = marca + 0.25, GControleEstoque = new GControleEstoqueConsumoComb { QComb = marca, UComb = "UN" } } } }; break;
                case 211150:
                    detalhe = new DetEventoSolicitacaoApropriacaoCreditoBensServicosAdquirente { GCredito = new List<GCredito> { new GCredito { NItem = marca, VCredIBS = marca, VCredCBS = marca + 0.25 } } }; break;
                case 112150:
                    detalhe = new DetEventoAtualizacaoDataPrevisaoEntrega { Versao = "1.00", COrgaoAutor = UFBrasil.PR, TpAutor = (TipoAutor)1, VerAplic = "TESTE", DPrevEntrega = new DateTimeOffset(2026, 12, 8, 12, 0, 0, TimeSpan.FromHours(-3)) }; break;
                case 211128:
                    detalhe = new DetEventoAceiteDebitoApuracaoEmissaoNotaCredito { Versao = "1.00", COrgaoAutor = UFBrasil.PR, TpAutor = (TipoAutor)2, VerAplic = "TESTE", IndAceitacao = SimNao.Sim }; break;
                case 110751:
                    detalhe = new DetEventoCancelamentoConciliacaoFinanceira { Versao = "1.00", VerAplic = "TESTE", NProtEvento = "141260000000101" }; break;
                default: detalhe = new DetEventoCCE { XCorrecao = "CORRECAO SINTETICA" }; break;
            }
            detalhe.Versao = "1.00";
            // Itens tipados distintos também verificam a ordem dentro de cada coleção.
            if (incluirSegundoItem && sequencia == 1 && Codigos.Contains(codigo))
                Lista(detalhe).Add(Lista(CriarEvento(codigo, sequencia + 10, numero, false).InfEvento.DetEvento)[0]);
            return new Evento
            {
                Versao = "1.00",
                InfEvento = new InfEvento(detalhe)
                {
                    COrgao = UFBrasil.PR, TpAmb = TipoAmbiente.Homologacao, CNPJ = "99999999000191",
                    ChNFe = XMLUtility.MontarChaveDFe(UFBrasil.PR, new DateTime(2026, 11, 18), "99999999000191", ModeloDFe.NFe, 59, numero, TipoEmissao.Normal, "10000001"),
                    DhEventoField = "2026-11-18T12:00:00-03:00", TpEvento = (TipoEventoNFe)codigo, NSeqEvento = sequencia, VerEvento = "1.00"
                }
            };
        }

        private static void Conferir(EnvEvento modelo, XmlDocument xml, EnvEvento leitura)
        {
            Assert.Equal(modelo.Evento.Count, leitura.Evento.Count);
            Assert.Equal(modelo.IdLote, leitura.IdLote);
            var gerado = leitura.GerarXML();
            var antes = xml.GetElementsByTagName("evento", Ns).Cast<XmlElement>().ToArray();
            var depois = gerado.GetElementsByTagName("evento", Ns).Cast<XmlElement>().ToArray();
            for (var i = 0; i < modelo.Evento.Count; i++)
            {
                Assert.Equal(modelo.Evento[i].InfEvento.Id, leitura.Evento[i].InfEvento.Id);
                Assert.Equal(modelo.Evento[i].InfEvento.TpAmb, leitura.Evento[i].InfEvento.TpAmb);
                Assert.Equal(modelo.Evento[i].InfEvento.DetEvento.GetType(), leitura.Evento[i].InfEvento.DetEvento.GetType());
                var grupos = new[] { "itemPedido", "detPag", "gConsumo", "gPerecimento", "gItemNaoFornecido", "gCredPresOper", "gImobilizacao", "gConsumoComb", "gCredito" };
                foreach (var nome in grupos)
                {
                    var origem = antes[i].GetElementsByTagName(nome, Ns).Cast<XmlElement>().ToArray();
                    var destino = depois[i].GetElementsByTagName(nome, Ns).Cast<XmlElement>().ToArray();
                    Assert.Equal(origem.Length, destino.Length);
                    for (var j = 0; j < origem.Length; j++) ConferirArvore(origem[j], destino[j]);
                }
            }
        }

        private static void ConferirArvore(XmlElement esperado, XmlElement atual)
        {
            Assert.Equal(esperado.LocalName, atual.LocalName);
            Assert.Equal(esperado.NamespaceURI, atual.NamespaceURI);
            Assert.Equal(esperado.GetAttribute("nItem"), atual.GetAttribute("nItem"));
            Assert.Equal(esperado.GetAttribute("numItem"), atual.GetAttribute("numItem"));
            var filhos = esperado.ChildNodes.OfType<XmlElement>().ToArray();
            var outros = atual.ChildNodes.OfType<XmlElement>().ToArray();
            Assert.Equal(filhos.Length, outros.Length);
            if (filhos.Length == 0) Assert.Equal(esperado.InnerText, atual.InnerText);
            for (var i = 0; i < filhos.Length; i++) ConferirArvore(filhos[i], outros[i]);
        }
    }
}
