using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using DeclaracaoServico = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;
using GerarIdServico = Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte;
using RetificacaoServico = Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte;
using ConsultaServico = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;
using ConsultarExcecaoServico = Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao;
using EFreteMapper = Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete.EFreteMapper;
using ProvedorCIOTFactory = Unimake.Business.DFe.Servicos.CIOT.Provedores.ProvedorCIOTFactory;
using OrigemCIOT = Unimake.Business.DFe.Xml.CIOT.Origem;

namespace Unimake.DFe.Test.CIOT.Servicos
{
    public class EFreteIntegracaoTest
    {
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProvedorPadraoPermaneceANTT()
        {
            Assert.Equal(ProvedorCIOT.ANTT, new Configuracao().ProvedorCIOT);
            Assert.Equal("ProvedorANTT", ProvedorCIOTFactory.Criar(ProvedorCIOT.ANTT).GetType().Name);
            Assert.Equal("ProvedorEFrete", ProvedorCIOTFactory.Criar(ProvedorCIOT.EFrete).GetType().Name);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void AusenciaDaTagProvedorSempreSelecionaANTT()
        {
            var configuracao = CriarConfiguracao();
            new ConsultaServico(new ConsultarCIOTGerado
            {
                CodigoIdentificacaoOperacao = "123456789012",
                AnoDeclaracao = 2026
            }, configuracao);

            Assert.Equal(ProvedorCIOT.ANTT, configuracao.ProvedorCIOT);
            Assert.Contains("antt.gov.br", configuracao.RequestURI, StringComparison.OrdinalIgnoreCase);

            var configuracaoXml = CriarConfiguracao();
            var xmlLegado = System.IO.File.ReadAllText(@"..\..\..\CIOT\Resources\consultarCIOTGeradoSemProvedor.xml");
            new ConsultaServico(xmlLegado, configuracaoXml);

            Assert.Equal(ProvedorCIOT.ANTT, configuracaoXml.ProvedorCIOT);
            Assert.Contains("antt.gov.br", configuracaoXml.RequestURI, StringComparison.OrdinalIgnoreCase);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void TagDoXmlPrevaleceEPermiteAlternarProvedorNaConfiguracao()
        {
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao, EFreteIntegrador = "INTEGRADOR-TESTE", EFreteToken = "TOKEN-TESTE" };
            var xmlEFrete = System.IO.File.ReadAllText(@"..\..\..\CIOT\Resources\efrete-consultar-ciot-gerado.xml");
            new ConsultaServico(xmlEFrete, configuracao);

            Assert.Equal(ProvedorCIOT.EFrete, configuracao.ProvedorCIOT);
            Assert.Contains("efrete.com.br", configuracao.RequestURI, StringComparison.OrdinalIgnoreCase);

            var xmlANTT = System.IO.File.ReadAllText(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml");
            new ConsultaServico(xmlANTT, configuracao);

            Assert.Equal(ProvedorCIOT.ANTT, configuracao.ProvedorCIOT);
            Assert.Contains("antt.gov.br", configuracao.RequestURI, StringComparison.OrdinalIgnoreCase);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitaProvedorDesconhecidoAntesDeConfigurarTransporte()
        {
            var xml = System.IO.File.ReadAllText(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml")
                .Replace("<ProvedorCIOT>ANTT</ProvedorCIOT>", "<ProvedorCIOT>OUTRO</ProvedorCIOT>");
            var configuracao = new Configuracao { TipoAmbiente = TipoAmbiente.Homologacao };

            Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() => new ConsultaServico(xml, configuracao));
            Assert.Null(configuracao.RequestURI);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public async System.Threading.Tasks.Task ConfiguraEndpointEFreteSemAlterarServicoPublico()
        {
            var configuracao = CriarConfiguracao();
            var servico = new DeclaracaoServico(CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao), configuracao);

            Assert.Contains("efrete.com.br", configuracao.RequestURI);
            Assert.Contains("AdicionarOperacaoTransporteV2", configuracao.RequestURI);
            Assert.Equal(Servico.CIOTDeclaracaoOperacaoTransporte, configuracao.Servico);
            Assert.Equal("CIOT-CLIENTE-001", servico.Envio.IdOperacaoCliente);
            Assert.Equal("post", configuracao.MetodoAPI);
            Assert.DoesNotContain("ProvedorCIOT", await configuracao.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken));
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ConfiguraConsultaEFreteComoGetComJsonNoCorpo()
        {
            var configuracao = CriarConfiguracao();
            new ConsultaServico(new ConsultarCIOTGerado { ProvedorCIOT = ProvedorCIOT.EFrete, MatrizCNPJ = "12345678000199", IdOperacaoCliente = "CIOT-CLIENTE-001" }, configuracao);

            Assert.Equal("get", configuracao.MetodoAPI);
            Assert.Contains("ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoCliente", configuracao.RequestURI);
            Assert.NotNull(configuracao.HttpContent);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(TipoOperacaoTransporteCIOT.CargaLotacao, "Padrao")]
        [InlineData(TipoOperacaoTransporteCIOT.CargaFracionada, "Fracionado")]
        [InlineData(TipoOperacaoTransporteCIOT.TACAgregado, "TAC_Agregado")]
        public void MapeiaModalidadesParaContratoEFrete(TipoOperacaoTransporteCIOT tipo, string esperado)
        {
            var xml = CriarDeclaracao(tipo);
            var json = JObject.Parse(EFreteMapper.CriarJson(xml, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Equal(esperado, json.Value<string>("TipoViagem"));
            Assert.Equal("CIOT-CLIENTE-001", json.Value<string>("IdOperacaoCliente"));
            Assert.Equal("TOKEN-TESTE", json.Value<string>("Token"));
            Assert.Null(json["IdOperacaoTransporte"]);
            Assert.Null(json["ProvedorCIOT"]);

            if (tipo == TipoOperacaoTransporteCIOT.TACAgregado)
            {
                Assert.Null(json["DataInicioViagem"]);
                Assert.Null(json["Viagens"]);
                Assert.Null(json["CodigoNCMNaturezaCarga"]);
                Assert.Null(json["Destinatario"]);
            }
            else
            {
                Assert.NotNull(json["Viagens"]);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void MapeiaGruposCompletosDaDeclaracao()
        {
            var json = JObject.Parse(EFreteMapper.CriarJson(CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao), Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Equal("EMPRESA TESTE", json.SelectToken("Contratante.NomeOuRazaoSocial")?.Value<string>());
            Assert.Equal("PAG-001", json.SelectToken("Pagamentos[0].IdPagamentoCliente")?.Value<string>());
            Assert.Equal("ContaCorrente", json.SelectToken("Pagamentos[0].InformacoesBancarias.TipoConta")?.Value<string>());
            Assert.Equal("DOC-001", json.SelectToken("Viagens[0].DocumentoViagem")?.Value<string>());
            Assert.Equal("NF-001", json.SelectToken("Viagens[0].NotasFiscais.NotaFiscal[0].Numero")?.Value<string>());
            Assert.IsType<JArray>(json.SelectToken("Viagens[0].NotasFiscais.NotaFiscal"));
            Assert.Equal(5000d, json.SelectToken("Viagens[0].Valores.TotalOperacao")?.Value<double>());
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void MapeiaUmaOuMaisNotasFiscaisSempreComoArray()
        {
            var declaracao = CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao);
            var notasFiscais = declaracao.OrigemDestino[0].NotasFiscais;

            var jsonUmaNota = JObject.Parse(EFreteMapper.CriarJson(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));
            var arrayUmaNota = Assert.IsType<JArray>(jsonUmaNota.SelectToken("Viagens[0].NotasFiscais.NotaFiscal"));
            Assert.Single(arrayUmaNota);

            notasFiscais.Add(new NotaFiscalCIOT
            {
                Numero = "NF-002",
                Serie = "1",
                ValorTotal = 20000,
                ValorDaMercadoriaPorUnidade = 2,
                CodigoNCMNaturezaCarga = "2701",
                UnidadeDeMedidaDaMercadoria = "Kg",
                TipoDeCalculo = "SemQuebra",
                QuantidadeDaMercadoriaNoEmbarque = 10000,
                ToleranciaDePerdaDeMercadoria = new ToleranciaCIOT { Tipo = "Nenhum" }
            });

            var jsonDuasNotas = JObject.Parse(EFreteMapper.CriarJson(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));
            var arrayDuasNotas = Assert.IsType<JArray>(jsonDuasNotas.SelectToken("Viagens[0].NotasFiscais.NotaFiscal"));
            Assert.Equal(2, arrayDuasNotas.Count);
            Assert.Equal("NF-002", arrayDuasNotas[1].Value<string>("Numero"));
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void NormalizaRetornosEFreteNoPadraoAtual()
        {
            var declaracao = new RetDeclaracaoOperacaoTransporte().LerXML<RetDeclaracaoOperacaoTransporte>(EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000126\",\"ProtocoloServico\":\"PROTO-1\"}", Servico.CIOTDeclaracaoOperacaoTransporte));
            var consulta = new RetConsultarCIOTGerado().LerXML<RetConsultarCIOTGerado>(EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000126\",\"EstadoCiot\":\"EmViagem\",\"ProtocoloServico\":\"PROTO-2\"}", Servico.CIOTConsultarCIOTGerado));
            var encerramento = new RetEncerramentoOperacaoTransporte().LerXML<RetEncerramentoOperacaoTransporte>(EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000126\",\"Protocolo\":\"PROTO-3\"}", Servico.CIOTEncerramentoOperacaoTransporte));

            Assert.Equal("992000000126", declaracao.IdOperacaoTransporte);
            Assert.Equal("PROTO-1", declaracao.Protocolo);
            Assert.Equal("EmViagem", consulta.EstadoCIOT);
            Assert.Equal("PROTO-2", consulta.Protocolo);
            Assert.Equal("PROTO-3", encerramento.Protocolo);
            Assert.Equal(default, encerramento.DataEncerramento);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void NormalizaCancelamentoSituacaoEFrota()
        {
            var cancelamento = new RetCancelamentoOperacaoTransporte().LerXML<RetCancelamentoOperacaoTransporte>(EFreteMapper.NormalizarRetorno("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000126\",\"Protocolo\":\"PROTO-C\",\"Data\":\"2026-08-12T10:00:00-03:00\"}", Servico.CIOTCancelamentoOperacaoTransporte));
            const string jsonSituacao = "{\"Sucesso\":true,\"CpfOuCnpj\":\"12345678901\",\"RNTRC\":\"123456789\",\"RNTRCAtivo\":true,\"TACouEquiparado\":true,\"ProtocoloServico\":\"PROTO-S\",\"Veiculos\":[{\"Placa\":\"BRA2E19\",\"FazParteDaFrota\":true}]}";
            var situacao = new RetConsultarSituacaoTransportador().LerXML<RetConsultarSituacaoTransportador>(EFreteMapper.NormalizarRetorno(jsonSituacao, Servico.CIOTConsultarSituacaoTransportador));
            var frota = new RetConsultarFrotaTransportador().LerXML<RetConsultarFrotaTransportador>(EFreteMapper.NormalizarRetorno(jsonSituacao, Servico.CIOTConsultarFrotaTransportador));

            Assert.Equal("PROTO-C", cancelamento.Protocolo);
            Assert.Equal(2026, cancelamento.DataCancelamento.Year);
            Assert.Equal(8, cancelamento.DataCancelamento.Month);
            Assert.Equal(12, cancelamento.DataCancelamento.Day);
            Assert.Equal(10, cancelamento.DataCancelamento.Hour);
            Assert.True(situacao.RNTRCAtivo);
            Assert.True(situacao.EquiparadoTAC);
            Assert.Equal("BRA2E19", frota.Frota[0].PlacaVeiculo);
            Assert.True(frota.Frota[0].SituacaoVeiculoFrotaTransportador);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void NormalizaErroEFreteSemPerderCodigoEMensagem()
        {
            var retorno = new RetDeclaracaoOperacaoTransporte().LerXML<RetDeclaracaoOperacaoTransporte>(EFreteMapper.NormalizarRetorno("{\"Sucesso\":false,\"Excecao\":{\"Codigo\":\"EF123\",\"Mensagem\":\"Operação rejeitada\"}}", Servico.CIOTDeclaracaoOperacaoTransporte));

            Assert.NotNull(retorno.Temp);
            Assert.Equal("EF123", retorno.Codigo);
            Assert.Equal("Operação rejeitada", retorno.Mensagem);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SerializaEDesserializaNovosGruposSemAlterarARaizCIOT()
        {
            var original = CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao);
            original.ObservacoesAoTransportador = new List<string> { "MASSA SINTÉTICA PARA TESTE" };
            var documento = original.GerarXML();
            var leitura = new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(documento);

            Assert.Equal("DeclaracaoOperacaoTransporte", documento.DocumentElement.LocalName);
            Assert.Equal(original.IdOperacaoCliente, leitura.IdOperacaoCliente);
            Assert.Equal("NF-001", leitura.OrigemDestino[0].NotasFiscais[0].Numero);
            Assert.Equal("MASSA SINTÉTICA PARA TESTE", leitura.ObservacoesAoTransportador[0]);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void AutenticacaoPorCertificadoNaoIncluiTokenNoJson()
        {
            var configuracao = CriarConfiguracao();
            configuracao.EFreteToken = null;
            var servico = new DeclaracaoServico(CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao), configuracao);
            var json = JObject.Parse(EFreteMapper.CriarJson(servico.Envio, Servico.CIOTDeclaracaoOperacaoTransporte, configuracao));

            Assert.True(configuracao.UsaCertificadoDigital);
            Assert.Null(json["Token"]);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void MontaLoginPorCredenciaisELeTokenSemExporNoXmlCIOT()
        {
            var configuracao = CriarConfiguracao();
            configuracao.EFreteToken = null;
            configuracao.EFreteUsuario = "12345678901";
            configuracao.EFreteSenha = "SENHA-SINTETICA";
            var login = JObject.Parse(EFreteMapper.CriarJsonLogin(configuracao));
            var token = EFreteMapper.ObterTokenLogin("{\"Sucesso\":true,\"Token\":\"TOKEN-OBTIDO\"}");

            Assert.Equal("12345678901", login.Value<string>("Usuario"));
            Assert.Equal("SENHA-SINTETICA", login.Value<string>("Senha"));
            Assert.Equal("TOKEN-OBTIDO", token);
            Assert.DoesNotContain("SENHA-SINTETICA", CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao).GerarXML().OuterXml);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RecusaServicosSemEquivalenteAntesDaRequisicao()
        {
            var configuracao = CriarConfiguracao();
            Assert.Throws<NotSupportedException>(() => new GerarIdServico(new GerarIdOperacaoTransporte { ProvedorCIOT = ProvedorCIOT.EFrete, CpfCnpj = "12345678000199" }, configuracao));
            Assert.Throws<NotSupportedException>(() => new RetificacaoServico(new RetificacaoOperacaoTransporte { ProvedorCIOT = ProvedorCIOT.EFrete, CodigoIdentificacaoOperacao = "992000000126" }, CriarConfiguracao()));
            Assert.Throws<NotSupportedException>(() => new ConsultarExcecaoServico(new ConsultarExcecao { ProvedorCIOT = ProvedorCIOT.EFrete, CpfCnpjTransportador = "12345678901" }, CriarConfiguracao()));
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExigeIdempotenciaExplicitaNaEFrete()
        {
            var xml = CriarDeclaracao(TipoOperacaoTransporteCIOT.CargaLotacao);
            xml.IdOperacaoCliente = null;
            Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() => new DeclaracaoServico(xml, CriarConfiguracao()));
        }

        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoAmbiente = TipoAmbiente.Homologacao,
            ProvedorCIOT = ProvedorCIOT.EFrete,
            EFreteIntegrador = "INTEGRADOR-TESTE",
            EFreteToken = "TOKEN-TESTE"
        };

        private static DeclaracaoOperacaoTransporte CriarDeclaracao(TipoOperacaoTransporteCIOT tipo)
        {
            var xml = new DeclaracaoOperacaoTransporte
            {
                ProvedorCIOT = ProvedorCIOT.EFrete,
                IdOperacaoCliente = "CIOT-CLIENTE-001",
                MatrizCNPJ = "12345678000199",
                TipoOperacao = tipo,
                DataInicioViagem = new DateTime(2026, 8, 12, 8, 0, 0),
                DataFimViagem = new DateTime(2026, 8, 13, 18, 0, 0),
                CpfCnpjContratado = "12345678901",
                RNTRCContratado = "123456789",
                Contratante = new PessoaCIOT { NomeOuRazaoSocial = "EMPRESA TESTE", CpfOuCnpj = "12345678000199", ResponsavelPeloPagamento = true, Endereco = new EnderecoCIOT { Bairro = "CENTRO", Rua = "RUA EXEMPLO", Numero = "100", CEP = "87000000", CodigoMunicipio = "4115200" } },
                Destinatario = new PessoaCIOT { NomeOuRazaoSocial = "CLIENTE TESTE", CpfOuCnpj = "98765432000100", Endereco = new EnderecoCIOT { Bairro = "CENTRO", Rua = "RUA DESTINO", Numero = "200", CEP = "80000000", CodigoMunicipio = "4106902" } },
                Motorista = new MotoristaCIOT { CpfOuCnpj = "12345678901", CNH = "12345678901", Celular = new TelefoneCIOT { DDD = "44", Numero = "999999999" } },
                DadosCarga = new DadosCarga { CodigoNaturezaCarga = "2701", PesoCarga = "18000", CodigoTipoCarga = TipoCargaCIOT.GranelSolido },
                Impostos = new ImpostosCIOT(),
                Veiculos = new List<Veiculo> { new Veiculo { Placa = "BRA2E19" } },
                InfIndicadoresOperacionais = new IndicadoresOperacionais(),
                TipoPagamentoEFrete = "TransferenciaBancaria",
                InfPagamento = new List<InfPagamento>
                {
                    new InfPagamento { IdPagamentoCliente = "PAG-001", DataDeLiberacao = "2026-08-12T08:00:00-03:00", ValorParcela = 5000, TipoPagamento = TipoPagamentoFreteCIOT.ContaCorrente, TipoPagamentoEFrete = "TransferenciaBancaria", Categoria = tipo == TipoOperacaoTransporteCIOT.TACAgregado ? "SemCategoria" : "Quitacao", Documento = "DOC-001", CpfCnpjCreditado = "12345678901", CodigoInstituicaoFinanceira = "001", NumeroAgencia = "1234-5", NumeroConta = "12345-6", TipoConta = "ContaCorrente" }
                }
            };

            if (tipo != TipoOperacaoTransporteCIOT.TACAgregado)
            {
                xml.OrigemDestino = new List<OrigemDestino>
                {
                    new OrigemDestino { DocumentoViagem = "DOC-001", Origem = new OrigemCIOT { CodigoMunicipioOrigem = "4115200" }, Destino = new Destino { CodigoMunicipioDestino = "4106902" }, DistanciaPercorrida = "430", TipoPagamentoEFrete = "TransferenciaBancaria", Valores = new ValoresViagemCIOT { TotalOperacao = 5000, TotalViagem = 5000, TotalDeQuitacao = 5000 }, NotasFiscais = new List<NotaFiscalCIOT> { new NotaFiscalCIOT { Numero = "NF-001", Serie = "1", ValorTotal = 10000, ValorDaMercadoriaPorUnidade = 1, CodigoNCMNaturezaCarga = "2701", UnidadeDeMedidaDaMercadoria = "Kg", TipoDeCalculo = "SemQuebra", QuantidadeDaMercadoriaNoEmbarque = 18000, ToleranciaDePerdaDeMercadoria = new ToleranciaCIOT { Tipo = "Nenhum" } } } }
                };
            }
            if (tipo == TipoOperacaoTransporteCIOT.CargaFracionada)
            {
                xml.DadosCarga.ContratantesCargFrac = new List<string> { "11122233000144" };
            }
            if (tipo == TipoOperacaoTransporteCIOT.TACAgregado)
            {
                xml.DataInicioViagem = default;
                xml.DadosCarga = null;
                xml.Destinatario = null;
                xml.InfIndicadoresOperacionais = null;
                xml.Contratante.RNTRC = "987654321";
            }
            return xml;
        }
    }
}
