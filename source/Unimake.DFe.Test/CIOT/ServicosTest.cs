using System;
using System.Xml;
using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTCancelamentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte;
using CIOTConsultarCIOTGerado = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;
using CIOTConsultarExcecao = Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao;
using CIOTConsultarFrotaTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarFrotaTransportador;
using CIOTConsultarSituacaoTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador;
using CIOTDeclaracaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;
using CIOTEncerramentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte;
using CIOTGerarIdOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte;
using CIOTRetificacaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte;

namespace Unimake.DFe.Test.CIOT
{
    /// <summary>
    /// Testar os serviços do CIOT
    /// </summary>
    public class ServicosTest
    {
        /// <summary>
        /// Consultar situação do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarSituacaoTransportador")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarSituacaoTransportador.xml")]
        public void ConsultarSituacaoTransportador(string arqXML)
        {
            var objeto = LerXML<ConsultarSituacaoTransportador>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarSituacaoTransportador(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarSituacaoTransportador>(servico.Result);
        }

        /// <summary>
        /// Consultar frota do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarFrotaTransportador")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarFrotaTransportador.xml")]
        public void ConsultarFrotaTransportador(string arqXML)
        {
            var objeto = LerXML<ConsultarFrotaTransportador>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarFrotaTransportador(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarFrotaTransportador>(servico.Result);
        }

        /// <summary>
        /// Declarar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "DeclaracaoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml")]
        public void DeclaracaoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<DeclaracaoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTDeclaracaoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetDeclaracaoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Gerar XML de distribuição da declaração de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "DeclaracaoOperacaoTransporte")]
        public void DeclaracaoOperacaoTransporteProcResult()
        {
            var envio = LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml");

            var servico = new CIOTDeclaracaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var proc = servico.DeclaracaoOperacaoTransporteProcResult;

            Assert.NotNull(proc);
            Assert.Equal("123456789012", proc.RetDeclaracaoOperacaoTransporte.IdOperacaoTransporte);
            Assert.Equal("123456789012-procCIOT.xml", proc.NomeArquivoDistribuicao);
            Assert.Equal("DeclaracaoOperacaoTransporteProc", proc.GerarXML().DocumentElement.Name);
        }

        /// <summary>
        /// Gravar XML de distribuição em pasta para declaração de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "DeclaracaoOperacaoTransporte")]
        public void GravarXmlDistribuicaoDeclaracaoOperacaoTransportePasta()
        {
            var envio = LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml");

            var servico = new CIOTDeclaracaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);

            try
            {
                servico.GravarXmlDistribuicao(pasta);

                var arquivo = Path.Combine(pasta, "123456789012-procCIOT.xml");
                Assert.True(File.Exists(arquivo));

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains("<DeclaracaoOperacaoTransporteProc", conteudo);
                Assert.Contains("<IdOperacaoTransporte>123456789012</IdOperacaoTransporte>", conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Gravar XML de distribuição em stream para declaração de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "DeclaracaoOperacaoTransporte")]
        public void GravarXmlDistribuicaoDeclaracaoOperacaoTransporteStream()
        {
            var envio = LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml");

            var servico = new CIOTDeclaracaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);
            var arquivo = Path.Combine(pasta, "stream-procCIOT.xml");

            try
            {
                using (var stream = new FileStream(arquivo, FileMode.Create, FileAccess.ReadWrite, FileShare.Read))
                {
                    servico.GravarXmlDistribuicao(stream);
                }

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains("<DeclaracaoOperacaoTransporteProc", conteudo);
                Assert.Contains("<CodigoVerificador>1234</CodigoVerificador>", conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Gerar XML de distribuição do cancelamento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        public void CancelamentoOperacaoTransporteProcResult()
        {
            var envio = LerXML<CancelamentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml");

            var servico = new CIOTCancelamentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var proc = servico.CancelamentoOperacaoTransporteProcResult;

            Assert.NotNull(proc);
            Assert.Equal("1234567890123456", proc.RetCancelamentoOperacaoTransporte.CodigoIdentificacaoOperacao);
            Assert.Equal("1234567890123456-procEventoCIOT.xml", proc.NomeArquivoDistribuicao);
            Assert.Equal("CancelamentoOperacaoTransporteProc", proc.GerarXML().DocumentElement.Name);
            Assert.NotNull(new CancelamentoOperacaoTransporteProc().LoadFromXML(proc.GerarXML().OuterXml));
        }

        /// <summary>
        /// Gravar XML de distribuição em pasta para cancelamento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        public void GravarXmlDistribuicaoCancelamentoOperacaoTransportePasta()
        {
            var envio = LerXML<CancelamentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml");

            var servico = new CIOTCancelamentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);

            try
            {
                servico.GravarXmlDistribuicao(pasta);

                var arquivo = Path.Combine(pasta, "1234567890123456-procEventoCIOT.xml");
                Assert.True(File.Exists(arquivo));

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains("<CancelamentoOperacaoTransporteProc", conteudo);
                Assert.Contains("<RetCancelamentoOperacaoTransporte", conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Gravar XML de distribuição em stream para cancelamento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        public void GravarXmlDistribuicaoCancelamentoOperacaoTransporteStream()
        {
            var envio = LerXML<CancelamentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml");

            var servico = new CIOTCancelamentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            GravarXmlDistribuicaoStream(servico, "<CancelamentoOperacaoTransporteProc", "<DataCancelamento>2026-05-25T12:20:42Z</DataCancelamento>");
        }

        /// <summary>
        /// Gerar XML de distribuição da retificação de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "RetificacaoOperacaoTransporte")]
        public void RetificacaoOperacaoTransporteProcResult()
        {
            var envio = LerXML<RetificacaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml");

            var servico = new CIOTRetificacaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var proc = servico.RetificacaoOperacaoTransporteProcResult;

            Assert.NotNull(proc);
            Assert.Equal("1234567890123456", proc.RetRetificacaoOperacaoTransporte.CodigoIdentificacaoOperacao);
            Assert.Equal("1234567890123456-procEventoCIOT.xml", proc.NomeArquivoDistribuicao);
            Assert.Equal("RetificacaoOperacaoTransporteProc", proc.GerarXML().DocumentElement.Name);
            Assert.NotNull(new RetificacaoOperacaoTransporteProc().LoadFromXML(proc.GerarXML().OuterXml));
        }

        /// <summary>
        /// Gravar XML de distribuição em pasta para retificação de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "RetificacaoOperacaoTransporte")]
        public void GravarXmlDistribuicaoRetificacaoOperacaoTransportePasta()
        {
            var envio = LerXML<RetificacaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml");

            var servico = new CIOTRetificacaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);

            try
            {
                servico.GravarXmlDistribuicao(pasta);

                var arquivo = Path.Combine(pasta, "1234567890123456-procEventoCIOT.xml");
                Assert.True(File.Exists(arquivo));

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains("<RetificacaoOperacaoTransporteProc", conteudo);
                Assert.Contains("<RetRetificacaoOperacaoTransporte", conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Gravar XML de distribuição em stream para retificação de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "RetificacaoOperacaoTransporte")]
        public void GravarXmlDistribuicaoRetificacaoOperacaoTransporteStream()
        {
            var envio = LerXML<RetificacaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml");

            var servico = new CIOTRetificacaoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            GravarXmlDistribuicaoStream(servico, "<RetificacaoOperacaoTransporteProc", "<DataRetificacao>2026-05-25T12:20:42Z</DataRetificacao>");
        }

        /// <summary>
        /// Gerar XML de distribuição do encerramento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "EncerramentoOperacaoTransporte")]
        public void EncerramentoOperacaoTransporteProcResult()
        {
            var envio = LerXML<EncerramentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml");

            var servico = new CIOTEncerramentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var proc = servico.EncerramentoOperacaoTransporteProcResult;

            Assert.NotNull(proc);
            Assert.Equal("1234567890123456", proc.RetEncerramentoOperacaoTransporte.CodigoIdentificacaoOperacao);
            Assert.Equal("1234567890123456-procEventoCIOT.xml", proc.NomeArquivoDistribuicao);
            Assert.Equal("EncerramentoOperacaoTransporteProc", proc.GerarXML().DocumentElement.Name);
            Assert.NotNull(new EncerramentoOperacaoTransporteProc().LoadFromXML(proc.GerarXML().OuterXml));
        }

        /// <summary>
        /// Gravar XML de distribuição em pasta para encerramento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "EncerramentoOperacaoTransporte")]
        public void GravarXmlDistribuicaoEncerramentoOperacaoTransportePasta()
        {
            var envio = LerXML<EncerramentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml");

            var servico = new CIOTEncerramentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);

            try
            {
                servico.GravarXmlDistribuicao(pasta);

                var arquivo = Path.Combine(pasta, "1234567890123456-procEventoCIOT.xml");
                Assert.True(File.Exists(arquivo));

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains("<EncerramentoOperacaoTransporteProc", conteudo);
                Assert.Contains("<RetEncerramentoOperacaoTransporte", conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Gravar XML de distribuição em stream para encerramento de operação de transporte.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "EncerramentoOperacaoTransporte")]
        public void GravarXmlDistribuicaoEncerramentoOperacaoTransporteStream()
        {
            var envio = LerXML<EncerramentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml");
            var retorno = new XmlDocument();
            retorno.Load(@"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml");

            var servico = new CIOTEncerramentoOperacaoTransporte(envio, CriarConfiguracao())
            {
                RetornoWSXML = retorno
            };

            GravarXmlDistribuicaoStream(servico, "<EncerramentoOperacaoTransporteProc", "<DataEncerramento>2026-05-26T19:30:38Z</DataEncerramento>");
        }

        /// <summary>
        /// Cancelar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml")]
        public void CancelamentoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<CancelamentoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTCancelamentoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetCancelamentoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Retificar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "RetificacaoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml")]
        public void RetificacaoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<RetificacaoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTRetificacaoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetRetificacaoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Encerrar operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "EncerramentoOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml")]
        public void EncerramentoOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<EncerramentoOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTEncerramentoOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetEncerramentoOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Consultar exceção do transportador.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarExcecao")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarExcecao.xml")]
        public void ConsultarExcecao(string arqXML)
        {
            var objeto = LerXML<ConsultarExcecao>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarExcecao(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarExcecao>(servico.Result);
        }

        /// <summary>
        /// Consultar CIOT gerado.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "ConsultarCIOTGerado")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml")]
        public void ConsultarCIOTGerado(string arqXML)
        {
            var objeto = LerXML<ConsultarCIOTGerado>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTConsultarCIOTGerado(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetConsultarCIOTGerado>(servico.Result);
        }

        /// <summary>
        /// Gerar identificador da operação de transporte.
        /// </summary>
        [Theory()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "GerarIdOperacaoTransporte")]
        [InlineData(@"..\..\..\CIOT\Resources\gerarIdOperacaoTransporte.xml")]
        public void GerarIdOperacaoTransporte(string arqXML)
        {
            var objeto = LerXML<GerarIdOperacaoTransporte>(arqXML);
            var configuracao = CriarConfiguracao();

            var servico = new CIOTGerarIdOperacaoTransporte(objeto, configuracao);
            servico.Executar();

            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetGerarIdOperacaoTransporte>(servico.Result);
        }

        /// <summary>
        /// Desserializar retorno de erro da API dentro do retorno tipado.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        [Trait("Servico", "CancelamentoOperacaoTransporte")]
        public void RetornoErroCancelamentoOperacaoTransporte()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><error>USUARIO_NAO_AUTORIZADO</error><message>Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC</message><timestamp>2026-05-26T20:34:17.2862302Z</timestamp><correlationId>ee174fab-71e7-4c1f-97e8-3029216bf457</correlationId><path>/pefServices/api/CancelamentoOperacaoTransporte</path></temp>");

            var servico = new CIOTCancelamentoOperacaoTransporte
            {
                RetornoWSXML = xml
            };

            Assert.Equal("USUARIO_NAO_AUTORIZADO", servico.Result.Temp.Error);
            Assert.Equal("Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC", servico.Result.Temp.Message);
            Assert.Equal(System.DateTimeOffset.Parse("2026-05-26T20:34:17.2862302Z"), servico.Result.Temp.Timestamp);
            Assert.Equal("ee174fab-71e7-4c1f-97e8-3029216bf457", servico.Result.Temp.CorrelationId);
            Assert.Equal("/pefServices/api/CancelamentoOperacaoTransporte", servico.Result.Temp.Path);
            Assert.Equal("RetCancelamentoOperacaoTransporte", servico.RetornoWSXML.DocumentElement.Name);
            Assert.Contains("<temp>", servico.RetornoWSString);
        }

        /// <summary>
        /// Desserializar retorno OK na propriedade Result dos serviços.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        public void ResultRetornoOKServicosCIOT()
        {
            ValidarResultOK(new CIOTConsultarSituacaoTransportador(), @"..\..\..\CIOT\Resources\retConsultarSituacaoTransportador.xml");
            ValidarResultOK(new CIOTConsultarFrotaTransportador(), @"..\..\..\CIOT\Resources\retConsultarFrotaTransportador.xml");
            ValidarResultOK(new CIOTDeclaracaoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTCancelamentoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTRetificacaoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTEncerramentoOperacaoTransporte(), @"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml");
            ValidarResultOK(new CIOTConsultarExcecao(), @"..\..\..\CIOT\Resources\retConsultarExcecao.xml");
            ValidarResultOK(new CIOTConsultarCIOTGerado(), @"..\..\..\CIOT\Resources\retConsultarCIOTGerado.xml");
            ValidarResultOK(new CIOTGerarIdOperacaoTransporte(), @"..\..\..\CIOT\Resources\retGerarIdOperacaoTransporte.xml");
        }

        /// <summary>
        /// Desserializar retorno de erro na propriedade Result dos serviços.
        /// </summary>
        [Fact()]
        [Trait("DFe", "CIOT")]
        public void ResultRetornoErroServicosCIOT()
        {
            ValidarResultErro(new CIOTConsultarSituacaoTransportador(), "/pefServices/api/ConsultarSituacaoTransportador");
            ValidarResultErro(new CIOTConsultarFrotaTransportador(), "/pefServices/api/ConsultarFrotaTransportador");
            ValidarResultErro(new CIOTDeclaracaoOperacaoTransporte(), "/pefServices/api/DeclaracaoOperacaoTransporte");
            ValidarResultErro(new CIOTCancelamentoOperacaoTransporte(), "/pefServices/api/CancelamentoOperacaoTransporte");
            ValidarResultErro(new CIOTRetificacaoOperacaoTransporte(), "/pefServices/api/RetificacaoOperacaoTransporte");
            ValidarResultErro(new CIOTEncerramentoOperacaoTransporte(), "/pefServices/api/EncerramentoOperacaoTransporte");
            ValidarResultErro(new CIOTConsultarExcecao(), "/pefServices/api/ConsultarExcecao");
            ValidarResultErro(new CIOTConsultarCIOTGerado(), "/pefServices/api/ConsultarCIOTGerado");
            ValidarResultErro(new CIOTGerarIdOperacaoTransporte(), "/pefServices/api/gerar");
        }

        private static T LerXML<T>(string arqXML) where T : Unimake.Business.DFe.Xml.XMLBase, new()
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            return new T().LerXML<T>(xml);
        }

        private static void ValidarResultOK(dynamic servico, string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            servico.RetornoWSXML = xml;
            var result = servico.Result;

            Assert.NotNull(result);
            Assert.Null(result.Temp);
            Assert.Equal(xml.DocumentElement.Name, result.GerarXML().DocumentElement.Name);
            Assert.Equal(xml.InnerText, result.GerarXML().InnerText);
            Assert.Equal(xml.DocumentElement.Name, servico.RetornoWSXML.DocumentElement.Name);
            Assert.Equal(servico.RetornoWSXML.OuterXml, servico.RetornoWSString);
        }

        private static void ValidarResultErro(dynamic servico, string path)
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><error>USUARIO_NAO_AUTORIZADO</error><message>Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC</message><timestamp>2026-05-26T20:34:17.2862302Z</timestamp><correlationId>ee174fab-71e7-4c1f-97e8-3029216bf457</correlationId><path>" + path + "</path></temp>");

            servico.RetornoWSXML = xml;
            var result = servico.Result;

            Assert.NotNull(result);
            Assert.Equal("USUARIO_NAO_AUTORIZADO", result.Temp.Error);
            Assert.Equal("Rejeição: O CPF/CNPJ do certificado digital não corresponde a nenhum transportador cadastrado no RNTRC", result.Temp.Message);
            Assert.Equal(System.DateTimeOffset.Parse("2026-05-26T20:34:17.2862302Z"), result.Temp.Timestamp);
            Assert.Equal("ee174fab-71e7-4c1f-97e8-3029216bf457", result.Temp.CorrelationId);
            Assert.Equal(path, result.Temp.Path);
            Assert.Equal(result.GetType().Name, servico.RetornoWSXML.DocumentElement.Name);
            Assert.Equal(servico.RetornoWSXML.OuterXml, servico.RetornoWSString);
            Assert.Contains("<temp>", servico.RetornoWSString);
        }

        private static void GravarXmlDistribuicaoStream(dynamic servico, string root, string conteudoEsperado)
        {
            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "CIOT", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);
            var arquivo = Path.Combine(pasta, "stream-procCIOT.xml");

            try
            {
                using (var stream = new FileStream(arquivo, FileMode.Create, FileAccess.ReadWrite, FileShare.Read))
                {
                    servico.GravarXmlDistribuicao(stream);
                }

                var conteudo = File.ReadAllText(arquivo);
                Assert.Contains(root, conteudo);
                Assert.Contains(conteudoEsperado, conteudo);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }

        private static Configuracao CriarConfiguracao()
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.CIOT,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = (int)UFBrasil.AN,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }
    }
}
