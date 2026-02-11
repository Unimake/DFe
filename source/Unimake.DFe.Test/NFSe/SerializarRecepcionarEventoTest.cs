using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos;

namespace Unimake.DFe.Test.NFSe
{
    public class SerializarRecepcionarEventoTest
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoCancelar-ped-regev.xml")]
        public void EventoCancelarNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E101101);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E101101 = new E101101
                    {
                        XDesc = lido.InfPedReg.E101101.XDesc,
                        CMotivo = lido.InfPedReg.E101101.CMotivo,
                        XMotivo = lido.InfPedReg.E101101.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            // Configuração para execução do serviço
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            // Executa o serviço
            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            // Usa as propriedades tipadas para obter o retorno
            if (recepcaoEvento.Result != null)
            {
                // Sucesso - Cast para Evento
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                // Erro
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\Retorno Serialização\RetornoPositivoCancelamento.xml")]
        public void DeserializarRetornoPositivoCancelamentoNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            // Deserializa o retorno de sucesso
            var retornoSucesso = new Evento().LerXML<Evento>(docFixture);

            // Validações do envelope <evento>
            Assert.Equal("1.01", retornoSucesso.Versao);
            Assert.NotNull(retornoSucesso.InfEvento);
            Assert.False(string.IsNullOrWhiteSpace(retornoSucesso.InfEvento.Id));
            Assert.NotNull(retornoSucesso.Signature);

            // Validações do <infEvento>
            Assert.Equal("EVT41069022248211664000126000000000003925126775998403101101001", retornoSucesso.InfEvento.Id);
            Assert.Equal("SefinNac_Pre_1.4.0", retornoSucesso.InfEvento.VerAplic);
            Assert.Equal(TipoAmbiente.Homologacao, retornoSucesso.InfEvento.AmbGer);
            Assert.Equal(0, retornoSucesso.InfEvento.NSeqEvento);
            Assert.Equal(0, retornoSucesso.InfEvento.NDFe);

            // Validações do <pedRegEvento> retornado
            Assert.NotNull(retornoSucesso.InfEvento.PedRegEvento);
            Assert.Equal("1.01", retornoSucesso.InfEvento.PedRegEvento.Versao);
            Assert.NotNull(retornoSucesso.InfEvento.PedRegEvento.InfPedReg);

            // Validações do <infPedReg>
            var infPedReg = retornoSucesso.InfEvento.PedRegEvento.InfPedReg;
            Assert.Equal("PRE41069022248211664000126000000000003925126775998403101101001", infPedReg.Id);
            Assert.Equal(TipoAmbiente.Homologacao, infPedReg.TpAmb);
            Assert.Equal("UNICO V8.01.04", infPedReg.VerAplic);
            Assert.Equal("48211664000126", infPedReg.CNPJAutor);
            Assert.Equal("41069022248211664000126000000000003925126775998403", infPedReg.ChNFSe);
            Assert.Equal("001", infPedReg.NPedRegEvento);

            // Validações do evento E101101 (Cancelamento)
            Assert.NotNull(infPedReg.E101101);
            Assert.Equal("Cancelamento de NFS-e", infPedReg.E101101.XDesc);
            Assert.Equal(CodigoJustificativaCancelamento.ErroNaEmissao, infPedReg.E101101.CMotivo);
            Assert.Equal("(SUPERVISOR-18/12/2025-17:23:54)", infPedReg.E101101.XMotivo);

            // Validação de data/hora
            Assert.True(retornoSucesso.InfEvento.DhProc > DateTimeOffset.MinValue);
            Assert.True(infPedReg.DhEvento > DateTimeOffset.MinValue);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoCancelarSubstituicao-ped-regev.xml")]
        public void EventoCancelarPorSubstituicaoNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E105102);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E105102 = new E105102
                    {
                        XDesc = lido.InfPedReg.E105102.XDesc,
                        CMotivo = lido.InfPedReg.E105102.CMotivo,
                        XMotivo = lido.InfPedReg.E105102.XMotivo,
                        ChSubstituta = lido.InfPedReg.E105102.ChSubstituta
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            // Configuração para execução do serviço
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            // Executa o serviço
            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            // Usa as propriedades tipadas para obter o retorno
            if (recepcaoEvento.Result != null)
            {
                // Sucesso - Cast para Evento
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                // Erro
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoConfirmacaoPrestador-ped-regev.xml")]
        public void EventoConfirmacaoPrestadorNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E202201);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E202201 = new E202201
                    {
                        XDesc = lido.InfPedReg.E202201.XDesc
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoRejeicaoPrestador-ped-regev.xml")]
        public void EventoRejeicaoPrestadorNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E202205);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E202205 = new E202205
                    {
                        XDesc = lido.InfPedReg.E202205.XDesc,
                        CMotivo = lido.InfPedReg.E202205.CMotivo,
                        XMotivo = lido.InfPedReg.E202205.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoRejeicaoTomador-ped-regev.xml")]
        public void EventoRejeicaoTomadorNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E203206);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E203206 = new E203206
                    {
                        XDesc = lido.InfPedReg.E203206.XDesc,
                        CMotivo = lido.InfPedReg.E203206.CMotivo,
                        XMotivo = lido.InfPedReg.E203206.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoRejeicaoIntermediario-ped-regev.xml")]
        public void EventoRejeicaoIntermediarioNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E204207);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E204207 = new E204207
                    {
                        XDesc = lido.InfPedReg.E204207.XDesc,
                        CMotivo = lido.InfPedReg.E204207.CMotivo,
                        XMotivo = lido.InfPedReg.E204207.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoConfirmacaoTomador-ped-regev.xml")]
        public void EventoConfirmacaoTomadorNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E203202);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E203202 = new E203202
                    {
                        XDesc = lido.InfPedReg.E203202.XDesc
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoConfirmacaoIntermediario-ped-regev.xml")]
        public void EventoConfirmacaoIntermediarioNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E204203);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E204203 = new E204203
                    {
                        XDesc = lido.InfPedReg.E204203.XDesc
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoConfirmacaoTacita-ped-regev.xml")]
        public void EventoConfirmacaoTacitaNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E205204);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E205204 = new E205204
                    {
                        XDesc = lido.InfPedReg.E205204.XDesc
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoCancelamentoAnaliseFiscal-ped-regev.xml")]
        public void EventoCancelamentoIndeferidoAnaliseFiscalNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E105105);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E105105 = new E105105
                    {
                        XDesc = lido.InfPedReg.E105105.XDesc,
                        CPFAgTrib = lido.InfPedReg.E105105.CPFAgTrib,
                        NProcAdm = lido.InfPedReg.E105105.NProcAdm,
                        CMotivo = lido.InfPedReg.E105105.CMotivo,
                        XMotivo = lido.InfPedReg.E105105.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\GerarNfseEnvio-env-loterps.xml")]
        public void GerarNfseNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(caminhoXml);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.01"
            };

            var gerarNfse = new GerarNfse(conteudoXML, configuracao);
            gerarNfse.Executar();

            Assert.False(string.IsNullOrWhiteSpace(gerarNfse.RetornoWSString));

            var nfse = gerarNfse.Result;
            if (nfse != null)
            {
                // Sucesso - NFSe gerada
                Assert.NotNull(nfse.InfNFSe);
                Assert.True(nfse.InfNFSe.NNFSe > 0);
                System.Diagnostics.Debug.WriteLine($"NFSe gerada com sucesso - Número: {nfse.InfNFSe.NNFSe}");
            }
            else
            {
                // Erro
                var retornoErro = gerarNfse.ResultErro;
                Assert.NotNull(retornoErro);

                var erro = retornoErro.Erros;

                Assert.NotNull(erro);
                Assert.False(string.IsNullOrWhiteSpace(erro.Codigo));
                Assert.False(string.IsNullOrWhiteSpace(erro.Descricao));

                Assert.Equal(TipoAmbiente.Homologacao, retornoErro.TipoAmbiente);
                Assert.False(string.IsNullOrWhiteSpace(retornoErro.VersaoAplicativo));

                System.Diagnostics.Debug.WriteLine($"Erro capturado corretamente - Código: {erro.Codigo}, Descrição: {erro.Descricao}");

                if (!string.IsNullOrWhiteSpace(retornoErro.IdDPS))
                {
                    Assert.StartsWith("DPS", retornoErro.IdDPS);
                    System.Diagnostics.Debug.WriteLine($"IdDPS: {retornoErro.IdDPS}");
                }
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoSolicitacaoAnaliseFiscalCancelamento-ped-regev.xml")]
        public void EventoSolicitacaoAnaliseFiscalCancelamentoNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E101103);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E101103 = new E101103
                    {
                        XDesc = lido.InfPedReg.E101103.XDesc,
                        CMotivo = lido.InfPedReg.E101103.CMotivo,
                        XMotivo = lido.InfPedReg.E101103.XMotivo,
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            // Configuração para execução do serviço
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            // Executa o serviço
            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            // Usa as propriedades tipadas para obter o retorno
            if (recepcaoEvento.Result != null)
            {
                // Sucesso - Cast para Evento
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                // Erro
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoAnulacaoRejeicao-ped-regev.xml")]
        public void EventoAnulacaoRejeicaoNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E205208);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E205208 = new E205208
                    {
                        XDesc = lido.InfPedReg.E205208.XDesc,
                        CPFAgTrib = lido.InfPedReg.E205208.CPFAgTrib,
                        IdEvManifRej = lido.InfPedReg.E205208.IdEvManifRej,
                        XMotivo = lido.InfPedReg.E205208.XMotivo
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoCancelamentoPorOficio-ped-regev.xml")]
        public void EventoCancelamentoPorOficioNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg? .E305101);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E305101 = new E305101
                    {
                        XDesc = lido.InfPedReg.E305101.XDesc,
                        CPFAgTrib = lido.InfPedReg.E305101.CPFAgTrib,
                        NProcAdm = lido.InfPedReg.E305101.NProcAdm,
                        XProcAdm = lido.InfPedReg.E305101.XProcAdm,
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoBloqueioPorOficio-ped-regev.xml")]
        public void EventoBloqueioPorOficioNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E305102);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E305102 = new E305102
                    {
                        XDesc = lido.InfPedReg.E305102.XDesc,
                        CPFAgTrib = lido.InfPedReg.E305102.CPFAgTrib,
                        CodEvento = lido.InfPedReg.E305102.CodEvento,
                        XMotivo = lido.InfPedReg.E305102.XMotivo,
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result != null)
            {
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\EventoCancelamentoDeferidoAnaliseFiscal-ped-regev.xml")]
        public void EventoCancelamentoDeferidoAnaliseFiscalNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();

            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.01", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfPedReg?.Id));
            Assert.NotNull(lido.InfPedReg?.E105104);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var autor = !string.IsNullOrWhiteSpace(lido.InfPedReg.CNPJAutor)
                        ? new { cnpj = lido.InfPedReg.CNPJAutor, cpf = (string)null }
                        : new { cnpj = (string)null, cpf = lido.InfPedReg.CPFAutor };

            var criado = new PedRegEvento
            {
                Versao = lido.Versao,
                InfPedReg = new InfPedReg
                {
                    Id = lido.InfPedReg.Id,
                    TpAmb = lido.InfPedReg.TpAmb,
                    VerAplic = lido.InfPedReg.VerAplic,
                    DhEvento = lido.InfPedReg.DhEvento,
                    CNPJAutor = autor.cnpj,
                    CPFAutor = autor.cpf,
                    ChNFSe = lido.InfPedReg.ChNFSe,
                    //NPedRegEvento = lido.InfPedReg.NPedRegEvento,
                    E105104 = new E105104
                    {
                        XDesc = lido.InfPedReg.E105104.XDesc,
                        CPFAgTrib = lido.InfPedReg.E105104.CPFAgTrib,
                        nProcAdm = lido.InfPedReg.E105104.nProcAdm,
                        CMotivo = lido.InfPedReg.E105104.CMotivo,
                        XMotivo = lido.InfPedReg.E105104.XMotivo,
                    }
                }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            // Configuração para execução do serviço
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = lido.InfPedReg.TpAmb,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = lido.Versao
            };

            // Executa o serviço
            var recepcaoEvento = new RecepcionarEvento(docCriado, configuracao);
            recepcaoEvento.Executar();

            // Usa as propriedades tipadas para obter o retorno
            if (recepcaoEvento.Result != null)
            {
                // Sucesso - Cast para Evento
                var evento = (Evento)recepcaoEvento.Result;
                Assert.NotNull(evento.InfEvento);
                Assert.False(string.IsNullOrWhiteSpace(evento.InfEvento.Id));
                System.Diagnostics.Debug.WriteLine($"Evento registrado - ID: {evento.InfEvento.Id}");
            }
            else
            {
                // Erro
                var erro = recepcaoEvento.ResultErro;
                Assert.NotNull(erro?.Erro);
                System.Diagnostics.Debug.WriteLine($"Erro: {erro.Erro.Codigo} - {erro.Erro.Descricao}");
            }
        }
    }
}
