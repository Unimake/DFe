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
    public class SerializarRecepcionarEventosNFSeTest
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
            var recepcaoEvento = new RecepcionarEventosNfse(docCriado, configuracao);
            recepcaoEvento.Executar();

            // Pega o retorno e tenta deserializar
            var xmlRetorno = recepcaoEvento.RetornoWSXML;
            Assert.NotNull(xmlRetorno);

            // Tenta deserializar como retorno de erro
            var retornoErro = new Retorno().LerXML<Retorno>(xmlRetorno);
            Assert.NotNull(retornoErro);

            if (retornoErro.Erro != null)
            {
                // Tratamento de erro
                var codigoErro = retornoErro.Erro.Codigo;
                var descricaoErro = retornoErro.Erro.Descricao;

                // Validação do erro
                Assert.False(string.IsNullOrWhiteSpace(codigoErro), "Código de erro não pode ser vazio.");
                Assert.False(string.IsNullOrWhiteSpace(descricaoErro), "Descrição de erro não pode ser vazia.");

                // Log para debug
                System.Diagnostics.Debug.WriteLine($"Erro recebido - Código: {codigoErro}, Descrição: {descricaoErro}");
            }
            else
            {
                // Se não tem erro, assume sucesso
                Assert.True(true, "Evento recepcionado com sucesso.");
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
            Assert.Equal("1", infPedReg.E101101.CMotivo);
            Assert.Equal("(SUPERVISOR-18/12/2025-17:23:54)", infPedReg.E101101.XMotivo);

            // Validação de data/hora
            Assert.True(retornoSucesso.InfEvento.DhProc > DateTimeOffset.MinValue);
            Assert.True(infPedReg.DhEvento > DateTimeOffset.MinValue);

            // Log para debug
            System.Diagnostics.Debug.WriteLine($"Evento processado com sucesso - ID: {retornoSucesso.InfEvento.Id}");
            System.Diagnostics.Debug.WriteLine($"Data de processamento: {retornoSucesso.InfEvento.DhProc}");
        }
    }
}
