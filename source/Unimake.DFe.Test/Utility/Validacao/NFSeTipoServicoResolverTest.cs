using System;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class NFSeTipoServicoResolverTest
    {
        [Theory]
        [InlineData(PadraoNFSe.SMARAPD, "<NFSe versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe><nNFSe>1</nNFSe></infNFSe></NFSe>", "1.01", Servico.NFSeGerarNfse)]
        [InlineData(PadraoNFSe.SMARAPD, "<NFSe versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe Id=\"NFS99999999999999999999999999999999999999999999999999\" /></NFSe>", "1.01", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.SMARAPD, "<DPS versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infDPS Id=\"DPS999999999999999999999999999999999999999999\" /></DPS>", "1.01", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.SMARAPD, "<evento versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infEvento><e101101 /></infEvento></evento>", "1.01", Servico.NFSeCancelarNfse)]
        [InlineData(PadraoNFSe.SMARAPD, "<evento versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infEvento><e105102 /></infEvento></evento>", "1.01", Servico.NFSeSubstituirNfse)]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarNfseServicoPrestadoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><Prestador /><PeriodoCompetencia /></ConsultarNfseServicoPrestadoEnvio>", "2.04", Servico.NFSeConsultarNfseServicoPrestado)]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarNfseServicoPrestadoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><Prestador /><NumeroNfse>201700000000806</NumeroNfse></ConsultarNfseServicoPrestadoEnvio>", "2.04", Servico.NFSeConsultarNfseServicoTomado)]
        [InlineData(PadraoNFSe.PRODATA, "<CancelarNfseEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><InfPedidoCancelamento /></CancelarNfseEnvio>", "2.01", Servico.NFSeCancelarNfse)]
        [InlineData(PadraoNFSe.PRODATA, "<ConsultarNotaPdfEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><IdentificacaoNfse /></ConsultarNotaPdfEnvio>", "2.01", Servico.NFSeConsultarNfsePDF)]
        [InlineData(PadraoNFSe.PRODATA, "<EnviarLoteRpsEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><LoteRps /></EnviarLoteRpsEnvio>", "2.01", Servico.NFSeRecepcionarLoteRps)]
        [InlineData(PadraoNFSe.ISSNET, "<ConsultarDadosCadastraisEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><Pedido /></ConsultarDadosCadastraisEnvio>", "2.04", Servico.NFSeConsultarDadosCadastrais)]
        [InlineData(PadraoNFSe.ISSNET, "<ConsultarNfseDpsEnvio xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><IM>123</IM></ConsultarNfseDpsEnvio>", "1.01", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.ISSNET, "<EnviarLoteDpsSincronoEnvio xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><LoteDps /></EnviarLoteDpsSincronoEnvio>", "1.01", Servico.NFSeRecepcionarLoteRpsSincrono)]
        [InlineData(PadraoNFSe.DSF, "<DPS versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infDPS><tpAmb>2</tpAmb></infDPS></DPS>", "1.01", Servico.NFSeGerarNfse)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ConsultaSeqRps xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></ns1:ConsultaSeqRps>", "1.00", Servico.NFSeConsultarSequenciaLoteNotaRPS)]
        [InlineData(PadraoNFSe.DSF, "<x:ConsultaSeqRps xmlns:x=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></x:ConsultaSeqRps>", "1.00", Servico.NFSeConsultarSequenciaLoteNotaRPS)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ReqConsultaNotas xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></ns1:ReqConsultaNotas>", "1.00", Servico.NFSeConsultarNotaValida)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ReqEnvioLoteRPS xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /><Lote /></ns1:ReqEnvioLoteRPS>", "1.00", Servico.NFSeRecepcionarLoteRpsSincrono)]
        [InlineData(PadraoNFSe.DSF, "<ConsultarSituacaoLoteRpsEnvio xmlns=\"http://www.ginfes.com.br/servico_consultar_situacao_lote_rps_envio_v03.xsd\"><Protocolo>1</Protocolo></ConsultarSituacaoLoteRpsEnvio>", "3.00", Servico.NFSeConsultarSituacaoLoteRps)]
        public void DeveDefinirTipoServicoNFSePorXmlString(PadraoNFSe padraoNFSe, string conteudoXML, string versao, Servico tipoServicoEsperado)
        {
            var tipoServico = ValidarEstruturaXML.DefinirTipoServicoNFSe(conteudoXML, padraoNFSe, versao);

            Assert.Equal(tipoServicoEsperado, tipoServico);
        }

        [Fact]
        public void DeveDefinirTipoServicoNFSeSmarapdPorXmlDocument()
        {
            var xml = CriarXml("<EnviarLoteRpsSincronoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><LoteRps /></EnviarLoteRpsSincronoEnvio>");

            var tipoServico = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SMARAPD, "2.04");

            Assert.Equal(Servico.NFSeRecepcionarLoteRpsSincrono, tipoServico);
        }

        [Fact]
        public void DeveUsarTagIdentificadoraParaDiferenciarServicosComMesmaRaiz()
        {
            var xmlPrestado = "<ConsultarNfseServicoPrestadoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><Prestador /><PeriodoCompetencia /></ConsultarNfseServicoPrestadoEnvio>";
            var xmlTomado = "<ConsultarNfseServicoPrestadoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><Prestador /><NumeroNfse>201700000000806</NumeroNfse></ConsultarNfseServicoPrestadoEnvio>";

            var tipoServicoPrestado = ValidarEstruturaXML.DefinirTipoServicoNFSe(xmlPrestado, PadraoNFSe.SMARAPD, "2.04");
            var tipoServicoTomado = ValidarEstruturaXML.DefinirTipoServicoNFSe(xmlTomado, PadraoNFSe.SMARAPD, "2.04");

            Assert.Equal(Servico.NFSeConsultarNfseServicoPrestado, tipoServicoPrestado);
            Assert.Equal(Servico.NFSeConsultarNfseServicoTomado, tipoServicoTomado);
        }

        [Fact]
        public void DeveRejeitarServicoSemTipoServicoConfigurado()
        {
            var xml = CriarXml("<ConsultarLoteRpsEnvio xmlns=\"http://www.tinus.com.br\"><CpfCnpj /></ConsultarLoteRpsEnvio>");

            var ex = Assert.Throws<Exception>(() =>
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.TINUS, "2.03"));

            Assert.Contains("A tag TipoServico não foi configurada", ex.Message);
        }

        private static XmlDocument CriarXml(string conteudoXML)
        {
            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);

            return xml;
        }
    }
}
