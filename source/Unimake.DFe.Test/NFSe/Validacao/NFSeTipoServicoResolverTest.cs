using System;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Validacao
{
    [Trait("DFe", "NFSe")]
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
        [InlineData(PadraoNFSe.ISSNET, "<ConsultarNfseDpsEnvio xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><IM>123</IM></ConsultarNfseDpsEnvio>", "1.01", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.ISSNET, "<EnviarLoteDpsSincronoEnvio xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><LoteDps /></EnviarLoteDpsSincronoEnvio>", "1.01", Servico.NFSeRecepcionarLoteRpsSincrono)]
        [InlineData(PadraoNFSe.DSF, "<DPS versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infDPS><tpAmb>2</tpAmb></infDPS></DPS>", "1.01", Servico.NFSeGerarNfse)]
        [InlineData(PadraoNFSe.DSF, "<DPS versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infDPS Id=\"DPS123\" /></DPS>", "1.01", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.DSF, "<NFSe versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe Id=\"NFS123\" /></NFSe>", "1.01", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.DSF, "<pedRegEvento versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infPedReg Id=\"PRE123\"><e101101 /></infPedReg></pedRegEvento>", "1.01", Servico.NFSeCancelarNfse)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ConsultaSeqRps xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></ns1:ConsultaSeqRps>", "1.00", Servico.NFSeConsultarSequenciaLoteNotaRPS)]
        [InlineData(PadraoNFSe.DSF, "<x:ConsultaSeqRps xmlns:x=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></x:ConsultaSeqRps>", "1.00", Servico.NFSeConsultarSequenciaLoteNotaRPS)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ReqConsultaNotas xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /></ns1:ReqConsultaNotas>", "1.00", Servico.NFSeConsultarNotaValida)]
        [InlineData(PadraoNFSe.DSF, "<ns1:ReqEnvioLoteRPS xmlns:ns1=\"http://localhost:8080/WsNFe2/lote\"><Cabecalho Versao=\"1.00\" /><Lote /></ns1:ReqEnvioLoteRPS>", "1.00", Servico.NFSeRecepcionarLoteRpsSincrono)]
        [InlineData(PadraoNFSe.DSF, "<ConsultarSituacaoLoteRpsEnvio xmlns=\"http://www.ginfes.com.br/servico_consultar_situacao_lote_rps_envio_v03.xsd\"><Protocolo>1</Protocolo></ConsultarSituacaoLoteRpsEnvio>", "3.00", Servico.NFSeConsultarSituacaoLoteRps)]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarNfseEnvio xmlns=\"http://www.ginfes.com.br/servico_consultar_nfse_envio_v03.xsd\"><Prestador /><PeriodoEmissao /></ConsultarNfseEnvio>", "3.01", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarNfseRpsEnvio xmlns=\"http://www.ginfes.com.br/servico_consultar_nfse_rps_envio_v03.xsd\"><Prestador /><IdentificacaoRps /></ConsultarNfseRpsEnvio>", "3.01", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.PROPRIOFORTALEZACE, "<ConsultarNfseEnvio><Prestador /><IntermediarioServico /></ConsultarNfseEnvio>", "4.00", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.PROPRIOFORTALEZACE, "<ConsultarNfseRpsEnvio><Prestador /><IdentificacaoRps /></ConsultarNfseRpsEnvio>", "4.00", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.EQUIPLANO, "<es:esConsultarNfseEnvio xmlns:es=\"http://www.equiplano.com.br/esnfs\"><prestador /></es:esConsultarNfseEnvio>", "1.00", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.AGILI, "<ConsultarRequerimentoCancelamentoEnvio><IdentificacaoPrestador /></ConsultarRequerimentoCancelamentoEnvio>", "1.00", Servico.NFSeConsultarRequerimentoCancelamento)]
        [InlineData(PadraoNFSe.WEBFISCO, "<ConsultaNfe><prf>1</prf><usr>1</usr><ctr>1</ctr><tipo>1</tipo></ConsultaNfe>", "1.00", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.WEBFISCO, "<ConsultaNfe><pass>?</pass><prf>?</prf><usr>?</usr><ctr>?</ctr><tipo>?</tipo><obs>?</obs></ConsultaNfe>", "1.00", Servico.NFSeObterNotaFiscalXml)]
        [InlineData(PadraoNFSe.WEBFISCO, "<EnvNfe><prf>1</prf><usr>1</usr><ctr>1</ctr></EnvNfe>", "1.00", Servico.NFSeRecepcionarLoteRpsSincrono)]
        [InlineData(PadraoNFSe.INTERSOL, "<p:ConsultarNfseEnvio xmlns:p=\"http://www.abrasf.org.br/nfse.xsd\"><Prestador /></p:ConsultarNfseEnvio>", "1.00", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.INTERSOL, "<p:ConsultarNfseRpsEnvio xmlns:p=\"http://www.abrasf.org.br/nfse.xsd\"><IdentificacaoRps /></p:ConsultarNfseRpsEnvio>", "1.00", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.METROPOLIS, "<ConsultarNfseEnvio><Prestador /></ConsultarNfseEnvio>", "1.00", Servico.NFSeConsultarNfse)]
        [InlineData(PadraoNFSe.METROPOLIS, "<ConsultarNfseRpsEnvio><IdentificacaoRps /></ConsultarNfseRpsEnvio>", "1.00", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.MEMORY, "<consultarLoteRPS><protocolo>1</protocolo></consultarLoteRPS>", "1.00", Servico.NFSeConsultarLoteRps)]
        [InlineData(PadraoNFSe.CONAM, "<Sdt_consultaprotocoloin xmlns=\"NFe\"><Protocolo>1</Protocolo></Sdt_consultaprotocoloin>", "4.00", Servico.NFSeConsultarNfsePorRps)]
        [InlineData(PadraoNFSe.CONAM, "<Sdt_consultanotasprotocoloin xmlns=\"NFe\"><Protocolo>1</Protocolo></Sdt_consultanotasprotocoloin>", "4.00", Servico.NFSeConsultarLoteRps)]
        [InlineData(PadraoNFSe.PAULISTANA, "<p1:PedidoInformacoesLote xmlns:p1=\"http://www.prefeitura.sp.gov.br/nfe\"><Cabecalho Versao=\"1\" /></p1:PedidoInformacoesLote>", "1.00", Servico.NFSeConsultaInformacoesLote)]
        [InlineData(PadraoNFSe.PAULISTANA, "<p1:PedidoInformacoesLote xmlns:p1=\"http://www.prefeitura.sp.gov.br/nfe\"><Cabecalho Versao=\"2\" /></p1:PedidoInformacoesLote>", "2.00", Servico.NFSeConsultaInformacoesLote)]
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
        public void DeveAplicarExcecaoMunicipalDoTipoServicoNFSe()
        {
            var xml = CriarXml("<EnviarLoteRpsSincronoEnvio xmlns=\"http://www.abrasf.org.br/nfse.xsd\"><LoteRps /></EnviarLoteRpsSincronoEnvio>");

            var tipoServicoPadrao = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SMARAPD, "2.04", 0);
            var tipoServicoExcecao = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SMARAPD, "2.04", 2111300);

            Assert.Equal(Servico.NFSeRecepcionarLoteRpsSincrono, tipoServicoPadrao);
            Assert.Equal(Servico.NFSeRecepcionarLoteRps, tipoServicoExcecao);
        }

        [Theory]
        [InlineData(TipoAmbiente.Producao, Servico.NFSeEnvioLoteRps)]
        [InlineData(TipoAmbiente.Homologacao, Servico.NFSeTesteEnvioLoteRps)]
        public void DeveAplicarExcecaoPorAmbienteDoTipoServicoNFSe(TipoAmbiente tipoAmbiente, Servico tipoServicoEsperado)
        {
            var xml = CriarXml("<PedidoEnvioLoteRPS xmlns=\"http://www.prefeitura.sp.gov.br/nfe\"><Cabecalho Versao=\"1\" /><RPS /></PedidoEnvioLoteRPS>");

            var tipoServico = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.PAULISTANA, "1.00", 3550308, tipoAmbiente);

            Assert.Equal(tipoServicoEsperado, tipoServico);
        }

        [Fact]
        public void DeveAplicarExcecaoPorAmbienteNoEnvioPaulistanaVersao2()
        {
            var xml = CriarXml("<PedidoEnvioLoteRPS xmlns=\"http://www.prefeitura.sp.gov.br/nfe\"><Cabecalho Versao=\"2\" /><IBSCBS /></PedidoEnvioLoteRPS>");

            var tipoServico = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.PAULISTANA, "2.00", 3550308, TipoAmbiente.Homologacao);

            Assert.Equal(Servico.NFSeTesteEnvioLoteRps, tipoServico);
        }

        [Fact]
        public void DeveManterProducaoComoAmbientePadraoNosOverloadsExistentes()
        {
            const string conteudoXML = "<PedidoEnvioLoteRPS xmlns=\"http://www.prefeitura.sp.gov.br/nfe\"><Cabecalho Versao=\"1\" /><RPS /></PedidoEnvioLoteRPS>";
            var xml = CriarXml(conteudoXML);

            var tipoServicoPorString = ValidarEstruturaXML.DefinirTipoServicoNFSe(conteudoXML, PadraoNFSe.PAULISTANA, "1.00", 3550308);
            var tipoServicoPorXml = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.PAULISTANA, "1.00", 3550308);
            var tipoServicoHomologacaoPorString = ValidarEstruturaXML.DefinirTipoServicoNFSe(conteudoXML, PadraoNFSe.PAULISTANA, "1.00", 3550308, TipoAmbiente.Homologacao);

            Assert.Equal(Servico.NFSeEnvioLoteRps, tipoServicoPorString);
            Assert.Equal(Servico.NFSeEnvioLoteRps, tipoServicoPorXml);
            Assert.Equal(Servico.NFSeTesteEnvioLoteRps, tipoServicoHomologacaoPorString);
        }

        [Theory]
        [InlineData(3550308, TipoAmbiente.Homologacao, "NFSeTesteEnvioLoteRps")]
        [InlineData(3550308, TipoAmbiente.Producao, "NFSeEnvioLoteRps")]
        [InlineData(4106902, TipoAmbiente.Homologacao, "NFSeRecepcionarLoteRps")]
        [InlineData(4106902, TipoAmbiente.Producao, "NFSeGerarNfse")]
        public void DeveRespeitarPrecedenciaDasExcecoesDeTipoServico(int codigoMunicipio, TipoAmbiente tipoAmbiente, string tipoServicoEsperado)
        {
            var configuracao = new XmlDocument();
            configuracao.LoadXml(
                "<Servico><TipoServico>NFSeGerarNfse" +
                "<Excecao tipoAmbiente=\"2\">NFSeRecepcionarLoteRps</Excecao>" +
                "<Excecao codMunicipio=\"3550308\">NFSeEnvioLoteRps</Excecao>" +
                "<Excecao codMunicipio=\"3550308\" tipoAmbiente=\"2\">NFSeTesteEnvioLoteRps</Excecao>" +
                "</TipoServico></Servico>");

            var method = typeof(ValidarEstruturaXML).GetMethod(
                "ObterTipoServico",
                BindingFlags.NonPublic | BindingFlags.Static);

            var tipoServico = (string)method.Invoke(
                null,
                new object[] { configuracao.DocumentElement, codigoMunicipio, tipoAmbiente });

            Assert.Equal(tipoServicoEsperado, tipoServico);
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
        public void DeveRejeitarServicoNFSeNaoConfigurado()
        {
            var xml = CriarXml("<ConsultarLoteRpsEnvio xmlns=\"http://www.tinus.com.br\"><CpfCnpj /></ConsultarLoteRpsEnvio>");

            var ex = Assert.Throws<Exception>(() =>
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.TINUS, "9.99"));

            Assert.Contains("Não foi possível encontrar", ex.Message);
        }

        private static XmlDocument CriarXml(string conteudoXML)
        {
            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);

            return xml;
        }
    }
}
