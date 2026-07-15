using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class NFSeVersionResolverTest
    {
        [Theory]
        [InlineData(PadraoNFSe.DSF, "<ns1:ConsultaSeqRps xmlns:ns1=\"urn:dsf\" />", 0, "1.00")]
        [InlineData(PadraoNFSe.DSF, "<CancelarNfseEnvio xmlns=\"http://www.ginfes.com.br/servico_cancelar_nfse_envio\" />", 0, "2.00")]
        [InlineData(PadraoNFSe.DSF, "<ConsultarSituacaoLoteRpsEnvio />", 0, "3.00")]
        [InlineData(PadraoNFSe.FIORILLI, "<ConsultarLoteRpsEnvio />", 0, "1.01")]
        [InlineData(PadraoNFSe.FIORILLI, "<GerarNfseEnvio />", 0, "2.01")]
        [InlineData(PadraoNFSe.GINFES, "<CancelarNfseEnvio><Prestador /></CancelarNfseEnvio>", 0, "2.00")]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarLoteRpsEnvio />", 0, "3.01")]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarLoteRpsEnvio />", 4125506, "3.00")]
        [InlineData(PadraoNFSe.GINFES, "<CancelarNfseEnvio><Pedido /></CancelarNfseEnvio>", 4125506, "3.00")]
        [InlineData(PadraoNFSe.GINFES, "<ConsultarLoteRpsEnvio xmlns=\"http://nfe.sjp.pr.gov.br/servico_consultar_lote_rps_envio_v03.xsd\" />", 0, "3.00")]
        [InlineData(PadraoNFSe.GISSONLINE, "<ns4:EnviarLoteRpsEnvio xmlns:ns4=\"http://www.giss.com.br/enviar-lote-rps-envio-v2_04.xsd\"><ns4:LoteRps><ns2:IBSCBS xmlns:ns2=\"http://www.giss.com.br/tipos-v2_04.xsd\" /></ns4:LoteRps></ns4:EnviarLoteRpsEnvio>", 0, "2.05")]
        [InlineData(PadraoNFSe.GISSONLINE, "<ns4:EnviarLoteRpsEnvio xmlns:ns4=\"http://www.giss.com.br/enviar-lote-rps-envio-v2_04.xsd\"><ns4:LoteRps /></ns4:EnviarLoteRpsEnvio>", 0, "2.04")]
        [InlineData(PadraoNFSe.PRONIM, "<ConsultarSituacaoLoteRpsEnvio />", 0, "1.00")]
        [InlineData(PadraoNFSe.PRONIM, "<GerarNfseEnvio />", 0, "2.03")]
        [InlineData(PadraoNFSe.SMARAPD, "<NFSe />", 0, "1.01")]
        [InlineData(PadraoNFSe.SMARAPD, "<nfd />", 0, "1.00")]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarLoteRpsEnvio />", 0, "2.04")]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarLoteRpsEnvio />", 3205002, "2.04")]
        [InlineData(PadraoNFSe.IPM, "<nfse><nfse_teste /></nfse>", 0, "1.20")]
        [InlineData(PadraoNFSe.IPM, "<GerarNfseEnvio />", 0, "2.04")]
        public void DeveResolverVersoesDependentesDaRaiz(
            PadraoNFSe padrao,
            string conteudoXML,
            int codigoMunicipio,
            string versaoEsperada)
        {
            Assert.Equal(
                versaoEsperada,
                DefinirVersao(conteudoXML, padrao, codigoMunicipio)
            );
        }

        [Fact]
        public void DevePriorizarVersaoDeclaradaNoXml()
        {
            Assert.Equal(
                "2.03",
                DefinirVersao(
                    "<EnviarLoteRpsEnvio versao=\"2.03\" />",
                    PadraoNFSe.TINUS,
                    0
                )
            );
        }

        [Fact]
        public void DevePriorizarVersaoDeclaradaEmTagVersao()
        {
            Assert.Equal(
                "4.00",
                DefinirVersao(
                    "<EnviarLoteRpsEnvio><Cabecalho><versao>4.0</versao></Cabecalho></EnviarLoteRpsEnvio>",
                    PadraoNFSe.CONAM,
                    0
                )
            );
        }

        [Fact]
        public void DeveUsarTagIdentificadoraMesmoComVersaoConhecida()
        {
            var xml = CriarXml("<DPS><infDPS><tpAmb>2</tpAmb></infDPS></DPS>");
            var configuracao = CriarXml(
                "<ServicosValidacao>" +
                "<NFSe><Padrao nome=\"NACIONAL\">" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"tpAmb\">" +
                "<Descricao>Gerar NFSe</Descricao>" +
                "</Servico>" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"infDPS\">" +
                "<Descricao>Consultar NFSe por DPS</Descricao>" +
                "</Servico>" +
                "</Padrao></NFSe>" +
                "</ServicosValidacao>"
            );

            var servico = TratarNFSe(xml, "1.01", configuracao, PadraoNFSe.NACIONAL);

            Assert.Equal("Gerar NFSe", servico.SelectSingleNode("Descricao").InnerText);
        }

        [Fact]
        public void DeveContinuarAteONodeGenericoQuandoEspecificoNaoCombinar()
        {
            var xml = CriarXml("<DPS><infDPS /></DPS>");
            var configuracao = CriarXml(
                "<ServicosValidacao>" +
                "<NFSe><Padrao nome=\"NACIONAL\">" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"tpAmb\">" +
                "<Descricao>Gerar NFSe</Descricao>" +
                "</Servico>" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\">" +
                "<Descricao>Genérico</Descricao>" +
                "</Servico>" +
                "</Padrao></NFSe>" +
                "</ServicosValidacao>"
            );

            var servico = TratarNFSe(xml, "1.01", configuracao, PadraoNFSe.NACIONAL);

            Assert.Equal("Genérico", servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData("<nfse><nfse_teste /></nfse>", "Gerar NFSe")]
        [InlineData("<nfse><nf><situacao>C</situacao></nf></nfse>", "Evento de cancelamento da NFSe")]
        [InlineData("<nfse><pesquisa /></nfse>", "Consulta da NFSe")]
        public void DeveIdentificarServicosIPM120(string conteudoXML, string descricaoEsperada)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                PadraoNFSe = PadraoNFSe.IPM,
                CodigoMunicipio = 4201307,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(
                CriarXml(conteudoXML),
                configuracao
            );

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal(descricaoEsperada, resultado.Descricao);
        }

        [Fact]
        public void DeveIdentificarConsultaLoteRpsIPM204()
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                PadraoNFSe = PadraoNFSe.IPM,
                CodigoMunicipio = 4202305,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(
                CriarXml(
                    "<ConsultarLoteRpsEnvio>" +
                    "<Prestador><CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj></Prestador>" +
                    "<Protocolo>1</Protocolo>" +
                    "</ConsultarLoteRpsEnvio>"
                ),
                configuracao
            );

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal("Consulta NFSe por RPS", resultado.Descricao);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\EnviarLoteRpsEnvio-env-loterps.xml", 4317103, "1.00", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\CancelarNfseEnvio-ped-cannfse.xml", 4317103, "1.00", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\SubstituirNfseEnvio-ped-substnfse.xml", 4317103, "1.00", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\ConsultarLoteRpsEnvio-ped-loterps.xml", 4317103, "1.00", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\ConsultarNfseRpsEnvio-ped-sitnfserps.XML", 4317103, "1.00", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\1.00\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", 4317103, "1.00", "Consulta situação lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\EnviarLoteRpsEnvio-env-loterps.xml", 4319901, "2.04", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\CancelarNfseEnvio-ped-cannfse.xml", 4319901, "2.04", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\SubstituirNfseEnvio-ped-substnfse.xml", 4319901, "2.04", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\ConsultarLoteRpsEnvio-ped-loterps.xml", 4319901, "2.04", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\ConsultarNfseRpsEnvio-ped-sitnfserps.XML", 4319901, "2.04", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\DBSELLER\2.04\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", 4319901, "2.04", "Consulta lote RPS")]
        public void DeveIdentificarServicosDBSELLER(
            string arquivoXML,
            int codigoMunicipio,
            string versaoEsperada,
            string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.DBSELLER, codigoMunicipio);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.DBSELLER);

            Assert.Equal(versaoEsperada, versao);
            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00 - SaoJoseDosPinhais\EnviarLoteRpsEnvio-env-loterps.xml", "3.00", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00 - SaoJoseDosPinhais\ConsultarLoteRpsEnvio-ped-loterps.xml", "3.00", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00 - SaoJoseDosPinhais\ConsultarNfseEnvio-ped-sitnfse.xml", "3.00", "Consulta de NFSe por Data")]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00 - SaoJoseDosPinhais\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "3.00", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00 - SaoJoseDosPinhais\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", "3.00", "Consultar Situação Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\GINFES\3.00\CancelarNfseEnvio-ped-cannfse.xml", "3.00", "Cancelar nota fiscal de serviço")]
        public void DeveIdentificarServicosGINFESParaSaoJoseDosPinhais(
            string arquivoXML,
            string versaoEsperada,
            string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.GINFES, 4125506);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.GINFES);

            Assert.Equal(versaoEsperada, versao);
            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\CancelarNfseEnvio-ped-cannfse.xml", "1.01", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarDadosCadastraisEnvio-ped-consdadoscad.xml", "1.01", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarLoteRpsEnvio-ped-loterps.xml", "1.01", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "1.01", "Consulta NFSe por Faixa")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "1.01", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "1.01", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "1.01", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarRpsDisponivelEnvio-ped-consrpsdisp.xml", "1.01", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\ConsultarUrlNfseEnvio-ped-urlnfse.xml", "1.01", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\GerarNfseEnvio-env-loterps.xml", "1.01", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\EnviarLoteRpsEnvio-env-loterps.xml", "1.01", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\1.01\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "1.01", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\CancelarNfseEnvio-ped-cannfse.xml", "2.04", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarDadosCadastraisEnvio-ped-consdadoscad.xml", "2.04", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarLoteRpsEnvio-ped-loterps.xml", "2.04", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "2.04", "Consulta NFSe por Faixa")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "2.04", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "2.04", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "2.04", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarRpsDisponivelEnvio-ped-consrpsdisp.xml", "2.04", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\ConsultarUrlNfseEnvio-ped-urlnfse.xml", "2.04", "Consulta os dados cadastrais")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\GerarNfseEnvio-env-loterps.xml", "2.04", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\EnviarLoteRpsEnvio-env-loterps.xml", "2.04", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "2.04", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\ISSNET\2.04\SubstituirNfseEnvio-ped-substnfse.xml", "2.04", "Substituir nota fiscal de Serviço")]
        public void DeveIdentificarServicosISSNET(
            string arquivoXML,
            string versaoEsperada,
            string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.ISSNET, 5208707);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.ISSNET);

            Assert.Equal(versaoEsperada, versao);
            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\PRONIM\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        public void DeveIdentificarServicosPRONIM203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.PRONIM, 4318309);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.PRONIM);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\RLZ_INFORMATICA\2.03\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosRLZInformatica203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.RLZ_INFORMATICA, 3505500);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.RLZ_INFORMATICA);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\1.03\GerarNfseEnvio-env-loterps.xml", "Gerar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\1.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\1.03\ConsultarRpsServicoPrestado-ped-sitnfse.xml", "Consultar Rps Serviço Prestado")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\1.03\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consulta NFSe de serviços prestados")]
        public void DeveIdentificarServicosSIGCORP103(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIGCORP, 4113700);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIGCORP);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.03\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosSIGCORP203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIGCORP, 3554102);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIGCORP);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.04\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.04\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\2.04\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        public void DeveIdentificarServicosSIGCORP204ComTagsCorrigidas(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIGCORP, 3530805);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIGCORP);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\3.00\GerarNfseEnvio-env-loterps.xml", "Envio de NFSe Síncrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\3.00\GerarNfseEnvioRTC-env-loterps.xml", "Envio de NFSe Síncrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\3.00\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\3.00\ConsultarNotaValida-ped-loterps.xml", "Consulta nota fiscal de serviço válida")]
        [InlineData(@"..\..\..\NFSe\Resources\SIGCORP\3.00\ConsultarNotaPrestador-ped-sitnfse.xml", "Consulta nota fiscal de serviço do prestador")]
        public void DeveIdentificarServicosSIGCORP300(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIGCORP, 3530706);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIGCORP);

            Assert.NotNull(servico);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.00\GerarNfseEnvio-env-loterps.xml", "Envio do DPS - Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.00\GerarNfseEnvio_RTC-env-loterps.xml", "Envio do DPS - Sincrono")]
        public void DeveIdentificarServicosSIMPLISS100(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIMPLISS, 0);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIMPLISS);

            Assert.NotNull(servico);
            Assert.Equal("1.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.01\GerarNfseEnvio-env-loterps.xml", "Envio do DPS - Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.01\CancelarNfseEnvio-ped-cannfse.xml", "Envio do cancelamento de NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.01\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\1.01\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consultar NFSe por RPS")]
        public void DeveIdentificarServicosSIMPLISS101(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIMPLISS, 3306305);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIMPLISS);

            Assert.NotNull(servico);
            Assert.Equal("1.01", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\2.03\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosSIMPLISS203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIMPLISS, 3306305);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIMPLISS);

            Assert.NotNull(servico);
            Assert.Equal("2.03", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consultar NFSe por rps")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\SIMPLISS\3.00\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", "Consultar Situação Lote RPS")]
        public void DeveIdentificarServicosSIMPLISS300(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SIMPLISS, 3538709);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SIMPLISS);

            Assert.NotNull(servico);
            Assert.Equal("3.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.00\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.00\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.00\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        public void DeveIdentificarServicosSMARAPD100(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SMARAPD, 3551702);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SMARAPD);

            Assert.NotNull(servico);
            Assert.Equal("1.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\ConsultarEventosNfse-ped-consevennfse.xml", "Consultar Eventos Diversos da NFSe NACIONAL")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\1.01\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consultar NFSe por DPS")]
        public void DeveIdentificarServicosSMARAPD101(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SMARAPD, 3506003);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SMARAPD);

            Assert.NotNull(servico);
            Assert.Equal("1.01", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosSMARAPD203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SMARAPD, 3530607);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SMARAPD);

            Assert.NotNull(servico);
            Assert.Equal("2.03", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\SMARAPD\2.04\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosSMARAPD204(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.SMARAPD, 3205002);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.SMARAPD);

            Assert.NotNull(servico);
            Assert.Equal("2.04", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consultar Situação Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\1.00\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        public void DeveIdentificarServicosTINUS100(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.TINUS, 2503209);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.TINUS);

            Assert.NotNull(servico);
            Assert.Equal("1.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TINUS\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        public void DeveIdentificarServicosTINUS203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.TINUS, 2412005);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.TINUS);

            Assert.NotNull(servico);
            Assert.Equal("2.03", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml", "Consultar Situação Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar NFSe")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.00\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        public void DeveIdentificarServicosTIPLAN100(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.TIPLAN, 3302403);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.TIPLAN);

            Assert.NotNull(servico);
            Assert.Equal("1.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\1.01\GerarNfseEnvio-env-loterps.xml", "Envio do DPS - Sincrono")]
        public void DeveIdentificarServicosTIPLAN101(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.TIPLAN, 3304524);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.TIPLAN);

            Assert.NotNull(servico);
            Assert.Equal("1.01", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\EnviarLoteRpsEnvio-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\EnviarLoteRpsEnvio-ComRTC-env-loterps.xml", "Recepcionar Lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\EnviarLoteRpsSincronoEnvio-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\EnviarLoteRpsSincronoEnvio-ComRTC-env-loterps.xml", "Recepcionar Lote RPS Sincrono")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\GerarNfseEnvio-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\GerarNfseEnvio-ComRTC-env-loterps.xml", "Gerar Nota Fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\CancelarNfseEnvio-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\CancelarNfseEnvio-ComRTC-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\SubstituirNfseEnvio-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\SubstituirNfseEnvio-ComRTC-ped-substnfse.xml", "Substituir nota fiscal de Serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\ConsultarLoteRpsEnvio-ped-loterps.xml", "Consulta lote RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\ConsultarNfseRpsEnvio-ped-sitnfserps.xml", "Consulta NFSe por RPS")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml", "Consulta NFSe de serviços prestados")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml", "Consulta NFSe de serviços tomados")]
        [InlineData(@"..\..\..\NFSe\Resources\TIPLAN\2.03\ConsultarNfseFaixaEnvio-ped-sitnfse.xml", "Consulta NFSe por Faixa")]
        public void DeveIdentificarServicosTIPLAN203(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.TIPLAN, 3302403);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.TIPLAN);

            Assert.NotNull(servico);
            Assert.Equal("2.03", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\EnvioLoteRps-env-loterps.xml", "Envio Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\TesteEnvioLoteRps-env-loterps.xml", "Envio Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\EnvioRps-env-loterps.xml", "Envio Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\CancelamentoNfe-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\ConsultaInformacoesLote-ped-sitloterps.xml", "Consulta Informações Lote")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\ConsultaLote-ped-loterps.xml", "Consulta lote")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\ConsultaNFeEmitidas-ped-nfseemit.xml", "Consulta NFSe Periodo")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\ConsultaNFeRecebidas-ped-sitnfserec.xml", "Consulta NFSe Periodo")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\1.00\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar Nfse")]
        public void DeveIdentificarServicosPAULISTANA100(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.PAULISTANA, 3550308);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.PAULISTANA);

            Assert.NotNull(servico);
            Assert.Equal("1.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\EnvioLoteRps-env-loterps.xml", "Envio Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\TesteEnvioLoteRps-env-loterps.xml", "Envio Lote Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\EnvioRps-env-loterps.xml", "Envio Rps")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\CancelamentoNfe-ped-cannfse.xml", "Cancelar nota fiscal de serviço")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\ConsultaInformacoesLote-ped-sitloterps.xml", "Consulta Informações Lote")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\ConsultaLote-ped-loterps.xml", "Consulta lote")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\ConsultaNFeEmitidas-ped-nfseemit.xml", "Consulta NFSe Periodo")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\ConsultaNFeRecebidas-ped-sitnfserec.xml", "Consulta NFSe Periodo")]
        [InlineData(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\ConsultarNfseEnvio-ped-sitnfse.xml", "Consultar Nfse")]
        public void DeveIdentificarServicosPAULISTANA200(string arquivoXML, string descricaoEsperada)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXML);

            var configuracaoValidacao = new XmlDocument();
            configuracaoValidacao.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var versao = DefinirVersao(xml.OuterXml, PadraoNFSe.PAULISTANA, 3550308);
            var servico = TratarNFSe(xml, versao, configuracaoValidacao, PadraoNFSe.PAULISTANA);

            Assert.NotNull(servico);
            Assert.Equal("2.00", versao);
            Assert.Equal(descricaoEsperada, servico.SelectSingleNode("Descricao").InnerText);
        }

        private static string DefinirVersao(
            string conteudoXML,
            PadraoNFSe padrao,
            int codigoMunicipio)
        {
            return ValidarEstruturaXML.DefinirVersaoNFSe(
                CriarXml(conteudoXML),
                padrao,
                codigoMunicipio
            );
        }

        private static XmlNode TratarNFSe(
            XmlDocument xml,
            string versao,
            XmlDocument configuracao,
            PadraoNFSe padrao)
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "TratarNFSe",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (XmlNode)method.Invoke(
                null,
                new object[]
                {
                    xml,
                    versao,
                    TipoDFe.NFSe,
                    xml.DocumentElement.Name,
                    configuracao,
                    padrao
                }
            );
        }

        private static XmlDocument CarregarConfigValidacao()
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "CarregarConfigValidacao",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (XmlDocument)method.Invoke(null, null);
        }

        private static XmlDocument CriarXml(string conteudoXML)
        {
            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);
            return xml;
        }
    }
}
