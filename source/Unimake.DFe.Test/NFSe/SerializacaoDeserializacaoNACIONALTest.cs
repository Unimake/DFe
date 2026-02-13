using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;

namespace Unimake.DFe.Test.NFSe.NACIONAL
{
    public class SerializacaoDesserializacaoNacionalTest
    {
        private XmlNamespaceManager CreateNamespaceManager(XmlDocument doc)
        {
            var nsmgr = new XmlNamespaceManager(doc.NameTable);
            nsmgr.AddNamespace("ns", "http://www.sped.fazenda.gov.br/nfse");
            return nsmgr;
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [Trait("Versao", "1.01")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\GerarNfseEnvio-env-loterps.xml")]
        public void GerarNfseNACIONAL_V101_ComIBSCBS_CasoReal(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            // Deserialização
            var lido = new DPS().LerXML<DPS>(docFixture);

            // Verificações básicas v1.01
            Assert.Equal("1.01", lido.Versao);
            Assert.NotNull(lido.InfDPS);
            Assert.Equal("DPS410690211234567800019500001000000000000001", lido.InfDPS.Id);

            // Verificações específicas do IBSCBS v1.01
            var ibscbs = lido.InfDPS.IBSCBS;
            Assert.NotNull(ibscbs);

            // Novos campos obrigatórios v1.01
            Assert.Equal(0, ibscbs.IndDest); // Novo campo obrigatório

            // Novos campos opcionais v1.01 (presentes neste XML)
            Assert.NotNull(ibscbs.TpOper);
            Assert.Equal(TpOperacaoGov.FornecimentoComPagamentoPosterior, ibscbs.TpOper); // tpOper=1

            // Campos existentes
            Assert.Equal(0, ibscbs.FinNFSe);
            Assert.Equal(0, ibscbs.IndFinal);
            Assert.Equal("000001", ibscbs.CIndOp); // cIndOp="000001"
            Assert.Equal(TipoEnteGovernamental.Uniao, ibscbs.TpEnteGov); // tpEnteGov=1

            // Campo opcional não presente neste XML
            Assert.Null(ibscbs.GRefNFSe); // Não informado neste caso

            // Verificação da estrutura de valores IBSCBS
            Assert.NotNull(ibscbs.Valores);
            Assert.NotNull(ibscbs.Valores.Trib);
            Assert.NotNull(ibscbs.Valores.Trib.GIBSCBS);

            var gIBSCBS = ibscbs.Valores.Trib.GIBSCBS;
            Assert.Equal("101", gIBSCBS.CST);
            Assert.Equal("000001", gIBSCBS.CClassTrib); // "000001"
            Assert.Equal("01", gIBSCBS.CCredPres); // "01"

            // Serialização (round-trip)
            var docRoundTrip = lido.GerarXML();
            Assert.NotNull(docRoundTrip);

            // Verificação da estrutura XML resultante
            var nsmgr = CreateNamespaceManager(docRoundTrip);
            var ibscbsNode = docRoundTrip.SelectSingleNode("//ns:IBSCBS", nsmgr);
            Assert.NotNull(ibscbsNode);

            // Verifica presença dos campos v1.01 corretos
            Assert.NotNull(ibscbsNode.SelectSingleNode("ns:indDest", nsmgr));
            Assert.NotNull(ibscbsNode.SelectSingleNode("ns:tpOper", nsmgr));
            Assert.NotNull(ibscbsNode.SelectSingleNode("ns:tpEnteGov", nsmgr));

            // Verifica ausência dos campos v1.00 removidos
            Assert.Null(ibscbsNode.SelectSingleNode("ns:indPessoas", nsmgr));
            Assert.Null(ibscbsNode.SelectSingleNode("ns:adq", nsmgr));

            // Verifica campo opcional não informado
            Assert.Null(ibscbsNode.SelectSingleNode("ns:gRefNFSe", nsmgr));

            // Teste de criação do zero com mesmos valores
            var criado = new DPS
            {
                Versao = "1.01",
                InfDPS = new InfDPS
                {
                    Id = lido.InfDPS.Id,
                    TpAmb = lido.InfDPS.TpAmb,
                    DhEmi = lido.InfDPS.DhEmi,
                    VerAplic = lido.InfDPS.VerAplic,
                    Serie = lido.InfDPS.Serie,
                    NDPS = lido.InfDPS.NDPS,
                    DCompet = lido.InfDPS.DCompet,
                    TpEmit = lido.InfDPS.TpEmit,
                    CLocEmi = lido.InfDPS.CLocEmi,
                    Prest = lido.InfDPS.Prest,
                    Toma = lido.InfDPS.Toma,
                    Serv = lido.InfDPS.Serv,
                    Valores = new Valores
                    {
                        VServPrest = lido.InfDPS.Valores.VServPrest,
                        Trib = lido.InfDPS.Valores.Trib
                    },
                    IBSCBS = new IBSCBS
                    {
                        FinNFSe = ibscbs.FinNFSe,
                        IndFinal = ibscbs.IndFinal,
                        CIndOp = ibscbs.CIndOp,
                        TpOper = ibscbs.TpOper, // v1.01
                        TpEnteGov = ibscbs.TpEnteGov,
                        IndDest = ibscbs.IndDest, // v1.01
                        Valores = ibscbs.Valores
                    }
                }
            };

            var docCriado = criado.GerarXML();

            // Compara elementos essenciais
            var ibscbsCriado = docCriado.SelectSingleNode("//ns:IBSCBS", nsmgr);
            Assert.NotNull(ibscbsCriado);

            // Verifica valores específicos
            Assert.Equal("0", ibscbsCriado.SelectSingleNode("ns:indDest", nsmgr)?.InnerText);
            Assert.Equal("1", ibscbsCriado.SelectSingleNode("ns:tpOper", nsmgr)?.InnerText);
            Assert.Equal("1", ibscbsCriado.SelectSingleNode("ns:tpEnteGov", nsmgr)?.InnerText);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [Trait("Versao", "1.01")]
        public void IBSCBS_V101_ShouldSerialize_CenarioReal()
        {

            var ibscbs = new IBSCBS
            {
                FinNFSe = 0,
                IndFinal = 0,
                CIndOp = "000001",
                TpOper = TpOperacaoGov.FornecimentoComPagamentoPosterior,
                TpEnteGov = TipoEnteGovernamental.Municipio,
                IndDest = 0,
            };

            Assert.True(ibscbs.ShouldSerializeTpOper());
            Assert.True(ibscbs.ShouldSerializeTpEnteGov());

            Assert.False(ibscbs.ShouldSerializeXTpEnteGov());

            var dps = new DPS
            {
                Versao = "1.01",
                InfDPS = new InfDPS
                {
                    Id = "DPS_TESTE_V101",
                    TpAmb = TipoAmbiente.Homologacao,
                    DhEmi = DateTime.Now,
                    VerAplic = "1.01.01",
                    Serie = "1",
                    NDPS = "1",
                    DCompet = DateTime.Now,
                    TpEmit = TipoEmitenteNFSe.Prestador,
                    CLocEmi = 4202909,

                    // Dados mínimos obrigatórios
                    Prest = new Prest
                    {
                        CNPJ = "99999999999999",
                        RegTrib = new RegTrib
                        {
                            OpSimpNac = OptSimplesNacional.NaoOptante,
                            RegEspTrib = RegEspTrib.Nenhum
                        }
                    },

                    Toma = new Toma
                    {
                        CNPJ = "00000000000000",
                        XNome = "TESTE LTDA"
                    },

                    Serv = new Serv
                    {
                        LocPrest = new LocPrest
                        {
                            CLocPrestacao = 4202909
                        },
                        CServ = new CServ
                        {
                            CTribNac = "140101",
                            XDescServ = "Teste de servico v1.01"
                        }
                    },

                    Valores = new Valores
                    {
                        VServPrest = new VServPrest
                        {
                            VServ = 10.00
                        }
                    },
                    IBSCBS = ibscbs
                }
            };

            // Agora a serialização deve funcionar
            var xml = dps.GerarXML();
            var nsmgr = CreateNamespaceManager(xml);
            var node = xml.SelectSingleNode("//ns:IBSCBS", nsmgr);

            Assert.NotNull(node);
            Assert.NotNull(node.SelectSingleNode("ns:tpOper", nsmgr));
            Assert.NotNull(node.SelectSingleNode("ns:indDest", nsmgr));
            Assert.Null(node.SelectSingleNode("ns:gRefNFSe", nsmgr));

            // Verifica valores corretos
            Assert.Equal("1", node.SelectSingleNode("ns:tpOper", nsmgr)?.InnerText);
            Assert.Equal("0", node.SelectSingleNode("ns:indDest", nsmgr)?.InnerText);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [Trait("Versao", "1.01")]
        public void IBSCBS_V101_ShouldSerialize_Isolado()
        {
            // Teste isolado apenas do ShouldSerialize (sem serialização XML completa)
            var ibscbs = new IBSCBS();

            // Testa ShouldSerialize para campos opcionais v1.01 (valores padrão)
            Assert.False(ibscbs.ShouldSerializeTpOper()); // null
            Assert.False(ibscbs.ShouldSerializeTpEnteGov()); // null
            Assert.False(ibscbs.ShouldSerializeXTpEnteGov()); // null/empty

            // Define valores
            ibscbs.TpOper = TpOperacaoGov.FornecimentoComPagamentoPosterior;
            ibscbs.TpEnteGov = TipoEnteGovernamental.Municipio;
            ibscbs.XTpEnteGov = "Descrição";

            // Testa ShouldSerialize com valores informados
            Assert.True(ibscbs.ShouldSerializeTpOper());
            Assert.True(ibscbs.ShouldSerializeTpEnteGov());
            Assert.True(ibscbs.ShouldSerializeXTpEnteGov());
        }

        /// <summary>
        /// Testa a geração automática do ID do DPS seguindo o padrão:
        /// DPS + Código IBGE Município(7) + Tipo Inscrição(1) + Inscrição Federal(14) + Série(5) + Núm. DPS(15)
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        public void GerarNfseNACIONAL_GeracaoAutomaticaID()
        {
            // Arrange - Exemplo do manual: Porto Alegre/RS, CNPJ 01878890000100, Série 10, Número 9147
            var dps = new DPS
            {
                Versao = "1.00",
                InfDPS = new InfDPS
                {
                    // ID não informado - será gerado automaticamente
                    CLocEmi = 4314902,              // Porto Alegre/RS
                    Serie = "10",
                    NDPS = "9147",
                    Prest = new Prest
                    {
                        CNPJ = "01878890000100"     // CNPJ do prestador
                    }
                }
            };

            // Act - Acessa a propriedade Id para forçar a geração
            var idGerado = dps.InfDPS.Id;

            // Assert - Valida o ID esperado: DPS + 4314902 + 2 + 01878890000100 + 00010 + 000000000009147
            var idEsperado = "DPS431490220187889000010000010000000000009147";
            Assert.Equal(idEsperado, idGerado);
            Assert.Equal(45, idGerado.Length); // 3 + 7 + 1 + 14 + 5 + 15
            Assert.StartsWith("DPS", idGerado);
        }

        /// <summary>
        /// Testa a desserialização do XML de retorno da NFSe (versão 1.01)
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [Trait("Versao", "1.01")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.01\RetornoNACIONAL.xml")]
        public void DeserializarRetornoNFSe_V101_Completo(string caminhoXml)
        {
            // Arrange
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            // Act - Desserialização
            var nfse = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe().LerXML<Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe>(docFixture);

            // Assert - Validações da estrutura principal
            Assert.NotNull(nfse);
            Assert.Equal("1.01", nfse.Versao);
            Assert.NotNull(nfse.Signature);
            Assert.NotNull(nfse.InfNFSe);

            // Validações do InfNFSe
            var infNFSe = nfse.InfNFSe;
            Assert.Equal("NFS43149022226263261000198000000000000225120787292537", infNFSe.Id);
            Assert.Equal("Porto Alegre", infNFSe.XLocEmi);
            Assert.Equal("Porto Alegre", infNFSe.XLocPrestacao);
            Assert.Equal(2, infNFSe.NNFSe);
            Assert.Equal(4314902, infNFSe.CLocIncid);
            Assert.Equal("Porto Alegre", infNFSe.XLocIncid);
            Assert.Equal("Outros serviços de transporte de natureza municipal.", infNFSe.XTribNac);
            Assert.Equal("Serviços de transporte rodoviário de cargas sólidas a granel", infNFSe.XNBS);
            Assert.Equal("SefinNac_Pre_1.4.0", infNFSe.VerAplic);
            Assert.Equal(2, (int)infNFSe.AmbGer); // Homologação
            Assert.Equal(1, infNFSe.TpEmis); // Normal
            Assert.Equal(1, infNFSe.ProcEmi); // Aplicativo do contribuinte
            Assert.Equal(100, infNFSe.CStat); // Autorizada
            Assert.NotEqual(default(DateTimeOffset), infNFSe.DhProc);
            Assert.Equal("151689", infNFSe.NDFSe);

            // Validações do Emitente
            var emit = infNFSe.Emit;
            Assert.NotNull(emit);
            Assert.Equal("26263261000198", emit.CNPJ);
            Assert.Equal("HPP COMERCIO E DISTRIBUICAO DE ALIMENTOS PARA ANIMAIS LTDA", emit.XNome);
            Assert.Null(emit.XFant);
            Assert.NotNull(emit.EnderNac);
            Assert.Equal("MARQUES DO MARICA", emit.EnderNac.XLgr);
            Assert.Equal("273", emit.EnderNac.Nro);
            Assert.Equal("VILA NOVA", emit.EnderNac.XBairro);
            Assert.Equal(4314902, emit.EnderNac.CMun);
            Assert.Equal("RS", emit.EnderNac.UF);
            Assert.Equal("91750460", emit.EnderNac.CEP);
            Assert.Equal("5132093097", emit.Fone);
            Assert.Equal("FISCAL@HPPDISTRIBUIDORA.COM.BR", emit.Email);

            // Validações dos Valores 
            var valores = infNFSe.Valores;
            Assert.NotNull(valores);
            Assert.Equal(1500.00, valores.VBC, 2); // 2 casas decimais de precisão
            Assert.Equal(5.00, valores.PAliqAplic, 2);
            Assert.Equal(75.00, valores.VISSQN, 2);
            Assert.Equal(1500.00, valores.VLiq, 2);

            // Validações do IBSCBS
            var ibscbs = infNFSe.IBSCBS;
            Assert.NotNull(ibscbs);
            Assert.Equal(4314902, ibscbs.CLocalidadeIncid);
            Assert.Equal("Porto Alegre", ibscbs.XLocalidadeIncid);

            // Validações dos Valores IBSCBS 
            var valoresIBSCBS = ibscbs.Valores;
            Assert.NotNull(valoresIBSCBS);
            Assert.Equal(1425.00, valoresIBSCBS.VBC, 2);
            Assert.Equal(0.00, valoresIBSCBS.VCalcReeRepRes, 2);

            // Validações UF 
            Assert.NotNull(valoresIBSCBS.UF);
            Assert.Equal(0.10, valoresIBSCBS.UF.PIBSUF, 2);
            Assert.Equal(0.10, valoresIBSCBS.UF.PAliqEfetUF, 2);

            // Validações Município 
            Assert.NotNull(valoresIBSCBS.Mun);
            Assert.Equal(0.00, valoresIBSCBS.Mun.PIBSMun, 2);
            Assert.Equal(0.00, valoresIBSCBS.Mun.PAliqEfetMun, 2);

            // Validações Federal (CBS)
            Assert.NotNull(valoresIBSCBS.Fed);
            Assert.Equal(0.90, valoresIBSCBS.Fed.PCBS, 2);
            Assert.Equal(0.90, valoresIBSCBS.Fed.PAliqEfetCBS, 2);

            // Validações dos Totalizadores
            var totCIBS = ibscbs.TotCIBS;
            Assert.NotNull(totCIBS);
            Assert.Equal(1500.00, totCIBS.VTotNF, 2);

            // Validações IBS 
            var gIBS = totCIBS.GIBS;
            Assert.NotNull(gIBS);
            Assert.Equal(1.43, gIBS.VIBSTot, 2);
            Assert.NotNull(gIBS.GIBSUFTot);
            Assert.Equal(1.43, gIBS.GIBSUFTot.VIBSUF, 2);
            Assert.NotNull(gIBS.GIBSMunTot);
            Assert.Equal(0.00, gIBS.GIBSMunTot.VIBSMun, 2);

            // Validações CBS
            var gCBS = totCIBS.GCBS;
            Assert.NotNull(gCBS);
            Assert.Equal(12.83, gCBS.VCBS, 2);

            // Validações da DPS dentro da NFSe
            var dps = infNFSe.DPS;
            Assert.NotNull(dps);
            Assert.Equal("1.01", dps.Versao);
            Assert.NotNull(dps.InfDPS);
            Assert.Equal("DPS431490222626326100019800001000000000000002", dps.InfDPS.Id);
            Assert.Equal(TipoAmbiente.Homologacao, dps.InfDPS.TpAmb); // Homologação
            Assert.NotNull(dps.Signature);

            // Act - Serialização (Round-trip)
            var docRoundTrip = nfse.GerarXML();
            Assert.NotNull(docRoundTrip);

            // Validação da estrutura XML gerada
            var nsmgr = CreateNamespaceManager(docRoundTrip);

            var nfseNode = docRoundTrip.SelectSingleNode("//ns:NFSe", nsmgr);
            Assert.NotNull(nfseNode);
            Assert.Equal("1.01", nfseNode.Attributes["versao"]?.Value);

            var infNFSeNode = docRoundTrip.SelectSingleNode("//ns:infNFSe", nsmgr);
            Assert.NotNull(infNFSeNode);
            Assert.Equal(infNFSe.Id, infNFSeNode.Attributes["Id"]?.Value);

            // Valida presença de elementos essenciais
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:emit", nsmgr));
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:valores", nsmgr));
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:IBSCBS", nsmgr));
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:totCIBS", nsmgr));
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:DPS", nsmgr));

            // Validação de valores específicos no XML gerado
            Assert.Equal("100", docRoundTrip.SelectSingleNode("//ns:cStat", nsmgr)?.InnerText);
            Assert.Equal("1500.00", docRoundTrip.SelectSingleNode("//ns:valores/ns:vLiq", nsmgr)?.InnerText);
            Assert.Equal("1.43", docRoundTrip.SelectSingleNode("//ns:gIBS/ns:vIBSTot", nsmgr)?.InnerText);
            Assert.Equal("12.83", docRoundTrip.SelectSingleNode("//ns:gCBS/ns:vCBS", nsmgr)?.InnerText);
        }


        /// <summary>
        /// Testar a desserialização do XML de NFSe com indicativo de decisão judicial
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void DesserializarNfseIndicativoDecisaoJudicial()
        {
            var arqXML = @"..\..\..\NFSe\Resources\NACIONAL\1.01\GerarNfseEnvio_IndicativoDecisaoJudicial-env-loterps.xml";

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var xmlOriginal = new XmlDocument();
            xmlOriginal.Load(arqXML);

            var xml = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe();
            xml = xml.LerXML<Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe>(xmlOriginal);

            var xmlGeradado = xml.GerarXML();

            Assert.True(xmlOriginal.InnerText == xmlGeradado.InnerText, "XML gerado pela DLL está diferente do XML criado manualmente.");
        }

        ///<summary>
        ///Testar serialização do XML de consulta de eventos
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(TipoAmbiente.Homologacao, "1.01", 1001058)]
        [InlineData(TipoAmbiente.Producao, "1.01", 1001058)]
        public void SerializarConsultaEventosNFSe(TipoAmbiente tipoAmbiente, string versaoSchema, int codMunicipio)
        {
            var consulta = new ConsPedRegEvento
            {
                Versao = "1.01",
                InfConsPedRegEvento = new InfConsPedRegEvento
                {
                    ChNFSe = "12345678901234567890123456789012345678901234567890",
                    TipoEvento = "101101",
                    NumSeqEvento = "001"
                }
            };

            var xmlDoc = consulta.GerarXML();

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeConsultarEventosDiversos,
                SchemaVersao = versaoSchema
            };

            var servico = new Unimake.Business.DFe.Servicos.NFSe.ConsultarEvento(xmlDoc, configuracao);
            servico.Executar();

            Assert.NotNull(servico.RetornoWSString);

            var resultado = servico.Result;
            Assert.NotNull(resultado);

            Assert.NotNull(resultado.DataHoraProcessamento);
            Assert.NotNull(resultado.VersaoAplicativo);
            Assert.InRange(resultado.TipoAmbiente, 1, 2);

            if (resultado.Eventos != null)
            {
                Assert.NotNull(resultado.Eventos.ChaveAcesso);
                Assert.NotNull(resultado.Eventos.TipoEvento);
                Assert.NotNull(resultado.Eventos.ArquivoXml);
                Assert.NotNull(resultado.Eventos.ArquivoXml.Evento);

                var evento = resultado.Eventos.ArquivoXml.Evento;
                Assert.NotNull(evento.InfEvento);
                Assert.NotNull(evento.InfEvento.Id);
                Assert.NotNull(evento.InfEvento.PedRegEvento);

                Assert.Equal(consulta.InfConsPedRegEvento.ChNFSe, resultado.Eventos.ChaveAcesso);
                Assert.Equal(consulta.InfConsPedRegEvento.TipoEvento, resultado.Eventos.TipoEvento);
            }
            else if (resultado.Erro != null)
            {
                Assert.NotNull(resultado.Erro.Codigo);
                Assert.NotNull(resultado.Erro.Descricao);
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [Trait("Versao", "1.01")]
        [InlineData(TipoAmbiente.Homologacao, "1.01", 1001058, @"..\..\..\NFSe\Resources\NACIONAL\1.01\ConsultarNfseRpsEnvio-ped-sitnfserps.xml")]
        public void SerializarConsultarNfsePorRpsNACIONAL(TipoAmbiente tipoAmbiente, string versaoSchema, int codMunicipio, string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new Business.DFe.Xml.NFSe.NACIONAL.Consulta.DPS().LerXML<Business.DFe.Xml.NFSe.NACIONAL.Consulta.DPS>(docFixture);

            Assert.Equal("1.01", lido.Versao);
            Assert.NotNull(lido.InfDPS);
            Assert.NotNull(lido.InfDPS.Id);

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var criado = new Business.DFe.Xml.NFSe.NACIONAL.Consulta.DPS
            {
                Versao = lido.Versao,
                InfDPS = lido.InfDPS
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeConsultarNfsePorRps,
                SchemaVersao = versaoSchema
            };

            var servico = new Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps(docCriado, configuracao);
            servico.Executar();

            Assert.NotNull(servico.RetornoWSString);

            var resultado = servico.Result;
            Assert.NotNull(resultado);

            Assert.NotEqual(default(DateTimeOffset), resultado.DataHoraProcessamento);

            if (!string.IsNullOrWhiteSpace(resultado.ChaveAcesso))
            {
                Assert.NotNull(resultado.VersaoAplicativo);
                Assert.Equal(50, resultado.ChaveAcesso.Length);
                Assert.Null(resultado.Erro);
            }
            else if (resultado.Erro != null)
            {

                Assert.NotNull(resultado.Erro.Codigo);
                Assert.NotNull(resultado.Erro.Descricao);

            }
            else
            {
                Assert.Fail("Retorno não contém chave de acesso nem erro");
            }
        }
    }
}
