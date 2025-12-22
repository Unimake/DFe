using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;
using ConsultarNfse = Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsultarNfse;
using ConsultarNfsePorRps = Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsultarNfsePorRps;

namespace Unimake.DFe.Test.NFSe.NACIONAL
{
    public class SerializacaoDesserializacaoNacionalTest
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNfseEnvio-ped-sitnfse.xml")]
        public void ConsultarNFSeNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            // Carrega fixture e desserializa
            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);
            var lido = new ConsultarNfse().LerXML<ConsultarNfse>(docFixture);

            // Sanity checks 
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfNFSe?.Id));

            // Serializa de volta (round-trip)
            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText,
                "Round-trip diferente do fixture.");

            // Cria do zero com os mesmos valores (pega problemas de defaults/ShouldSerialize)
            var criado = new ConsultarNfse
            {
                Versao = "1.00",
                InfNFSe = new InfNFSe { Id = lido.InfNFSe.Id }
            };
            var docCriado = criado.GerarXML();

            // Compara com o round-trip (mesmo serializer, expectativa idêntica)
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText,
                "XML criado do zero difere do XML do round-trip (possível problema de defaults/namespace).");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNfseRpsEnvio-ped-sitnfserps.xml")]
        public void ConsultarNFSePorRPSNACIONAL(string arqXml)
        {
            Assert.True(File.Exists(arqXml), $"Arquivo {arqXml} não encontrado.");

            // Carrega fixture e desserializa como DPS
            var docFixture = new XmlDocument();
            docFixture.Load(arqXml);

            var lido = new ConsultarNfsePorRps().LerXML<ConsultarNfsePorRps>(docFixture);

            // Sanity checks
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfDPS?.Id));

            // Round-trip
            var docRoundTrip = lido.GerarXML();
            Assert.Equal(docFixture.OuterXml, docRoundTrip.OuterXml);

            // Cria do zero com os mesmos valores (root DPS + infDPS)
            var criado = new ConsultarNfsePorRps
            {
                Versao = lido.Versao,
                InfDPS = new InfDPS { Id = lido.InfDPS.Id }
            };
            var docCriado = criado.GerarXML();

            Assert.Equal(docRoundTrip.OuterXml, docCriado.OuterXml);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\ConsultarNotaPdfEnvio-ped-nfsepdf.xml")]
        public void ConsultarNFSePDFNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new ConsultarNfsePDFEnvio().LerXML<ConsultarNfsePDFEnvio>(docFixture);
            Assert.Equal("1.00", lido.Versao);
            Assert.False(string.IsNullOrWhiteSpace(lido.InfNFSe?.Id));

            var docRoundTrip = lido.GerarXML();
            Assert.True(docFixture.InnerText == docRoundTrip.InnerText, "Round-trip diferente do fixture.");

            var criado = new ConsultarNfsePDFEnvio
            {
                Versao = "1.00",
                InfNFSe = new InfNFSe { Id = lido.InfNFSe.Id }
            };

            var docCriado = criado.GerarXML();
            Assert.True(docRoundTrip.InnerText == docCriado.InnerText, "XML criado do zero difere do XML do round-trip.");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\CancelarNfseEnvio-ped-cannfse.xml")]
        public void CancelarNfseNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            var lido = new PedRegEvento().LerXML<PedRegEvento>(docFixture);
            Assert.Equal("1.00", lido.Versao);
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
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\1111111111111111111111-env-loterps.xml")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNFSeEnvio-env-loterps.xml")]
        public void GerarNfseNACIONAL(string caminhoXml)
        {
            Assert.True(File.Exists(caminhoXml), $"Arquivo {caminhoXml} não encontrado.");

            var docFixture = new XmlDocument();
            docFixture.Load(caminhoXml);

            // Teste de deserialização
            var lido = new DPS().LerXML<DPS>(docFixture);
            Assert.Equal("1.00", lido.Versao);
            Assert.NotNull(lido.infDPS);

            // Teste de serialização
            var docRoundTrip = lido.GerarXML();
            Assert.NotNull(docRoundTrip);

            // Verifica se elementos essenciais estão presentes
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:DPS", CreateNamespaceManager(docRoundTrip)));
            Assert.NotNull(docRoundTrip.SelectSingleNode("//ns:infDPS", CreateNamespaceManager(docRoundTrip)));
        }

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
            Assert.NotNull(lido.infDPS);
            Assert.Equal("DPS410690211234567800019500001000000000000001", lido.infDPS.Id);

            // Verificações específicas do IBSCBS v1.01
            var ibscbs = lido.infDPS.IBSCBS;
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
                infDPS = new infDPS
                {
                    Id = lido.infDPS.Id,
                    TpAmb = lido.infDPS.TpAmb,
                    DhEmi = lido.infDPS.DhEmi,
                    VerAplic = lido.infDPS.VerAplic,
                    Serie = lido.infDPS.Serie,
                    NDPS = lido.infDPS.NDPS,
                    DCompet = lido.infDPS.DCompet,
                    TpEmit = lido.infDPS.TpEmit,
                    CLocEmi = lido.infDPS.CLocEmi,
                    Prest = lido.infDPS.Prest,
                    Toma = lido.infDPS.Toma,
                    Serv = lido.infDPS.Serv,
                    Valores = new Valores
                    {
                        VServPrest = lido.infDPS.Valores.VServPrest,
                        Trib = lido.infDPS.Valores.Trib
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
                infDPS = new infDPS
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
                infDPS = new infDPS
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
            var idGerado = dps.infDPS.Id;

            // Assert - Valida o ID esperado: DPS + 4314902 + 2 + 01878890000100 + 00010 + 000000000009147
            var idEsperado = "DPS431490220187889000010000010000000000009147";
            Assert.Equal(idEsperado, idGerado);
            Assert.Equal(45, idGerado.Length); // 3 + 7 + 1 + 14 + 5 + 15
            Assert.StartsWith("DPS", idGerado);
        }

        /// <summary>
        /// Testa a geração de DPS montando o objeto completo a partir do XML de exemplo.
        /// Valida a geração automática do ID e a serialização completa.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        public void GerarNfseNACIONAL_Montar1_00()
        {

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.00",
                MunicipioToken = "99n0556af8e4218e05b88e266fhca55be17b14a4495c269d1db0af57f925f04e77c38f9870842g5g60b6827a9fje8ec9", //Tem município que exige token, então já vamos deixar algo definido para que utilize nos padrões necessários durante o teste unitário. Não é obrigatório para todos os padrões e será utilizado somente nos que solicitam.
                MunicipioSenha = "123456",
                MunicipioUsuario = "01001001000113"
            };

            var dps = new DPS
            {
                Versao = "1.00",
                infDPS = new infDPS
                {

                    TpAmb = TipoAmbiente.Homologacao,
                    DhEmi = new DateTime(2022, 9, 28, 13, 50, 29),
                    VerAplic = "EmissorWeb_1.0.0.0",
                    Serie = "900",
                    NDPS = "6",
                    DCompet = new DateTime(2022, 9, 28),
                    TpEmit = TipoEmitenteNFSe.Prestador,
                    CLocEmi = 1400159, 


                    Prest = new Prest
                    {
                        CNPJ = "01761135000132",
                        IM = "01761135000132",
                        RegTrib = new RegTrib
                        {
                            OpSimpNac = OptSimplesNacional.ME_EPP,
                            RegApTribSN = RegApTribSN.RegApurTribSN,
                            RegEspTrib = RegEspTrib.Nenhum
                        }
                    },

                    Serv = new Serv
                    {
                        LocPrest = new LocPrest
                        {
                            CLocPrestacao = 1400159
                        },
                        CServ = new CServ
                        {
                            CTribNac = "010101",
                            XDescServ = "teste teste teste teste teste teste teste teste teste teste teste"
                        }
                    },

                    Valores = new Valores
                    {
                        VServPrest = new VServPrest
                        {
                            VServ = 999999999.99
                        },
                        vDescCondIncond = new VDescCondIncond
                        {
                            VDescIncond = 9999999.99,
                            VDescCond = 9.99
                        },
                        Trib = new Trib
                        {
                            TribMun = new TribMun
                            {
                                TribISSQN = TribISSQN.OperacaoTributavel,
                                TpRetISSQN = TipoRetencaoISSQN.NaoRetido
                            },
                            TotTrib = new TotTrib
                            {
                                PTotTribSN = 0.01
                            }
                        }
                    }
                }
            };

            var idGerado = dps.infDPS.Id;
            var xmlGerado = dps.GerarXML();

            var idEsperado = "DPS140015920176113500013200900000000000000006";
            Assert.Equal(idEsperado, idGerado);
            Assert.Equal(45, idGerado.Length);
            Assert.StartsWith("DPS", idGerado);

            Assert.NotNull(xmlGerado);
            var nsmgr = CreateNamespaceManager(xmlGerado);

            var dpsNode = xmlGerado.SelectSingleNode("//ns:DPS", nsmgr);
            Assert.NotNull(dpsNode);

            var infDPSNode = xmlGerado.SelectSingleNode("//ns:infDPS", nsmgr);
            Assert.Equal(idEsperado, infDPSNode.Attributes["Id"]?.Value);

            var dpsDeserializado = new DPS().LerXML<DPS>(xmlGerado);
            Assert.Equal(idEsperado, dpsDeserializado.infDPS.Id);

            var gerarNfse = new GerarNfse(xmlGerado, configuracao);
            gerarNfse.Executar();
        }
    }
}
