using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.MDFe;
using Unimake.Business.DFe.Xml.MDFe;
using Xunit;

namespace Unimake.DFe.Test.MDFe
{
    /// <summary>
    /// Testar o serviço de envio do MDFe
    /// </summary>
    public class AutorizacaoTest
    {
        /// <summary>
        /// Enviar um MDFe no modo assincrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void EnviarMDFeAssincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            #region CriarMDFe

            var xml = new EnviMDFe
            {
                Versao = "3.00",
                IdLote = "000000000000001",
                MDFe = new Business.DFe.Xml.MDFe.MDFe
                {

                    InfMDFe = new InfMDFe
                    {
                        Versao = "3.00",

                        Ide = new Ide
                        {
                            CUF = ufBrasil,
                            TpAmb = tipoAmbiente,
                            TpEmit = TipoEmitenteMDFe.PrestadorServicoTransporte,
                            Mod = ModeloDFe.MDFe,
                            Serie = 1,
                            NMDF = 861,
                            CMDF = "01722067",
                            Modal = ModalidadeTransporteMDFe.Rodoviario,
                            DhEmi = DateTime.Now,
                            TpEmis = TipoEmissao.Normal,
                            ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                            VerProc = "UNICO V8.0",
                            UFIni = ufBrasil,
                            UFFim = UFBrasil.SP,
                            InfMunCarrega = new List<InfMunCarrega>
                                {
                                    new InfMunCarrega
                                    {
                                        CMunCarrega = 4118402,
                                        XMunCarrega = "PARANAVAI"

                                    }
                                },
                            DhIniViagem = DateTime.Now,

                        },
                        Emit = new Emit
                        {
                            CNPJ = "06117473000150",
                            IE = "9032000301",
                            XNome = "TESTE DE ENVIO DE MDFE",
                            XFant = "TESTE DE ENVIO DE MDFE",
                            EnderEmit = new EnderEmit
                            {
                                XLgr = "RUA TESTE DE SOUZA",
                                Nro = "01112",
                                XBairro = "JD. SIMARA",
                                CMun = 4118402,
                                XMun = "PARANAVAI",
                                CEP = "87706111",
                                UF = UFBrasil.PR,
                                Fone = "04431421010",
                            },
                        },
                        InfModal = new InfModal
                        {
                            VersaoModal = "3.00",
                            Rodo = new Rodo
                            {
                                InfANTT = new InfANTT
                                {
                                    RNTRC = "99778899",
                                    InfContratante = new List<InfContratante>
                                        {
                                            new InfContratante
                                            {
                                                CNPJ = "06117473000150"
                                            },
                                            new InfContratante
                                            {
                                                CNPJ = "06117473000150"
                                            }
                                        }
                                },
                                VeicTracao = new VeicTracao
                                {
                                    CInt = "ARR5555",
                                    Placa = "ARR5555",
                                    Tara = 0,
                                    CapKG = 5000,
                                    Prop = new Prop
                                    {
                                        CNPJ = "06117473000150",
                                        RNTRC = "12345678",
                                        XNome = "TESTE TESTE TESTE X",
                                        IE = "1234567890",
                                        UF = ufBrasil,
                                        TpProp = TipoProprietarioMDFe.Outros
                                    },
                                    Condutor = new List<Condutor>
                                        {
                                            new Condutor
                                            {
                                                XNome = "TESTE NOME DO CONDUTOR",
                                                CPF = "00000000000"
                                            }
                                        },
                                    TpRod = TipoRodado.Toco,
                                    TpCar = TipoCarroceriaMDFe.FechadaBau,
                                    UF = UFBrasil.PR
                                },
                            }
                        },
                        InfDoc = new InfDocInfMDFe
                        {
                            InfMunDescarga = new List<InfMunDescarga>
                                {
                                    new InfMunDescarga
                                    {
                                        CMunDescarga = 3505708,
                                        XMunDescarga = "BARUERI",
                                        InfCTe = new List<InfMunDescargaInfCTe>
                                        {
                                            new InfMunDescargaInfCTe
                                            {
                                                ChCTe = "41200506117473000150570010000001113565666658"
                                            }
                                        },
                                        InfNFe = new List<InfMunDescargaInfNFe>
                                        {
                                            new InfMunDescargaInfNFe
                                            {
                                                ChNFe = "12345678901234567890123456789012345678901234",
                                                InfUnidTransp = new List<InfUnidTransp>
                                                {
                                                    new InfUnidTransp
                                                    {
                                                        IdUnidTransp = "122",
                                                        TpUnidTransp = TipoUnidadeTransporte.RodoviarioReboque,
                                                        LacUnidTransp = new List<LacUnidTransp>
                                                        {
                                                            new LacUnidTransp
                                                            {
                                                                NLacre = "12334"
                                                            }
                                                        },
                                                        InfUnidCarga = new List<InfUnidCarga>
                                                        {
                                                            new InfUnidCarga
                                                            {
                                                                TpUnidCarga = TipoUnidadeCarga.Container,
                                                                IdUnidCarga = "123",
                                                                LacUnidCarga = new List<LacUnidCarga>
                                                                {
                                                                    new LacUnidCarga
                                                                    {
                                                                        NLacre = "3333333"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                    },
                                    new InfMunDescarga
                                    {
                                        CMunDescarga = 3550308,
                                        XMunDescarga = "SAO PAULO",
                                        InfCTe = new List<InfMunDescargaInfCTe>
                                        {
                                            new InfMunDescargaInfCTe
                                            {
                                                ChCTe = "41200506117473000150570010000003335655655666"
                                            }
                                        }
                                    }
                                }
                        },
                        ProdPred = new ProdPred
                        {
                            TpCarga = TipoCargaMDFe.CargaGeral,
                            XProd = "TESTE DE PRODUTO PREDOMINANTE",
                            InfLotacao = new InfLotacao
                            {
                                InfLocalCarrega = new InfLocalCarrega
                                {
                                    CEP = "87302080"
                                },
                                InfLocalDescarrega = new InfLocalDescarrega
                                {
                                    CEP = "25650208"
                                }
                            }
                        },
                        Seg = new List<Seg>
                            {
                                new Seg
                                {
                                    InfResp = new InfResp
                                    {
                                        RespSeg = ResponsavelSeguroMDFe.EmitenteMDFe,
                                        CNPJ = "06117473000150"
                                    },
                                    InfSeg = new Unimake.Business.DFe.Xml.MDFe.InfSeg
                                    {
                                        XSeg = "PORTO SEGURO",
                                        CNPJ = "06117473000150"
                                    },
                                    NApol = "053179456362",
                                    NAver = new List<string>
                                    {
                                        {
                                            "0000000000000000000000000000000000000000"
                                        },
                                        {
                                            "0000000000000000000000000000000000000001"
                                        },
                                    }
                                }
                            },
                        Tot = new Tot
                        {
                            QCTe = 3,
                            VCarga = 56599.09,
                            CUnid = CodigoUnidadeMedidaMDFe.KG,
                            QCarga = 2879.00
                        },
                        Lacres = new List<Lacre>
                            {
                                new Lacre
                                {
                                    NLacre = "1111111"
                                },
                                new Lacre
                                {
                                    NLacre = "22222"
                                }

                            },
                        InfAdic = new InfAdic
                        {
                            InfCpl = "DATA/HORA PREVISTA PARA O INICO DA VIAGEM: 10/08/2020 as 08:00"
                        },
                        InfRespTec = new InfRespTec
                        {
                            CNPJ = "06117473000150",
                            XContato = "TESTE TESTE TESTE",
                            Email = "wandrey@unimake.com.br",
                            Fone = "04431421010",
                        },
                    },
                },
            };

            #endregion CriarMDFe

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Autorizacao(xml, configuracao);
            autorizacao.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacao.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CStat.Equals(103) || autorizacao.Result.CStat.Equals(203) || autorizacao.Result.CStat.Equals(213), "Lote não foi recebido - <xMotivo> = " + autorizacao.Result.XMotivo);
        }
    }
}