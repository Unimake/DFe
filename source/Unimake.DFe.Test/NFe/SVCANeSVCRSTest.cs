using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar o serviço de envio da NFe
    /// </summary>
    public class SVCANeSVCRSTest
    {
        /// <summary>
        /// Enviar uma NFe no modo síncrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a NFe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a NFe</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCRS)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao, TipoEmissao.ContingenciaSVCAN)]
        public void EnviarNFeAssincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, TipoEmissao tipoEmissao)
        {
            var xml = new EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Nao,
                NFe = new List<Business.DFe.Xml.NFe.NFe> {
                        new Business.DFe.Xml.NFe.NFe
                        {
                            InfNFe = new List<InfNFe> {
                                new InfNFe
                                {
                                    Versao = "4.00",

                                    Ide = new Ide
                                    {
                                        CUF = ufBrasil,
                                        NatOp = "VENDA PRODUC.DO ESTABELEC",
                                        Mod = ModeloDFe.NFe,
                                        Serie = 16,
                                        NNF = 57962,
                                        DhEmi = DateTime.Now,
                                        DhSaiEnt = DateTime.Now,
                                        TpNF = TipoOperacao.Saida,
                                        IdDest = DestinoOperacao.OperacaoInterestadual,
                                        CMunFG = 4118402,
                                        TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                        TpEmis = tipoEmissao,
                                        TpAmb = tipoAmbiente,
                                        FinNFe = FinalidadeNFe.Normal,
                                        IndFinal = SimNao.Sim,
                                        IndPres = IndicadorPresenca.OperacaoPresencial,
                                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                        VerProc = "TESTE 1.00"
                                    },
                                    Emit = new Emit
                                    {
                                        CNPJ = "06117473000150",
                                        XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                        XFant = "UNIMAKE - PARANAVAI",
                                        EnderEmit = new EnderEmit
                                        {
                                            XLgr = "RUA ANTONIO FELIPE",
                                            Nro = "1500",
                                            XBairro = "CENTRO",
                                            CMun = 4118402,
                                            XMun = "PARANAVAI",
                                            UF = ufBrasil,
                                            CEP = "87704030",
                                            Fone = "04431414900"
                                        },
                                        IE = "9032000301",
                                        IM = "14018",
                                        CNAE = "6202300",
                                        CRT = CRT.SimplesNacional
                                    },
                                    Dest = new Dest
                                    {
                                        CNPJ = "02131087000161",
                                        XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                        EnderDest = new EnderDest
                                        {
                                            XLgr = "AVENIDA DE TESTE DE NFE",
                                            Nro = "82",
                                            XBairro = "CAMPOS ELISEOS",
                                            CMun = 3543402,
                                            XMun = "RIBEIRAO PRETO",
                                            UF = UFBrasil.SP,
                                            CEP = "14080000",
                                            Fone = "01666994533"
                                        },
                                        IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                        IE = "124815618820",
                                        Email = "testenfe@hotmail.com"
                                    },
                                    Det = new List<Det> {
                                        new Det
                                        {
                                            NItem = 1,
                                            Prod = new Prod
                                            {
                                                CProd = "01042",
                                                CEAN = "SEM GTIN",
                                                XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                                NCM = "84714900",
                                                CFOP = "6101",
                                                UCom = "LU",
                                                QCom = 1.00m,
                                                VUnCom = 84.9000000000M,
                                                VProd = 84.90,
                                                CEANTrib = "SEM GTIN",
                                                UTrib = "LU",
                                                QTrib = 1.00m,
                                                VUnTrib = 84.9000000000M,
                                                IndTot = SimNao.Sim,
                                                XPed = "300474",
                                                NItemPed = "1"
                                            },
                                            Imposto = new Imposto
                                            {
                                                VTotTrib = 12.63,
                                                ICMS = new ICMS
                                                {
                                                    ICMSSN101 = new ICMSSN101
                                                    {
                                                        Orig = OrigemMercadoria.Nacional,
                                                        PCredSN = 2.8255,
                                                        VCredICMSSN = 2.40
                                                    }
                                                },
                                                PIS = new PIS
                                                {
                                                    PISOutr = new PISOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PPIS = 0.00,
                                                        VPIS = 0.00
                                                    }
                                                },
                                                COFINS = new COFINS
                                                {
                                                    COFINSOutr = new COFINSOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PCOFINS = 0.00,
                                                        VCOFINS = 0.00
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    Total = new Total
                                    {
                                        ICMSTot = new ICMSTot
                                        {
                                            VBC = 0,
                                            VICMS = 0,
                                            VICMSDeson = 0,
                                            VFCP = 0,
                                            VBCST = 0,
                                            VST = 0,
                                            VFCPST = 0,
                                            VFCPSTRet = 0,
                                            VProd = 84.90,
                                            VFrete = 0,
                                            VSeg = 0,
                                            VDesc = 0,
                                            VII = 0,
                                            VIPI = 0,
                                            VIPIDevol = 0,
                                            VPIS = 0,
                                            VCOFINS = 0,
                                            VOutro = 0,
                                            VNF = 84.90,
                                            VTotTrib = 12.63
                                        }
                                    },
                                    Transp = new Transp
                                    {
                                        ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                        Vol = new List<Vol>
                                        {
                                            new Vol
                                            {
                                                QVol = 1,
                                                Esp = "LU",
                                                Marca = "UNIMAKE",
                                                PesoL = 0.000,
                                                PesoB = 0.000
                                            }
                                        }
                                    },
                                    Cobr = new Unimake.Business.DFe.Xml.NFe.Cobr()
                                    {
                                        Fat = new Fat
                                        {
                                            NFat = "057910",
                                            VOrig = 84.90,
                                            VDesc = 0,
                                            VLiq = 84.90
                                        },
                                        Dup = new List<Dup>
                                        {
                                            new Dup
                                            {
                                                NDup = "001",
                                                DVenc = DateTime.Now,
                                                VDup = 84.90
                                            }
                                        }
                                    },
                                    Pag = new Pag
                                    {
                                        DetPag = new List<DetPag>
                                        {
                                             new DetPag
                                             {
                                                 IndPag = IndicadorPagamento.PagamentoVista,
                                                 TPag = MeioPagamento.Dinheiro,
                                                 VPag = 84.90
                                             }
                                        }
                                    },
                                    InfAdic = new InfAdic
                                    {
                                        InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                    },
                                    InfRespTec = new InfRespTec
                                    {
                                        CNPJ = "06117473000150",
                                        XContato = "Contato teste para NFe",
                                        Email = "testenfe@gmail.com",
                                        Fone = "04431414900"
                                    }
                                }
                            }
                        }
                    }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Autorizacao(xml, configuracao);
            autorizacao.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacao.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CStat.Equals(114) || autorizacao.Result.CStat.Equals(103), "Lote não foi processado. <xMotivo>" + autorizacao.Result.XMotivo + "<xMotivo>");

            if (tipoEmissao == TipoEmissao.ContingenciaSVCAN)
            {
                Assert.True(autorizacao.Result.VerAplic.Contains("SVC_AN"), "Web-service retornou uma versão de aplicação diferente da comum para SVC-AN");
            }
            else
            {
                Assert.True(autorizacao.Result.VerAplic.Contains("SVRS"), "Web-service retornou uma versão de aplicação diferente da comum para SVC-RS");
            }
        }
   }
}