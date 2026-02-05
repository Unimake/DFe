using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.Utility;

public class ValidarSchemaTest
{
    [Fact]
    public void ValidarSchemaNFeTest()
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
                                CUF = UFBrasil.PR,
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
                                TpEmis = TipoEmissao.Normal,
                                TpAmb = TipoAmbiente.Homologacao,
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
                                    UF = UFBrasil.PR,
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
            TipoAmbiente = TipoAmbiente.Homologacao,
            CertificadoArquivo = @"c:\projetos\unimakepv.pfx",
            CertificadoSenha = "12345678"
        };

        var autorizacao = new Business.DFe.Servicos.NFe.Autorizacao(xml, configuracao);

        var validar = new ValidarSchema();
        validar.Validar(autorizacao.ConteudoXMLAssinado, configuracao.TipoDFe.ToString() + "." + autorizacao.Configuracoes.SchemaArquivo, "");

        Assert.True(validar.Success);
    }
}