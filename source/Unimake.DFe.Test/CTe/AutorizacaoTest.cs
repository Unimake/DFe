using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    /// <summary>
    /// Testar o serviço de envio da CTe
    /// </summary>
    public class AutorizacaoTest
    {
        /// <summary>
        /// Enviar uma CTe no modo assíncrono somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a CTe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a CTe</param>
        [Theory]
        [Trait("DFe", "CTe")]
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
        public void EnviarCTeAssincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            #region CriarCTe

            var xml = new EnviCTe
            {
                Versao = "3.00",
                IdLote = "000000000000001",
                CTe = new List<Business.DFe.Xml.CTe.CTe> {
                        new Business.DFe.Xml.CTe.CTe
                        {
                            InfCTe = new InfCTe
                            {
                                Versao = "3.00",

                                Ide = new Ide
                                {
                                    CUF = ufBrasil,
                                    CCT = "01722067",
                                    CFOP  = "6352",
                                    NatOp = "PREST.SERV.TRANSP.INDUSTR",
                                    Mod = ModeloDFe.CTe,
                                    Serie = 1,
                                    NCT = 861 ,
                                    DhEmi = DateTime.Now,
                                    TpImp = FormatoImpressaoDACTE.NormalPaisagem,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = tipoAmbiente,
                                    TpCTe = TipoCTe.Normal,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "UNICO V8.0",
                                    CMunEnv = "4118402" ,
                                    XMunEnv = "PARANAVAI",
                                    UFEnv = ufBrasil,
                                    Modal =  ModalidadeTransporteCTe.Rodoviario,
                                    TpServ = TipoServicoCTe.Normal,
                                    CMunIni = "4118402",
                                    XMunIni = "PARANAVAI",
                                    UFIni = UFBrasil.PR,
                                    CMunFim = "3305109",
                                    XMunFim = "SAO JOAO DE MERITI",
                                    UFFim =  UFBrasil.RJ,
                                    Retira = SimNao.Nao,
                                    IndIEToma = IndicadorIEDestinatario.ContribuinteICMS,
                                    Toma3 =  new Toma3
                                    {
                                        Toma= TomadorServicoCTe.Remetente,
                                    },
                                },
                                Emit = new Emit
                                {
                                    CNPJ = "31905001000109",
                                    IE = "9079649730",
                                    XNome = "EXATUS MOVEIS EIRELI",
                                    XFant = "EXATUS MOVEIS",
                                    EnderEmit = new Unimake.Business.DFe.Xml.CTe.EnderEmit
                                    {
                                        XLgr = "RUA JOAQUIM F. DE SOUZA",
                                        Nro = "01112",
                                        XBairro = "VILA TEREZINHA",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        CEP = "87706675",
                                        UF = ufBrasil,
                                        Fone = "04434237530",
                                    },
                                },
                                Rem = new Rem
                                {
                                    CNPJ = "10197843000183",
                                    IE = "9044791606",
                                    XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    XFant = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    Fone = "04434225480",
                                    EnderReme = new Unimake.Business.DFe.Xml.CTe.EnderReme
                                    {
                                        XLgr = "RUA AMAZONAS, 1140",
                                        Nro = "1140",
                                        XBairro = "JD. SAO FELICIO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        CEP = "87702300",
                                        UF = UFBrasil.PR,
                                        CPais = 1058,
                                        XPais = "BRASIL",
                                    }
                                },
                                Dest = new Dest
                                {
                                    CNPJ = "00000000075108",
                                    IE = "ISENTO",
                                    XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new Unimake.Business.DFe.Xml.CTe.EnderDest
                                    {
                                        XLgr = "R. GESSYR GONCALVES FONTES, 55",
                                        Nro = "55",
                                        XBairro = "CENTRO",
                                        CMun = 3305109,
                                        XMun = "SAO JOAO DE MERITI",
                                        CEP = "25520570",
                                        UF = UFBrasil.RJ,
                                        CPais = 1058,
                                        XPais = "BRASIL",
                                    },
                                },
                                VPrest = new VPrest
                                {
                                    VTPrest = 50.00,
                                    VRec = 50.00,
                                    Comp = new List<Comp>
                                    {
                                        new Comp
                                        {
                                            XNome = "FRETE VALOR",
                                            VComp = 50.00,
                                        },
                                    },
                                },
                                Imp = new Imp
                                {
                                    ICMS = new ICMS
                                    {
                                        ICMSSN = new ICMSSN
                                        {
                                            CST = "90",
                                            IndSN = SimNao.Sim,
                                        }
                                        }
                                },
                                InfCTeNorm = new InfCTeNorm
                                {
                                    InfCarga = new InfCarga
                                    {
                                        VCarga = 6252.96,
                                        ProPred = "MOVEIS",
                                        InfQ = new List<InfQ>
                                        {
                                            new InfQ
                                            {
                                                CUnid = CodigoUnidadeMedidaCTe.KG,
                                                TpMed ="PESO BRUTO",
                                                QCarga = 320.0000,
                                            },
                                            new InfQ
                                            {
                                                CUnid = CodigoUnidadeMedidaCTe.UNIDADE,
                                                TpMed ="UNIDADE",
                                                QCarga = 1.0000,
                                            },
                                        },
                                    },
                                    InfDoc = new InfDoc
                                    {
                                        InfNFe = new List<InfNFe>
                                        {
                                            new InfNFe
                                            {
                                                Chave = "41200306117473000150550030000652511417023254"
                                            },
                                        },
                                    },
                                    InfModal = new InfModal
                                    {
                                        VersaoModal="3.00",
                                        Rodo = new Rodo
                                        {
                                            RNTRC = "44957333",
                                            Occ = new List<Occ>
                                            {
                                                new Occ
                                                {
                                                    NOcc = 810,
                                                    DEmi = DateTime.Now,
                                                    EmiOcc = new EmiOcc
                                                    {
                                                        CNPJ = "31905001000109",
                                                        CInt = "0000001067",
                                                        IE = "9079649730",
                                                        UF = UFBrasil.PR,
                                                        Fone = "04434237530",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                InfRespTec = new InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email= "wandrey@unimake.com.br",
                                    Fone = "04431414900",
                                },
                            },
                        },
                    },
            };

            #endregion CriarCTe

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
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