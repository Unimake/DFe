using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.DCe;
using Xunit;
using DCeAutorizacaoSinc = Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc;
using DCeConsultaProtocolo = Unimake.Business.DFe.Servicos.DCe.ConsultaProtocolo;
using DCeRecepcaoEvento = Unimake.Business.DFe.Servicos.DCe.RecepcaoEvento;
using DCeStatusServico = Unimake.Business.DFe.Servicos.DCe.StatusServico;
using XmlDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe
{
    /// <summary>
    /// Testar a serialização e desserialização dos serviços da DCe
    /// </summary>
    public class ServicosTest
    {
        private const string ChaveDCe = "41260500000000000199990000000000110000123456";

        /// <summary>
        /// Cria a configuração para os serviços da DCe em homologação
        /// </summary>
        /// <returns>Configuração dos serviços da DCe</returns>
        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoDFe = TipoDFe.DCe,
            CodigoUF = (int)UFBrasil.PR,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CertificadoDigital = PropConfig.CertificadoDigital
        };

        /// <summary>
        /// Cria o XML da DCe para testes de autorização
        /// </summary>
        /// <returns>XML da DCe</returns>
        private static XmlDCe CriarDCe() => new XmlDCe
        {
            InfDCe = new InfDCe
            {
                Versao = "1.00",
                Ide = new Ide
                {
                    CUF = UFBrasil.PR,
                    CDC = "123456",
                    Mod = ModeloDFe.DCe,
                    Serie = 0,
                    NDC = 1,
                    DhEmi = DateTime.Now,
                    TpEmis = TipoEmissao.Normal,
                    TpEmit = TipoEmitenteDCe.EmissorProprio,
                    NSiteAutoriz = "0",
                    CDV = 6,
                    TpAmb = TipoAmbiente.Homologacao,
                    VerProc = "Unimake-Test"
                },
                Emit = new Emit
                {
                    CNPJ = "00000000000199",
                    XNome = "Emitente Teste",
                    EnderEmit = new EnderEmit
                    {
                        XLgr = "Rua Teste",
                        Nro = "100",
                        XBairro = "Centro",
                        CMun = "4106902",
                        XMun = "Curitiba",
                        UF = UFBrasil.PR,
                        CEP = "80010000",
                        CPais = "1058",
                        XPais = "Brasil"
                    }
                },
                Dest = new Dest
                {
                    CPF = "12345678909",
                    XNome = "Destinatario Teste",
                    EnderDest = new EnderDest
                    {
                        XLgr = "Rua Destino",
                        Nro = "200",
                        XBairro = "Centro",
                        CMun = "4106902",
                        XMun = "Curitiba",
                        UF = UFBrasil.PR,
                        CEP = "80010000",
                        Email = "destino@teste.com"
                    }
                },
                AutXML = new List<AutXML>
                {
                    new AutXML { CPF = "12345678909" }
                },
                Det = new List<Det>
                {
                    new Det
                    {
                        NItem = 1,
                        Prod = new Prod
                        {
                            XProd = "Produto teste",
                            NCM = "99",
                            QCom = 1,
                            VUnCom = 10,
                            VProd = 10
                        },
                        InfAdProd = "Item preservado"
                    }
                },
                Total = new Total { VDC = 10 },
                Transp = new Transp
                {
                    ModTrans = "0",
                    CNPJTransp = "00000000000199"
                },
                InfAdic = new InfAdic
                {
                    InfCpl = "Informacao complementar"
                },
                InfDec = new InfDec
                {
                    XObs1 = "Declaracao 1",
                    XObs2 = "Declaracao 2"
                }
            }
        };

        /// <summary>
        /// Cria o XML de evento da DCe para testes de recepção
        /// </summary>
        /// <returns>XML de evento da DCe</returns>
        private static EventoDCe CriarEventoDCe()
        {
            var infEvento = new InfEvento
            {
                COrgao = UFBrasil.PR,
                TpAmb = TipoAmbiente.Homologacao,
                CNPJ = "00000000000199",
                ChDCe = ChaveDCe,
                DhEvento = DateTime.Now,
                TpEvento = TipoEventoDCe.Cancelamento,
                NSeqEvento = 1,
                DetEvento = new DetEventoCanc()
            };

            var detEvento = (DetEventoCanc)infEvento.DetEvento;
            detEvento.VersaoEvento = "1.00";
            detEvento.NProt = "1352600000000001";
            detEvento.XJust = "Justificativa de teste valida";

            return new EventoDCe
            {
                Versao = "1.00",
                InfEvento = infEvento
            };
        }

        /// <summary>
        /// Testar o serviço de consulta status da DCe
        /// </summary>
        [Fact]
        public void StatusServico()
        {
            ConsStatServDCe xml = new ConsStatServDCe
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR
            };

            var servico = new DCeStatusServico(xml, CriarConfiguracao());
            servico.Executar();

            Assert.Contains("<consStatServDCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeStatusServico, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeStatusServico", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(107, servico.Result.CStat);
            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
        }

        /// <summary>
        /// Testar o serviço de consulta protocolo da DCe
        /// </summary>
        [Fact]
        public void ConsultaProtocolo()
        {
            var xml = new ConsSitDCe
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                ChDCe = ChaveDCe
            };

            var servico = new DCeConsultaProtocolo(xml, CriarConfiguracao());
            servico.Executar();

            Assert.Contains("<consSitDCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeConsultaProtocolo, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeConsulta", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.False(string.IsNullOrWhiteSpace(servico.RetornoWSString));
            Assert.True(servico.Result.CStat > 0);
            Assert.Equal(TipoAmbiente.Homologacao, servico.Result.TpAmb);
            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
        }

        /// <summary>
        /// Testar o serviço de autorização síncrona da DCe
        /// </summary>
        [Fact]
        public void AutorizacaoSinc()
        {
            var xml = CriarDCe();

            var servico = new DCeAutorizacaoSinc(xml, CriarConfiguracao());
            servico.Executar();

            Assert.Contains("<DCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeAutorizacaoSinc, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeAutorizacao", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.False(string.IsNullOrWhiteSpace(servico.RetornoWSString));
            Assert.True(servico.Result.CStat > 0);
            Assert.Equal(TipoAmbiente.Homologacao, servico.Result.TpAmb);
            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
        }

        /// <summary>
        /// Testar o serviço de recepção de evento da DCe
        /// </summary>
        [Fact]
        public void RecepcaoEvento()
        {
            var xml = CriarEventoDCe();

            var servico = new DCeRecepcaoEvento(xml, CriarConfiguracao());
            servico.Executar();

            Assert.Contains("<eventoDCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeRecepcaoEvento, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeRecepcaoEvento", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.False(string.IsNullOrWhiteSpace(servico.RetornoWSString));
            Assert.True(servico.Result.InfEvento.CStat > 0);
            Assert.Equal(TipoAmbiente.Homologacao, servico.Result.InfEvento.TpAmb);
            Assert.Equal(UFBrasil.PR, servico.Result.InfEvento.COrgao);
        }
    }
}
