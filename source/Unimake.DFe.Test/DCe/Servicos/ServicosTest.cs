using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Xunit;
using DCeAutorizacaoSinc = Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc;
using DCeConsultaProtocolo = Unimake.Business.DFe.Servicos.DCe.ConsultaProtocolo;
using DCeRecepcaoEvento = Unimake.Business.DFe.Servicos.DCe.RecepcaoEvento;
using DCeStatusServico = Unimake.Business.DFe.Servicos.DCe.StatusServico;
using XmlDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe.Servicos
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
                    ModTrans = ModalidadeTransporteDCe.Correios,
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
                TpEmit = TipoEmitenteDCe.EmissorProprio,
                CNPJAutor = "00000000000199",
                CNPJUsEmit = "00000000000199",
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
        /// Cria um XML da DCe com dados determinísticos para validação da montagem da chave
        /// conforme o tipo de emitente informado.
        /// </summary>
        /// <param name="tipoEmitente">Tipo de emitente que define qual identificador será utilizado na chave.</param>
        /// <returns>Instância de DCe pronta para validação de geração da chave.</returns>
        private static XmlDCe CriarDCeParaTesteChave(TipoEmitenteDCe tipoEmitente)
        {
            var dce = CriarDCe();

            dce.InfDCe.Ide.CUF = UFBrasil.PR;
            dce.InfDCe.Ide.CDC = "123456";
            dce.InfDCe.Ide.Mod = ModeloDFe.DCe;
            dce.InfDCe.Ide.Serie = 0;
            dce.InfDCe.Ide.NDC = 1;
            dce.InfDCe.Ide.DhEmi = new DateTime(2026, 05, 01, 10, 20, 30);
            dce.InfDCe.Ide.TpEmis = TipoEmissao.Normal;
            dce.InfDCe.Ide.TpEmit = tipoEmitente;
            dce.InfDCe.Ide.NSiteAutoriz = "0";

            dce.InfDCe.Fisco = new Fisco
            {
                CNPJ = "11111111000111",
                XOrgao = "SEFAZ PR",
                UF = UFBrasil.PR
            };

            dce.InfDCe.Marketplace = new Marketplace
            {
                CNPJ = "22222222000122",
                XNome = "Marketplace Teste",
                Site = "https://marketplace.teste"
            };

            dce.InfDCe.Transportadora = new Transportadora
            {
                CNPJ = "33333333000133",
                XNome = "Transportadora Teste"
            };

            dce.InfDCe.Emit.CNPJ = "00000000000199";

            return dce;
        }

        /// <summary>
        /// Monta a chave esperada da DCe com base no identificador informado para validar a regra
        /// de seleção do CNPJ/CPF durante a composição da chave.
        /// </summary>
        /// <param name="dce">Documento DCe com os dados estruturais da chave.</param>
        /// <param name="identificador">CNPJ/CPF esperado para ocupar a posição do emitente na chave.</param>
        /// <returns>Chave da DCe calculada para comparação no teste.</returns>
        private static string MontarChaveEsperada(XmlDCe dce, string identificador)
        {
            var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
            {
                UFEmissor = dce.InfDCe.Ide.CUF,
                AnoEmissao = dce.InfDCe.Ide.DhEmi.ToString("yy"),
                MesEmissao = dce.InfDCe.Ide.DhEmi.ToString("MM"),
                CNPJCPFEmissor = identificador.PadLeft(14, '0'),
                Modelo = dce.InfDCe.Ide.Mod,
                Serie = dce.InfDCe.Ide.Serie,
                NumeroDoctoFiscal = dce.InfDCe.Ide.NDC,
                TipoEmissao = dce.InfDCe.Ide.TpEmis,
                TipoEmitenteDCe = dce.InfDCe.Ide.TpEmit,
                NSiteAutoriz = dce.InfDCe.Ide.NSiteAutoriz,
                CodigoNumerico = dce.InfDCe.Ide.CDC
            };

            return XMLUtility.MontarChaveDCe(ref conteudoChaveDFe);
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
        /// Validar se a chave da DCe utiliza o identificador correto na composição conforme o valor de <c>TpEmit</c>.
        /// </summary>
        /// <param name="tipoEmitente">Tipo de emitente informado na DCe.</param>
        /// <param name="identificadorEsperado">Identificador esperado para composição da chave (CNPJ do participante).</param>
        [Theory]
        [InlineData(TipoEmitenteDCe.AppFisco, "11111111000111")]
        [InlineData(TipoEmitenteDCe.Marketplace, "22222222000122")]
        [InlineData(TipoEmitenteDCe.EmissorProprio, "00000000000199")]
        [InlineData(TipoEmitenteDCe.Transportadora, "33333333000133")]
        public void GerarChaveDCeConformeTipoEmitente(TipoEmitenteDCe tipoEmitente, string identificadorEsperado)
        {
            var xml = CriarDCeParaTesteChave(tipoEmitente);

            var chaveGerada = xml.InfDCe.Chave;
            var chaveEsperada = MontarChaveEsperada(xml, identificadorEsperado);

            Assert.Equal(chaveEsperada, chaveGerada);
            Assert.Equal(identificadorEsperado.PadLeft(14, '0'), chaveGerada.Substring(6, 14));
        }

        /// <summary>
        /// Validar se a mensagem de exceção orienta corretamente o grupo obrigatório conforme o valor de <c>TpEmit</c>
        /// quando não houver identificador para composição da chave da DCe.
        /// </summary>
        /// <param name="tipoEmitente">Tipo de emitente informado na DCe.</param>
        /// <param name="mensagemEsperada">Mensagem esperada para diagnóstico do campo ausente.</param>
        [Theory]
        [InlineData(TipoEmitenteDCe.AppFisco, "Fisco.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = AppFisco.")]
        [InlineData(TipoEmitenteDCe.Marketplace, "Marketplace.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = Marketplace.")]
        [InlineData(TipoEmitenteDCe.EmissorProprio, "Emit.CNPJ, Emit.CPF ou Emit.IdOutros não foi informado para montar a chave da DCe quando Ide.TpEmit = EmissorProprio.")]
        [InlineData(TipoEmitenteDCe.Transportadora, "Transportadora.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = Transportadora.")]
        public void GerarChaveDCeSemIdentificadorDeveRetornarMensagemCorreta(TipoEmitenteDCe tipoEmitente, string mensagemEsperada)
        {
            var xml = CriarDCeParaTesteChave(tipoEmitente);

            xml.InfDCe.Fisco = null;
            xml.InfDCe.Marketplace = null;
            xml.InfDCe.Transportadora = null;
            xml.InfDCe.Emit.CNPJ = null;
            xml.InfDCe.Emit.CPF = null;
            xml.InfDCe.Emit.IdOutros = null;

            var exception = Assert.Throws<NullReferenceException>(() =>
            {
                var chave = xml.InfDCe.Chave;
            });

            Assert.Equal(mensagemEsperada, exception.Message);
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
