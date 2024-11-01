using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.DARE
{
    public class EnvioDARETest
    {
        /// <summary>
        /// Testar o envio do DARE Unico
        /// </summary>
        [Theory]
        [Trait("DFe", "DARE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void DAREEnvio(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.DARE,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.DAREEnvio,
                CertificadoDigital = PropConfig.CertificadoDigital,
                SchemaVersao = "1.031230",
                ApiKey = "jArkFGc5dxkxGjdmVQK7FiPQ2EJQqi7J"
            };

            var conteudoXML = new Business.DFe.Xml.DARE.DARE
            {
                Cnpj = "06117473000150",
                Cidade = "São Paulo",
                CodigoBarra44 = "12345678901234567890123456789012345678901234",
                DataVencimento = DateTime.Now,
                Endereco = "Rua Exemplo, 123",
                Erro = new Business.DFe.Xml.DARE.Erro
                {
                    EstaOk = true,
                    Mensagens = new List<string>
                        {
                            "teste1",
                            "teste2",
                            "teste3"
                        }
                },
                GerarPDF = true,
                NumeroControleDarePrincipal = "123456789",
                Observacao = "Pagamento referente ao imposto X",
                PixCopiaCola = "abc123def456ghi789",
                RazaoSocial = "Empresa Exemplo S/A",
                Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                {
                    Codigo = "046-2",
                    CodigoServicoDARE = "4601",
                    Nome = "ICMS- Operações Próprias- RPA (04601)"
                },
                Referencia = "10/2024",
                Telefone = "(11) 1234-5678",
                Uf = "SP",
                Valor = 1.00,
                ValorJuros = 10.00,
                ValorMulta = 5.00,
                ValorTotal = 16.00,
            };

            var envioDARE = new Business.DFe.Servicos.DARE.EnvioDARE(conteudoXML, configuracao);
            envioDARE.Executar();
        }

        /// <summary>
        /// Testar o envio do DARE Lote
        /// </summary>
        [Theory]
        [Trait("DFe", "DARE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void DAREEnvioLote(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.DARE,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.DAREEnvio,
                CertificadoDigital = PropConfig.CertificadoDigital,
                SchemaVersao = "1.00",
                ApiKey = "jArkFGc5dxkxGjdmVQK7FiPQ2EJQqi7J"
            };

            var conteudoXML = new Business.DFe.Xml.DARE.DARELote
            {
                DadosContribuinteNaoCadastrado = new Business.DFe.Xml.DARE.DadosContribuinteNaoCadastrado
                {
                    Cnpj = "12345678000195",
                    Cidade = "São Paulo",
                    CodigoBarra44 = "12345678901234",
                    DataVencimento = DateTime.Now,
                    Endereco = "Rua Exemplo, 123",
                    Erro = new Business.DFe.Xml.DARE.Erro
                    {
                        EstaOk = true,
                        Mensagens = new List<string>
                        {
                            "teste1",
                            "teste2",
                            "teste3"
                        }
                    },
                    GerarPDF = true,
                    NumeroControleDarePrincipal = "123456789",
                    Observacao = "Pagamento referente ao imposto X",
                    PixCopiaCola = "abc123def456ghi789",
                    RazaoSocial = "Empresa Exemplo S/A",
                    Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                    {
                        Codigo = "046-2",
                        CodigoServicoDARE = "4601",
                        Nome = "ICMS - Operações Próprias- RPA (04601)",
                        EscopoUso = "2"
                    },
                    Referencia = "07/2024",
                    Telefone = "(11) 1234-5678",
                    Uf = "SP",
                    Valor = 1.00,
                    ValorJuros = 1.00,
                    ValorMulta = 1.00,
                    ValorTotal = 1.00,
                },
                Erro = new Business.DFe.Xml.DARE.Erro
                {
                    EstaOk = true,
                    Mensagens = new List<string>
                    {
                        "teste1",
                        "teste2",
                        "teste3"
                    }
                },
                TipoAgrupamentoFilhotes = "1",

                ItensParaGeracao = new System.Collections.Generic.List<Business.DFe.Xml.DARE.ItensParaGeracao>
                     {
                        new Business.DFe.Xml.DARE.ItensParaGeracao
                        {

                                    Cnpj = "12345678000195",
                                    Cidade = "São Paulo",
                                    CodigoBarra44 = "12345678901234",
                                    DataVencimento = DateTime.Now,
                                    Endereco = "Rua Exemplo, 123",
                                    Erro = new Business.DFe.Xml.DARE.Erro
                                    {
                                        EstaOk = true,
                                        Mensagens = new List<string>
                                        {
                                            "teste4",
                                            "teste5",
                                            "teste6"
                                        }
                                    },
                                    GerarPDF = true,
                                    NumeroControleDarePrincipal = "123456789",
                                    Observacao = "Pagamento referente ao imposto X",
                                    PixCopiaCola = "abc123def456ghi789",
                                    RazaoSocial = "Empresa Exemplo S/A",
                                    Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                                    {
                                        Codigo = "046-2",
                                        CodigoServicoDARE = "4601",
                                        Nome = "ICMS - Operações Próprias- RPA (04601)",
                                        EscopoUso = "2"
                                    },
                                    Referencia = "07/2024",
                                    Telefone = "(11) 1234-5678",
                                    Uf = "SP",
                                    Valor = 2.00,
                                    ValorJuros = 2.00,
                                    ValorMulta = 2.00,
                                    ValorTotal = 2.00,
                        },

                        new Business.DFe.Xml.DARE.ItensParaGeracao
                        {
                               Cnpj = "12345678000195",
                               Cidade = "São Paulo",
                               CodigoBarra44 = "12345678901234",
                               DataVencimento = DateTime.Now,
                               Endereco = "Rua Exemplo, 123",
                               Erro = new Business.DFe.Xml.DARE.Erro
                               {
                                  EstaOk = true,
                                  Mensagens = new List<string>
                                  {
                                      "teste1",
                                      "teste2",
                                      "teste3"
                                  }
                               },
                               GerarPDF = true,
                               NumeroControleDarePrincipal = "123456789",
                               Observacao = "Pagamento referente ao imposto X",
                               PixCopiaCola = "abc123def456ghi789",
                               RazaoSocial = "Empresa Exemplo S/A",
                               Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                               {
                                  Codigo = "046-2",
                                  CodigoServicoDARE = "4601",
                                  Nome = "ICMS - Operações Próprias- RPA (04601)",
                                  EscopoUso = "2"
                               },
                               Referencia = "07/2024",
                               Telefone = "(11) 1234-5678",
                               Uf = "SP",
                               Valor = 3.00,
                               ValorJuros = 3.00,
                               ValorMulta = 3.00,
                               ValorTotal = 3.00,
                        }
                }

            };

            var envioDARELote = new Business.DFe.Servicos.DARE.EnvioDARELote(conteudoXML, configuracao);
            envioDARELote.Executar();
        }
    }
}