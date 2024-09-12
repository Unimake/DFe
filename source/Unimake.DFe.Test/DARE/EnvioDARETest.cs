using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.DARE
{
    public class EnvioDARE
    {
        /// <summary>
        /// Testar o envio do DARE Unico
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void DAREEnvioUnitario(TipoAmbiente tipoAmbiente)
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
                Cnpj = "12345678000195",
                Cidade = "São Paulo",
                CodigoBarra44 = "12345678901234",
                DataVencimento = "2024-12-31T00:00:00",
                Endereco = "Rua Exemplo, 123",
                Erro = new Business.DFe.Xml.DARE.Erro
                {
                    EstaOk = "true"
                },
                GerarPDF = "true",
                NumeroControleDarePrincipal = "123456789",
                Observacao = "Pagamento referente ao imposto X",
                PixCopiaCola = "abc123def456ghi789",
                RazaoSocial = "Empresa Exemplo S/A",
                Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                {
                    Codigo = "001",
                    CodigoServicoDARE = "101",
                    EscopoUso = "1",
                    Nome = "Receita Exemplo"
                },
                Referencia = "REF123",
                Telefone = "(11) 1234-5678",
                Uf = "SP",
                Valor = "1000.50",
                ValorJuros = "10.00",
                ValorMulta = "5.00",
                ValorTotal = "1015.50",
            };

            var envioDARE = new Business.DFe.Servicos.DARE.EnvioDARE(conteudoXML, configuracao);
            envioDARE.Executar();
        }

        /// <summary>
        /// Testar o envio do DARE Unico
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
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
                    DataVencimento = "2024-12-31T00:00:00",
                    Endereco = "Rua Exemplo, 123",
                    Erro = new Business.DFe.Xml.DARE.Erro
                    {
                        EstaOk = "true",
                        Mensagens = new List<Business.DFe.Xml.DARE.MensagensEnvio>
                                        {
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste",
                                            },
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste2",
                                            }
                                        }
                    },
                    GerarPDF = "true",
                    NumeroControleDarePrincipal = "123456789",
                    Observacao = "Pagamento referente ao imposto X",
                    PixCopiaCola = "abc123def456ghi789",
                    RazaoSocial = "Empresa Exemplo S/A",
                    Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                    {
                        Codigo = "001",
                        CodigoServicoDARE = "101",
                        EscopoUso = "1",
                        Nome = "Receita Exemplo"
                    },
                    Referencia = "REF123",
                    Telefone = "(11) 1234-5678",
                    Uf = "SP",
                    Valor = "1000.50",
                    ValorJuros = "10.00",
                    ValorMulta = "5.00",
                    ValorTotal = "1015.50",
                    //PossiveisReceitas = new System.Collections.Generic.List<string>
                    //{
                    //    "Receita1",
                    //    "Receita2",
                    //    "Receita3"
                    //}
                },
                Erro = new Business.DFe.Xml.DARE.Erro
                {
                    EstaOk = "true",
                    Mensagens = new List<Business.DFe.Xml.DARE.MensagensEnvio>
                    {
                       new Business.DFe.Xml.DARE.MensagensEnvio
                       {
                            Item = "teste",
                       },
                       new Business.DFe.Xml.DARE.MensagensEnvio
                       {
                            Item = "teste2",
                       }
                    }
                },
                TipoAgrupamentoFilhotes = "1",

                ItensParaGeracao = new System.Collections.Generic.List<Business.DFe.Xml.DARE.ItensParaGeracao>
                     {
                        new Business.DFe.Xml.DARE.ItensParaGeracao
                        {
                            DARE = new System.Collections.Generic.List<Business.DFe.Xml.DARE.DARE>
                            {
                                new Business.DFe.Xml.DARE.DARE
                                {
                                    Cnpj = "12345678000195",
                                    Cidade = "São Paulo",
                                    CodigoBarra44 = "12345678901234",
                                    DataVencimento = "2024-12-31T00:00:00",
                                    Endereco = "Rua Exemplo, 123",
                                    Erro = new Business.DFe.Xml.DARE.Erro
                                    {
                                        EstaOk = "true",
                                        Mensagens = new List<Business.DFe.Xml.DARE.MensagensEnvio>
                                        {
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste",
                                            },
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste2",
                                            }
                                        }
                                    },
                                    GerarPDF = "true",
                                    NumeroControleDarePrincipal = "123456789",
                                    Observacao = "Pagamento referente ao imposto X",
                                    PixCopiaCola = "abc123def456ghi789",
                                    RazaoSocial = "Empresa Exemplo S/A",
                                    Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                                    {
                                        Codigo = "001",
                                        CodigoServicoDARE = "101",
                                        EscopoUso = "1",
                                        Nome = "Receita Exemplo"
                                    },
                                    Referencia = "REF123",
                                    Telefone = "(11) 1234-5678",
                                    Uf = "SP",
                                    Valor = "1000.50",
                                    ValorJuros = "10.00",
                                    ValorMulta = "5.00",
                                    ValorTotal = "1015.50",
                                    },

                                new Business.DFe.Xml.DARE.DARE
                                {
                                    Cnpj = "12345678000195",
                                    Cidade = "São Paulo",
                                    CodigoBarra44 = "12345678901234",
                                    DataVencimento = "2024-12-31T00:00:00",
                                    Endereco = "Rua Exemplo, 123",
                                    Erro = new Business.DFe.Xml.DARE.Erro
                                    {
                                        EstaOk = "true",
                                        Mensagens = new List<Business.DFe.Xml.DARE.MensagensEnvio>
                                        {
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste",
                                            },
                                            new Business.DFe.Xml.DARE.MensagensEnvio
                                            {
                                                Item = "teste2",
                                            }
                                        }
                                    },
                                    GerarPDF = "true",
                                    NumeroControleDarePrincipal = "123456789",
                                    Observacao = "Pagamento referente ao imposto X",
                                    PixCopiaCola = "abc123def456ghi789",
                                    RazaoSocial = "Empresa Exemplo S/A",
                                    Receita = new Business.DFe.Xml.DARE.ReceitaDARE
                                    {
                                        Codigo = "001",
                                        CodigoServicoDARE = "101",
                                        EscopoUso = "1",
                                        Nome = "Receita Exemplo"
                                    },
                                    Referencia = "REF123",
                                    Telefone = "(11) 1234-5678",
                                    Uf = "SP",
                                    Valor = "1000.50",
                                    ValorJuros = "10.00",
                                    ValorMulta = "5.00",
                                    ValorTotal = "1015.50",
                                    }
                                }
                            }
                    }
            };

            var envioDARELote = new Business.DFe.Servicos.DARE.EnvioDARELote(conteudoXML, configuracao);
            envioDARELote.Executar();
        }
    }
}