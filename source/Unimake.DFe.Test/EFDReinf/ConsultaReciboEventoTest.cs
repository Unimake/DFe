using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Teste da consulta recibo evento
    /// </summary>
    public class ConsultaReciboEventoTest
    {
        /// <summary>
        /// Testar a consulta recibo do Evento 1000 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento1000(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "1000",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 1070 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento1070(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "1070",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2010 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2010(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2010",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    TpInscEstab = TipoInscricaoEstabelecimento.CNPJ,
                    NrInscEstab = "00000000000000",
                    CnpjPrestador = "00000000000000"
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2020 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2020(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2020",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    NrInscEstabPrest = "00000000000000",
                    TpInscTomador = TipoInscricaoEstabelecimento.CNPJ,
                    NrInscTomador = "00000000000000"
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2030 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2030(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2030",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    NrInscEstab = "00000000000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2040 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2040(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2040",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    NrInscEstab = "00000000000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2050 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2050(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2050",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    NrInscEstab = "00000000000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2055 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2055(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2055",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    TpInscAdq = TipoInscricaoAdquirente.CNPJ,
                    NrInscAdq = "00000000000000",
                    TpInscProd = TiposInscricao.CNPJ,
                    NrInscProd = "00000000000000"
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2060 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2060(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2060",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    TpInscEstab = TipoInscricaoEstabelecimento.CNPJ,
                    NrInscEstab = "00000000000000"
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2098 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2098(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2098",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2099 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento2099(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "2099",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Testar a consulta recibo do Evento 2099 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento3010(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "3010",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                    PerApur = new DateTime(2018, 12, 20),
                    NrInscEstabelecimento = "00000000000000"
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"), "O código retornado é diferente de 3");
        }

        /// <summary>
        /// Teste Construtor simplificado para API
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEventoConstrutor(TipoAmbiente tipoAmbiente)
        {
            var tipoEvento = "1000";
            var tipoInscricao = TiposInscricao.CNPJ;
            var numeroInscricao = "00000000";

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(
                tipoEvento,
                tipoInscricao,
                numeroInscricao,
                tipoAmbiente,
                configuracao);

            consultaReciboEvento.Executar();

            // Verificar se as configurações foram definidas corretamente
            Assert.Equal(Servico.EFDReinfConsultaReciboEvento, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoEvento, configuracao.TipoEventoEFDReinf);

            // Verificar se o XML foi gerado corretamente
            Assert.Contains(tipoEvento, consultaReciboEvento.ConteudoXMLOriginal.InnerXml);
            Assert.Contains(numeroInscricao, consultaReciboEvento.ConteudoXMLOriginal.InnerXml);

            // Verificar se a chamada ao serviço retornou o código esperado
            Assert.True(consultaReciboEvento.Result.IdeStatus.CdRetorno.Equals("3"),
                "O código retornado é diferente de 3");
        }
    }
}
