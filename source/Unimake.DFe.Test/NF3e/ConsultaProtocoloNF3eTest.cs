﻿using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Business.DFe.Xml.NF3e;
using Xunit;

namespace Unimake.DFe.Test.NF3e
{
    /// <summary>
    /// Testar o serviço de consulta protocolo da NF3e
    /// </summary>
    public class ConsultaProtocoloTest
    {
        /// <summary>
        /// Consultar uma chave de NF3e somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a consulta situação</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta situação</param>
        [Theory]
        [Trait("DFe", "NF3e")]
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
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
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
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void ConsultarProtocoloNF3e(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsSitNF3e
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                ChNF3e = ((int)ufBrasil).ToString() + "200106117473000150550010000606641403753210" //Chave qualquer somente para termos algum tipo de retorno para sabe se a conexão com a sefaz funcionou
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaProtocolo = new ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(consultaProtocolo.Result.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(consultaProtocolo.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());

            if (consultaProtocolo.Result.ProtNF3e != null)
            {
                Assert.True(consultaProtocolo.Result.ProtNF3e.InfProt.ChNF3e.Equals(xml.ChNF3e), "Webservice retornou uma chave da NF3e diferente da enviada na consulta.");
            }
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void ConsultarProtocoloNF3eString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsSitNF3e
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                ChNF3e = ((int)ufBrasil).ToString() + "200106117473000150550010000606641403753210" //Chave qualquer somente para termos algum tipo de retorno para sabe se a conexão com a sefaz funcionou
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaProtocolo = new ConsultaProtocolo(xml.GerarXML().OuterXml, configuracao);
            consultaProtocolo.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(consultaProtocolo.Result.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(consultaProtocolo.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());

            if (consultaProtocolo.Result.ProtNF3e != null)
            {
                Assert.True(consultaProtocolo.Result.ProtNF3e.InfProt.ChNF3e.Equals(xml.ChNF3e), "Webservice retornou uma chave da NF3e diferente da enviada na consulta.");
            }
                
        }

    }
}