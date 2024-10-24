using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.DARE;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.DARE
{
    public class ReceitasDARETest
    {
        /// <summary>
        /// Testar o consumo da API do receitas do DARE
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ReceitasDare(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.DARE,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.DAREReceita,
                CertificadoDigital = PropConfig.CertificadoDigital,
                SchemaVersao = "1.00",
                ApiKey = "jArkFGc5dxkxGjdmVQK7FiPQ2EJQqi7J"
            };

            var consulta = new Business.DFe.Xml.DARE.Receitas
            {
                Consulta = "CONSULTAR RECEITA"
            };

            var receitaDARE = new Business.DFe.Servicos.DARE.ReceitasDARE(consulta, configuracao);
            receitaDARE.Executar();
        }
    }
}