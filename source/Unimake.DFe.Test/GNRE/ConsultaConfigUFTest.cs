using System;
using System.Collections.Generic;
using Diag = System.Diagnostics;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.GNRE;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Consultar uma chave do MDFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
    /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
    /// </summary>
    public class ConsultaConfigUFTest
    {
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultarConfigUFGNRE(TipoAmbiente tipoAmbiente)
        {
            var xml = new TConsultaConfigUf
            {
                Ambiente = tipoAmbiente,
                UF = UFBrasil.RS,
                Receita = new Receita
                {
                    Courier = SimNaoLetra.Sim,
                    Value = 100012
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = 41,
                Servico = Servico.GNREConsultaConfigUF
            };

            var consultaConfigUF = new ConsultaConfigUF(xml, configuracao);
            consultaConfigUF.Executar();

            Diag.Debug.Assert(consultaConfigUF.Result != null);
            Diag.Debug.Assert(consultaConfigUF.Result.Ambiente.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Diag.Debug.Assert(consultaConfigUF.Result.SituacaoConsulta.Codigo.Equals("100") || consultaConfigUF.Result.SituacaoConsulta.Codigo.Equals("102"), "Código de retorno diferente de 100 e 102.");  //Lote de consulta de guia recebido com sucesso!
        }
    }
}
