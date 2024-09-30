using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.DARE;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.DARE
{
    public class ReceitasDARE
    {
        /// <summary>
        /// Testar o consumo da API do receitas do DARE
        /// </summary>
        //[Theory]
        //[Trait("DFe", "ESocial")]
        //[InlineData(TipoAmbiente.Producao)]
        //[InlineData(TipoAmbiente.Homologacao)]
        //public void ReceitasDare(TipoAmbiente tipoAmbiente)
        //{
        //    var configuracao = new Configuracao
        //    {
        //        TipoDFe = TipoDFe.DARE,
        //        TipoAmbiente = tipoAmbiente,
        //        Servico = Servico.DAREEnvio,
        //        CertificadoDigital = PropConfig.CertificadoDigital,
        //        SchemaVersao = "1.031230",
        //        ApiKey = "jArkFGc5dxkxGjdmVQK7FiPQ2EJQqi7J"
        //    };

        //    var RD = new Business.DFe.Xml.DARE.Receitas
        //    {

        //    };

        //    var Receita = new Business.DFe.Servicos.DARE.EnvioDARE(/*NÃO SEI*/, configuracao);
        //    ReceitaDARE.Executar();
        //}

        ///// <summary>
        ///// Testar o consumo da API do receitas do DARE
        ///// </summary>
        //[Theory]
        //[Trait("DFe", "ESocial")]
        //[InlineData(TipoAmbiente.Producao)]
        //[InlineData(TipoAmbiente.Homologacao)]
        //public void DAREEnvioLote(TipoAmbiente tipoAmbiente)
        //{
        //    var configuracao = new Configuracao
        //    {
        //        TipoDFe = TipoDFe.DARE,
        //        TipoAmbiente = tipoAmbiente,
        //        Servico = Servico.DAREEnvio,
        //        CertificadoDigital = PropConfig.CertificadoDigital,
        //        SchemaVersao = "1.00",
        //        ApiKey = "jArkFGc5dxkxGjdmVQK7FiPQ2EJQqi7J"
        //    };

        //    /* TEM A VER COM Receita*/ = new Business.DFe.Xml.DARE.ReceitaDARE
        //    {
        //        Codigo = "001",
        //        CodigoServicoDARE = "101",
        //        EscopoUso = "1",
        //        Nome = "Receita Exemplo"

        //    };
        //}
    }
}