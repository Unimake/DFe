using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar o serviço de consulta protocolo da NFe
    /// </summary>
    public class DistribuicaoDFeTest
    {
        /// <summary>
        /// Consultar o status do serviço da NFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta do DFe</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultarDFeDestinado(TipoAmbiente tipoAmbiente)
        {
            var nsu = "000000000000000";
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            while (true)
            {
                var xml = new DistDFeInt
                {
                    Versao = "1.01",
                    TpAmb = tipoAmbiente,
                    CNPJ = PropConfig.CNPJEmpresaCertificado,
                    CUFAutor = PropConfig.UFEmpresaCertificado,
                    DistNSU = new DistNSU
                    {
                        UltNSU = nsu
                    }
                };

                var distribuicaoDFe = new DistribuicaoDFe(xml, configuracao);
                distribuicaoDFe.Executar();

                Assert.True(configuracao.CodigoUF.Equals(91), "UF definida nas configurações diferente de 91-Ambiente Nacional.");
                Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
                Assert.True(distribuicaoDFe.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
                //Assert.True(statusServico.Result.CStat.Equals(107), "Serviço não está em operação");

                if (distribuicaoDFe.Result.CStat.Equals(138)) //Documentos localizados
                {
                    //TODO: WANDREY - Preciso, de alguma forma, testar os arquivos gravados para ver se deu certo.
                    //var folder = @"c:\testenfe\doczip";

                    //if(Environment.MachineName == "MARCELO-PC")
                    //{
                    //    folder = @"D:\temp\uninfe";
                    //}                       

                    ////Salvar os XMLs do docZIP no HD
                    //distribuicaoDFe.GravarXMLDocZIP(folder, true);

                    foreach (var item in distribuicaoDFe.ResEventos)
                    {
                        Assert.True(!string.IsNullOrWhiteSpace(item.ChNFe), "Chave da NFe está nula ou em branco, algo está errado.");
                    }
                    foreach (var item in distribuicaoDFe.ResNFes)
                    {
                        Assert.True(!string.IsNullOrWhiteSpace(item.ChNFe), "Chave da NFe está nula ou em branco, algo está errado.");
                    }
                }

                nsu = distribuicaoDFe.Result.UltNSU;

                if (Convert.ToInt64(distribuicaoDFe.Result.UltNSU) >= Convert.ToInt64(distribuicaoDFe.Result.MaxNSU))
                {
                    break;
                }
            }
        }
    }
}