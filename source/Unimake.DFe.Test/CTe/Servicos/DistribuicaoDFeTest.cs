using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Servicos
{
    /// <summary>
    /// Testar o serviço de distribuição do CTe
    /// </summary>
    public class DistribuicaoDFeTest
    {
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(false)]
        [InlineData(true)]
        public void GravarXMLDocZIPSemDocumentosNaoDeveLancarExcecao(bool incluirLoteVazio)
        {
            using var distribuicaoDFe = new DistribuicaoDFe();
            var lote = incluirLoteVazio ? "<loteDistDFeInt />" : string.Empty;
            var retorno =
                $"<retDistDFeInt versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/cte\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>137</cStat><xMotivo>Nenhum documento localizado</xMotivo><dhResp>2026-08-17T10:00:00-03:00</dhResp><ultNSU>000000000000000</ultNSU><maxNSU>000000000000000</maxNSU>{lote}</retDistDFeInt>";
            var retornoXML = new System.Xml.XmlDocument();
            retornoXML.LoadXml(retorno);
            distribuicaoDFe.RetornoWSString = retorno;
            distribuicaoDFe.RetornoWSXML = retornoXML;

            var exception = Record.Exception(() => distribuicaoDFe.GravarXMLDocZIP("pasta-inexistente"));

            Assert.Null(exception);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(false, false, "41260899999999000199570010000000011000000010-procCTe.xml")]
        [InlineData(true, false, "000000000000123-procCTe.xml")]
        [InlineData(false, true, "41260899999999000199570010000000011000000010_110110_01-procEventoCTe.xml")]
        [InlineData(true, true, "000000000000123-procEventoCTe.xml")]
        public void GravarXMLDocZIPDeveNomearPorChaveOuNSU(
            bool fileNameWithNSU,
            bool evento,
            string nomeEsperado)
        {
            const string chave = "41260899999999000199570010000000011000000010";
            const string nsu = "000000000000123";
            var conteudo = evento
                ? $"<procEventoCTe versao=\"4.00\" xmlns=\"http://www.portalfiscal.inf.br/cte\"><eventoCTe versao=\"4.00\"><infEvento><chCTe>{chave}</chCTe><tpEvento>110110</tpEvento><nSeqEvento>1</nSeqEvento></infEvento></eventoCTe><retEventoCTe /></procEventoCTe>"
                : $"<cteProc versao=\"4.00\" xmlns=\"http://www.portalfiscal.inf.br/cte\"><CTe><infCte Id=\"CTe{chave}\" /></CTe><protCTe /></cteProc>";
            var schema = evento ? "procEventoCTe_v4.00.xsd" : "procCTe_v4.00.xsd";
            var retorno =
                $"<retDistDFeInt versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/cte\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>138</cStat><xMotivo>Documento localizado</xMotivo><dhResp>2026-08-17T10:00:00-03:00</dhResp><ultNSU>{nsu}</ultNSU><maxNSU>{nsu}</maxNSU><loteDistDFeInt><docZip NSU=\"{nsu}\" schema=\"{schema}\">{Unimake.Business.DFe.Utility.Compress.GZIPCompress(conteudo)}</docZip></loteDistDFeInt></retDistDFeInt>";
            var retornoXML = new System.Xml.XmlDocument();
            retornoXML.LoadXml(retorno);
            using var distribuicaoDFe = new DistribuicaoDFe
            {
                RetornoWSString = retorno,
                RetornoWSXML = retornoXML
            };
            var pasta = System.IO.Path.Combine(
                System.IO.Path.GetTempPath(),
                "unimake-dfe-cte-doczip-" + Guid.NewGuid().ToString("N"));

            try
            {
                System.IO.Directory.CreateDirectory(pasta);
                distribuicaoDFe.GravarXMLDocZIP(pasta, fileNameWithNSU);

                var arquivo = Assert.Single(System.IO.Directory.EnumerateFiles(pasta));
                Assert.Equal(nomeEsperado, System.IO.Path.GetFileName(arquivo));
                Assert.Equal(conteudo, System.IO.File.ReadAllText(arquivo));
            }
            finally
            {
                if (System.IO.Directory.Exists(pasta))
                {
                    System.IO.Directory.Delete(pasta, true);
                }
            }
        }

        /// <summary>
        /// Consultar de distribuição do CTe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua a consulta DFe ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta do DFe</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultarDFeDestinado(TipoAmbiente tipoAmbiente)
        {
            var nsu = "000000000000000";
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            while (true)
            {
                var xml = new DistDFeInt
                {
                    Versao = "1.00",
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
                }

                nsu = distribuicaoDFe.Result.UltNSU;

                if (Convert.ToInt64(distribuicaoDFe.Result.UltNSU) >= Convert.ToInt64(distribuicaoDFe.Result.MaxNSU))
                {
                    break;
                }
            }
        }

        /// <summary>
        /// Consultar de distribuição do CTe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua a consulta DFe ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta do DFe</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultarDFeDestinadoString(TipoAmbiente tipoAmbiente)
        {
            var nsu = "000000000000000";
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            while (true)
            {
                var xml = new DistDFeInt
                {
                    Versao = "1.00",
                    TpAmb = tipoAmbiente,
                    CNPJ = PropConfig.CNPJEmpresaCertificado,
                    CUFAutor = PropConfig.UFEmpresaCertificado,
                    DistNSU = new DistNSU
                    {
                        UltNSU = nsu
                    }
                };

                var distribuicaoDFe = new DistribuicaoDFe(xml.GerarXML().OuterXml, configuracao);
                distribuicaoDFe.Executar();

                Assert.True(configuracao.CodigoUF.Equals(91), "UF definida nas configurações diferente de 91-Ambiente Nacional.");
                Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
                Assert.True(distribuicaoDFe.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());

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
