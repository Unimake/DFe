using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CCG;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CCG;
using Xunit;

namespace Unimake.DFe.Test.CCG
{
    /// <summary>
    /// Testar o serviço de consulta protocolo da NFe
    /// </summary>
    public class CcgConsGTINTest
    {
        /// <summary>
        /// Consultar o código GTIN somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta</param>
        [Theory]
        [Trait("DFe", "CCG")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaGTIN(TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsGTIN
            {
                Versao = "1.00",
                GTIN = "7896714200217" //NEOTAREN 50MG COMP REV CT BL 1X20
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CCG,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var ccgConsGTIN = new CcgConsGTIN(xml, configuracao);
            ccgConsGTIN.Executar();

            Assert.True(configuracao.TipoAmbiente.Equals(TipoAmbiente.Producao), "Tipo de ambiente definido não pode ser diferente de produção. Consulta GTIN só tem endereço de produção.");
            Assert.True(ccgConsGTIN.Result.CStat.Equals(9490), "Não encontrou o GTIN consultado, deveria ter encontrado, pois se trata do GTIN da Coca Cola.");
            Assert.True(ccgConsGTIN.Result.TpGTIN.Equals(TipoCodigoGTIN.GTIN13), "Tipo do GTIN retornado está incorreto.");
            Assert.True(ccgConsGTIN.Result.NCM.Equals("30049037"), "NCM da coca cola retornado está incorreto.");
            Assert.True(ccgConsGTIN.Result.GTIN.Equals(xml.GTIN), "NCM da coca cola retornado está incorreto.");
        }

        /// <summary>
        /// Consultar o códigos GTIN de um XML
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta</param>
        [Theory]
        [Trait("DFe", "CCG")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaGTINdaNFe(TipoAmbiente tipoAmbiente)
        {
            var tabelaNCMs = new List<TabelaNCM>
            {
                new TabelaNCM { CodigoInicial = "2401", CodigoFinal = "2403" },
                new TabelaNCM { CodigoInicial = "3001", CodigoFinal = "3006" },
                new TabelaNCM { CodigoInicial = "9503", CodigoFinal = "9505" },
                new TabelaNCM { CodigoInicial = "2201", CodigoFinal = "2209" },
                new TabelaNCM { CodigoInicial = "2523", CodigoFinal = "2523" },
                new TabelaNCM { CodigoInicial = "3816", CodigoFinal = "3816" },
                new TabelaNCM { CodigoInicial = "2814", CodigoFinal = "2814" },
                new TabelaNCM { CodigoInicial = "2847", CodigoFinal = "2847" },
                new TabelaNCM { CodigoInicial = "3301", CodigoFinal = "3307" },
                new TabelaNCM { CodigoInicial = "3401", CodigoFinal = "3401" },
                new TabelaNCM { CodigoInicial = "4818", CodigoFinal = "4818" },
                new TabelaNCM { CodigoInicial = "8212", CodigoFinal = "8212" },
                new TabelaNCM { CodigoInicial = "9605", CodigoFinal = "9605" },
                new TabelaNCM { CodigoInicial = "9615", CodigoFinal = "9615" },
                new TabelaNCM { CodigoInicial = "9619", CodigoFinal = "9619" },
                new TabelaNCM { CodigoInicial = "0401", CodigoFinal = "0410" },
                new TabelaNCM { CodigoInicial = "0811", CodigoFinal = "0814" },
                new TabelaNCM { CodigoInicial = "0901", CodigoFinal = "0910" },
                new TabelaNCM { CodigoInicial = "1101", CodigoFinal = "1109" },
                new TabelaNCM { CodigoInicial = "1501", CodigoFinal = "1518" },
                new TabelaNCM { CodigoInicial = "1520", CodigoFinal = "1522" },
                new TabelaNCM { CodigoInicial = "1701", CodigoFinal = "1704" },
                new TabelaNCM { CodigoInicial = "1801", CodigoFinal = "1806" },
                new TabelaNCM { CodigoInicial = "1901", CodigoFinal = "1905" },
                new TabelaNCM { CodigoInicial = "2001", CodigoFinal = "2009" },
                new TabelaNCM { CodigoInicial = "2101", CodigoFinal = "2106" },
                new TabelaNCM { CodigoInicial = "2201", CodigoFinal = "2209" },
                new TabelaNCM { CodigoInicial = "2301", CodigoFinal = "2309" },
                new TabelaNCM { CodigoInicial = "3501", CodigoFinal = "3507" },
                new TabelaNCM { Codigo = "33061000" },
                new TabelaNCM { Codigo = "34013000" },
                new TabelaNCM { Codigo = "96032100" }
            };

            var doc = new XmlDocument();
            doc.Load(@"..\..\..\NFe\Resources\35240411111111111111550010000011111000001119-Nfe.xml");
            var nfe = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.NFe>(doc);

            foreach (var det in nfe.InfNFe[0].Det)
            {
                var cEan = det.Prod.CEAN;
                var cNCM = det.Prod.NCM;

                bool consultar = false;

                for (int i = 0; i < tabelaNCMs.Count; i++)
                {
                    if (!string.IsNullOrEmpty(tabelaNCMs[i].Codigo))
                    {
                        if (cNCM.Equals(tabelaNCMs[i].Codigo))
                        {
                            consultar = true;
                        }
                    }
                    else
                    {
                        if (Convert.ToInt32(cNCM.Substring(0,4)) >= Convert.ToInt32(tabelaNCMs[i].CodigoInicial) && Convert.ToInt32(cNCM.Substring(0, 4)) <= Convert.ToInt32(tabelaNCMs[i].CodigoFinal))
                        {
                            consultar = true;
                        }
                    }
                }

                if (consultar)
                {
                    var configuracao = new Configuracao
                    {
                        TipoDFe = TipoDFe.CCG,
                        TipoAmbiente = tipoAmbiente,
                        CertificadoDigital = PropConfig.CertificadoDigital
                    };

                    var xml = new ConsGTIN
                    {
                        Versao = "1.00",
                        GTIN = cEan
                    };

                    var ccgConsGTIN = new CcgConsGTIN(xml, configuracao);
                    ccgConsGTIN.Executar();

                    Assert.True(configuracao.TipoAmbiente.Equals(TipoAmbiente.Producao), "Tipo de ambiente definido não pode ser diferente de produção. Consulta GTIN só tem endereço de produção.");
                    Assert.True(ccgConsGTIN.Result.CStat.Equals(9490), "Não encontrou o GTIN consultado, deveria ter encontrado, pois se trata do GTIN da Coca Cola.");
                    Assert.True(ccgConsGTIN.Result.TpGTIN.Equals(TipoCodigoGTIN.GTIN13), "Tipo do GTIN retornado está incorreto.");
                    Assert.True(ccgConsGTIN.Result.NCM.Equals("22021000"), "NCM da coca cola retornado está incorreto.");
                    Assert.True(ccgConsGTIN.Result.GTIN.Equals(xml.GTIN), "NCM da coca cola retornado está incorreto.");
                }
            }
        }
    }

    /// <summary>
    /// Tabela de NCM sujeita a validação do GTIN
    /// </summary>
    public class TabelaNCM
    {
        public string Codigo { get; set; }
        public string CodigoInicial { get; set; }
        public string CodigoFinal { get; set; }

        public TabelaNCM()
        {

        }
    }
}
