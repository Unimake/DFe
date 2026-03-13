using System;
using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCe;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFCe
{
    /// <summary>
    /// Testar serialização e deserialização do XML de listagem de chaves de NFCe
    /// </summary>
    public class NFCeListagemChavesTest
    {
        /// <summary>
        /// Testar a deserialização e serialização do XML NFCeListagemChaves
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFCe\NFCe SP\NFCeListagemChaves.xml")]
        public void SerializarDeserializarNFCeListagemChaves(string arqXML)
        {
            Assert.True(File.Exists(arqXML), $"Arquivo {arqXML} não encontrado.");

            var conteudoXML = File.ReadAllText(arqXML);
            var xml = XMLUtility.Deserializar<NFCeListagemChaves>(conteudoXML);

            Assert.NotNull(xml);
            Assert.Equal("1.00", xml.Versao);
            Assert.Equal(TipoAmbiente.Homologacao, xml.TpAmb);
            Assert.Contains("2026-01-10T08:30", xml.DataHoraInicialField);
            Assert.Contains("2026-02-20T08:30", xml.DataHoraFinalField);

            var xmlDoc = xml.GerarXML();
            Assert.NotNull(xmlDoc);

            var xmlString = xmlDoc.OuterXml;
            Assert.Contains("nfceListagemChaves", xmlString);
            Assert.Contains("versao=\"1.00\"", xmlString);
            Assert.Contains("<tpAmb>2</tpAmb>", xmlString);
        }

        /// <summary>
        /// Testar a comunicação com o serviço de listagem de chaves de NFCe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        public void ConsultarListagemChavesNFCe(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new NFCeListagemChaves
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                DataHoraInicial = new DateTime(2026, 01, 10, 08, 30, 00),
                DataHoraFinal = new DateTime(2026, 02, 20, 18, 00, 00)
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoUF = (int)ufBrasil
            };

            var servico = new ConsultaChaves(xml, configuracao);
            servico.Executar();

            Assert.NotNull(servico.Result);
            Assert.False(string.IsNullOrWhiteSpace(servico.Result.CStat));
            Assert.NotNull(servico.Result.XMotivo);
            Assert.Equal(tipoAmbiente, servico.Result.TpAmb);

            if (servico.Result.CStat == "100") Assert.NotNull(servico.Result.ChNFCe);
            Console.WriteLine($"Status: {servico.Result.CStat} - {servico.Result.XMotivo}");
        }

        /// <summary>
        /// Testar a comunicação com o serviço usando string XML (Simulação INTEROP)
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        public void ConsultarListagemChavesNFCeComString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xmlReq = new NFCeListagemChaves
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                DataHoraInicialField = "2026-01-10T08:30",
                DataHoraFinalField = "2026-02-20T18:00"
            };

            var xmlString = xmlReq.GerarXML().OuterXml;

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoUF = (int)ufBrasil
            };

            var servico = new ConsultaChaves(xmlString, configuracao);
            servico.Executar();

            Assert.NotNull(servico.Result);
            Assert.False(string.IsNullOrWhiteSpace(servico.Result.CStat));
            Assert.Equal(tipoAmbiente, servico.Result.TpAmb);

            Console.WriteLine($"Status: {servico.Result.CStat} - {servico.Result.XMotivo}");
        }
    }
}
