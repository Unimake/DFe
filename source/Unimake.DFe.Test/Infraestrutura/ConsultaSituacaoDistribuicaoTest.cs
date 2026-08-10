using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;
using BPeAutorizacao = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe;
using BPeXml = Unimake.Business.DFe.Xml.BPe.BPe;
using DCeAutorizacao = Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc;
using DCeXml = Unimake.Business.DFe.Xml.DCe.DCe;
using NF3eAutorizacao = Unimake.Business.DFe.Servicos.NF3e.AutorizacaoSinc;
using NF3eXml = Unimake.Business.DFe.Xml.NF3e.NF3e;
using NFComAutorizacao = Unimake.Business.DFe.Servicos.NFCom.AutorizacaoSinc;
using NFComXml = Unimake.Business.DFe.Xml.NFCom.NFCom;
using NFGasAutorizacao = Unimake.Business.DFe.Servicos.NFGas.AutorizacaoSinc;
using NFGasXml = Unimake.Business.DFe.Xml.NFGas.NFGas;

namespace Unimake.DFe.Test.Infraestrutura
{
    /// <summary>
    /// Caracteriza a reconstrução dos XMLs de distribuição a partir da consulta situação.
    /// </summary>
    public class ConsultaSituacaoDistribuicaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(150, true)]
        [InlineData(999, false)]
        public void BPePreservaProtocoloDaConsultaSituacao(int cStat, bool deveGravar)
        {
            var documento = LerXml<BPeXml>(@"..\..\..\BPe\Resources\bpe_minimo.xml");
            documento.Signature = null;
            var retorno = LerXml<Business.DFe.Xml.BPe.RetConsSitBPe>(@"..\..\..\BPe\Resources\retConsSitBPe.xml");
            retorno.ProtBPe[0].InfProt.ChBPe = documento.InfBPe.Chave;
            retorno.ProtBPe[0].InfProt.CStat = cStat;
            var servico = new BPeAutorizacao(documento, CriarConfiguracao(TipoDFe.BPe, documento.InfBPe.Ide.CUF));
            servico.RetConsSitBPe.Add(retorno);

            ValidarGravacao(pasta => servico.GravarXmlDistribuicao(pasta), deveGravar,
                documento.InfBPe.Chave + "-procBPe.xml", "bpeProc", documento.InfBPe.Chave, cStat);
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(150, true)]
        [InlineData(999, false)]
        public void NF3ePreservaProtocoloDaConsultaSituacao(int cStat, bool deveGravar)
        {
            var documento = LerXml<NF3eXml>(@"..\..\..\NF3e\Resources\nota_energia-nf3e.xml");
            var retorno = LerXml<Business.DFe.Xml.NF3e.RetConsSitNF3e>(@"..\..\..\NF3e\Resources\retConsSitNF3e.xml");
            retorno.ProtNF3e.InfProt.ChNF3e = documento.InfNF3e.Chave;
            retorno.ProtNF3e.InfProt.CStat = cStat;
            var servico = new NF3eAutorizacao(documento, CriarConfiguracao(TipoDFe.NF3e, documento.InfNF3e.Ide.CUF));
            servico.RetConsSitNF3.Add(retorno);

            ValidarGravacao(pasta => servico.GravarXmlDistribuicao(pasta), deveGravar,
                documento.InfNF3e.Chave + "-procnf3e.xml", "nf3eProc", documento.InfNF3e.Chave, cStat);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(150, true)]
        [InlineData(999, false)]
        public void NFComPreservaProtocoloDaConsultaSituacao(int cStat, bool deveGravar)
        {
            var documento = LerXml<NFComXml>(@"..\..\..\NFCom\Resources\nfcom.xml");
            var retorno = LerXml<Business.DFe.Xml.NFCom.RetConsSitNFCom>(@"..\..\..\NFCom\Resources\retConsSitNFCom.xml");
            retorno.ProtNFCom.InfProt.ChNFCom = documento.InfNFCom.Chave;
            retorno.ProtNFCom.InfProt.CStat = cStat;
            var servico = new NFComAutorizacao(documento, CriarConfiguracao(TipoDFe.NFCom, documento.InfNFCom.Ide.CUF));
            servico.RetConsSitNFCom.Add(retorno);

            ValidarGravacao(pasta => servico.GravarXmlDistribuicao(pasta), deveGravar,
                documento.InfNFCom.Chave + "-procNFCom.xml", "nfcomProc", documento.InfNFCom.Chave, cStat);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(150, true)]
        [InlineData(999, false)]
        public void NFGasPreservaProtocoloDaConsultaSituacao(int cStat, bool deveGravar)
        {
            var documento = LerXml<NFGasXml>(@"..\..\..\NFGas\Resources\nfgas.xml");
            var retorno = LerXml<Business.DFe.Xml.NFGas.RetConsSitNFGas>(@"..\..\..\NFGas\Resources\retConsSitNFGas.xml");
            retorno.ProtNFGas.InfProt.ChNFGas = documento.InfNFGas.Chave;
            retorno.ProtNFGas.InfProt.CStat = cStat;
            var servico = new NFGasAutorizacao(documento, CriarConfiguracao(TipoDFe.NFGas, documento.InfNFGas.Ide.CUF));
            servico.RetConsSitNFGas.Add(retorno);

            ValidarGravacao(pasta => servico.GravarXmlDistribuicao(pasta), deveGravar,
                documento.InfNFGas.Chave + "-procNFGas.xml", "nfgasProc", documento.InfNFGas.Chave, cStat);
        }

        [Theory]
        [Trait("DFe", "DCe")]
        [InlineData(150, true)]
        [InlineData(999, false)]
        public void DCePreservaProtocoloDaConsultaSituacao(int cStat, bool deveGravar)
        {
            var documento = DCe.Servicos.ServicosTest.CriarDCe();
            var retorno = LerXml<Business.DFe.Xml.DCe.RetConsSitDCe>(@"..\..\..\DCe\Resources\retConsSitDCe.xml");
            retorno.ProtDCe.InfProt.ChDCe = documento.InfDCe.Chave;
            retorno.ProtDCe.InfProt.CStat = cStat;
            var servico = new DCeAutorizacao(documento, CriarConfiguracao(TipoDFe.DCe, documento.InfDCe.Ide.CUF));
            servico.RetConsSitDCe.Add(retorno);

            ValidarGravacao(pasta => servico.GravarXmlDistribuicao(pasta), deveGravar,
                documento.InfDCe.Chave + "-procDCe.xml", "dceProc", documento.InfDCe.Chave, cStat);
        }

        private static Configuracao CriarConfiguracao(TipoDFe tipoDFe, UFBrasil uf) => new Configuracao
        {
            TipoDFe = tipoDFe,
            TipoEmissao = TipoEmissao.Normal,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CodigoUF = (int)uf,
            CertificadoDigital = PropConfig.CertificadoDigital
        };

        private static T LerXml<T>(string caminho) where T : new()
        {
            var xml = new XmlDocument();
            xml.Load(caminho);
            return XMLUtility.Deserializar<T>(xml.OuterXml);
        }

        private static void ValidarGravacao(Action<string> gravar, bool deveGravar, string nomeEsperado,
            string raizEsperada, string chaveEsperada, int cStatEsperado)
        {
            var pasta = Path.Combine(Path.GetTempPath(), "Unimake.DFe.Test", "ConsultaSituacao", Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(pasta);
            try
            {
                if (!deveGravar)
                {
                    Assert.ThrowsAny<Exception>(() => gravar(pasta));
                    Assert.Empty(Directory.EnumerateFiles(pasta));
                    return;
                }

                gravar(pasta);
                var arquivo = Assert.Single(Directory.EnumerateFiles(pasta));
                Assert.Equal(nomeEsperado, Path.GetFileName(arquivo));
                var xml = new XmlDocument();
                xml.Load(arquivo);
                Assert.Equal(raizEsperada, xml.DocumentElement.LocalName);
                Assert.Contains(chaveEsperada, xml.OuterXml);
                Assert.Equal(cStatEsperado.ToString(),
                    xml.SelectSingleNode("//*[local-name()='infProt']/*[local-name()='cStat']")?.InnerText);
            }
            finally
            {
                if (Directory.Exists(pasta))
                {
                    Directory.Delete(pasta, true);
                }
            }
        }
    }
}
