using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Xunit;
using DCeAutorizacaoSinc = Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc;
using DCeConsultaProtocolo = Unimake.Business.DFe.Servicos.DCe.ConsultaProtocolo;
using DCeRecepcaoEvento = Unimake.Business.DFe.Servicos.DCe.RecepcaoEvento;
using DCeStatusServico = Unimake.Business.DFe.Servicos.DCe.StatusServico;
using XmlDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe
{
    /// <summary>
    /// Testar a serialização e desserialização dos serviços da DCe
    /// </summary>
    public class ServicosTest
    {
        private const string ChaveDCe = "41260500000000000199990000000000110000123456";
        private static readonly string ResourcesPath = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, @"..\..\..\DCe\Resources"));

        /// <summary>
        /// Cria a configuração para os serviços da DCe em homologação
        /// </summary>
        /// <returns>Configuração dos serviços da DCe</returns>
        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoDFe = TipoDFe.DCe,
            CodigoUF = (int)UFBrasil.PR,
            TipoAmbiente = TipoAmbiente.Homologacao
        };

        /// <summary>
        /// Carrega um XML de recurso da DCe
        /// </summary>
        /// <param name="fileName">Nome do arquivo XML</param>
        /// <returns>Conteúdo do XML</returns>
        private static string LoadXml(string fileName) =>
            File.ReadAllText(Path.Combine(ResourcesPath, fileName))
                .Replace("3526050000000000019959900000000011000001234567", ChaveDCe)
                .Replace("<cUF>35</cUF>", "<cUF>41</cUF>")
                .Replace("<cOrgao>35</cOrgao>", "<cOrgao>41</cOrgao>")
                .Replace("ID1101113526050000000000019959900000000011000001234567001", "ID110111" + ChaveDCe + "001");

        /// <summary>
        /// Define o retorno mockado do webservice no serviço
        /// </summary>
        /// <param name="servico">Serviço da DCe</param>
        /// <param name="xml">XML de retorno do serviço</param>
        private static void SetRetorno(ServicoBase servico, string xml)
        {
            var doc = new XmlDocument();
            doc.LoadXml(xml);

            servico.RetornoWSString = xml;
            servico.RetornoWSXML = doc;
        }

        /// <summary>
        /// Testar o serviço de consulta status da DCe
        /// </summary>
        [Fact]
        public void StatusServico()
        {
            var xml = XMLUtility.Deserializar<ConsStatServDCe>(LoadXml("consStatServDCe.xml"));
            xml.CUF = UFBrasil.PR;
            var servico = new DCeStatusServico(xml, CriarConfiguracao());

            SetRetorno(servico, LoadXml("retConsStatServDCe.xml"));

            Assert.Contains("<consStatServDCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeStatusServico, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeStatusServico?wsdl", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(107, servico.Result.CStat);
            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
        }

        /// <summary>
        /// Testar o serviço de consulta protocolo da DCe
        /// </summary>
        [Fact]
        public void ConsultaProtocolo()
        {
            var xml = XMLUtility.Deserializar<ConsSitDCe>(LoadXml("consSitDCe.xml"));
            xml.ChDCe = ChaveDCe;
            var servico = new DCeConsultaProtocolo(xml, CriarConfiguracao());

            SetRetorno(servico, LoadXml("retConsSitDCe.xml"));

            Assert.Contains("<consSitDCe", servico.ConteudoXMLOriginal.OuterXml);
            Assert.Equal(Servico.DCeConsultaProtocolo, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeConsulta?wsdl", servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(100, servico.Result.CStat);
            Assert.Equal(ChaveDCe, servico.Result.ChDCe);
        }

        /// <summary>
        /// Testar o serviço de autorização síncrona da DCe
        /// </summary>
        [Fact]
        public void AutorizacaoSinc()
        {
            var xml = XMLUtility.Deserializar<XmlDCe>(LoadXml("dce.xml"));
            var config = CriarConfiguracao();
            config.Servico = Servico.DCeAutorizacaoSinc;
            config.SchemaVersao = "1.00";
            config.Load("AutorizacaoSinc");
            var servico = new DCeAutorizacaoSinc();

            SetRetorno(servico, LoadXml("retDCe.xml"));

            Assert.Contains("<DCe", xml.GerarXML().OuterXml);
            Assert.Equal(Servico.DCeAutorizacaoSinc, config.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeAutorizacao?wsdl", config.WebEnderecoHomologacao);
            Assert.Equal(100, servico.Result.CStat);
            Assert.Equal(ChaveDCe, servico.Result.ProtDCe.InfProt.ChDCe);
        }

        /// <summary>
        /// Testar o serviço de recepção de evento da DCe
        /// </summary>
        [Fact]
        public void RecepcaoEvento()
        {
            var xml = XMLUtility.Deserializar<EventoDCe>(LoadXml("eventoDCe.xml"));
            var config = CriarConfiguracao();
            config.Servico = Servico.DCeRecepcaoEvento;
            config.SchemaVersao = "1.00";
            config.Load("RecepcaoEvento");
            var servico = new DCeRecepcaoEvento();

            SetRetorno(servico, LoadXml("retEventoDCe.xml"));

            Assert.Contains("<eventoDCe", xml.GerarXML().OuterXml);
            Assert.Equal(Servico.DCeRecepcaoEvento, config.Servico);
            Assert.Equal("https://homologacao.dce.fazenda.pr.gov.br/dce/DCeRecepcaoEvento?wsdl", config.WebEnderecoHomologacao);
            Assert.Equal(135, servico.Result.InfEvento.CStat);
            Assert.Equal(ChaveDCe, servico.Result.InfEvento.ChDCe);
        }
    }
}
