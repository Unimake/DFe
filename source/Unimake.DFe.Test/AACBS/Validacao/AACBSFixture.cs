using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Validacao
{
    internal static class AACBSFixture
    {
        internal const string Namespace = "http://www.unimake.com.br/dfe/aacbs";

        private static readonly MethodInfo DeserializarXml = typeof(XMLUtility).GetMethod(
            nameof(XMLUtility.Deserializar), new[] { typeof(XmlDocument) });

        internal static TheoryData<string, Type, Servico> Raizes => new()
        {
            { "autenticar", typeof(Autenticar), Servico.AACBSAutenticar },
            { "retAutenticar", typeof(RetAutenticar), Servico.AACBSAutenticar },
            { "consultarDebitos", typeof(ConsultarDebitos), Servico.AACBSConsultarDebitos },
            { "retConsultarDebitos", typeof(RetConsultarDebitos), Servico.AACBSConsultarDebitos },
            { "consultarCreditos", typeof(ConsultarCreditos), Servico.AACBSConsultarCreditos },
            { "retConsultarCreditos", typeof(RetConsultarCreditos), Servico.AACBSConsultarCreditos },
            { "consultarPagamentos", typeof(ConsultarPagamentos), Servico.AACBSConsultarPagamentos },
            { "retConsultarPagamentos", typeof(RetConsultarPagamentos), Servico.AACBSConsultarPagamentos },
            { "consultarRecolhimentos", typeof(ConsultarRecolhimentos), Servico.AACBSConsultarRecolhimentos },
            { "retConsultarRecolhimentos", typeof(RetConsultarRecolhimentos), Servico.AACBSConsultarRecolhimentos },
            { "consultarSituacaoSolicitacao", typeof(ConsultarSituacaoSolicitacao), Servico.AACBSConsultarSituacaoSolicitacao },
            { "retConsultarSituacaoSolicitacao", typeof(RetConsultarSituacaoSolicitacao), Servico.AACBSConsultarSituacaoSolicitacao },
            { "consultarLinkDownload", typeof(ConsultarLinkDownload), Servico.AACBSConsultarLinkDownload },
            { "retConsultarLinkDownload", typeof(RetConsultarLinkDownload), Servico.AACBSConsultarLinkDownload },
            { "baixarArquivoRetorno", typeof(BaixarArquivoRetorno), Servico.AACBSBaixarArquivoRetorno },
            { "retBaixarArquivoRetorno", typeof(RetBaixarArquivoRetorno), Servico.AACBSBaixarArquivoRetorno },
            { "retArquivoDebitos", typeof(RetArquivoDebitos), Servico.AACBSBaixarArquivoRetorno },
            { "retArquivoCreditos", typeof(RetArquivoCreditos), Servico.AACBSBaixarArquivoRetorno },
            { "retArquivoPagamentos", typeof(RetArquivoPagamentos), Servico.AACBSBaixarArquivoRetorno },
            { "retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos), Servico.AACBSBaixarArquivoRetorno },
            { "gerarDarfAdquirente", typeof(GerarDarfAdquirente), Servico.AACBSGerarDarfAdquirente },
            { "retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente), Servico.AACBSGerarDarfAdquirente },
            { "gerarDarfContribuinte", typeof(GerarDarfContribuinte), Servico.AACBSGerarDarfContribuinte },
            { "retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte), Servico.AACBSGerarDarfContribuinte }
        };

        internal static XmlDocument Carregar(string raiz)
        {
            var xml = new XmlDocument { XmlResolver = null };
            xml.Load(Path.Combine(AppContext.BaseDirectory, "AACBS", "Resources", raiz + ".xml"));
            return xml;
        }

        internal static XmlDocument Ler(string conteudo)
        {
            var xml = new XmlDocument { XmlResolver = null };
            xml.LoadXml(conteudo);
            return xml;
        }

        internal static XmlElement Procurar(XmlDocument xml, string caminho)
        {
            if (caminho.Length == 0)
            {
                return xml.DocumentElement;
            }

            var namespaces = new XmlNamespaceManager(xml.NameTable);
            namespaces.AddNamespace("a", Namespace);
            var xpath = string.Join("/", caminho.Split('/').Select(segmento => "a:" + segmento));
            return Assert.IsAssignableFrom<XmlElement>(xml.DocumentElement.SelectSingleNode(xpath, namespaces));
        }

        internal static XMLBase Desserializar(XmlDocument original, Type modelo) =>
            Assert.IsAssignableFrom<XMLBase>(
                DeserializarXml.MakeGenericMethod(modelo).Invoke(null, new object[] { original }));

        internal static XmlDocument Gerar(XmlDocument original, Type modelo) => Desserializar(original, modelo).GerarXML();

        internal static ValidarSchema ValidarSchemaDe(XmlDocument xml, string raiz)
        {
            var validacao = new ValidarSchema();
            validacao.Validar(xml, $"AACBS.{raiz}_v1.00.xsd");
            return validacao;
        }
    }
}
