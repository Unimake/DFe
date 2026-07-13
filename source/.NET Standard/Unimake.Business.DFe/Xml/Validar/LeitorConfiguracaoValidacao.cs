using System;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Converte as regras do catálogo em informações utilizadas pelo pipeline de validação.
    /// </summary>
    internal static class LeitorConfiguracaoValidacao
    {
        internal static ValidarEstruturaXML.InformacaoXML Ler(XmlNode servico, int codigoConfiguracao)
        {
            return new ValidarEstruturaXML.InformacaoXML
            {
                TagRaiz = servico.Attributes?["tagRaiz"]?.Value,
                Versao = servico.Attributes?["versao"]?.Value,
                Descricao = ObterTexto(servico, "Descricao"),
                SchemaArquivo = ObterTexto(servico, "SchemaArquivo"),
                TargetNS = ObterTexto(servico, "TargetNS"),
                TagAssinatura = ObterTexto(servico, "TagAssinatura"),
                TagAtributoID = ObterTexto(servico, "TagAtributoID"),
                TagEvento = ObterTexto(servico, "TagEvento"),
                TagLoteAssinatura = ObterTexto(servico, "TagLoteAssinatura"),
                TagLoteAtributoID = ObterTexto(servico, "TagLoteAtributoID"),
                TagExtraAssinatura = ObterTexto(servico, "TagExtraAssinatura"),
                TagExtraAtributoID = ObterTexto(servico, "TagExtraAtributoID"),
                NaoAssina = ObterAmbienteSemAssinatura(servico, codigoConfiguracao),
                UsaCertificadoDigital = UsaCertificadoDigital(servico, codigoConfiguracao),
                AssinaCanonicalizacaoExclusiva = UsaCanonicalizacaoExclusiva(servico, codigoConfiguracao),
                SignatureAlgorithmType = ObterAlgoritmoAssinatura(servico, codigoConfiguracao),
                GerarQRCode = string.Equals(ObterTexto(servico, "GerarQrCode")?.Trim(), "true", StringComparison.Ordinal)
            };
        }

        internal static void AplicarInformacaoEspecifica(XmlNode tipo, ValidarEstruturaXML.InformacaoXML informacao)
        {
            informacao.SchemaArquivo = ObterTexto(tipo, "SchemaArquivo");
            informacao.SchemaArquivoEspecifico = ObterTexto(tipo, "SchemaArquivoEspecifico");
            informacao.TargetNSEspecifico = ObterTexto(tipo, "TargetNS") ?? informacao.TargetNS;
            informacao.TagAtributoID = ObterTexto(tipo, "TagAtributoID") ?? informacao.TagAtributoID;
        }

        internal static bool UsaCertificadoDigital(XmlNode servico, int codigoConfiguracao)
        {
            return ObterExcecao(servico, codigoConfiguracao, "UsaCertificadoDigital")?.InnerText?.Trim() != "false";
        }

        internal static TipoAmbiente? ObterAmbienteSemAssinatura(XmlNode servico, int codigoConfiguracao)
        {
            var ambiente = ObterExcecao(servico, codigoConfiguracao, "NaoAssina")?.InnerText?.Trim();

            if (string.Equals(ambiente, "homologação", StringComparison.OrdinalIgnoreCase))
            {
                return TipoAmbiente.Homologacao;
            }

            if (string.Equals(ambiente, "produção", StringComparison.OrdinalIgnoreCase))
            {
                return TipoAmbiente.Producao;
            }

            return null;
        }

        internal static bool UsaCanonicalizacaoExclusiva(XmlNode servico, int codigoConfiguracao)
        {
            return ObterExcecao(servico, codigoConfiguracao, "AssinaCanonicalizacaoExclusiva")?.InnerText?.Trim() == "true";
        }

        internal static AlgorithmType ObterAlgoritmoAssinatura(XmlNode servico, int codigoConfiguracao)
        {
            var valor = ObterValor(servico, codigoConfiguracao, "SignatureAlgorithmType")
                ?? ObterValor(servico?.ParentNode, codigoConfiguracao, "SignatureAlgorithmType");

            if (string.IsNullOrWhiteSpace(valor))
            {
                return AlgorithmType.Sha1;
            }

            if (Enum.TryParse(valor, true, out AlgorithmType algoritmo) && Enum.IsDefined(typeof(AlgorithmType), algoritmo))
            {
                return algoritmo;
            }

            throw new Exception($"Valor inválido para SignatureAlgorithmType: {valor}.");
        }

        internal static string ObterValor(XmlNode node, int codigoConfiguracao, string nomeTag)
        {
            var nodeTag = ObterElemento(node, nomeTag);

            if (nodeTag == null)
            {
                return null;
            }

            var excecao = nodeTag.ChildNodes
                .Cast<XmlNode>()
                .FirstOrDefault(x => x.NodeType == XmlNodeType.Element &&
                    x.LocalName == "Excecao" &&
                    x.Attributes?["codMunicipio"]?.Value == codigoConfiguracao.ToString());

            if (excecao != null)
            {
                return excecao.InnerText?.Trim();
            }

            return nodeTag.ChildNodes.Cast<XmlNode>().Any(x => x.NodeType == XmlNodeType.Element)
                ? null
                : nodeTag.InnerText?.Trim();
        }

        internal static XmlNode ObterExcecao(XmlNode servico, int codigoConfiguracao, string nomeTag)
        {
            var nodeTag = ObterElemento(servico, nomeTag);
            return nodeTag?.ChildNodes
                .Cast<XmlNode>()
                .FirstOrDefault(x => x.NodeType == XmlNodeType.Element &&
                    x.LocalName == "Excecao" &&
                    x.Attributes?["codMunicipio"]?.Value == codigoConfiguracao.ToString());
        }

        private static string ObterTexto(XmlNode node, string nomeTag)
        {
            return ObterElemento(node, nomeTag)?.InnerText;
        }

        private static XmlNode ObterElemento(XmlNode node, string nomeTag)
        {
            return node?.ChildNodes
                .Cast<XmlNode>()
                .FirstOrDefault(x => x.NodeType == XmlNodeType.Element && x.LocalName == nomeTag);
        }
    }
}
