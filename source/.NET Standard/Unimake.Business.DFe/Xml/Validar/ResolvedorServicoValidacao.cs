using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Localiza no catálogo a regra aplicável ao XML que será validado.
    /// </summary>
    internal static class ResolvedorServicoValidacao
    {
        internal static XmlNode Resolver(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument catalogo, PadraoNFSe padraoNFSe)
        {
            if (tipoDFe == TipoDFe.NFSe)
            {
                return ResolverNFSe(xml, versao, tagRaiz, catalogo, padraoNFSe);
            }

            if (tipoDFe == TipoDFe.ESocial || tipoDFe == TipoDFe.EFDReinf)
            {
                return ResolverPorTagIdentificadora(xml, tipoDFe, catalogo);
            }

            return ResolverDFe(versao, tipoDFe, tagRaiz, catalogo);
        }

        internal static XmlNode ResolverPorTagIdentificadora(XmlDocument xml, TipoDFe tipoDFe, XmlDocument catalogo)
        {
            foreach (var servico in ObterServicosDFe(catalogo, tipoDFe))
            {
                var identificador = servico.Attributes?["tagIdentificadora"]?.Value;

                if (!string.IsNullOrWhiteSpace(identificador) && ContemElemento(xml, identificador))
                {
                    return servico;
                }
            }

            return null;
        }

        internal static XmlNode ResolverNFSe(XmlDocument xml, string versao, string tagRaiz, XmlDocument catalogo, PadraoNFSe padraoNFSe)
        {
            var padrao = catalogo
                .SelectNodes("/ServicosValidacao/NFSe/Padrao")
                .Cast<XmlNode>()
                .FirstOrDefault(x => string.Equals(x.Attributes?["nome"]?.Value, padraoNFSe.ToString(), StringComparison.Ordinal));

            if (padrao == null)
            {
                return null;
            }

            var candidatos = padrao
                .SelectNodes("Servico")
                .Cast<XmlNode>()
                .Where(x => string.Equals(x.Attributes?["tagRaiz"]?.Value, tagRaiz, StringComparison.Ordinal))
                .Where(x => string.IsNullOrWhiteSpace(versao) || string.Equals(x.Attributes?["versao"]?.Value, versao, StringComparison.Ordinal))
                .ToList();

            if (candidatos.Count == 1)
            {
                return candidatos[0];
            }

            XmlNode generico = null;

            foreach (var servico in candidatos)
            {
                var identificador = servico.Attributes?["tagIdentificadora"]?.Value?.Split(':').Last();

                if (string.IsNullOrWhiteSpace(identificador))
                {
                    if (generico == null)
                    {
                        generico = servico;
                    }

                    continue;
                }

                if (string.Equals(xml.DocumentElement?.LocalName, identificador, StringComparison.Ordinal) ||
                    ContemElemento(xml, identificador))
                {
                    return servico;
                }
            }

            return generico;
        }

        internal static XmlNode ResolverDFe(string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument catalogo)
        {
            var servicos = ObterServicosDFe(catalogo, tipoDFe);
            var servico = Procurar(servicos, tagRaiz, versao);

            if (servico == null && !string.IsNullOrWhiteSpace(versao))
            {
                servico = Procurar(servicos, tagRaiz, string.Empty);
            }

            if (servico == null && string.IsNullOrWhiteSpace(versao))
            {
                var candidatos = servicos
                    .Where(x => string.Equals(x.Attributes?["tagRaiz"]?.Value, tagRaiz, StringComparison.Ordinal))
                    .ToList();

                if (candidatos.Count == 1)
                {
                    servico = candidatos[0];
                }
            }

            if (servico == null && tipoDFe == TipoDFe.CTe && tagRaiz == "consStatServCte")
            {
                servico = Procurar(servicos, "consStatServCTe", versao);
            }

            return servico;
        }

        private static List<XmlNode> ObterServicosDFe(XmlDocument catalogo, TipoDFe tipoDFe)
        {
            var raiz = catalogo.DocumentElement?
                .ChildNodes
                .Cast<XmlNode>()
                .FirstOrDefault(x => x.NodeType == XmlNodeType.Element && x.Name == tipoDFe.ToString());

            return raiz?
                .ChildNodes
                .Cast<XmlNode>()
                .Where(x => x.NodeType == XmlNodeType.Element && x.Name == "Servico")
                .ToList() ?? new List<XmlNode>();
        }

        private static XmlNode Procurar(IEnumerable<XmlNode> servicos, string tagRaiz, string versao)
        {
            return servicos.FirstOrDefault(x =>
                string.Equals(x.Attributes?["tagRaiz"]?.Value, tagRaiz, StringComparison.Ordinal) &&
                string.Equals(x.Attributes?["versao"]?.Value ?? string.Empty, versao ?? string.Empty, StringComparison.Ordinal));
        }

        private static bool ContemElemento(XmlDocument xml, string localName)
        {
            return xml
                .SelectNodes("//*")
                .Cast<XmlNode>()
                .Any(x => string.Equals(x.LocalName, localName, StringComparison.Ordinal));
        }
    }
}
