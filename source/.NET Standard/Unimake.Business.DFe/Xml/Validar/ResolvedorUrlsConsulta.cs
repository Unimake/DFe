using System;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Resolve as URLs de consulta declaradas no catálogo de validação, inclusive referências e configurações de serviços.
    /// </summary>
    internal static class ResolvedorUrlsConsulta
    {
        /// <summary>
        /// Prepara as URLs antes da assinatura e validação, preservando a precedência declarada para cada origem.
        /// </summary>
        internal static void Atribuir(XmlDocument xml, XmlNode servico, Configuracao configuracao)
        {
            var grupoUrl = servico?.SelectSingleNode("GrupoUrl");
            if (grupoUrl == null)
            {
                return;
            }

            // Alguns documentos recriam o suplemento após a normalização e ainda precisam preparar as URLs.
            var tagSuplementar = grupoUrl.Attributes["tagSuplementar"]?.Value;
            var suplementoExistente = !string.IsNullOrWhiteSpace(tagSuplementar) && xml?.GetElementsByTagName(tagSuplementar).Count > 0;
            if (suplementoExistente
                && !string.Equals(grupoUrl.Attributes["prepararComSuplemento"]?.Value, "true", StringComparison.OrdinalIgnoreCase))
            {
                return;
            }

            // A referência da NF-e preserva a condição histórica, inclusive quando o ambiente é produção.
            if (string.Equals(grupoUrl.Attributes["somenteSeUrlQrCodeHomologacaoAusente"]?.Value, "true", StringComparison.OrdinalIgnoreCase)
                && !string.IsNullOrWhiteSpace(configuracao.UrlQrCodeHomologacao))
            {
                return;
            }

            grupoUrl = ResolverReferencia(servico, grupoUrl);
            if (!string.IsNullOrWhiteSpace(grupoUrl.Attributes["servicoConfiguracao"]?.Value))
            {
                CompletarPeloServico(servico, grupoUrl, configuracao);
                return;
            }

            var codigoUF = ((UFBrasil)configuracao.CodigoUF).ToString();
            foreach (XmlNode grupoUF in grupoUrl.SelectNodes("Grupo"))
            {
                foreach (XmlNode uf in grupoUF.SelectNodes("UF"))
                {
                    if (!uf.InnerText.Equals(codigoUF, StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }

                    var urls = grupoUF.SelectSingleNode("Urls");
                    if (urls != null)
                    {
                        configuracao.UrlChaveHomologacao = urls["UrlChaveHomologacao"]?.InnerText;
                        configuracao.UrlChaveProducao = urls["UrlChaveProducao"]?.InnerText;
                        configuracao.UrlQrCodeHomologacao = urls["UrlQrCodeHomologacao"]?.InnerText;
                        configuracao.UrlQrCodeProducao = urls["UrlQrCodeProducao"]?.InnerText;
                    }

                    return;
                }
            }
        }

        private static XmlNode ResolverReferencia(XmlNode servico, XmlNode grupoUrl)
        {
            var referenciaDFe = grupoUrl.Attributes["referenciaDFe"]?.Value;
            if (string.IsNullOrWhiteSpace(referenciaDFe))
            {
                return grupoUrl;
            }

            var tagRaiz = servico.Attributes["tagRaiz"]?.Value;
            var versao = servico.Attributes["versao"]?.Value;
            var origem = servico.OwnerDocument.SelectSingleNode("ServicosValidacao/" + referenciaDFe
                + "/Servico[@tagRaiz='" + tagRaiz + "' and @versao='" + versao + "']/GrupoUrl");
            if (origem == null)
            {
                throw new InvalidOperationException("Não foi possível resolver as URLs de consulta de '" + tagRaiz
                    + "', versão '" + versao + "', na referência '" + referenciaDFe + "' do catálogo de validação.");
            }

            return origem;
        }

        private static void CompletarPeloServico(XmlNode servico, XmlNode grupoUrl, Configuracao configuracao)
        {
            var urlAtual = configuracao.TipoAmbiente == TipoAmbiente.Homologacao
                ? configuracao.UrlQrCodeHomologacao : configuracao.UrlQrCodeProducao;
            if (!string.IsNullOrWhiteSpace(urlAtual))
            {
                return;
            }

            var origem = new Configuracao
            {
                TipoDFe = configuracao.TipoDFe,
                CodigoUF = configuracao.CodigoUF,
                TipoAmbiente = configuracao.TipoAmbiente,
                TipoEmissao = configuracao.TipoEmissao,
                SchemaVersao = servico.Attributes["versao"]?.Value,
                BuscarConfiguracaoPastaBase = configuracao.BuscarConfiguracaoPastaBase,
                PastaArquivoConfiguracao = configuracao.PastaArquivoConfiguracao
            };
            origem.Load(grupoUrl.Attributes["servicoConfiguracao"].Value);
            if (configuracao.TipoAmbiente == TipoAmbiente.Homologacao)
            {
                configuracao.UrlQrCodeHomologacao = origem.UrlQrCodeHomologacao;
            }
            else
            {
                configuracao.UrlQrCodeProducao = origem.UrlQrCodeProducao;
            }
        }
    }
}
