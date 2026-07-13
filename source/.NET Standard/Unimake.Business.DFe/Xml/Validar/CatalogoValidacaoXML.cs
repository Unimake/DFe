using System;
using System.Xml;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Disponibiliza o catálogo embutido com as regras de validação dos documentos fiscais.
    /// </summary>
    internal static class CatalogoValidacaoXML
    {
        private const string NomeRecurso = "Unimake.Business.DFe.Xml.Validar.ValidarConfig.xml";

        private static readonly Lazy<XmlDocument> Catalogo =
            new Lazy<XmlDocument>(Carregar, true);

        /// <summary>
        /// Cria uma cópia do catálogo para impedir que uma validação altere o estado compartilhado.
        /// </summary>
        /// <returns>Cópia independente do catálogo de validação.</returns>
        internal static XmlDocument CriarCopia()
        {
            return (XmlDocument)Catalogo.Value.CloneNode(true);
        }

        private static XmlDocument Carregar()
        {
            var assembly = typeof(CatalogoValidacaoXML).Assembly;

            using (var stream = assembly.GetManifestResourceStream(NomeRecurso))
            {
                if (stream == null)
                {
                    throw new InvalidOperationException($"Recurso não encontrado: {NomeRecurso}");
                }

                var xmlConfig = new XmlDocument
                {
                    XmlResolver = null
                };
                xmlConfig.Load(stream);
                return xmlConfig;
            }
        }
    }
}
