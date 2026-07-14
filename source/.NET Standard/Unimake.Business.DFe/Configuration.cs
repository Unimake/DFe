using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Configurações gerais
    /// </summary>
    public static class Configuration
    {
        #region Private Fields

        private static readonly Lazy<Dictionary<int, PadraoNFSe>> PadraoNFSePorMunicipio =
            new Lazy<Dictionary<int, PadraoNFSe>>(CarregarPadraoNFSePorMunicipio);

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Arquivo de configurações gerais (Namespace + Nome do Arquivo)
        /// </summary>
        public static string ArquivoConfigGeral => NamespaceConfig + ArquivoConfigPadrao;
        /// <summary>
        /// Nome do arquivo de configuração padrão
        /// </summary>
        public static string ArquivoConfigPadrao => "Config.xml";
        /// <summary>
        /// Namespace da localização das configurações dos serviços
        /// </summary>
        public static string NamespaceConfig => "Unimake.Business.DFe.Servicos.Config.";
        /// <summary>
        /// Namespace da localização dos schemas para validação dos XML
        /// </summary>
        public static string NamespaceSchema => "Unimake.Business.DFe.Xml.Schemas.";

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Retorna o padrão da NFSe com base no código do município no arquivo de configuração geral.
        /// </summary>
        /// <param name="codigoMunicipio">Código IBGE do município (7 dígitos)</param>
        /// <returns>Enumerador do padrão da NFSe encontrado para o município</returns>
        public static PadraoNFSe GetPadraoNFSe(int codigoMunicipio)
        {
            if (codigoMunicipio < 1000000 || codigoMunicipio > 9999999)
            {
                return PadraoNFSe.None;
            }

            if (PadraoNFSePorMunicipio.Value.TryGetValue(codigoMunicipio, out var padraoNFSe))
            {
                return padraoNFSe;
            }

            return PadraoNFSe.None;
        }

        #endregion Public Methods

        #region Private Methods

        private static Dictionary<int, PadraoNFSe> CarregarPadraoNFSePorMunicipio()
        {
            var retorno = new Dictionary<int, PadraoNFSe>();
            var document = new XmlDocument();

            using (var stream = typeof(Configuration).Assembly.GetManifestResourceStream(ArquivoConfigGeral))
            {
                if (stream == null)
                {
                    return retorno;
                }

                document.Load(stream);
            }

            var listArquivos = document.GetElementsByTagName("Arquivo");

            foreach (XmlNode nodeArquivo in listArquivos)
            {
                var elementArquivo = (XmlElement)nodeArquivo;
                var id = elementArquivo.GetAttribute("ID");

                if (id.Length != 7 || !int.TryParse(id, out var codigoMunicipio))
                {
                    continue;
                }

                var padraoNode = elementArquivo.GetElementsByTagName("PadraoNFSe");
                if (padraoNode.Count == 0)
                {
                    continue;
                }

                var padraoTag = padraoNode[0].InnerText;

                if (Enum.TryParse(padraoTag, true, out PadraoNFSe padraoNFSe))
                {
                    retorno[codigoMunicipio] = padraoNFSe;
                    continue;
                }

                throw new Exception("Caro desenvolvedor, você esqueceu de definir no enumerador \"PadraoNFSe\" o tipo " + padraoTag + " e eu não tenho como resolver esta encrenca. Por favor, va lá e defina.");
            }

            return retorno;
        }

        #endregion Private Methods
    }
}