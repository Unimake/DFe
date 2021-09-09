namespace Unimake.Business.DFe
{
    /// <summary>
    /// Configurações gerais
    /// </summary>
    public static class Configuration
    {
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
    }
}