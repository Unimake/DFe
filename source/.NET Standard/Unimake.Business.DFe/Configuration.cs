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

        private static readonly Lazy<ConfiguracoesNFSeCache> ConfiguracoesNFSe =
            new Lazy<ConfiguracoesNFSeCache>(CarregarConfiguracoesNFSe);

        private static readonly Lazy<Dictionary<int, PadraoNFSe>> PadraoNFSePorMunicipio =
            new Lazy<Dictionary<int, PadraoNFSe>>(CriarPadraoNFSePorMunicipio);

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
        /// Carrega os municípios e padrões de NFSe definidos no arquivo de configuração geral.
        /// </summary>
        /// <returns>Lista com os municípios configurados para NFSe e suas respectivas informações</returns>
        public static List<MunicipioNFSeConfiguracao> CarregarMunicipio()
        {
            return new List<MunicipioNFSeConfiguracao>(ConfiguracoesNFSe.Value.Municipios);
        }

        /// <summary>
        /// Carrega os estados definidos no arquivo de configuração geral.
        /// </summary>
        /// <returns>Lista com os estados configurados</returns>
        public static List<MunicipioNFSeConfiguracao> CarregarEstados()
        {
            return new List<MunicipioNFSeConfiguracao>(ConfiguracoesNFSe.Value.Estados);
        }

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

        private static Dictionary<int, PadraoNFSe> CriarPadraoNFSePorMunicipio()
        {
            var retorno = new Dictionary<int, PadraoNFSe>();
            var municipios = ConfiguracoesNFSe.Value.Municipios;

            foreach (var municipio in municipios)
            {
                retorno[municipio.CodigoMunicipio] = municipio.PadraoNFSe;
            }

            return retorno;
        }

        private static ConfiguracoesNFSeCache CarregarConfiguracoesNFSe()
        {
            var retorno = new ConfiguracoesNFSeCache();
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

                if (!int.TryParse(id, out var codigoConfig))
                {
                    continue;
                }

                var nome = string.Empty;
                var uf = string.Empty;
                var arqConfig = string.Empty;

                var nomeNode = elementArquivo.GetElementsByTagName("Nome");
                if (nomeNode.Count > 0)
                {
                    nome = nomeNode[0].InnerText;
                }

                var ufNode = elementArquivo.GetElementsByTagName("UF");
                if (ufNode.Count > 0)
                {
                    uf = ufNode[0].InnerText;
                }

                var arqConfigNode = elementArquivo.GetElementsByTagName("ArqConfig");
                if (arqConfigNode.Count > 0)
                {
                    arqConfig = arqConfigNode[0].InnerText;
                }

                if (id.Length <= 2)
                {
                    if (uf == "SVRS" || uf == "AN")
                    {
                        continue;
                    }

                    retorno.Estados.Add(new MunicipioNFSeConfiguracao(codigoConfig, uf, nome, arqConfig, PadraoNFSe.None));
                    continue;
                }

                var padraoNode = elementArquivo.GetElementsByTagName("PadraoNFSe");
                var padraoNFSe = PadraoNFSe.None;

                if (padraoNode.Count > 0)
                {
                    var padraoTag = padraoNode[0].InnerText;

                    if (Enum.TryParse(padraoTag, true, out padraoNFSe))
                    {
                        retorno.Municipios.Add(new MunicipioNFSeConfiguracao(codigoConfig, uf, nome, arqConfig, padraoNFSe));
                        continue;
                    }

                    throw new Exception("Caro desenvolvedor, você esqueceu de definir no enumerador \"PadraoNFSe\" o tipo " + padraoTag + " e eu não tenho como resolver esta encrenca. Por favor, va lá e defina.");
                }

                retorno.Municipios.Add(new MunicipioNFSeConfiguracao(codigoConfig, uf, nome, arqConfig, padraoNFSe));
            }

            return retorno;
        }

        #endregion Private Methods

        #region Private Types

        private sealed class ConfiguracoesNFSeCache
        {
            public List<MunicipioNFSeConfiguracao> Municipios { get; } = new List<MunicipioNFSeConfiguracao>();

            public List<MunicipioNFSeConfiguracao> Estados { get; } = new List<MunicipioNFSeConfiguracao>();
        }

        #endregion Private Types
    }

    /// <summary>
    /// Representa os dados de configuração de NFSe de um município ou padrão de configuração especial.
    /// </summary>
    public class MunicipioNFSeConfiguracao
    {
        /// <summary>
        /// Inicializa uma nova instância com os dados de configuração de NFSe.
        /// </summary>
        /// <param name="codigoMunicipio">Código do município ou código especial de configuração</param>
        /// <param name="uf">Sigla da unidade federativa associada</param>
        /// <param name="nome">Nome do município ou da configuração</param>
        /// <param name="arquivoConfiguracao">Nome do arquivo de configuração específico</param>
        /// <param name="padraoNFSe">Padrão de NFSe utilizado</param>
        public MunicipioNFSeConfiguracao(int codigoMunicipio, string uf, string nome, string arquivoConfiguracao, PadraoNFSe padraoNFSe)
        {
            CodigoMunicipio = codigoMunicipio;
            UF = uf;
            Nome = nome;
            ArquivoConfiguracao = arquivoConfiguracao;
            PadraoNFSe = padraoNFSe;
        }

        /// <summary>
        /// Código do município ou código especial de configuração.
        /// </summary>
        public int CodigoMunicipio { get; }

        /// <summary>
        /// Sigla da unidade federativa associada ao município ou padrão.
        /// </summary>
        public string UF { get; }

        /// <summary>
        /// Nome do município ou do padrão especial.
        /// </summary>
        public string Nome { get; }

        /// <summary>
        /// Nome do arquivo de configuração específico associado ao município.
        /// </summary>
        public string ArquivoConfiguracao { get; }

        /// <summary>
        /// Padrão de NFSe utilizado pelo município.
        /// </summary>
        public PadraoNFSe PadraoNFSe { get; }
    }
}