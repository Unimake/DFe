#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Arquivo final da consulta de débitos da Apuração Assistida da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetArquivoDebitos")]
    [ComVisible(true)]
#endif
    [XmlRoot("retArquivoDebitos", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    [XmlType(TypeName = "RetArquivoDebitosTipo", Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class RetArquivoDebitos : XMLBase
    {
#if INTEROP
        /// <summary>Serializa em texto XML preservando retornos de carro nos dados AACBS.</summary>
        /// <returns>Texto XML sem normalização das quebras de linha dos campos.</returns>
        public override string GerarXMLString() => Utility.XMLUtility.ObterTextoXmlAACBS(GerarXML());
#endif
        /// <summary>
        /// Versão do leiaute XML do arquivo.
        /// </summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";

        /// <summary>
        /// Tíquete da solicitação que originou o arquivo.
        /// </summary>
        [XmlElement("tiqueteSolicitacao", Order = 0)]
        public string TiqueteSolicitacao { get; set; }

        /// <summary>
        /// CNPJ base do contribuinte.
        /// </summary>
        [XmlElement("ni", Order = 1)]
        public string Ni { get; set; }

        /// <summary>
        /// Identificação do consumidor que solicitou o arquivo.
        /// </summary>
        [XmlElement("niConsumidor", Order = 2)]
        public string NiConsumidor { get; set; }

        /// <summary>
        /// Instante de geração do arquivo.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset GeradoEm
        {
            get => AACBSFormat.ParseInstant(GeradoEmField);
            set => GeradoEmField = AACBSFormat.FormatInstant(value);
        }

        /// <summary>
        /// Instante de geração do arquivo na representação textual original.
        /// </summary>
        [XmlElement("geradoEm", Order = 3)]
        public string GeradoEmField { get; set; }

        /// <summary>
        /// Grupos de apuração do arquivo.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("apuracao", Order = 4)]
        [XmlArrayItem("item", IsNullable = true)]
        public List<ApuracaoDebitos> Apuracao { get; set; } = new List<ApuracaoDebitos>();

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Apuracao.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddApuracao(ApuracaoDebitos item)
        {
            if (Apuracao == null)
            {
                Apuracao = new List<ApuracaoDebitos>();
            }

            Apuracao.Add(item);
        }

        /// <summary>
        /// Retorna um item da coleção Apuracao.
        /// </summary>
        /// <param name="index">Índice baseado em zero.</param>
        /// <returns>Item da coleção, ou nulo quando a coleção está vazia.</returns>
        public ApuracaoDebitos GetApuracao(int index) => GetApuracaoCount() == 0 ? null : Apuracao[index];

        /// <summary>
        /// Retorna a quantidade de itens da coleção Apuracao.
        /// </summary>
        public int GetApuracaoCount() => Apuracao?.Count ?? 0;
#endif
    }
}
