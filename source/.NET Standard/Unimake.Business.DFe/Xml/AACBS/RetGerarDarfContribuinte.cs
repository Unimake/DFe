using System;
using System.Collections.Generic;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using System.Xml;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Resultado da geração de DARF pelo contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetGerarDarfContribuinte")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetGerarDarfContribuinteTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retGerarDarfContribuinte", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetGerarDarfContribuinte : RetornoAACBSBase
    {
        /// <summary>Preserva a ausência da coleção opcional, que o XmlSerializer pode inicializar vazia.</summary>
        /// <param name="document">XML original já validado.</param>
        public override void ReadXml(XmlDocument document)
        {
            base.ReadXml(document);
            if (document.DocumentElement["dadosInvalidos", AACBSFormat.Namespace] == null)
            {
                DadosInvalidos = null;
            }
        }

        /// <summary>
        /// Código de retorno da geração de DARF.
        /// </summary>
        [XmlElement("cdRetorno")]
        public string CdRetorno { get; set; }

        /// <summary>
        /// Dados do DARF gerado.
        /// </summary>
        [XmlElement("darf")]
        public GerarDarfContribuinteDadosDarf Darf { get; set; }

        /// <summary>
        /// Informações de pagamento fornecidas pela Receita Federal.
        /// </summary>
        [XmlElement("informacoesPagamento")]
        public GerarDarfContribuinteInformacoesPagamento InformacoesPagamento { get; set; }

        /// <summary>
        /// Mensagem de erro da geração de DARF.
        /// </summary>
        [XmlElement("mensagem")]
        public string Mensagem { get; set; }

        /// <summary>
        /// Campos inválidos informados na resposta.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("dadosInvalidos")]
        [XmlArrayItem("item", IsNullable = true)]
        public List<string> DadosInvalidos { get; set; }

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção DadosInvalidos.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddDadosInvalidos(string item)
        {
            if (item == null)
            {
                throw new ArgumentNullException(nameof(item));
            }

            if (DadosInvalidos == null)
            {
                DadosInvalidos = new List<string>();
            }

            DadosInvalidos.Add(item);
        }

        /// <summary>
        /// Obtém um item da coleção DadosInvalidos.
        /// </summary>
        /// <param name="index">Índice do item, começando em zero.</param>
        public string GetDadosInvalidos(int index) => DadosInvalidos?[index];

        /// <summary>
        /// Obtém a quantidade de itens da coleção DadosInvalidos.
        /// </summary>
        public int GetDadosInvalidosCount() => DadosInvalidos?.Count ?? 0;
#endif
    }
}
