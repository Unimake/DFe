using System;
using System.Collections.Generic;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Solicitação de geração de DARF pelo adquirente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.GerarDarfAdquirente")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "GerarDarfAdquirenteTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("gerarDarfAdquirente", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class GerarDarfAdquirente : AACBSBase
    {
        /// <summary>
        /// CNPJ completo, com quatorze dígitos.
        /// </summary>
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// Solicitações de DARF para os documentos do adquirente.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlArray("solicitacoes")]
        [XmlArrayItem("item", IsNullable = true)]
        public List<SolicitacaoDarfAdquirente> Solicitacoes { get; set; }

#if INTEROP
        /// <summary>
        /// Adiciona um item à coleção Solicitacoes.
        /// </summary>
        /// <param name="item">Item a adicionar.</param>
        public void AddSolicitacoes(SolicitacaoDarfAdquirente item)
        {
            if (item == null)
            {
                throw new ArgumentNullException(nameof(item));
            }

            if (Solicitacoes == null)
            {
                Solicitacoes = new List<SolicitacaoDarfAdquirente>();
            }

            Solicitacoes.Add(item);
        }

        /// <summary>
        /// Obtém um item da coleção Solicitacoes.
        /// </summary>
        /// <param name="index">Índice do item, começando em zero.</param>
        public SolicitacaoDarfAdquirente GetSolicitacoes(int index) => Solicitacoes?[index];

        /// <summary>
        /// Obtém a quantidade de itens da coleção Solicitacoes.
        /// </summary>
        public int GetSolicitacoesCount() => Solicitacoes?.Count ?? 0;
#endif
    }
}
