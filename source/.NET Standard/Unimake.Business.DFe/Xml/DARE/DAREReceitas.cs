#pragma warning disable CS1591

using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{
    /// <summary>
    /// Classe para serialização e deserialização para Receitas - DARE SP
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Receitas")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Receitas")]
    public class Receitas : XMLBase
    {

        [XmlElement("consulta")]
        public string Consulta { get; set; }

        [JsonIgnore]
        [XmlElement("Receita")]
        public List<ReceitaDARE> Receita { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReceita(ReceitaDARE item)
        {
            if (Receita == null)
            {
                Receita = new List<ReceitaDARE>();
            }

            Receita.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ReceitaDARE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ReceitaDARE</returns>
        public ReceitaDARE GetReceita(int index)
        {
            if ((Receita?.Count ?? 0) == 0)
            {
                return default;
            };

            return Receita[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ReceitaDARE
        /// </summary>
        public int GetReceitaCount => (Receita != null ? Receita.Count : 0);
#endif
    }

    /// <summary>
    /// Classe de retorno do serviço de Receitas DARE - SP
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ReceitaDARE")]
    [ComVisible(true)]
#endif
    public class ReceitaDARE
    {
        [XmlElement("codigo")]
        [JsonProperty("codigo")]
        public string Codigo { get; set; }

        [XmlElement("codigoServicoDARE")]
        [JsonProperty("codigoServicoDARE")]
        public string CodigoServicoDARE { get; set; }

        [XmlElement("escopoUso")]
        [JsonProperty("escopoUso")]
        public string EscopoUso { get; set; }

        [XmlElement("nome")]
        [JsonProperty("nome")]
        public string Nome { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodigo() => !string.IsNullOrEmpty(Codigo);
        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);
        public bool ShouldSerializeEscopoUso() => !string.IsNullOrEmpty(EscopoUso);

        #endregion ShouldSerialize
    }
}
