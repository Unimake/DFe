#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.GNRE;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Receitas")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Classe para serialização e deserialização para Receitas - DARE SP
    /// </summary>
    [Serializable()]
    [XmlRoot("Receitas")]
    public class Receitas : XMLBase
    {
        [XmlElement("Receita")]        
        public List<ReceitaDARE>  Receita { get; set; }
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ReceitaDARE")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Classe de retorno do serviço de Receitas DARE - SP
    /// </summary>
    public class ReceitaDARE
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("codigoServicoDARE")]
        public string CodigoServicoDARE { get; set; }

        [XmlElement("escopoUso")]
        public string EscopoUso { get; set; }

        [XmlElement("nome")]
        public string Nome { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodigo() => !string.IsNullOrEmpty(Codigo);
        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);
        #endregion ShouldSerialize
    }
}
