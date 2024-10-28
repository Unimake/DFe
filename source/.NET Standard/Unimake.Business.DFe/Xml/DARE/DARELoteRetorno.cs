#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{
    /// <summary>
    /// Retorno DARE Lote (negativo e positivo)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARELoteRetorno")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("DareLoteRetorno", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARELoteRetorno : XMLBase
    {
        #region Retorno negativo

        [XmlElement("errors")]
        public Errors Errors { get; set; }

        [XmlElement("type")]
        public string Type { get; set; }

        [XmlElement("title")]
        public string Title { get; set; }

        [XmlElement("status")]
        public string Status { get; set; }

        [XmlElement("traceId")]
        public string TraceId { get; set; }

        #endregion Retorno Negativo

        #region Retorno Positivo

        [XmlElement("itensParaGeracao")]
        public ItensParaGeracaoRetorno ItensParaGeracaoRetorno { get; set; }

        [XmlElement("erro")]
        public ErroLoteRetorno Erro { get; set; }

        [XmlElement("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        [XmlElement("zipDownload")]
        public string ZipDownload { get; set; }

        #endregion Retorno Positivo
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Errors")]
    [ComVisible(true)]
#endif
    public class Errors
    {
        [XmlAnyElement]
        public List<XmlElement> ErrorElements { get; set; } = new List<XmlElement>();

        [XmlIgnore]
        public Dictionary<string, string> ErrorMessages
        {
            get
            {
                var errors = new Dictionary<string, string>();
                foreach (var element in ErrorElements)
                {
                    var messageElement = element["message"];
                    if (messageElement != null)
                    {
                        errors[element.Name] = messageElement.InnerText;
                    }
                }
                return errors;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensParaGeracaoRetorno")]
    [ComVisible(true)]
#endif
    public class ItensParaGeracaoRetorno
    {
        [XmlElement("DARE")]
        public List<ItemDARELoteRetorno> DARE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDARE(ItemDARELoteRetorno item)
        {
            if (DARE == null)
            {
                DARE = new List<ItemDARELoteRetorno>();
            }

            DARE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DARE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DARE</returns>
        public ItemDARELoteRetorno GetDARE(int index)
        {
            if ((DARE?.Count ?? 0) == 0)
            {
                return default;
            };

            return DARE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DARE
        /// </summary>
        public int GetDARECount => (DARE != null ? DARE.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ErroLoteRetorno")]
    [ComVisible(true)]
#endif
    public class ErroLoteRetorno : Erro { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItemDARELoteRetorno")]
    [ComVisible(true)]
#endif
    public class ItemDARELoteRetorno : DARE { }
}
