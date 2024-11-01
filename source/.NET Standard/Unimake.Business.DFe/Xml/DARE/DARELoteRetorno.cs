#pragma warning disable CS1591

using Newtonsoft.Json;
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
        [JsonProperty("errors")]
        public Errors Errors { get; set; }

        [XmlElement("type")]
        [JsonProperty("type")]
        public string Type { get; set; }

        [XmlElement("title")]
        [JsonProperty("title")]
        public string Title { get; set; }

        [XmlElement("status")]
        [JsonProperty("status")]
        public string Status { get; set; }

        [XmlElement("traceId")]
        [JsonProperty("traceId")]
        public string TraceId { get; set; }

        #endregion Retorno Negativo

        #region Retorno Positivo

        [XmlElement("itensParaGeracao")]
        [JsonProperty("itensParaGeracao")]
        public List<ItensParaGeracaoRetorno> ItensParaGeracaoRetorno { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensParaGeracaoRetorno(ItensParaGeracaoRetorno item)
        {
            if (ItensParaGeracaoRetorno == null)
            {
                ItensParaGeracaoRetorno = new List<ItensParaGeracaoRetorno>();
            }

            ItensParaGeracaoRetorno.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensParaGeracaoRetorno (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensParaGeracaoRetorno</returns>
        public ItensParaGeracaoRetorno GetItensParaGeracaoRetorno(int index)
        {
            if ((ItensParaGeracaoRetorno?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensParaGeracaoRetorno[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensParaGeracaoRetorno
        /// </summary>
        public int GetItensParaGeracaoRetornoCount => (ItensParaGeracaoRetorno != null ? ItensParaGeracaoRetorno.Count : 0);
#endif

        [XmlElement("erro")]
        [JsonProperty("erro")]
        public ErroLoteRetorno Erro { get; set; }

        [XmlElement("tipoAgrupamentoFilhotes")]
        [JsonProperty("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        [XmlElement("zipDownload")]
        [JsonProperty("zipDownload")]
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
                    errors.Add(element.Name, element.InnerText);
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
    public class ItensParaGeracaoRetorno : DARE { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ErroLoteRetorno")]
    [ComVisible(true)]
#endif
    public class ErroLoteRetorno : Erro { }

}
