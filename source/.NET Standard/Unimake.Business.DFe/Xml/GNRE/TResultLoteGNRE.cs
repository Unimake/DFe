#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.GNRE
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TResultLoteGNRE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("TResultLote_GNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TResultLoteGNRE : XMLBase
    {
        [XmlElement("ambiente")]
        public TipoAmbiente Ambiente { get; set; }

        [XmlElement("numeroRecibo")]
        public string NumeroRecibo { get; set; }

        [XmlElement("situacaoProcess")]
        public SituacaoProcess SituacaoProcess { get; set; }

        [XmlElement("resultado")]
        public Resultado Resultado { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.SituacaoProcess")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class SituacaoProcess
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Resultado")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Resultado
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "2.00";

        [XmlElement("guia")]
        public List<Guia> Guia { get; set; }

        /// <summary>
        /// PDF com as Guias processadas com sucesso, no formato "String base64".
        /// </summary>
        [XmlElement("pdfGuias")]
        public string PDFGuias { get; set; }

        /// <summary>
        /// Arquivo de pagamento das Guias do Lote
        /// </summary>
        [XmlElement("arquivoPagamento")]
        public string ArquivoPagamento { get; set; }

        /// <summary>
        /// Campo pai das notícias vigentes publicadas no Portal GNRE.
        /// </summary>
        [XmlElement("noticias")]
        public Noticias Noticias { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="guia">Elemento</param>
        public void AddGuia(Guia guia)
        {
            if(Guia == null)
            {
                Guia = new List<Guia>();
            }

            Guia.Add(guia);
        }

        /// <summary>
        /// Retorna o elemento da lista Guia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Guia</returns>
        public Guia GetGuia(int index)
        {
            if ((Guia?.Count ?? 0) == 0)
            {
                return default;
            };

            return Guia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Guia
        /// </summary>
        public int GetGuiaCount => (Guia != null ? Guia.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Guia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Guia
    {
        [XmlElement("situacaoGuia")]
        public SituacaoGuiaGNRE SituacaoGuia { get; set; }

        [XmlElement("TDadosGNRE")]
        public TDadosGNRE TDadosGNRE { get; set; }

        [XmlElement("representacaoNumerica")]
        public string RepresentacaoNumerica { get; set; }

        [XmlElement("codigoBarras")]
        public string CodigoBarras { get; set; }

        [XmlElement("linhaDigitavel")]
        public string LinhaDigitavel { get; set; }

        [XmlElement("motivosRejeicao")]
        public MotivosRejeicao MotivosRejeicao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.MotivosRejeicao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class MotivosRejeicao
    {
        [XmlElement("motivo")]
        public List<Motivo> Motivo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="motivo">Elemento</param>
        public void AddMotivo(Motivo motivo)
        {
            if(Motivo == null)
            {
                Motivo = new List<Motivo>();
            }

            Motivo.Add(motivo);
        }

        /// <summary>
        /// Retorna o elemento da lista Motivo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Motivo</returns>
        public Motivo GetMotivo(int index)
        {
            if ((Motivo?.Count ?? 0) == 0)
            {
                return default;
            };

            return Motivo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Motivo
        /// </summary>
        public int GetMotivoCount => (Motivo != null ? Motivo.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Motivo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Motivo
    {
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }

        [XmlElement("campo")]
        public string Campo { get; set; }
    }

    /// <summary>
    /// Notícias vigentes publicadas no Portal GNRE.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Noticias")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Noticias
    {
        /// <summary>
        /// Notícia vigente publicada no Portal GNRE
        /// </summary>
        [XmlElement("noticia")]
        public List<Noticia> Noticia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNoticia(Noticia item)
        {
            if(Noticia == null)
            {
                Noticia = new List<Noticia>();
            }

            Noticia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Noticia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Noticia</returns>
        public Noticia GetNoticia(int index)
        {
            if ((Noticia?.Count ?? 0) == 0)
            {
                return default;
            };

            return Noticia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Noticia
        /// </summary>
        public int GetNoticiaCount => (Noticia != null ? Noticia.Count : 0);

#endif
    }

    /// <summary>
    /// Notícia vigente publicada no Portal GNRE.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Noticia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Noticia
    {
        /// <summary>
        /// Data/hora de publicação da notícia.
        /// </summary>
        [XmlIgnore]
        public DateTime DhPublicacao { get; set; }

        /// <summary>
        /// Data/hora de publicação da notícia. - Utilize a propriedade dhPublicacao
        /// </summary>
        [XmlElement("dhPublicacao")]
        public string DhPublicacaoField
        {
            get => DhPublicacao.ToString("yyyy-MM-dd HH:mm:ss");
            set => DhPublicacao = DateTime.Parse(value);
        }

        /// <summary>
        /// Título da notícia
        /// </summary>
        [XmlElement("titulo")]
        public string Titulo { get; set; }

        /// <summary>
        /// Texto da notícia
        /// </summary>
        [XmlElement("texto")]
        public string Texto { get; set; }
    }
}