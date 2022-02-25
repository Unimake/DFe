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
    [ProgId("Unimake.Business.DFe.Xml.GNRE.TLoteConsultaGNRE")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("TLote_ConsultaGNRE", Namespace = "http://www.gnre.pe.gov.br", IsNullable = false)]
    public class TLoteConsultaGNRE : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "2.00";

        [XmlElement("consulta")]
        public List<Consulta> Consulta { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="consulta">Elemento</param>
        public void AddConsulta(Consulta consulta)
        {
            if (Consulta == null)
            {
                Consulta = new List<Consulta>();
            }

            Consulta.Add(consulta);
        }

        /// <summary>
        /// Retorna o elemento da lista Consulta (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Consulta</returns>
        public Consulta GetConsulta(int index)
        {
            if ((Consulta?.Count ?? 0) == 0)
            {
                return default;
            };

            return Consulta[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Consulta
        /// </summary>
        public int GetConsultaCount => (Consulta != null ? Consulta.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.Consulta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class Consulta
    {
        [XmlElement("uf")]
        public UFBrasil Uf { get; set; }

        [XmlElement("emitenteId")]
        public EmitenteId EmitenteId { get; set; }

        [XmlElement("codBarras")]
        public string CodBarras { get; set; }

        [XmlElement("numControle")]
        public string NumControle { get; set; }

        [XmlElement("docOrigem")]
        public DocOrigem DocOrigem { get; set; }

        [XmlElement("idConsulta")]
        public string IdConsulta { get; set; }

        [XmlElement("tipoConsulta")]
        public TipoConsultaGNRE TipoConsulta { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodBarras() => !string.IsNullOrWhiteSpace(CodBarras);
        public bool ShouldSerializeNumControle() => !string.IsNullOrWhiteSpace(NumControle);
        public bool ShouldSerializeIdConsulta() => !string.IsNullOrWhiteSpace(IdConsulta);

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.EmitenteId")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class EmitenteId
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.GNRE.DocOrigem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.gnre.pe.gov.br")]
    public class DocOrigem
    {
        [XmlAttribute("tipo")]
        public string Tipo { get; set; }

        [XmlText()]
        public string Value { get; set; }

    }
}