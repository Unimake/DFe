#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2298 - Reintegração/Outros Provimentos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2298")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReintegr/v_S_01_03_00", IsNullable = false)]
    public class ESocial2298 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Reintegração/Outros Provimentos
        /// </summary>
        [XmlElement("evtReintegr")]
        public EvtReintegr EvtReintegr { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Reintegração/Outros Provimentos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtReintegr")]
    [ComVisible(true)]
#endif
    public class EvtReintegr
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2298 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2298 IdeVinculo { get; set; }

        /// <summary>
        /// Informações da reintegração
        /// </summary>
        [XmlElement("infoReintegr")]
        public InfoReintegr InfoReintegr { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2298")]
    [ComVisible(true)]
#endif
    public class IdeEvento2298 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2298")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2298 : IdeVinculo2206 { }

    /// <summary>
    /// Informações da reintegração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReintegr")]
    [ComVisible(true)]
#endif
    public class InfoReintegr
    {
        /// <summary>
        /// Tipo de reintegração/outro provimento
        /// </summary>
        [XmlElement("tpReint")]
        public TpReint TpReint { get; set; }

        /// <summary>
        /// Em caso de reintegração por determinação judicial, preencher com o número do processo
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Informar a lei de anistia, descrevendo seu número e ano de publicação
        /// </summary>
        [XmlElement("nrLeiAnistia")]
        public string NrLeiAnistia { get; set; }

        /// <summary>
        /// Informar a data do efetivo retorno ao trabalho
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEfetRetorno { get; set; }
#else
        public DateTimeOffset DtEfetRetorno { get; set; }
#endif

        [XmlElement("dtEfetRetorno")]
        public string DtEfetRetornoField
        {
            get => DtEfetRetorno.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEfetRetorno = DateTime.Parse(value);
#else
            set => DtEfetRetorno = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informar a data de início dos efeitos financeiros da reintegração
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEfeito { get; set; }
#else
        public DateTimeOffset DtEfeito { get; set; }
#endif

        [XmlElement("dtEfeito")]
        public string DtEfeitoField
        {
            get => DtEfeito.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEfeito = DateTime.Parse(value);
#else
            set => DtEfeito = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeNrProcJud() => !string.IsNullOrEmpty(NrProcJud);
     
        public bool ShouldSerializeNrLeiAnistia() => !string.IsNullOrEmpty(NrLeiAnistia);

        #endregion ShouldSerialize
    }
}
