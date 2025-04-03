#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1270 - Contratação de Trabalhadores Avulsos Não Portuários
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1270")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContratAvNP/v_S_01_03_00", IsNullable = false)]
    public class ESocial1270 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Contratação de Trabalhadores Avulsos Não Portuários
        /// </summary>
        [XmlElement("evtContratAvNP")]
        public EvtContratAvNP EvtContratAvNP { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Contratação de Trabalhadores Avulsos Não Portuários
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtContratAvNP")]
    [ComVisible(true)]
#endif
    public class EvtContratAvNP
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
        public IdeEvento1270 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Remuneração dos trabalhadores avulsos não portuários
        /// </summary>
        [XmlElement("remunAvNP")]
        public RemunAvNP RemunAvNP { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1270")]
    [ComVisible(true)]
#endif
    public class IdeEvento1270 : IdeEvento1210 { }

    /// <summary>
    /// Remuneração dos trabalhadores avulsos não portuários
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAvNP")]
    [ComVisible(true)]
#endif
    public class RemunAvNP
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento do contribuinte de acordo com o tipo de inscrição indicado no campo remunAvNP/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária
        /// </summary>
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração dos trabalhadores avulsos não portuários.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp00")]
        public string VrBcCp00Field
        {
            get => VrBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 15 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp15")]
        public string VrBcCp15Field
        {
            get => VrBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 20 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp20")]
        public string VrBcCp20Field
        {
            get => VrBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 25 anos de contribuição.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp25")]
        public string VrBcCp25Field
        {
            get => VrBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp25 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária sobre o 13°
        /// salário dos trabalhadores avulsos não portuários contratados.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp13 { get; set; }

        [XmlElement("vrBcCp13")]
        public string VrBcCp13Field
        {
            get => VrBcCp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo do FGTS sobre a remuneração
        /// dos trabalhadores avulsos não portuários contratados.
        /// </summary>
        [XmlIgnore]
        public double VrBcFgts { get; set; }

        [XmlElement("vrBcFgts")]
        public string VrBcFgtsFIeld
        {
            get => VrBcFgts.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcFgts = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com o valor total da contribuição descontada
        /// dos trabalhadores avulsos não portuários.
        /// </summary>
        [XmlIgnore]
        public double VrDescCP { get; set; }

        [XmlElement("vrDescCP")]
        public string VrDescCPField
        {
            get => VrDescCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescCP = Converter.ToDouble(value);
        }
    }
}
