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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1270")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContratAvNP/v_S_01_02_00", IsNullable = false)]
    public class ESocial1270 : XMLBase
    {
        [XmlElement("evtContratAvNP")]
        public EvtContratAvNP EvtContratAvNP { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtContratAvNP")]
    [ComVisible(true)]
#endif
    public class EvtContratAvNP
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento1270 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("remunAvNP")]
        public RemunAvNP RemunAvNP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1270")]
    [ComVisible(true)]
#endif
    public class IdeEvento1270 : IdeEvento1210 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAvNP")]
    [ComVisible(true)]
#endif
    public class RemunAvNP
    {
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

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
