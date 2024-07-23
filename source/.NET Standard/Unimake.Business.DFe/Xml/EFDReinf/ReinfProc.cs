#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("reinfProc", IsNullable = false)]
    public class ReinfProc : XMLBase
    {
        [XmlIgnore]
        public string ID { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02")]
        public Reinf1000 Reinf1000 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt1050TabLig/v2_01_02")]
        public Reinf1050 Reinf1050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02")]
        public Reinf1070 Reinf1070 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTomadorServicos/v2_01_02")]
        public Reinf2010 Reinf2010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtPrestadorServicos/v2_01_02")]
        public Reinf2020 Reinf2020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRecebidoAssociacao/v2_01_02")]
        public Reinf2030 Reinf2030 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRepassadoAssociacao/v2_01_02")]
        public Reinf2040 Reinf2040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoProdRural/v2_01_02")]
        public Reinf2050 Reinf2050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt2055AquisicaoProdRural/v2_01_02")]
        public Reinf2055 Reinf2055 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoCPRB/v2_01_02")]
        public Reinf2060 Reinf2060 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtReabreEvPer/v2_01_02")]
        public Reinf2098 Reinf2098 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtFechamento/v2_01_02")]
        public Reinf2099 Reinf2099 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtEspDesportivo/v2_01_02")]
        public Reinf3010 Reinf3010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4010PagtoBeneficiarioPF/v2_01_02")]
        public Reinf4010 Reinf4010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4020PagtoBeneficiarioPJ/v2_01_02")]
        public Reinf4020 Reinf4020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4040PagtoBenefNaoIdentificado/v2_01_02")]
        public Reinf4040 Reinf4040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4080RetencaoRecebimento/v2_01_02")]
        public Reinf4080 Reinf4080 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4099FechamentoDirf/v2_01_02")]
        public Reinf4099 Reinf4099 { get; set; }

        [XmlElement("retornoEvento")]
        public RetornoEvento RetornoEvento { get; set; }

        /// <summary>
        /// Nome do arquivo XML de distribuição do evento
        /// </summary>
        public string NomeArquivoDistribuicao { get => ID + "-reinfproc.xml"; }

    }

}
