#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-3500 - Evento Informações do Empregador. eSocial - 3500
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial3500")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExcProcTrab/v_S_01_03_00", IsNullable = false)]
    public class ESocial3500 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Exclusão de Eventos - Processo Trabalhista
        /// </summary>
        [XmlElement("evtExcProcTrab")]
        public EvtExcProcTrab EvtExcProcTrab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

    /// <summary>
    /// Evento Exclusão de Eventos - Processo Trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtExcProcTrab")]
    [ComVisible(true)]
#endif
    public class EvtExcProcTrab
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
        public IdeEvento3500 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Grupo que identifica o evento objeto da exclusão
        /// </summary>
        [XmlElement("infoExclusao")]
        public InfoExclusao3500 InfoExclusao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento3500")]
    [ComVisible(true)]
#endif
    public class IdeEvento3500 : IdeEvento { }

    /// <summary>
    /// Informação do evento que será excluído
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoExclusao3500")]
    [ComVisible(true)]
#endif
    public class InfoExclusao3500
    {
        /// <summary>
        /// Preencher com o tipo de evento (S-2500 ou S-2501).
        /// Validação:
        /// Deve ser igual a[S - 2500, S - 2501].
        /// </summary>
        [XmlElement("tpEvento")]
        public string TpEvento { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do evento que seráexcluído.
        /// Validação:
        /// O recibo deve ser relativo ao mesmo tipo deevento indicado em
        /// tpEvento
        /// </summary>
        [XmlElement("nrRecEvt")]
        public string NrRecEvt { get; set; }

        /// <summary>
        /// Identificação do processo, do trabalhador e do período a que se refere o evento que será excluído
        /// </summary>
        [XmlElement("ideProcTrab")]
        public IdeProcTrab IdeProcTrab { get; set; }
    }

    /// <summary>
    /// Identificação do processo, do trabalhador e do período aque se refere o evento que será excluído.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcTrab")]
    [ComVisible(true)]
#endif
    public class IdeProcTrab
    {
        /// <summary>
        /// Número do processo trabalhista, da ata ou número deidentificação da conciliação.
        /// Validação:
        /// Deve ser o mesmo número do processoinformado no evento objeto da exclusão.
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// Validação:
        /// Preenchimento obrigatório e exclusivo se
        /// tpEvento
        /// = [S - 2500].Deve ser o mesmo CPF informadono evento S-2500 objeto da exclusão.
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Mês/ano em que é devida a obrigação de pagar a parcelaprevista no acordo/sentença.
        /// Validação:
        /// Preenchimento obrigatório e exclusivo se
        /// tpEvento
        /// = [S - 2501].Deve ser o mesmo períodoinformado no evento S-2501 objeto da exclusão.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApurPgto { get; set; }
#else
        public DateTimeOffset PerApurPgto { get; set; }
#endif

        [XmlElement("perApurPgto")]
        public string PerApurPgtoField
        {
            get => PerApurPgto.ToString("yyyy-MM");
#if INTEROP
            set => PerApurPgto = DateTime.Parse(value);
#else
            set => PerApurPgto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número sequencial atribuído pela empresa a cada conjunto de dados de tributos decorrentes de processo trabalhista, quando for necessário enviar o mesmo processo em múltiplos S-2501, para o mesmo { perApurPgto }.
        /// </summary>
        [XmlElement("ideSeqProc")]
        public string IdeSeqProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCpfTrab() => !string.IsNullOrEmpty(CpfTrab);

        public bool ShouldSerializePerApurPgtoField() => PerApurPgto > DateTime.MinValue;

        public bool ShouldSerializeIdeSeqProc() => !string.IsNullOrEmpty(IdeSeqProc);

        #endregion ShouldSerialize
    }
}