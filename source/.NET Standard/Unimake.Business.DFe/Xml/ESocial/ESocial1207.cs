﻿#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Benefícios - Entes Públicos
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBenPrRP/v_S_01_02_00", IsNullable = false)]
    public class ESocial1207 : XMLBase
    {
        /// <summary>
        ///  Benefícios - Entes Públicos
        /// </summary>
        [XmlElement("evtBenPrRP")]
        public EvtBenPrRP EvtBenPrRP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Remuneração de Trabalhador vinculado ao RGPS
    /// </summary>
    [Serializable()]
    public class EvtBenPrRP
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }
        /// <summary>
        /// Informações de identificação do evento.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial1207 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário.
        /// </summary>
        [XmlElement("ideBenef")]
        public IdeBenefESocial1207 IdeBenefESocial1207 { get; set; }

        /// <summary>
        /// 
        /// </summary>
        [XmlElement("dmDev")]
        public DmDevESocial1207 DmDevESocial1207 { get; set; }
    }

    #region IdeEventoESocial1207
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.IdeEventoESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
    [Serializable]
    public class IdeEventoESocial1207
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação.
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado.
        /// Validação: O preenchimento é obrigatório se indRetif = [2].
        /// Deve ser um recibo de entrega válido, correspondente ao arquivo que está sendo retificado.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Indicativo de período de apuração.
        /// </summary>
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

#if INTEROP
        public DateTime PerApur {get; set; }
#else
        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset PerApur { get; set; }
#endif

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// </summary>
        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Processo de emissão do evento.
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializePerApurField () => PerApur > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    #endregion IdeEventoESocial1207

    #region IdeBenefESocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.IdeBenefESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do beneficiário.
    /// </summary>
    public class IdeBenefESocial1207
    {
        /// <summary>
        /// Informar o CPF do beneficiário
        /// Validação: Deve ser um CPF válido.
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }
    }
    #endregion IdeBenefESocial1207

    #region DmDevEsocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao beneficiário
    /// </summary>
    [Serializable()]
    public class DmDevESocial1207
    {
        /// <summary>
        /// Identificador atribuído pelo órgão público para o
        ///demonstrativo de valores devidos ao beneficiário.O ente
        ///público pode preencher este campo utilizando-se de um
        ///identificador padrão para todos os beneficiários; no
        ///entanto, havendo mais de um demonstrativo relativo a
        ///uma mesma competência, devem ser utilizados
        ///identificadores diferentes para cada um dos
        ///demonstrativos.
        ///Validação: Deve ser um identificador único dentro do mesmo perApur para cada um dos demonstrativos do beneficiário
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Preencher com o número do benefício.
        /// </summary>
        [XmlElement("nrBeneficio")]
        public string NrBeneficio { get; set; }

        /// <summary>
        /// Informações relativas ao período de apuração
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApurESocial1207 InfoPerApurESocial1207 { get; set; }

        /// <summary>
        /// Grupo destinado às informações relativas a períodos
        /// anteriores.Somente preencher esse grupo se houver
        /// proventos ou pensões retroativos
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfPerAnt InfPerAnt { get; set; }
    }

    #region InfoPerApurESocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207.InfoPerApurESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao período de apuração
    /// </summary>
    [Serializable()]
    public class InfoPerApurESocial1207
    {
        /// <summary>
        /// Identificação da unidade do órgão público na qual o beneficiário possui provento ou pensão.
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstabESocial1207 IdeEstabESocial1207 { get; set; }
    }

    #region IdeEstabESocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207.InfoPerApurESocial1207.IdeEstabESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação da unidade do órgão público na qual o beneficiário possui provento ou pensão.
    /// </summary>
    [Serializable()]
    public class IdeEstabESocial1207
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição da unidade do órgão público.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Rubricas que compõem o provento ou pensão do beneficiário.
        /// </summary>
        [XmlElement("itensRemun")]
        public List<ItensRemunESocial1207> ItensRemun { get; set; }
    }

    #region ItensRemunESocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207.InfoPerApurESocial1207.ItensRemunESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Rubricas que compõem o provento ou pensão do beneficiário.
    /// </summary>
    [Serializable()]
    public class ItensRemunESocial1207 : ItensRemun
    {
        /// <summary>
        /// Indicativo de tipo de apuração de IR
        /// </summary>
        [XmlElement("indApurIR")]
        public IndApurIR IndApurIR { get; set; }
    }


    #endregion ItensRemunESocial1207

    #endregion IdeEstabESocial1207

    #endregion InfoPerApurESocial1207

    #region  InfPerAnt

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207.InfPerAnt")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo destinado às informações relativas a períodos
    /// anteriores.Somente preencher esse grupo se houver
    /// proventos ou pensões retroativos
    /// </summary>
    [Serializable()]
    public class InfPerAnt
    {
        /// <summary>
        /// Identificação do período ao qual se referem as diferenças de provento ou pensão.
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodoESocial1207 IdePeriodoESocial1207 { get; set; }

    }

    #region IdePeriodoESocial1207

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207.EvtBenPrRP.DmDevESocial1207.InfPerAnt.IdePeriodoESocial1207")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do período ao qual se referem as diferenças de provento ou pensão.
    /// </summary>
    [Serializable()]
    public class IdePeriodoESocial1207
    {
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        /// <summary>
        /// Informar o período ao qual se refere o complemento de
        /// provento ou pensão, no formato AAAA-MM.
        /// Validação: Deve ser igual ou anterior ao período de
        /// apuração informado em perApur.
        /// Deve ser informado no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset PerRef { get; set; }
#endif

        /// <summary>
        /// Informar o período ao qual se refere o complemento de
        /// provento ou pensão, no formato AAAA-MM.
        /// Validação: Deve ser igual ou anterior ao período de
        /// apuração informado em perApur.
        /// Deve ser informado no formato AAAA-MM
        /// </summary>
        [XmlElement("perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Identificação da unidade do órgão público na qual o beneficiário possui provento ou pensão.
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstabESocial1207 IdeEstabESocial1207 { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializePerRefField () => PerRef > DateTime.MinValue;
        #endregion ShouldSerialize
    }

    #endregion IdePeriodoESocial1207

    #endregion InfPerAnt

    #endregion DmDevEsocial1207
}