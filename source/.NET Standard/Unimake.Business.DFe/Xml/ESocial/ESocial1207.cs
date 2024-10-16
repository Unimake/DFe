#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    ///  Benefícios - Entes Públicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1207")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBenPrRP/v_S_01_02_00", IsNullable = false)]
    public class ESocial1207 : XMLBase
    {
        /// <summary>
        ///  Benefícios - Entes Públicos
        /// </summary>
        [XmlElement("evtBenPrRP")]
        public EvtBenPrRP EvtBenPrRP { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Remuneração de Trabalhador vinculado ao RGPS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtBenPrRP")]
    [ComVisible(true)]
#endif
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
        public IdeEvento1207 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário.
        /// </summary>
        [XmlElement("ideBenef")]
        public IdeBenef1207 IdeBenef { get; set; }

        /// <summary>
        /// 
        /// </summary>
        [XmlElement("dmDev")]
        public DmDev1207 DmDev { get; set; }
    }

    #region IdeEvento1207

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1207")]
    [ComVisible(true)]
#endif
    public class IdeEvento1207
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

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
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

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion ShouldSerialize
    }

    #endregion IdeEvento1207

    #region IdeBenef1207

    /// <summary>
    /// Identificação do beneficiário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBenef1207")]
    [ComVisible(true)]
#endif
    public class IdeBenef1207
    {
        /// <summary>
        /// Informar o CPF do beneficiário
        /// Validação: Deve ser um CPF válido.
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }
    }
    #endregion IdeBenef1207

    #region DmDev1207

    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao beneficiário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev1207")]
    [ComVisible(true)]
#endif
    public class DmDev1207
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

        [XmlElement("indRRA")]
        public string IndRRA { get; set; }

        [XmlElement("infoRRA")]
        public InfoRRA InfoRRA { get; set; }

        /// <summary>
        /// Informações relativas ao período de apuração
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur1207 InfoPerApur { get; set; }

        /// <summary>
        /// Grupo destinado às informações relativas a períodos
        /// anteriores.Somente preencher esse grupo se houver
        /// proventos ou pensões retroativos
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfPerAnt InfPerAnt { get; set; }
    }

    #region InfoPerApur1207

    /// <summary>
    /// Informações relativas ao período de apuração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur1207")]
    [ComVisible(true)]
#endif
    public class InfoPerApur1207
    {
        /// <summary>
        /// Identificação da unidade do órgão público na qual o beneficiário possui provento ou pensão.
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab1207 IdeEstab { get; set; }
    }

    #region IdeEstab1207

    /// <summary>
    /// Identificação da unidade do órgão público na qual o beneficiário possui provento ou pensão.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab1207")]
    [ComVisible(true)]
#endif
    public class IdeEstab1207
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
        public List<ItensRemun1207> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun(ItensRemun1207 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemun1207>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun1207 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun1207</returns>
        public ItensRemun1207 GetItensRemun(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemun1207
        /// </summary>
        public int GetItensRemunCount => (ItensRemun != null ? ItensRemun.Count : 0);
#endif
    }

    #region ItensRemun1207

    /// <summary>
    /// Rubricas que compõem o provento ou pensão do beneficiário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemun1207")]
    [ComVisible(true)]
#endif
    public class ItensRemun1207
    {

        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        [XmlElement("qtdRubr")]
        public double QtdRubr { get; set; }

        [XmlElement("fatorRubr")]
        public double FatorRubr { get; set; }

        [XmlElement("vrRubr")]
        public double VrRubr { get; set; }
        /// <summary>
        /// Indicativo de tipo de apuração de IR
        /// </summary>
        [XmlElement("indApurIR")]
        public IndApurIR IndApurIR { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQtdRubr() => QtdRubr > 0;

        public bool ShouldSerializeFatorRubr() => FatorRubr > 0;

        #endregion
    }


    #endregion ItensRemun1207

    #endregion IdeEstab1207

    #endregion InfoPerApur1207

    #region  InfPerAnt

    /// <summary>
    /// Grupo destinado às informações relativas a períodos
    /// anteriores.Somente preencher esse grupo se houver
    /// proventos ou pensões retroativos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfPerAnt")]
    [ComVisible(true)]
#endif
    public class InfPerAnt
    {
        [XmlElement("idePeriodo")]
        public List<IdePeriodo1207> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo1207 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo1207>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo</returns>
        public IdePeriodo1207 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif

    }

    #region IdePeriodo1207

    /// <summary>
    /// Identificação do período ao qual se referem as diferenças de provento ou pensão.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo1207")]
    [ComVisible(true)]
#endif
    public class IdePeriodo1207
    {
        /// <summary>
        /// Informar o período ao qual se refere o complemento de
        /// provento ou pensão, no formato AAAA-MM.
        /// Validação: Deve ser igual ou anterior ao período de
        /// apuração informado em perApur.
        /// Deve ser informado no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
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
        public IdeEstab1207 IdeEstab { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializePerRefField() => PerRef > DateTime.MinValue;
        #endregion ShouldSerialize
    }

    #endregion IdePeriodo1207

    #endregion InfPerAnt

    #endregion DmDev1207
}