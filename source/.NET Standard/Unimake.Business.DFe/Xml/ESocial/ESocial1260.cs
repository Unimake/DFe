#pragma warning disable CS1591
using System;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Comercialização da Produção Rural Pessoa Física
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtComProd/v_S_01_02_00", IsNullable = false)]
    public class ESocial1260 : XMLBase
    {
        /// <summary>
        ///  Evento Comercialização da Produção Rural Pessoa Física
        /// </summary>
        [XmlElement("evtComProd")]
        public EvtComProdESocial1260 EvtBenPrRP { get; set; }

    }

    #region  EvtComProdESocial1260

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtComProdESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Evento Comercialização da Produção Rural Pessoa Física
    /// </summary>
    public class EvtComProdESocial1260
    {
        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial1260 IdeEventoESocial1260 { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informação da comercialização de produção
        /// </summary>
        [XmlElement("infoComProd")]
        public InfoComProdESocial1260 InfoComProdESocial1260 { get; set; }
    }

    #region  IdeEventoESocial1260 

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
    public class IdeEventoESocial1260
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

        [XmlIgnore]
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
        public bool ShouldSerializeNrReciboField() => !string.IsNullOrEmpty(NrRecibo);

        #endregion ShouldSerialize
    }
    #endregion IdeEventoESocial1260 

    #region InfoComProdESocial1260

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComProdESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informação da comercialização de produção
    /// </summary>
    public class InfoComProdESocial1260
    {
        /// <summary>
        /// Identificação do estabelecimento que comercializou a produção
        /// </summary>
        [XmlElement("ideEstabel")]
        public IdeEstabelESocial1260 IdeEstabESocial1260 { get; set; }
    }

    #region IdeEstabESocial1260

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento que comercializou a produção
    /// </summary>
    public class IdeEstabelESocial1260
    {
        /// <summary>
        /// Preencher com o número de inscrição no CAEPF do estabelecimento rural.
        /// Validação: Deve ser um número de inscrição válido e
        /// existente na Tabela de Estabelecimentos (S-1005).
        /// </summary>
        [XmlElement("nrInscEstabRural")]
        public string NrInscEstabRural { get; set; }

        /// <summary>
        /// Valor total da comercialização por "tipo" de comercialização.
        /// </summary>
        [XmlElement("tpComerc")]
        public TpComerc TpComerc { get; set; }
    }

    #region TpComerc

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TpComerc")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Valor total da comercialização por "tipo" de comercialização.
    /// </summary>
    public class TpComerc
    {
        /// <summary>
        /// Indicativo de comercialização.
        /// Valores válidos:
        /// 2 - Comercialização da produção efetuada diretamente no
        /// varejo a consumidor final ou a outro produtor rural pessoa
        /// física por produtor rural pessoa física, inclusive por
        /// segurado especial, ou por pessoa física não produtor rural
        /// 3 - Comercialização da produção por prod.rural PF/seg.
        /// especial - Vendas a PJ(exceto entidade inscrita no
        /// Programa de Aquisição de Alimentos - PAA) ou a
        /// intermediário PF
        /// 7 - Comercialização da produção isenta de acordo com a
        /// Lei 13.606/2018 efetuada diretamente no varejo a
        /// consumidor final ou a outro produtor rural pessoa física
        /// por produtor rural pessoa física, inclusive por segurado
        /// especial, ou por pessoa física não produtor rural
        /// 8 - Comercialização da produção da pessoa física/segurado especial para entidade inscrita no PAA
        /// 9 - Comercialização da produção no mercado externo
        /// </summary>
        [XmlElement("indComerc")]
        public IndComerc IndComerc { get; set; }

        /// <summary>
        /// Preencher com o valor total da comercialização.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrTotCom { get; set; }

        /// <summary>
        /// Preencher com o valor total da comercialização.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlElement("vrTotCom")]
        public string VrTotComContribField
        {
            get => VrTotCom.ToString("F2", CultureInfo.InvariantCulture);
            set => VrTotCom = Converter.ToDouble(value);
        }

        /// <summary>
        /// Identificação dos adquirentes da produção.
        /// </summary>
        [XmlElement("ideAdquir")]
        public IdeAdquir IdeAdquir { get; set; }

        /// <summary>
        /// Informações de processos judiciais com decisão/sentença
        /// favorável ao contribuinte e relativos à contribuição
        /// incidente sobre a comercialização.
        /// </summary>
        [XmlElement("infoProcJud")]
        public InfoProcJudESocial1260 InfoProcJudESocial1260 { get; set; }

        #region ShouldSerialize 

        public bool ShouldSerializeIdeAdquirField() => IdeAdquir != null;
        public bool ShouldSerializeInfoProcJudField() => InfoProcJudESocial1260 != null;

        #endregion ShouldSerialize
    }

    #region IdeAdquir

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdquir")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação dos adquirentes da produção.
    /// </summary>
    public class IdeAdquir
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição
        /// 1 - CNPJ
        /// 2 - CPF
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo
        /// ideAdquir/tpInsc.
        /// Validação: A inscrição informada deve ser compatível com
        /// o ideAdquir/tpInsc e diferente da inscrição do declarante.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Preencher com o valor total da comercialização.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlElement("vrComerc")]
        public double VrComerc { get; set; }

        /// <summary>
        /// Detalhamento das notas fiscais relativas à comercialização
        /// de produção com o adquirente identificado no grupo superior
        /// </summary>
        [XmlElement("nfs")]
        public Nfs Nfs { get; set; }

        #region shouldSerialize

        public bool ShouldSerializeNfsField() => Nfs != null;

        #endregion shouldSerialize
    }

    #region Nfs

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Nfs")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Detalhamento das notas fiscais relativas à comercialização
    /// de produção com o adquirente identificado no grupo superior
    /// </summary>
    public class Nfs
    {
        /// <summary>
        /// Informar o número de série da nota fiscal/fatura.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da nota fiscal/fatura
        /// </summary>
        [XmlElement("nrDocto")]
        public string NrDocto { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEmisNF { get; set; }
#else
        /// <summary>
        /// Data de emissão da nota fiscal/fatura. Validação: O mês/ano da emissão da nota fiscal deve ser
        ///igual ao mês/ano indicado no registro de abertura do arquivo
        /// </summary>
        public DateTimeOffset DtEmisNF { get; set; }
#endif

        /// <summary>
        /// Data de emissão da nota fiscal/fatura. Validação: O mês/ano da emissão da nota fiscal deve ser
        ///igual ao mês/ano indicado no registro de abertura do arquivo
        /// </summary>
        [XmlElement("dtEmisNF")]
        public string DtEmisNFField
        {
            get => DtEmisNF.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEmisNF = DateTime.Parse(value);
#else
            set => DtEmisNF = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o valor bruto da(s) nota(s) fiscal(is).
        /// </summary>
        [XmlElement("vlrBruto")]
        public double VlrBruto { get; set; }

        /// <summary>
        /// Preencher com o valor da contribuição previdenciária descontada pelo adquirente na comercialização de
        /// produção.Se não houver informação, preencher com 0 (zero).
        /// </summary>
 
        [XmlElement("vrCPDescPR")]
        public double VrCPDescPR { get; set; }

        /// <summary>
        /// Valor da contribuição destinada ao financiamento dos benefícios concedidos 
        /// em razão do grau de incidência da incapacidade laborativa decorrente dos riscos ambientais
        /// do trabalho, incidente sobre a comercialização de
        /// produção rural de produtor rural.Se não houver informação, preencher com 0 (zero).
        /// </summary>
        [XmlElement("vrRatDescPR")]
        public double VrRatDescPR { get; set; }

        /// <summary>
        /// Valor da contribuição destinada ao SENAR, incidente sobre
        /// a comercialização de produção rural de produtor rural
        /// pessoa física/segurado especial.Se não houver
        /// informação, preencher com 0 (zero).
        /// </summary>
        [XmlElement("vrSenarDesc")]
        public double VrSenarDesc { get; set; }

        #region ShouldSerialize 

        public bool ShouldSerializeSerieField() => !string.IsNullOrEmpty(Serie);

        #endregion ShouldSerialize
    }
    #endregion Nfs

    #endregion IdeAdquir

    #region InfoProcJudESocial1260

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJudESocial1260")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de processos judiciais com decisão/sentença
    /// favorável ao contribuinte e relativos à contribuição
    /// incidente sobre a comercialização.
    /// </summary>
    [Serializable]
    public class InfoProcJudESocial1260
    {
        /// <summary>
        ///  Preencher com o código correspondente ao tipo de processo.
        /// Valores válidos:
        /// 1 - Administrativo
        /// 2 - Judicial
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcesso TipoProcesso { get; set; }

        /// <summary>
        /// Informar um número de processo cadastrado através do evento S-1070, cujo indMatProc seja igual a [1].
        /// Validação: Deve ser um número de processo
        ///administrativo ou judicial válido e existente na Tabela de Processos(S-1070), com indMatProc = [1].
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070. 
        /// Validação: A informação prestada deve estar de acordo com o que foi informado em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        /// <summary>
        /// Valor da contribuição previdenciária com exigibilidade suspensa. 
        /// Validação: Preenchimento obrigatório se vrRatSusp e vrSenarSusp não tiverem sido preenchidos.
        /// Deve ser um valor maior que 0 (zero).
        /// </summary>
        [XmlElement("vrCPSusp")]
        public double VrCPSusp { get; set; }

        /// <summary>
        /// Valor da contribuição para GILRAT com exigibilidade suspensa.
        /// Validação: Preenchimento obrigatório se vrCPSusp e vrSenarSusp não tiverem sido preenchidos.
        /// Deve ser um valor maior que 0 (zero).
        /// </summary>
        [XmlElement("vrRatSusp")]
        public double VrRatSusp { get; set; }


        /// <summary>
        /// Valor da contribuição para o SENAR com exigibilidade suspensa.
        /// Validação: Preenchimento obrigatório se VrCPSusp e vrRatSusp não tiverem sido preenchidos. 
        /// Deve ser um valor maior que 0 (zero).
        [XmlElement("vrSenarSusp")]
        public double VrSenarSusp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrCPSuspField() => VrCPSusp > 0;
        
        public bool ShouldSerializeVrRatSuspField() => VrRatSusp > 0;

        public bool ShouldSerializeVrSenarSuspField() => VrSenarSusp > 0;


        #endregion ShouldSerialize
    }
    #endregion InfoProcJudESocial1260

    #endregion TpComerc

    #endregion  IdeEstabESocial1260

    #endregion InfoComProdESocial1260

    #endregion EvtComProdESocial1260
}