#pragma warning disable CS1591
using System;
using System.Runtime.InteropServices;
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Remuneração de Trabalhador vinculado ao Regime Geral de Previdência Social
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRemun/v_S_01_02_00", IsNullable = false)]
    public class ESocial1200 : XMLBase
    {
        /// <summary>
        /// Remuneração de Trabalhador vinculado ao Regime Geral de Previdência Social
        /// </summary>
        [XmlElement("evtRemun")]
        public EvtRemun EvtRemun { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.EvtRemun")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Remuneração de Trabalhador vinculado ao RGPS
    /// </summary>
    [Serializable()]
    public class EvtRemun
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
        public IdeEventoESocial1200 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador.
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador IdeTrabalhador { get; set; }

        /// <summary>
        /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador.
        /// </summary>
        [XmlElement("dmDev")]
        public DmDev DmDev { get; set; }

    }

    #region IdeEventoESocial1200

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.EvtRemun.IdeEventoESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    [Serializable()]
    public class IdeEventoESocial1200
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação.
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Indicativo de período de apuração.
        /// </summary>
        [XmlElement("indApuracao")]
        public string IndApuracao { get; set; }
        //Existe um enum "IndApuracao", mas por causa de erros na conversão do xml, será colocado como string

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

    }

    #endregion IdeEventoESocial1200

    #region IdeTrabalhador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.IdeTrabalhador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do trabalhador.
    /// </summary>
    [Serializable()]
    public class IdeTrabalhador
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// Validação: Deve ser um CPF válido
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente em caso de trabalhador
        /// que possua outros vínculos/atividades nos quais já tenha
        /// ocorrido desconto de contribuição previdenciária.
        /// </summary>
        [XmlElement("infoMV")]
        public InfoMV InfoMV { get; set; }

        /// <summary>
        /// Grupo preenchido quando o evento de remuneração se
        /// referir a trabalhador cuja categoria não está sujeita ao
        /// evento de admissão ou ao evento TSVE - Início, bem
        /// como para informar remuneração devida pela empresa
        /// sucessora a empregado desligado ainda na sucedida.No
        /// caso das categorias em que o envio do evento TSVE -
        /// Início for opcional, o preenchimento do grupo somente é
        /// exigido se não houver o respectivo evento.As
        /// informações complementares são necessárias para correta
        /// identificação do trabalhador.
        /// </summary>
        [XmlElement("infoComplem")]
        public InfoComplem InfoComplem { get; set; }

        /// <summary>
        /// Informações sobre a existência de processos judiciais do
        ///trabalhador com decisão favorável quanto à não
        ///incidência de contribuições sociais e/ou Imposto de Renda.
        /// </summary>
        [XmlElement("procJudTrab")]
        public ProcJudTrab ProcJudTrab { get; set; }
    }

    #region InfoMV

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.IdeTrabalhador.InfoMV")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador
    /// que possua outros vínculos/atividades nos quais já tenha
    /// ocorrido desconto de contribuição previdenciária.
    /// </summary>
    [Serializable()]
    public class InfoMV
    {
        /// <summary>
        /// Indicador de desconto da contribuição previdenciária do trabalhador.
        /// </summary>
        [XmlElement("indMV")]
        public string IndMV { get; set; }
        //Existe um enum "IndMV", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Informações relativas ao trabalhador que possui vínculo
        /// empregatício com outra(s) empresa(s) e/ou que exerce
        /// outras atividades como contribuinte individual,
        /// detalhando as empresas que efetuaram(ou efetuarão)
        /// desconto da contribuição.
        /// </summary>
        [XmlElement("remunOutrEmpr")]
        public RemunOutrEmpr RemunOutrEmpr { get; set; }
    }

    #region RemunOutrEmpr

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.IdeTrabalhador.InfoMV.RemunOutrEmpr")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao trabalhador que possui vínculo
    /// empregatício com outra(s) empresa(s) e/ou que exerce
    /// outras atividades como contribuinte individual,
    /// detalhando as empresas que efetuaram(ou efetuarão)
    /// desconto da contribuição.
    /// </summary>
    [Serializable()]
    public class RemunOutrEmpr
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo
        /// com o tipo de inscrição indicado no campo
        /// remunOutrEmpr/tpInsc;
        /// 
        /// Validação: a) Se indApuracao = [1] e
        ///remunOutrEmpr/tpInsc = [1], deve ser um CNPJ válido,
        ///diferente do CNPJ base indicado no evento de
        ///Informações do Empregador(S-1000) e dos
        ///estabelecimentos informados através do evento S-1005.
        ///b) Se indApuracao = [1] e remunOutrEmpr/tpInsc = [2],
        ///deve ser um CPF válido e diferente do CPF do trabalhador
        ///e ainda, caso o empregador seja pessoa física, diferente
        ///do CPF do empregador.
        ///c) Se indApuracao = [2] e remunOutrEmpr/tpInsc = [1], é
        ///permitido informar número de inscrição igual ao CNPJ
        ///base indicado no evento de Informações do Empregador
        ///(S-1000) e aos estabelecimentos informados através do
        ///evento S-1005.
        ///d) Se indApuracao = [2] e remunOutrEmpr/tpInsc = [2],
        ///deve ser um CPF válido e diferente do CPF do trabalhador,
        ///mas é permitido informar número de inscrição igual ao
        ///CPF do empregador.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador na qual houve a remuneração.
        /// </summary>
        [XmlElement("codCateg")]
        public string CodCateg { get; set; }

        /// <summary>
        /// Preencher com o valor da remuneração recebida pelo
        /// trabalhador na outra empresa/atividade, sobre a qual
        /// houve desconto/recolhimento da contribuição do segurado.
        /// </summary>
        //[XmlIgnore]
        [XmlElement("vlrRemunOE")]
        public double VlrRemunOE { get; set; }

        ///// <summary>
        ///// Preencher com o valor da remuneração recebida pelo
        ///// trabalhador na outra empresa/atividade, sobre a qual
        ///// houve desconto/recolhimento da contribuição do segurado.
        ///// </summary>
        //[XmlElement("vlrRemunOE")]
        //public string VlrRemunOEField
        //{
        //    get => VlrRemunOE.ToString("F2", CultureInfoESocial.Info);
        //    set => VlrRemunOE = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}
    }

    #endregion RemunOutrEmpr

    #endregion InfoMV

    #region InfoComplem

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.IdeTrabalhador.InfoComplem")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido quando o evento de remuneração se
    /// referir a trabalhador cuja categoria não está sujeita ao
    /// evento de admissão ou ao evento TSVE - Início, bem
    /// como para informar remuneração devida pela empresa
    /// sucessora a empregado desligado ainda na sucedida.No
    /// caso das categorias em que o envio do evento TSVE -
    /// Início for opcional, o preenchimento do grupo somente é
    /// exigido se não houver o respectivo evento.As
    /// informações complementares são necessárias para correta
    /// identificação do trabalhador.
    /// </summary>
    [Serializable()]
    public class InfoComplem
    {
        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

#if INTEROP
         public DateTime DtNascto {get; set; }
#else
        /// <summary>
        /// Preencher com a data de nascimento
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtNascto { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de nascimento
        /// </summary>
        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

    }

    #endregion InfoComplem

    #region ProcJudTrab

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.IdeTrabalhador.ProcJudTrab")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações sobre a existência de processos judiciais do
    /// trabalhador com decisão favorável quanto à não
    /// incidência de contribuições sociais e/ou Imposto de Renda.
    /// </summary>
    [Serializable()]
    public class ProcJudTrab
    {
        /// <summary>
        /// Abrangência da decisão.
        /// Valores válidos:
        /// 1 - IRRF
        /// 2 - Contribuições sociais do trabalhador
        /// </summary>
        [XmlElement("tpTrib")]
        public string TpTrib { get; set; }
        //Existe um enum "TpTrib", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Informar um número de processo judicial cadastrado
        /// através do evento S-1070, cujo indMatProc seja igual a[1].
        /// Validação: Deve ser um número de processo judicial
        /// válido e existente na Tabela de Processos (S-1070), com indMatProc = [1].
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070.
        /// Validação: A informação prestada deve estar de acordo
        /// com o que foi informado em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    #endregion ProcJudTrab

    #endregion IdeTrabalhador

    #region DmDev

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador.
    /// </summary>
    [Serializable()]
    public class DmDev
    {
        /// <summary>
        /// Identificador atribuído pela empresa para o demonstrativo
        ///de valores devidos ao trabalhador.O empregador pode
        ///preencher este campo utilizando-se de um identificador
        ///padrão para todos os trabalhadores; no entanto, havendo
        ///mais de um demonstrativo relativo a uma mesma
        ///competência, devem ser utilizados identificadores
        ///diferentes para cada um dos demonstrativos.
        ///Validação: Deve ser um identificador único dentro do
        ///mesmo perApur para cada um dos demonstrativos do trabalhador
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador.
        /// </summary>
        [XmlElement("codCateg")]
        public string CodCateg { get; set; }
        //Existe um enum "CodCateg", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Informações relativas ao período de apuração.
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur InfoPerApur { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
        /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
        /// houver evento S-2300 correspondente.
        /// </summary>
        [XmlElement("infoComplCont")]
        public InfoComplCont InfoComplCont { get; set; }

    }

    #region InfoPerApur

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoPerApur")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao período de apuração. 
    /// </summary>
    [Serializable()]
    public class InfoPerApur
    {
        /// <summary>
        /// Identificação do estabelecimento e da lotação nos quais o
        /// trabalhador possui remuneração no período de apuração.
        /// O estabelecimento identificado no grupo pode ser: o
        /// número do CNPJ do estabelecimento da própria empresa
        /// (matriz/filial), o número da obra(própria) no CNO, ou o
        /// número do CAEPF(no caso de pessoa física obrigada a
        /// inscrição no Cadastro de Atividade Econômica da Pessoa Física).
        /// </summary>
        [XmlElement("ideEstabLot")]
        public IdeEstabLot IdeEstabLot { get; set; }
    }

    #region IdeEstabLot

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoPerApur.IdeEstabLot")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o
    /// trabalhador possui remuneração no período de apuração.
    /// O estabelecimento identificado no grupo pode ser: o
    /// número do CNPJ do estabelecimento da própria empresa
    /// (matriz/filial), o número da obra(própria) no CNO, ou o
    /// número do CAEPF(no caso de pessoa física obrigada a
    /// inscrição no Cadastro de Atividade Econômica da Pessoa Física).
    /// </summary>
    [Serializable()]
    public class IdeEstabLot
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento
        /// Valores válidos:
        /// 1 - CNPJ;
        /// 3 - CAEPF;
        /// 4 - CNO;
        /// </summary>
        [XmlElement("tpInsc")]
        public string TpInsc { get; set; }
        //Existe um enum "TpInsc", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Informar o número de inscrição do estabelecimento do contribuinte de acordo 
        /// com o tipo de inscrição indicado no campo ideEstabLot/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária.
        /// </summary>
        [XmlElement("codLotacao")]
        public string codlotacao { get; set; }

        /// <summary>
        /// 
        /// </summary>
        [XmlElement("qtdDiasAv")]
        public int QtdDiasAv { get; set; }

        /// <summary>
        /// Remuneração do trabalhador 
        /// </summary>
        [XmlElement("remunPerApur")]
        public RemunPerApur RemunPerApur { get; set; }
    }

    #region RemunPerApur

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoPerApur.IdeEstabLot.RemunPerApur")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Remuneração do trabalhador 
    /// </summary>
    [Serializable()]
    public class RemunPerApur
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público,
        /// a matrícula constante no Sistema de Administração de Recursos Humanos do órgão.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Indicador de contribuição substituída.
        /// Valores válidos:
        /// 1 - Contribuição substituída integralmente
        /// 2 - Contribuição não substituída
        /// 3 - Contribuição não substituída concomitante com
        /// </summary>
        [XmlElement("indSimples")]
        public string IndSimples { get; set; }
        //Existe um enum "IndSimples", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Rubricas que compõem a remuneração do trabalhador
        /// </summary>
        [XmlElement("itensRemun")]
        public ItensRemun ItensRemun { get; set; }

        /// <summary>
        /// Grau de exposição a agentes nocivos 

        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo InfoAgNocivo { get; set; }
    }

    #region ItensRemun

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoPerApur.IdeEstabLot.RemunPerApur.ItensRemun")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Rubricas que compõem a remuneração do trabalhador
    /// </summary>
    [Serializable()]
    public class ItensRemun
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador que identifica a rubrica em sua 
        /// folha de pagamento ou ocódigo da rubrica constante da Tabela de Rubricas Padrão.
        /// </summary>
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        /// <summary>
        /// Preencher com o identificador da Tabela de Rubricas para a rubrica definida em codRubr.
        /// </summary>
        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        /// <summary>
        /// Informar a quantidade de referência para apuração (em horas, cotas, meses, etc.). 
        /// Ex.: Quantidade de horas extras trabalhadas relacionada com uma rubrica de hora extra,
        /// quantidade de dias trabalhados relacionada com uma rubrica de salário, etc.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("qtdRubr")]
        public double QtdRubr { get; set; }

        ///// <summary>
        ///// Informar a quantidade de referência para apuração (em horas, cotas, meses, etc.). 
        ///// Ex.: Quantidade de horas extras trabalhadas relacionada com uma rubrica de hora extra,
        ///// quantidade de dias trabalhados relacionada com uma rubrica de salário, etc.
        ///// Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("qtdRubr")]
        //public string QtdRubrField
        //{
        //    get => QtdRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => QtdRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}

        /// <summary>
        /// Informar o fator, percentual, etc. da rubrica, quando necessário.
        /// Ex.: Adicional de horas extras 50%, relacionado a uma rubrica de horas extras: Fator = 50.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("fatorRubr")]
        public double FatorRubr { get; set; }

        ///// <summary>
        ///// Informar o fator, percentual, etc. da rubrica, quando necessário.
        ///// Ex.: Adicional de horas extras 50%, relacionado a uma rubrica de horas extras: Fator = 50.
        ///// Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("fatorRubr")]
        //public string fatorRubrField
        //{
        //    get => FatorRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => FatorRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}

        /// <summary>
        /// Valor total da rubrica. Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("vrRubr")]
        public double VrRubr { get; set; }

        ///// <summary>
        ///// Valor total da rubrica. Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("vrRubr")]
        //public string VrRubrField
        //{
        //    get => VrRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => VrRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}
    }

    #endregion ItensRemun

    #region InfoAgNocivo

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoPerApur.IdeEstabLot.RemunPerApur.InfoAgNocivo")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grau de exposição a agentes nocivos 
    /// </summary>
    [Serializable]
    public class InfoAgNocivo
    {
        /// <summary>
        /// O (se codCateg = [1XX, 2XX,3XX, 731, 734, 738] ou se codCateg = [4XX] com
        /// {categOrig} em S-2300 = [1XX,2XX, 3XX, 731, 734, 738]); N(nos demais casos)
        /// </summary>
        [XmlElement("grauExp")]
        public string GrauExp {  get; set; }
    }
    #endregion InfoAgNocivo

    #endregion RemunPerApur

    #endregion IdeEstabLot

    #endregion InfoPerApur

    #region InfoComplCont

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200.DmDev.InfoComplCont")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
    /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
    /// houver evento S-2300 correspondente.
    /// </summary>
    [Serializable()]
    public class InfoComplCont
    {
        /// <summary>
        /// Classificação Brasileira de Ocupações - CBO
        /// Validação: Deve ser um código válido e existente na tabela de CBO, com 6 (seis) posições.
        /// </summary>
        [XmlElement("codCBO")]
        public string CodCBO { get; set; }

        /// <summary>
        /// Natureza da atividade
        /// Validação: 
        /// O campo deve ser preenchido apenas se atendida uma das condições a seguir apresentadas: 
        /// a) classTrib em S-1000 = [06, 07];
        /// b) classTrib em S-1000 = [21,22] e existir remuneração para o trabalhador vinculada 
        /// a um tipo de CAEPF informado em S-1005 como produtor rural ou segurado especial.
        /// </summary>
        [XmlElement("natAtividade")]
        public string NatAtividade { get; set; }
        //Existe um enum "NatAtividade", mas por causa de erros na conversão do xml, será colocado como string

        /// <summary>
        /// Informação prestada exclusivamente pelo segurado especial em caso de 
        /// contratação de contribuinte individual, indicando a quantidade de dias trabalhados pelo mesmo.
        /// Caso não tenha havido trabalho no mês, informar 0 (zero).
        /// Validação: Preenchimento obrigatório e exclusivo se classTrib em S-1000 = [22], natAtividade = [2]
        /// e indApuracao = [1]. Neste caso, preencher com um número entre 0 e 31, de acordo com o calendário anual.
        /// </summary>
        [XmlElement("qtdDiasTrab")]
        public int QtdDiasTrab { get; set; }

    }
    #endregion InfoComplCont

    #endregion DmDev
}
