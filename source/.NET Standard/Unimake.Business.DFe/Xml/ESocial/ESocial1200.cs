#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Net;
using System.Runtime.ConstrainedExecution;
using System.Security.Cryptography;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.CTeOS;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Xml.SNCM;
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtRemun")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Remuneração de Trabalhador vinculado ao RGPS
    /// </summary>
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    public class IdeEventoESocial1200
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
        /// Indicativo do tipo de guia.
        /// Valores válidos:
        /// 1 - Documento de Arrecadação do eSocial - DAE
        /// </summary>
        [XmlElement("indGuia")]
        public string IndGuia { get; set; }

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
        public bool ShouldSerializeIndGuiaField() => !IndGuia.IsNullOrEmpty();
        #endregion ShouldSerialize
    }

    #endregion IdeEventoESocial1200

    #region IdeTrabalhador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do trabalhador.
    /// </summary>
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

        /// <summary>
        /// Informações relativas ao trabalho intermitente.
        /// </summary>
        [XmlElement("infoInterm")]
        public InfoInterm InfoInterm { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeInfoIntermField() => !InfoInterm.IsNullOrEmpty();
        #endregion ShouldSerialize

    }

    #region InfoMV

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMV")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador
    /// que possua outros vínculos/atividades nos quais já tenha
    /// ocorrido desconto de contribuição previdenciária.
    /// </summary>
    public class InfoMV
    {
        /// <summary>
        /// Indicador de desconto da contribuição previdenciária do trabalhador.
        /// </summary>
        [XmlElement("indMV")]
        public IndMV IndMV { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunOutrEmpr")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao trabalhador que possui vínculo
    /// empregatício com outra(s) empresa(s) e/ou que exerce
    /// outras atividades como contribuinte individual,
    /// detalhando as empresas que efetuaram(ou efetuarão)
    /// desconto da contribuição.
    /// </summary>
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
    }

    #endregion RemunOutrEmpr

    #endregion InfoMV

    #region InfoComplem

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplem")]
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
    public class InfoComplem
    {
        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto {get; set; }
#else
        /// <summary>
        /// Preencher com a data de nascimento
        /// </summary>
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

        /// <summary>
        /// Grupo de informações da sucessão de vínculo trabalhista.
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVincESocial1200 SucessaoVinc { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeSucessaoVincField() => !SucessaoVinc.IsNullOrEmpty();
        #endregion ShouldSerialize
    }

    #region SucessaoVincESocial1200 
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVincESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo de informações da sucessão de vínculo trabalhista.
    /// </summary>
    public class SucessaoVincESocial1200
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de
        /// inscrição, conforme Tabela 05.
        /// Valores válidos:
        /// 1 - CNPJ
        /// 2 - CPF
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do empregador anterior,
        /// de acordo com o tipo de inscrição indicado no campo sucessaoVinc/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Matrícula do trabalhador no empregador anterior.
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        /// <summary>
        /// Preencher com a data de admissão do trabalhador. No
        /// caso de transferência do empregado, deve ser preenchida
        /// a data inicial do vínculo no primeiro empregador(data de
        /// início do vínculo).
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAdm { get; set; }
#else
        public DateTimeOffset DtAdm { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de admissão do trabalhador. No
        /// caso de transferência do empregado, deve ser preenchida
        /// a data inicial do vínculo no primeiro empregador(data de
        /// início do vínculo).
        /// </summary>
        [XmlElement("dtAdm")]
        public string DtAdmField
        {
            get => DtAdm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAdm = DateTime.Parse(value);
#else
            set => DtAdm = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeMatricAntField() => !string.IsNullOrEmpty(MatricAnt);
        public bool ShouldSerializeObservacaoField() => !string.IsNullOrEmpty(Observacao);
        #endregion ShouldSerialize
    }
    #endregion SucessaoVincESocial1200 

    #endregion InfoComplem

    #region ProcJudTrab

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações sobre a existência de processos judiciais do
    /// trabalhador com decisão favorável quanto à não
    /// incidência de contribuições sociais e/ou Imposto de Renda.
    /// </summary>
    public class ProcJudTrab
    {
        /// <summary>
        /// Abrangência da decisão.
        /// Valores válidos:
        /// 1 - IRRF
        /// 2 - Contribuições sociais do trabalhador
        /// </summary>
        [XmlElement("tpTrib")]
        public TpTrib TpTrib { get; set; }

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

    #region InfoInterm

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoInterm")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao trabalho intermitente.
    /// </summary>
    public class InfoInterm
    {
        /// <summary>
        /// Dia do mês efetivamente trabalhado pelo empregado
        /// com contrato de trabalho intermitente.
        /// Caso não tenha havido trabalho no mês, informar 0 (zero).
        /// Validação: Deve ser um número entre 0 e 31, de acordo
        /// com o calendário anual.
        /// Retornar alerta caso seja informado 0 (zero).
        /// </summary>
        [XmlElement("dia")]
        public int Dia { get; set; }
    }
    #endregion  InfoInterm

    #endregion IdeTrabalhador

    #region DmDev

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador.
    /// </summary>
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
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Indicativo de Rendimentos Recebidos Acumuladamente -
        /// RRA.
        /// Somente preencher este campo se for um demonstrativo
        /// de RRA.
        /// Valores válidos:
        /// S - Sim
        /// </summary>
        [XmlElement("indRRA")]
        public SimNaoLetra? IndRRA { get; set; }

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRAESocial1200 InfoRRA { get; set; }

        /// <summary>
        /// Informações relativas ao período de apuração.
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur InfoPerApur { get; set; }

        /// <summary>
        /// Grupo destinado às informações de:
        /// a) remuneração relativa a diferenças salariais provenientes
        /// de acordo coletivo, convenção coletiva e dissídio;
        /// b) remuneração relativa a diferenças de vencimento
        /// provenientes de disposições legais;
        /// c) bases de cálculo para efeitos de apuração de FGTS
        /// resultantes de conversão de licença saúde em acidente de
        /// trabalho;
        /// d) verbas de natureza salarial ou não salarial devidas após
        /// o desligamento.
        /// OBS.: As informações previstas acima podem se referir ao
        /// período de apuração definido em perApur ou a períodos
        /// anteriores a perApur.
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfoPerAntESocial1200 InfoPerAnt { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
        /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
        /// houver evento S-2300 correspondente.
        /// </summary>
        [XmlElement("infoComplCont")]
        public InfoComplCont InfoComplCont { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeIndRRAField() => IndRRA.IsNullOrEmpty();
        #endregion ShouldSerialize

    }

    #region InfoRRAESocial1200
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRAESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
    /// </summary>
    public class InfoRRAESocial1200 : InfoRRA { }
    #endregion InfoRRAESocial1200

    #region InfoPerApur

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas ao período de apuração. 
    /// </summary>
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot")]
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
        public TpInsc TpInsc { get; set; }

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
        public string Codlotacao { get; set; }

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

        #region ShouldSerialize
        public bool ShouldSerializeQtdDiasAvField() => QtdDiasAv != 0;
        #endregion ShouldSerialize
    }

    #region RemunPerApur

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerApur")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Remuneração do trabalhador 
    /// </summary>
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
        public IndSimples IndSimples { get; set; }

        /// <summary>
        /// Rubricas que compõem a remuneração do trabalhador
        /// </summary>
        [XmlElement("itensRemun")]
        public ItensRemunESocial1200 ItensRemun { get; set; }

        /// <summary>
        /// Grau de exposição a agentes nocivos 
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo InfoAgNocivo { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeMatriculaField() => !string.IsNullOrEmpty(Matricula);
        public bool ShouldSerializeIndSimplesField() => IndSimples > 0;
        #endregion ShouldSerialize
    }

    #region ItensRemun

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemunESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Rubricas que compõem a remuneração do trabalhador
    /// </summary>
    public class ItensRemunESocial1200 : ItensRemun
    {

    }

    #endregion ItensRemun

    #region InfoAgNocivo

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grau de exposição a agentes nocivos 
    /// </summary>
    public class InfoAgNocivo
    {
        /// <summary>
        /// O (se codCateg = [1XX, 2XX,3XX, 731, 734, 738] ou se codCateg = [4XX] com
        /// {categOrig} em S-2300 = [1XX,2XX, 3XX, 731, 734, 738]); N(nos demais casos)
        /// </summary>
        [XmlElement("grauExp")]
        public string GrauExp { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeGrauExpField() => !string.IsNullOrEmpty(GrauExp);
        #endregion ShouldSerialize
    }
    #endregion InfoAgNocivo

    #endregion RemunPerApur

    #endregion IdeEstabLot

    #endregion InfoPerApur

    #region InfoPerAntESocial1200
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerAntESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo destinado às informações de:
    /// a) remuneração relativa a diferenças salariais provenientes
    /// de acordo coletivo, convenção coletiva e dissídio;
    /// b) remuneração relativa a diferenças de vencimento
    /// provenientes de disposições legais;
    /// c) bases de cálculo para efeitos de apuração de FGTS
    /// resultantes de conversão de licença saúde em acidente de
    /// trabalho;
    /// d) verbas de natureza salarial ou não salarial devidas após
    /// o desligamento.
    /// OBS.: As informações previstas acima podem se referir ao
    /// período de apuração definido em perApur ou a períodos
    /// anteriores a perApur.
    /// </summary>
    public class InfoPerAntESocial1200
    {
        /// <summary>
        /// Identificação do instrumento ou situação ensejadora da
        /// remuneração relativa a períodos de apuração anteriores.
        /// </summary>
        [XmlElement("ideADC")]
        public IdeADC IdeADC { get; set; }
    }

    #region IdeADC
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeADC")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do instrumento ou situação ensejadora da
    /// remuneração relativa a períodos de apuração anteriores.
    /// </summary>
    public class IdeADC
    {
        /// <summary>
        /// Data da assinatura do acordo, convenção coletiva,
        /// sentença normativa ou da conversão da licença saúde em
        /// acidente de trabalho.
        /// Validação: Informação obrigatória se tpAcConv = [A,
        /// B,
        /// C,
        /// D,
        /// E].Se preenchida, seu mês/ano deve ser igual ou
        /// anterior ao período de apuração, informado em perApur.
        /// A data deve ser igual ou posterior a 01/01/1890.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAcConv { get; set; }
#else
        public DateTimeOffset DtAcConv { get; set; }
#endif

        /// <summary>
        /// Data da assinatura do acordo, convenção coletiva,
        /// sentença normativa ou da conversão da licença saúde em
        /// acidente de trabalho.
        /// Validação: Informação obrigatória se tpAcConv = [A,
        /// B,
        /// C,
        /// D,
        /// E].Se preenchida, seu mês/ano deve ser igual ou
        /// anterior ao período de apuração, informado em perApur.
        /// A data deve ser igual ou posterior a 01/01/1890.
        /// </summary>
        [XmlElement("dtAcConv")]
        public string DtAcConvField
        {
            get => DtAcConv.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAcConv = DateTime.Parse(value);
#else
            set => DtAcConv = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo do instrumento ou situação ensejadora da
        /// remuneração relativa a períodos de apuração anteriores.
        /// Valores válidos:
        /// A - Acordo Coletivo de Trabalho
        /// B - Legislação federal, estadual, municipal ou distrital
        /// C - Convenção Coletiva de Trabalho
        /// D - Sentença normativa - Dissídio
        /// E - Conversão de licença saúde em acidente de trabalho
        /// F - Outras verbas de natureza salarial ou não salarial
        /// devidas após o desligamento
        /// G - Antecipação de diferenças de acordo, convenção ou
        /// dissídio coletivo
        /// H - Declaração de base de cálculo de FGTS anterior ao
        /// início do FGTS Digital
        /// I - Sentença judicial(exceto reclamatória trabalhista)
        /// J - Parcelas complementares conhecidas após o
        /// fechamento da folha
        /// Validação: Se classTrib em S-1000 = [04, 22], não pode
        /// ser informado[E, H, I].
        /// </summary>
        [XmlElement("tpAcConv")]
        public TpAcConv TpAcConv { get; set; }

        /// <summary>
        /// Descrição do instrumento ou situação que originou o
        /// pagamento das verbas relativas a períodos anteriores.
        /// </summary>
        [XmlElement("dsc")]
        public string Dsc { get; set; }

        /// <summary>
        /// Indicar se a remuneração é relativa a verbas de natureza
        /// salarial ou não salarial devidas pela empresa sucessora a
        /// empregados desligados ainda na sucedida.
        /// Valores válidos:
        /// S - Sim
        /// N - Não
        /// </summary>
        [XmlElement("remunSuc")]
        public SimNaoLetra RemunSuc { get; set; }

        /// <summary>
        ///  Identificação do período ao qual se referem as diferenças de remuneração.
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodoESocial1200 IdePeriodoESocial1200 { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeDtAcConvField () => DtAcConv > DateTimeOffset.MinValue;
        #endregion ShouldSerialize
    }

    #region  IdePeriodoESocial1200
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodoESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Identificação do período ao qual se referem as diferenças de remuneração.
    /// </summary>
    public class IdePeriodoESocial1200
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef {get; set; }
#else
        /// <summary>
        /// Informar o período ao qual se refere o complemento de remuneração, no formato AAAA-MM.
        /// </summary>
        public DateTimeOffset PerRef { get; set; }
#endif

        /// <summary>
        /// Informar o período ao qual se refere o complemento de remuneração, no formato AAAA-MM.
        /// Validação: Deve ser igual ou anterior ao período de
        /// apuração informado em perApur.
        /// Deve ser informado no formato AAAA-MM.
        /// Se tpAcConv = [H], deve ser anterior ao início do FGTS
        /// Digital e igual ou posterior a[1994 - 07].
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
        /// Identificação do estabelecimento e da lotação ao qual se
        /// referem as diferenças de remuneração do mês
        /// identificado no grupo superior.
        /// </summary>
        [XmlElement("ideEstabLot")]
        public IdeEstabLotESocial1200 IdeEstabLot { get; set; }
    }

    #region IdeEstabLotESocial1200

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLotESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento e da lotação ao qual se
    /// referem as diferenças de remuneração do mês
    /// identificado no grupo superior.
    /// </summary>
    public class IdeEstabLotESocial1200
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento
        /// Valores válidos:
        /// 1 - CNPJ;
        /// 3 - CAEPF;
        /// 4 - CNO;
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

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
        public string Codlotacao { get; set; }

        /// <summary>
        /// Informações relativas à remuneração do trabalhador em períodos anteriores.
        /// </summary>
        public RemunPerAntESocial1200 RemunPerAnt { get; set; }
    }

    #region  RemunPerAntESocial1200
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerAntESocial1200")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas à remuneração do trabalhador em períodos anteriores.
    /// </summary>
    public class RemunPerAntESocial1200
    {
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Indicador de contribuição substituída.
        /// Valores válidos:
        /// 1 - Contribuição substituída integralmente
        /// 2 - Contribuição não substituída
        /// 3 - Contribuição não substituída concomitante com
        /// contribuição substituída
        /// Validação: O preenchimento do campo é obrigatório
        /// apenas no caso das empresas enquadradas no regime de
        /// tributação Simples Nacional, com tributação
        /// previdenciária substituída e não substituída(classTrib em
        /// S-1000 = [03]). Para os demais empregadores, não deve
        /// ser informado.
        /// </summary>
        [XmlElement("indSimples")]
        public IndSimples IndSimples { get; set; }

        /// <summary>
        /// Rubricas que compõem a remuneração do trabalhador.
        /// </summary>
        [XmlElement("itensRemun")]
        public List<ItensRemunESocial1200> ItensRemun { get; set; }

        /// <summary>
        ///  Grupo referente ao detalhamento do grau de exposição
        /// do trabalhador aos agentes nocivos que ensejam a
        /// cobrança da contribuição adicional para financiamento
        /// dos benefícios de aposentadoria especial.
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo InfoAgNocivo { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeMatriculaField () => !Matricula.IsNullOrEmpty();
        public bool ShouldSerializeIndSimplesField () => !IndSimples.IsNullOrEmpty();
        public bool ShouldSerializeInfoAgNocivoField() => !InfoAgNocivo.IsNullOrEmpty();
        #endregion ShouldSerialize
    }

    #endregion RemunPerAntESocial1200
    #endregion IdeEstabLotESocial1200
    #endregion IdePeriodoESocial1200
    #endregion IdeADC
    #endregion InfoPerAntESocial1200

    #region InfoComplCont

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplCont")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
    /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
    /// houver evento S-2300 correspondente.
    /// </summary>
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
        public NatAtividade NatAtividade { get; set; }

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
