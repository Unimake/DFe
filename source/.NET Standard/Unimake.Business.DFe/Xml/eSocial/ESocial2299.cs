#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2299 - Desligamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2299")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtDeslig/v_S_01_03_00", IsNullable = false)]
    public class ESocial2299 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Desligamento
        /// </summary>
        [XmlElement("evtDeslig")]
        public EvtDeslig EvtDeslig { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Desligamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtDeslig")]
    [ComVisible(true)]
#endif
    public class EvtDeslig
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
        public IdeEvento2299 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2299 IdeVinculo { get; set; }

        /// <summary>
        /// Informações relativas ao desligamento do vínculo
        /// </summary>
        [XmlElement("infoDeslig")]
        public InfoDeslig InfoDeslig { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2299")]
    [ComVisible(true)]
#endif
    public class IdeEvento2299
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Indicativo do tipo de guia.
        /// Valores válidos:
        /// 1 - Documento de Arrecadação do eSocial - DAE
        /// </summary>
        [XmlElement("indGuia")]
#if INTEROP
        public IndGuia IndGuia { get; set; } = (IndGuia)(-1);
#else
        public IndGuia? IndGuia { get; set; }
#endif

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Processo de emissão do evento
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

#if INTEROP
        public bool ShouldSerializeIndGuia() => IndGuia != (IndGuia)(-1);
#else
        public bool ShouldSerializeIndGuia() => IndGuia != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2299")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2299 : IdeVinculo2206 { }

    /// <summary>
    /// Informações relativas ao desligamento do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeslig")]
    [ComVisible(true)]
#endif
    public class InfoDeslig
    {
        /// <summary>
        /// Código de motivo do desligamento
        /// </summary>
        [XmlElement("mtvDeslig")]
        public string MtvDeslig { get; set; }

        /// <summary>
        /// Preencher com a data de desligamento do vínculo (último dia trabalhado)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtDeslig { get; set; }
#else
        public DateTimeOffset DtDeslig { get; set; }
#endif

        [XmlElement("dtDeslig")]
        public string DtDesligField
        {
            get => DtDeslig.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDeslig = DateTime.Parse(value);
#else
            set => DtDeslig = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de concessão do aviso prévio
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAvPrv { get; set; }
#else
        public DateTimeOffset DtAvPrv { get; set; }
#endif

        [XmlElement("dtAvPrv")]
        public string DtAvPrvField
        {
            get => DtAvPrv.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAvPrv = DateTime.Parse(value);
#else
            set => DtAvPrv = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicativo de pagamento de aviso prévio indenizado pelo empregador, ao empregado
        /// </summary>
        [XmlElement("indPagtoAPI")]
        public SimNaoLetra IndPagtoAPI { get; set; }

        /// <summary>
        /// Data projetada para o término do aviso prévio indenizado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtProjFimAPI { get; set; }
#else
        public DateTimeOffset DtProjFimAPI { get; set; }
#endif

        [XmlElement("dtProjFimAPI")]
        public string DtProjFimAPIField
        {
            get => DtProjFimAPI.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtProjFimAPI = DateTime.Parse(value);
#else
            set => DtProjFimAPI = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Indicativo de pensão alimentícia para fins de retenção de FGTS
        /// </summary>
        [XmlElement("pensAlim")]
#if INTEROP
        public PensAlim PensAlim { get; set; } = (PensAlim)(-1);
#else
        public PensAlim? PensAlim { get; set; }
#endif

        /// <summary>
        /// Percentual a ser destinado a pensão alimentícia
        /// </summary>
        [XmlIgnore]
        public double PercAliment { get; set; }

        [XmlElement("percAliment")]
        public string PercAlimentField
        {
            get => PercAliment.ToString("F2", CultureInfo.InvariantCulture);
            set => PercAliment = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da pensão alimentícia
        /// </summary>
        [XmlIgnore]
        public double VrAlim { get; set; }

        [XmlElement("vrAlim")]
        public string VrAlimField
        {
            get => VrAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VrAlim = Converter.ToDouble(value);
        }

        /// <summary>
        /// Número que identifica o processo trabalhista, quando o desligamento se der por decisão judicial
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Indicativo se o desligamento ocorreu por meio de adesão a Programa de Demissão Voluntária (PDV)
        /// </summary>
        [XmlElement("indPDV")]
#if INTEROP
        public SimNaoLetra IndPDV { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndPDV { get; set; }
#endif

        /// <summary>
        /// Informações relativas ao trabalho intermitente
        /// </summary>
        [XmlElement("infoInterm")]
        public List<InfoInterm2299> InfoInterm { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoInterm(InfoInterm2299 item)
        {
            if (InfoInterm == null)
            {
                InfoInterm = new List<InfoInterm2299>();
            }

            InfoInterm.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoInterm2299 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoInterm</returns>
        public InfoInterm2299 GetInfoInterm(int index)
        {
            if ((InfoInterm?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoInterm[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoInterm
        /// </summary>
        public int GetInfoIntermCount => (InfoInterm != null ? InfoInterm.Count : 0);
#endif

        /// <summary>
        /// Observações sobre o desligamento
        /// </summary>
        [XmlElement("observacoes")]
        public List<Observacoes2299> Observacoes { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddObservacoes(Observacoes2299 item)
        {
            if (Observacoes == null)
            {
                Observacoes = new List<Observacoes2299>();
            }

            Observacoes.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Observacoes2299 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Observacoes</returns>
        public Observacoes2299 GetObservacoes(int index)
        {
            if ((Observacoes?.Count ?? 0) == 0)
            {
                return default;
            };

            return Observacoes[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Observacoes
        /// </summary>
        public int GetObservacoesCount => (Observacoes != null ? Observacoes.Count : 0);
#endif

        /// <summary>
        /// Grupo preenchido exclusivamente nos casos de sucessão do vínculo trabalhista, com a identificação da empresa sucessora
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc2299 SucessaoVinc { get; set; }

        /// <summary>
        /// Transferência de titularidade do empregado doméstico para outro representante da mesma unidade familiar
        /// </summary>
        [XmlElement("transfTit")]
        public TransfTit TransfTit { get; set; }

        /// <summary>
        /// Informação do novo CPF do trabalhador
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCPF2299 MudancaCPF { get; set; }

        /// <summary>
        /// Grupo onde são prestadas as informações relativas às verbas devidas ao trabalhador na rescisão contratual
        /// </summary>
        [XmlElement("verbasResc")]
        public VerbasResc VerbasResc { get; set; }

        /// <summary>
        /// Informações sobre a "quarentena" remunerada de trabalhador desligado ou outra situação de desligamento com data anterior
        /// </summary>
        [XmlElement("remunAposDeslig")]
        public RemunAposDeslig2299 RemunAposDeslig { get; set; }

        /// <summary>
        /// Informações sobre operação de crédito consignado com garantia de FGTS
        /// </summary>
        [XmlElement("consigFGTS")]
        public List<ConsigFGTS> ConsigFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddConsigFGTS(ConsigFGTS item)
        {
            if (ConsigFGTS == null)
            {
                ConsigFGTS = new List<ConsigFGTS>();
            }

            ConsigFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ConsigFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ConsigFGTS</returns>
        public ConsigFGTS GetConsigFGTS(int index)
        {
            if ((ConsigFGTS?.Count ?? 0) == 0)
            {
                return default;
            };

            return ConsigFGTS[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ConsigFGTS
        /// </summary>
        public int GetConsigFGTSCount => (ConsigFGTS != null ? ConsigFGTS.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtAvPrvField() => DtAvPrv > DateTime.MinValue;

        public bool ShouldSerializeDtProjFimAPIField() => DtProjFimAPI > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializePensAlim() => PensAlim != (PensAlim)(-1);
#else
        public bool ShouldSerializePensAlim() => PensAlim != null;
#endif

        public bool ShouldSerializePercAlimentField() => PercAliment > 0;

        public bool ShouldSerializeVrAlimField() => VrAlim > 0;

        public bool ShouldSerializeNrProcTrab() => !string.IsNullOrEmpty(NrProcTrab);

#if INTEROP
        public bool ShouldSerializeIndPDV() => IndPDV != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndPDV() => IndPDV != null;
#endif
        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas ao trabalho intermitente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoInterm2299")]
    [ComVisible(true)]
#endif
    public class InfoInterm2299 : InfoInterm1200 { }

    /// <summary>
    /// Observações sobre o desligamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Observacoes2299")]
    [ComVisible(true)]
#endif
    public class Observacoes2299 : Observacoes2306 { }

    /// <summary>
    /// Grupo preenchido exclusivamente nos casos de sucessão do vínculo trabalhista, com a identificação da empresa sucessora
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc2299")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc2299
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do empregador sucessor, de acordo com o tipo de inscrição indicado no campo sucessaoVinc/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Transferência de titularidade do empregado doméstico para outro representante da mesma unidade familiar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TransfTit")]
    [ComVisible(true)]
#endif
    public class TransfTit
    {
        /// <summary>
        /// Preencher com o CPF do novo titular
        /// </summary>
        [XmlElement("cpfSubstituto")]
        public string CpfSubstituto { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento do novo titular
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

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

    /// <summary>
    /// Informação do novo CPF do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPF2299")]
    [ComVisible(true)]
#endif
    public class MudancaCPF2299
    {
        /// <summary>
        /// Preencher com o novo CPF do trabalhador
        /// </summary>
        [XmlElement("novoCPF")]
        public string NovoCPF { get; set; }
    }

    /// <summary>
    /// Grupo onde são prestadas as informações relativas às verbas devidas ao trabalhador na rescisão contratual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.VerbasResc")]
    [ComVisible(true)]
#endif
    public class VerbasResc
    {
        /// <summary>
        /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("dmDev")]
        public List<DmDev2299> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev2299 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev2299>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDev2299 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDev
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif

        /// <summary>
        /// Informações sobre a existência de processos judiciais do trabalhador com decisão favorável 
        /// quanto à não incidência de contribuições sociais e/ou Imposto de Renda
        /// </summary>
        [XmlElement("procJudTrab")]
        public List<ProcJudTrab2299> ProcJudTrab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrab2299 item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrab2299>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrab2299 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrab2299 GetProcJudTrab(int index)
        {
            if ((ProcJudTrab?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcJudTrab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcJudTrab
        /// </summary>
        public int GetProcJudTrabCount => (ProcJudTrab != null ? ProcJudTrab.Count : 0);
#endif

        /// <summary>
        /// Grupo preenchido exclusivamente em caso de trabalhador que possua outros vínculos/atividades 
        /// nos quais já tenha ocorrido desconto de contribuição previdenciária
        /// </summary>
        [XmlElement("infoMV")]
        public InfoMV2299 InfoMV { get; set; }

        /// <summary>
        /// Informação sobre processo judicial que suspende a exigibilidade da Contribuição Social Rescisória
        /// </summary>
        [XmlElement("procCS")]
        public ProcCS2299 ProcCS { get; set; }
    }

    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev2299")]
    [ComVisible(true)]
#endif
    public class DmDev2299
    {
        /// <summary>
        /// Identificador atribuído pela empresa para o demonstrativo de valores devidos ao trabalhador relativo a verbas rescisórias
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Indicativo de Rendimentos Recebidos Acumuladamente - RRA.
        /// Somente preencher este campo se for um demonstrativo de RRA
        /// </summary>
        [XmlElement("indRRA")]
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRRA { get; set; }
#endif

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA2299 InfoRRA { get; set; }

        /// <summary>
        /// Verbas rescisórias relativas ao mês/ano da data do desligamento
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur2299 InfoPerApur { get; set; }

        /// <summary>
        /// Remuneração relativa a períodos anteriores, devida em função de acordos coletivos, legislação específica, convenção coletiva de trabalho, 
        /// dissídio ou conversão de licença saúde em acidente de trabalho
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfoPerAnt2299 InfoPerAnt { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif

#endregion
    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA2299")]
    [ComVisible(true)]
#endif
    public class InfoRRA2299 : InfoRRA1202 { }

    /// <summary>
    /// Verbas rescisórias relativas ao mês/ano da data do desligamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur2299")]
    [ComVisible(true)]
#endif
    public class InfoPerApur2299
    {
        /// <summary>
        /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLot2299> IdeEstabLot { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLot2299 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLot2299>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLot2299 GetDmDev(int index)
        {
            if ((IdeEstabLot?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstabLot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabLot
        /// </summary>
        public int GetIdeEstabLotCount => (IdeEstabLot != null ? IdeEstabLot.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot2299")]
    [ComVisible(true)]
#endif
    public class IdeEstabLot2299
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento, de acordo com as opções da Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento do contribuinte de acordo com o tipo de inscrição indicado no campo ideEstabLot/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária
        /// </summary>
        [XmlElement("codLotacao")]
        public string Codlotacao { get; set; }

        /// <summary>
        /// Detalhamento das verbas rescisórias devidas ao trabalhador. Deve haver pelo menos uma rubrica de folha, mesmo que o valor 
        /// líquido a ser pago ao trabalhador seja 0 (zero) em função de descontos
        /// </summary>
        [XmlElement("detVerbas")]
        public List<DetVerbas> DetVerbas { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetVerbas(DetVerbas item)
        {
            if (DetVerbas == null)
            {
                DetVerbas = new List<DetVerbas>();
            }

            DetVerbas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetVerbas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetVerbas</returns>
        public DetVerbas GetDetVerbas(int index)
        {
            if ((DetVerbas?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetVerbas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetVerbas
        /// </summary>
        public int GetDetVerbasCount => (DetVerbas != null ? DetVerbas.Count : 0);
#endif

        /// <summary>
        /// Grupo referente ao detalhamento do grau de exposição do trabalhador aos agentes nocivos que ensejam a cobrança da 
        /// contribuição adicional para financiamento dos benefícios de aposentadoria especial
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo2299 InfoAgNocivo { get; set; }

        /// <summary>
        /// Informação relativa a empresas enquadradas no regime de tributação Simples Nacional
        /// </summary>
        [XmlElement("infoSimples")]
        public InfoSimples InfoSimples { get; set; }
    }

    /// <summary>
    /// Detalhamento das verbas rescisórias devidas ao trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetVerbas")]
    [ComVisible(true)]
#endif
    public class DetVerbas
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador que identifica a rubrica em sua folha de pagamento ou 
        /// o código da rubrica constante da Tabela de Rubricas Padrão
        /// </summary>
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        /// <summary>
        /// Preencher com o identificador da Tabela de Rubricas para a rubrica definida em codRubr
        /// </summary>
        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        /// <summary>
        /// Informar a quantidade de referência para apuração (em horas, cotas, meses, etc.).
        /// Ex.: Quantidade de horas extras trabalhadas relacionada com uma rubrica de hora extra,
        /// quantidade de dias trabalhados relacionada com uma rubrica de salário, etc.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double QtdRubr { get; set; }

        [XmlElement("qtdRubr")]
        public string QtdRubrField
        {
            get => QtdRubr.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdRubr = Converter.ToDouble(value);

        }

        /// <summary>
        /// Informar o fator, percentual, etc. da rubrica, quando necessário.
        /// Ex.: Adicional de horas extras 50%, relacionado a uma rubrica de horas extras: Fator = 50.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double FatorRubr { get; set; }

        [XmlElement("fatorRubr")]
        public string FatorRubrField
        {
            get => FatorRubr.ToString("F2", CultureInfo.InvariantCulture);
            set => FatorRubr = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da rubrica.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRubr { get; set; }

        [XmlElement("vrRubr")]
        public string VrRubrField
        {
            get => VrRubr.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRubr = Converter.ToDouble(value);

        }

        /// <summary>
        /// Indicativo de tipo de apuração de IR
        /// </summary>
        [XmlElement("indApurIR")]
#if INTEROP
        public IndApurIR IndApurIR { get; set; } = (IndApurIR)(-1);
#else
        public IndApurIR? IndApurIR { get; set; }
#endif

        /// <summary>
        /// Informações de desconto do empréstimo em folha
        /// </summary>
        [XmlElement("descFolha")]
        public DescFolha DescFolha { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQtdRubrField() => QtdRubr > 0;
        public bool ShouldSerializeFatorRubrField() => FatorRubr > 0;

#if INTEROP
        public bool ShouldSerializeIndApurIR() => IndApurIR != (IndApurIR)(-1);
#else
        public bool ShouldSerializeIndApurIR() => IndApurIR != null;
#endif

        #endregion ShouldSerialize

    }

    /// <summary>
    /// Informações de desconto do empréstimo em folha
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DescFolha")]
    [ComVisible(true)]
#endif
    public class DescFolha
    {
        /// <summary>
        /// Indicativo do tipo de desconto
        /// </summary>
        [XmlElement("tpDesc")]
        public TipoDesconto TpDesc { get; set; }

        /// <summary>
        /// Código da Instituição Financeira concedente do empréstimo
        /// </summary>
        [XmlElement("instFinanc")]
        public string InstFinanc { get; set; }

        /// <summary>
        /// Número do contrato referente ao empréstimo
        /// </summary>
        [XmlElement("nrDoc")]
        public string NrDoc { get; set; }

        /// <summary>
        /// Outras informações do desconto
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo referente ao detalhamento do grau de exposição do trabalhador aos agentes nocivos que 
    /// ensejam a cobrança da contribuição adicional para financiamento dos benefícios de aposentadoria especial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo2299")]
    [ComVisible(true)]
#endif
    public class InfoAgNocivo2299
    {
        /// <summary>
        /// Preencher com o código que representa o grau de exposição a agentes nocivos, conforme Tabela 02
        /// </summary>
        [XmlElement("grauExp")]
        public string GrauExp { get; set; }
    }

    /// <summary>
    /// Informação relativa a empresas enquadradas no regime de tributação Simples Nacional
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSimples")]
    [ComVisible(true)]
#endif
    public class InfoSimples
    {
        [XmlElement("indSimples")]
        public IndSimples IndSimples { get; set; }
    }

    /// <summary>
    /// Remuneração relativa a períodos anteriores, devida em função de acordos coletivos, legislação específica, 
    /// convenção coletiva de trabalho, dissídio ou conversão de licença saúde em acidente de trabalho
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerAnt2299")]
    [ComVisible(true)]
#endif
    public class InfoPerAnt2299
    {
        /// <summary>
        /// Identificação do instrumento ou situação ensejadora da remuneração relativa a períodos de apuração anteriores
        /// </summary>
        [XmlElement("ideADC")]
        public List<IdeADC2299> IdeADC { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeADC(IdeADC2299 item)
        {
            if (IdeADC == null)
            {
                IdeADC = new List<IdeADC2299>();
            }

            IdeADC.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeADC (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeADC</returns>
        public IdeADC2299 GetIdeADC(int index)
        {
            if ((IdeADC?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeADC[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeADC
        /// </summary>
        public int GetIdeADCCount => (IdeADC != null ? IdeADC.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do instrumento ou situação ensejadora da remuneração relativa a períodos de apuração anteriores
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeADC2299")]
    [ComVisible(true)]
#endif
    public class IdeADC2299
    {
        /// <summary>
        /// Data da assinatura do acordo, convenção coletiva, sentença normativa ou da conversão da licença saúde em acidente de trabalho
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAcConv { get; set; }
#else
        public DateTimeOffset DtAcConv { get; set; }
#endif

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
        /// Tipo do instrumento ou situação ensejadora da remuneração relativa a períodos de apuração anteriores
        /// </summary>
        [XmlElement("tpAcConv")]
        public string TpAcConv { get; set; }

        /// <summary>
        /// Descrição do instrumento ou situação que originou o pagamento das verbas relativas a períodos anteriores
        /// </summary>
        [XmlElement("dsc")]
        public string Dsc { get; set; }

        /// <summary>
        /// Identificação do período ao qual se referem as diferenças de remuneração
        /// </summary>
        [XmlElement("idePeriodo")]
        public List<IdePeriodo2299> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo2299 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo2299>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo</returns>
        public IdePeriodo2299 GetIdePeriodo(int index)
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

        #region ShouldSerialize

        public bool ShouldSerializeDtAcConvField() => DtAcConv > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação do período ao qual se referem as diferenças de remuneração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo2299")]
    [ComVisible(true)]
#endif
    public class IdePeriodo2299
    {
        /// <summary>
        /// Informar o período ao qual se refere o complemento de remuneração, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

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
        /// Identificação do estabelecimento e da lotação aos quais se referem as diferenças de remuneração do mês identificado no grupo superior
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLot2299> IdeEstabLot { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLot2299 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLot2299>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLot2299 GetIdeEstabLot(int index)
        {
            if ((IdeEstabLot?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstabLot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabLot
        /// </summary>
        public int GetIdePeriodoCount => (IdeEstabLot != null ? IdeEstabLot.Count : 0);
#endif
    }

    /// <summary>
    /// Informações sobre a existência de processos judiciais do trabalhador com decisão favorável quanto à não incidência de contribuições sociais e/ou Imposto de Renda
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab2299")]
    [ComVisible(true)]
#endif
    public class ProcJudTrab2299 : ProcJudTrab1200 { }

    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador que possua outros vínculos/atividades nos quais já tenha ocorrido desconto de contribuição previdenciária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab2299")]
    [ComVisible(true)]
#endif
    public class InfoMV2299 : InfoMV1200 { }

    /// <summary>
    /// Informação sobre processo judicial que suspende a exigibilidade da Contribuição Social Rescisória
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcCS2299")]
    [ComVisible(true)]
#endif
    public class ProcCS2299
    {
        /// <summary>
        /// Informar um número de processo judicial cadastrado através do evento S-1070, cujo indMatProc seja igual a [1, 7]
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }
    }

    /// <summary>
    /// Informações sobre a "quarentena" remunerada de trabalhador desligado ou outra situação de desligamento com data anterior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAposDeslig2299")]
    [ComVisible(true)]
#endif
    public class RemunAposDeslig2299
    {
        /// <summary>
        /// Indicativo de situação de remuneração após o desligamento
        /// </summary>
        [XmlElement("indRemun")]
#if INTEROP
        public IndRemun IndRemun { get; set; } = (IndRemun)(-1);
#else
        public IndRemun? IndRemun { get; set; }
#endif

        /// <summary>
        /// Preencher com a data final da quarentena a que está sujeito o trabalhador
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtFimRemun { get; set; }
#else
        public DateTimeOffset DtFimRemun { get; set; }
#endif

        [XmlElement("dtFimRemun")]
        public string DtFimRemunField
        {
            get => DtFimRemun.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFimRemun = DateTime.Parse(value);
#else
            set => DtFimRemun = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRemun() => IndRemun != (IndRemun)(-1);
#else
        public bool ShouldSerializeIndRemun() => IndRemun != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre operação de crédito consignado com garantia de FGTS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsigFGTS")]
    [ComVisible(true)]
#endif
    public class ConsigFGTS
    {
        /// <summary>
        /// Matrícula da instituição consignatária, cadastrada na Caixa Econômica Federal
        /// </summary>
        [XmlElement("insConsig")]
        public string InsConsig { get; set; }

        /// <summary>
        /// Número do contrato de empréstimo consignado existente na instituição consignatária
        /// </summary>
        [XmlElement("nrContr")]
        public string NrContr { get; set; }
    }
}
