#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1200 - Remuneração de Trabalhador vinculado ao Regime Geral de Previd. Social
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1200")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRemun/v_S_01_02_00", IsNullable = false)]
    public class ESocial1200 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Remuneração de Trabalhador vinculado ao RGPS
        /// </summary>
        [XmlElement("evtRemun")]
        public EvtRemun EvtRemun { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Remuneração de Trabalhador vinculado ao RGPS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtRemun")]
    [ComVisible(true)]
#endif
    public class EvtRemun
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
        public IdeEvento1200 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador IdeTrabalhador { get; set; }

        /// <summary>
        /// Demonstrativo de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("dmDev")]
        public List<DmDev> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDev GetDmDev(int index)
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
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1200")]
    [ComVisible(true)]
#endif
    public class IdeEvento1200
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
        [XmlElement("perApur")]
        public string PerApur { get; set; }

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

#if INTEROP
        public bool ShouldSerializeIndGuia() => IndGuia != (IndGuia)(-1);
#else
        public bool ShouldSerializeIndGuia() => IndGuia != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// Validação: Deve ser um CPF válido
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Informação de múltiplos vínculos
        /// </summary>
        [XmlElement("infoMV")]
        public InfoMV1200 InfoMV { get; set; }

        /// <summary>
        /// Informações complementares de identificação do trabalhador
        /// </summary>
        [XmlElement("infoComplem")]
        public InfoComplem InfoComplem { get; set; }

        /// <summary>
        /// Informações complementares de identificação do trabalhador
        /// </summary>
        [XmlElement("procJudTrab")]
        public List<ProcJudTrab1200> ProcJudTrab { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrab1200 item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrab1200>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrab1200 GetProcJudTrab(int index)
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
        /// Informações relativas ao trabalho intermitente
        /// </summary>
        [XmlElement("infoInterm")]
        public List<InfoInterm1200> InfoInterm { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoInterm(InfoInterm1200 item)
        {
            if (InfoInterm == null)
            {
                InfoInterm = new List<InfoInterm1200>();
            }

            InfoInterm.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoInterm1200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoInterm</returns>
        public InfoInterm1200 GetInfoInterm(int index)
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
    }

    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador
    /// que possua outros vínculos/atividades nos quais já tenha
    /// ocorrido desconto de contribuição previdenciária.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMV1200")]
    [ComVisible(true)]
#endif
    public class InfoMV1200
    {
        /// <summary>
        /// Indicador de desconto da contribuição previdenciária do trabalhador.
        /// </summary>
        [XmlElement("indMV")]
        public IndMV IndMV { get; set; }

        /// <summary>
        /// Remuneração recebida pelo trabalhador em outras empresas ou atividades
        /// </summary>
        [XmlElement("remunOutrEmpr")]
        public List<RemunOutrEmpr> RemunOutrEmpr { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemunOutrEmpr(RemunOutrEmpr item)
        {
            if (RemunOutrEmpr == null)
            {
                RemunOutrEmpr = new List<RemunOutrEmpr>();
            }

            RemunOutrEmpr.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RemunOutrEmpr (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RemunOutrEmpr</returns>
        public RemunOutrEmpr GetRemunOutrEmpr(int index)
        {
            if ((RemunOutrEmpr?.Count ?? 0) == 0)
            {
                return default;
            };

            return RemunOutrEmpr[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RemunOutrEmpr
        /// </summary>
        public int GetRemunOutrEmprCount => (RemunOutrEmpr != null ? RemunOutrEmpr.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas ao trabalhador que possui vínculo
    /// empregatício com outra(s) empresa(s) e/ou que exerce
    /// outras atividades como contribuinte individual,
    /// detalhando as empresas que efetuaram(ou efetuarão)
    /// desconto da contribuição.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunOutrEmpr")]
    [ComVisible(true)]
#endif
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
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Preencher com o valor da remuneração recebida pelo
        /// trabalhador na outra empresa/atividade, sobre a qual
        /// houve desconto/recolhimento da contribuição do segurado.
        /// </summary>
        //[XmlIgnore]
        [XmlIgnore]
        public double VlrRemunOE { get; set; }

        /// <summary>
        /// Preencher com o valor da remuneração recebida pelo
        /// trabalhador na outra empresa/atividade, sobre a qual
        /// houve desconto/recolhimento da contribuição do segurado.
        /// </summary>
        [XmlElement("vlrRemunOE")]
        public string VlrRemunOEField
        {
            get => VlrRemunOE.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRemunOE = Converter.ToDouble(value);
        }
    }

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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplem")]
    [ComVisible(true)]
#endif
    public class InfoComplem
    {
        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
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
        /// Grupo de informações da sucessão de vínculo trabalhista
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc1200 SucessaoVinc { get; set; }

    }

    /// <summary>
    /// Grupo de informações da sucessão de vínculo trabalhista.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc1200")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc1200
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

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);
        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre a existência de processos judiciais do
    /// trabalhador com decisão favorável quanto à não
    /// incidência de contribuições sociais e/ou Imposto de Renda.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab1200")]
    [ComVisible(true)]
#endif
    public class ProcJudTrab1200
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

    /// <summary>
    /// Informações relativas ao trabalho intermitente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoInterm1200")]
    [ComVisible(true)]
#endif
    public class InfoInterm1200
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
        public string Dia { get; set; }
    }

    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev")]
    [ComVisible(true)]
#endif
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
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRRA { get; set; }
#endif

        /// <summary>
        /// Informações complementares de RRA
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA1200 InfoRRA { get; set; }

        /// <summary>
        /// Informações relativas ao período de apuração
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur InfoPerApur { get; set; }

        /// <summary>
        /// Informações relativas a períodos anteriores
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfoPerAnt1200 InfoPerAnt { get; set; }

        /// <summary>
        /// Informações complementares contratuais do trabalhador
        /// </summary>
        [XmlElement("infoComplCont")]
        public InfoComplCont InfoComplCont { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif

        #endregion ShouldSerialize

    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA1200")]
    [ComVisible(true)]
#endif
    public class InfoRRA1200 : InfoRRA1202 { }

    /// <summary>
    /// Informações relativas ao período de apuração. 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur")]
    [ComVisible(true)]
#endif
    public class InfoPerApur
    {
        /// <summary>
        /// Identificação do estabelecimento e lotação
        /// </summary>
        [XmlElement("ideEstabLot")]
        public IdeEstabLot IdeEstabLot { get; set; }
    }

    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o
    /// trabalhador possui remuneração no período de apuração.
    /// O estabelecimento identificado no grupo pode ser: o
    /// número do CNPJ do estabelecimento da própria empresa
    /// (matriz/filial), o número da obra(própria) no CNO, ou o
    /// número do CAEPF(no caso de pessoa física obrigada a
    /// inscrição no Cadastro de Atividade Econômica da Pessoa Física).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot")]
    [ComVisible(true)]
#endif
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
        /// Quantidade de dias trabalhados no mês pelo trabalhador
        /// avulso no tomador de serviços identificado em
        /// ideEstabLot/codLotacao.Cada dia, total ou parcial, em
        /// que o trabalhador tenha prestado serviços ao tomador
        /// deve ser considerado.Ex.: Se, em um mesmo mês, o
        /// trabalhador prestou serviços durante uma hora em um dia
        /// e durante mais uma hora em outro dia, deve-se informar a
        /// quantidade de 2 dias.
        /// Caso não tenha havido trabalho no mês, informar 0 (zero).
        /// Validação: Informação obrigatória e exclusiva se
        /// ideEstabLot/codLotacao possuir tpLotacao em S-1020 =
        /// [08, 09] em perApur e se indApuracao = [1].
        /// </summary>
        [XmlElement("qtdDiasAv")]
        public string QtdDiasAv { get; set; }

        /// <summary>
        /// Remuneração do trabalhador
        /// </summary>
        [XmlElement("remunPerApur")]
        public List<RemunPerApur1200> RemunPerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemunPerApur(RemunPerApur1200 item)
        {
            if (RemunPerApur == null)
            {
                RemunPerApur = new List<RemunPerApur1200>();
            }

            RemunPerApur.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RemunPerApur1200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RemunPerApur</returns>
        public RemunPerApur1200 GetRemunPerApur(int index)
        {
            if ((RemunPerApur?.Count ?? 0) == 0)
            {
                return default;
            };

            return RemunPerApur[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RemunPerApur
        /// </summary>
        public int GetRemunPerApurCount => (RemunPerApur != null ? RemunPerApur.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeQtdDiasAv() => !QtdDiasAv.IsNullOrEmpty();

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Remuneração do trabalhador 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerApur1200")]
    [ComVisible(true)]
#endif
    public class RemunPerApur1200
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
#if INTEROP
        public IndSimples IndSimples { get; set; } = (IndSimples)(-1);
#else
        public IndSimples? IndSimples { get; set; }
#endif

        /// <summary>
        /// Itens da remuneração do trabalhador
        /// </summary>
        [XmlElement("itensRemun")]
        public List<ItensRemun1200> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun1200(ItensRemun1200 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemun1200>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun1200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun1200</returns>
        public ItensRemun1200 GetItensRemun1200(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemun1200
        /// </summary>
        public int GetItensRemun1200Count => (ItensRemun != null ? ItensRemun.Count : 0);
#endif

        /// <summary>
        /// Grau de exposição a agentes nocivos
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo1200 InfoAgNocivo { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeIndSimples() => IndSimples != (IndSimples)(-1);
#else
        public bool ShouldSerializeIndSimples() => IndSimples != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Rubricas que compõem a remuneração do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemun1200")]
    [ComVisible(true)]
#endif
    public class ItensRemun1200 : ItensRemun { }

    /// <summary>
    /// Grau de exposição a agentes nocivos 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAgNocivo1200")]
    [ComVisible(true)]
#endif
    public class InfoAgNocivo1200
    {
        /// <summary>
        /// O (se codCateg = [1XX, 2XX,3XX, 731, 734, 738] ou se codCateg = [4XX] com
        /// {categOrig} em S-2300 = [1XX,2XX, 3XX, 731, 734, 738]); N(nos demais casos)
        /// </summary>
        [XmlElement("grauExp")]
        public string GrauExp { get; set; }

    }

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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerAnt1200")]
    [ComVisible(true)]
#endif
    public class InfoPerAnt1200
    {
        /// <summary>
        /// Instrumento ou situação ensejadora da remuneração em períodos anteriores
        /// </summary>
        [XmlElement("ideADC")]
        public List<IdeADC> IdeADC { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeADC(IdeADC item)
        {
            if (IdeADC == null)
            {
                IdeADC = new List<IdeADC>();
            }

            IdeADC.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeADC (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeADC</returns>
        public IdeADC GetIdeADC(int index)
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
    /// Identificação do instrumento ou situação ensejadora da
    /// remuneração relativa a períodos de apuração anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeADC")]
    [ComVisible(true)]
#endif
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
        public string TpAcConv { get; set; }

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
        /// Identificação do período de referência da remuneração
        /// </summary>
        [XmlElement("idePeriodo")]
        public List<IdePeriodo1200> IdePeriodo1200 { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo1200(IdePeriodo1200 item)
        {
            if (IdePeriodo1200 == null)
            {
                IdePeriodo1200 = new List<IdePeriodo1200>();
            }

            IdePeriodo1200.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo1200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo1200</returns>
        public IdePeriodo1200 GetIdePeriodo1200(int index)
        {
            if ((IdePeriodo1200?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo1200[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo1200
        /// </summary>
        public int GetIdePeriodo1200Count => (IdePeriodo1200 != null ? IdePeriodo1200.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtAcConvField() => DtAcConv > DateTimeOffset.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    ///  Identificação do período ao qual se referem as diferenças de remuneração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo1200")]
    [ComVisible(true)]
#endif
    public class IdePeriodo1200
    {
        /// <summary>
        /// Informar o período ao qual se refere o complemento de remuneração, no formato AAAA-MM.
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
        /// Identificação do estabelecimento e lotação
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLotAnt> IdeEstabLot { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLotAnt(IdeEstabLotAnt item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLotAnt>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLotAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLotAnt</returns>
        public IdeEstabLotAnt GetIdeEstabLotAnt(int index)
        {
            if ((IdeEstabLot?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstabLot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabLotAnt
        /// </summary>
        public int GetIdeEstabLotAntCount => (IdeEstabLot != null ? IdeEstabLot.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do estabelecimento e da lotação ao qual se
    /// referem as diferenças de remuneração do mês
    /// identificado no grupo superior.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLotAnt")]
    [ComVisible(true)]
#endif
    public class IdeEstabLotAnt
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
        /// Informações relativas à remuneração do trabalhador em períodos anteriores
        /// </summary>
        [XmlElement("remunPerAnt")]
        public List<RemunPerAnt1200> RemunPerAnt { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemunPerAnt1200(RemunPerAnt1200 item)
        {
            if (RemunPerAnt == null)
            {
                RemunPerAnt = new List<RemunPerAnt1200>();
            }

            RemunPerAnt.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RemunPerAnt1200 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RemunPerAnt1200</returns>
        public RemunPerAnt1200 GetRemunPerAnt1200(int index)
        {
            if ((RemunPerAnt?.Count ?? 0) == 0)
            {
                return default;
            };

            return RemunPerAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RemunPerAnt1200
        /// </summary>
        public int GetRemunPerAnt1200Count => (RemunPerAnt != null ? RemunPerAnt.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas à remuneração do trabalhador em períodos anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerAnt1200")]
    [ComVisible(true)]
#endif
    public class RemunPerAnt1200
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público, 
        /// a matrícula constante no Sistema de Administração de Recursos Humanos do órgão
        /// </summary>
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
#if INTEROP
        public IndSimples IndSimples { get; set; } = (IndSimples)(-1);
#else
        public IndSimples? IndSimples { get; set; }
#endif

        /// <summary>
        /// Rubricas que compõem a remuneração do trabalhador
        /// </summary>
        [XmlElement("itensRemun")]
        public List<ItensRemun1200> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun(ItensRemun1200 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemun1200>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun</returns>
        public ItensRemun GetItensRemun(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemun
        /// </summary>
        public int GetItensRemunCount => (ItensRemun != null ? ItensRemun.Count : 0);
#endif

        /// <summary>
        /// Grupo referente ao detalhamento do grau de exposição do trabalhador aos agentes nocivos que 
        /// ensejam a cobrança da contribuição adicional para financiamento dos benefícios de aposentadoria especial
        /// </summary>
        [XmlElement("infoAgNocivo")]
        public InfoAgNocivo1200 InfoAgNocivo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeIndSimples() => IndSimples != (IndSimples)(-1);
#else
        public bool ShouldSerializeIndSimples() => IndSimples != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
    /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
    /// houver evento S-2300 correspondente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplCont")]
    [ComVisible(true)]
#endif
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
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Informação prestada exclusivamente pelo segurado especial em caso de 
        /// contratação de contribuinte individual, indicando a quantidade de dias trabalhados pelo mesmo.
        /// Caso não tenha havido trabalho no mês, informar 0 (zero).
        /// Validação: Preenchimento obrigatório e exclusivo se classTrib em S-1000 = [22], natAtividade = [2]
        /// e indApuracao = [1]. Neste caso, preencher com um número entre 0 e 31, de acordo com o calendário anual.
        /// </summary>
        [XmlElement("qtdDiasTrab")]
        public string QtdDiasTrab { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        public bool ShouldSerializeQtdDiasTrab() => !string.IsNullOrEmpty(QtdDiasTrab);

        #endregion ShouldSerialize

    }
}
