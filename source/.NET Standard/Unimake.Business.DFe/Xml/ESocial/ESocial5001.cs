#pragma warning disable CS1591
using System;
using System.Collections.Generic;
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
    /// <summary>
    /// S-5001 - Informações das Contribuições Sociais por Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5001")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_03_00", IsNullable = false)]
    public class ESocial5001 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Informações das Contribuições Sociais por Trabalhador
        /// </summary>
        [XmlElement("evtBasesTrab")]
        public EvtBasesTrab EvtBasesTrab { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Informações das Contribuições Sociais por Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtBasesTrab")]
    [ComVisible(true)]
#endif
    public class EvtBasesTrab
    {
        /// <summary>
        /// Versão do schema XML - Utilizado somente em tempo de serialização/desserialização, mas não é gerado no XML. Somente de uso interno da DLL para fazer tratamentos entre versões de schemas.
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "S_01_02_00";

        /// <summary>
        /// Retorna somente o valor inteiro da versão para facilitar comparações
        /// </summary>
        private int VersaoSchemaInt => Convert.ToInt32(VersaoSchema.Replace("S_", "").Replace("_", ""));

        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Identificação do evento de retorno
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento5001 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador.
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador5001 IdeTrabalhador { get; set; }

        /// <summary>
        /// Cálculo da contribuição previdenciária do segurado,
        /// incidente sobre a remuneração do período de apuração e
        /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
        /// </summary>
        [XmlElement("infoCpCalc")]
        public List<InfoCpCalc> InfoCpCalc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCpCalc(InfoCpCalc item)
        {
            if (InfoCpCalc == null)
            {
                InfoCpCalc = new List<InfoCpCalc>();
            }

            InfoCpCalc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCpCalc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCpCalc</returns>
        public InfoCpCalc GetInfoCpCalc(int index)
        {
            if ((InfoCpCalc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCpCalc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCpCalc
        /// </summary>
        public int GetInfoCpCalcCount => (InfoCpCalc != null ? InfoCpCalc.Count : 0);
#endif

        /// <summary>
        /// Informações sobre bases e valores das contribuições sociais (opcional).
        /// </summary>
        [XmlElement("infoCp")]
        public InfoCp InfoCp { get; set; }

        /// <summary>
        /// Informações sobre bases de cálculo do PIS/PASEP
        /// </summary>
        [XmlElement("infoPisPasep")]
        public InfoPisPasep InfoPisPasep { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeInfoPisPasep() => VersaoSchemaInt >= 10300;

        #endregion
    }

    /// <summary>
    /// Identificação do evento de retorno.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5001")]
    [ComVisible(true)]
#endif
    public class IdeEvento5001
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu
        /// origem ao presente arquivo de retorno ao empregador.
        /// Validação: Deve ser um recibo de entrega válido,
        /// correspondente ao arquivo que deu origem ao presente
        /// arquivo de retorno (S-1200, S-2299, S-2399 ou S-3000).
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

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
    }

    /// <summary>
    /// Identificação do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador5001")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador5001
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// Validação: Deve ser um CPF válido
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Informações complementares do trabalhador e do contrato.
        /// </summary>
        [XmlElement("infoCompl")]
        public InfoCompl5001 InfoCompl { get; set; }

        /// <summary>
        /// Informações sobre processos judiciais do trabalhador
        /// com decisão favorável quanto à não incidência ou
        /// alterações na incidência de contribuição previdenciária.
        /// </summary>
        [XmlElement("procJudTrab")]
        public List<ProcJudTrab5001> ProcJudTrab { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrab5001 item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrab5001>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrab5001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrab5001 GetProcJudTrab(int index)
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
    }

    /// <summary>
    /// Informações complementares do trabalhador e do contrato.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCompl5001")]
    [ComVisible(true)]
#endif
    public class InfoCompl5001
    {
        /// <summary>
        /// Grupo de informações da sucessão de vínculo trabalhista. Evento de origem: S-1200.
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc5001 SucessaoVinc { get; set; }

        /// <summary>
        ///  Informações relativas ao trabalho intermitente. Evento de origem: S-1200 ou S-2299.
        /// </summary>
        [XmlElement("infoInterm")]
        public List<InfoInterm5001> InfoInterm { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoInterm(InfoInterm5001 item)
        {
            if (InfoInterm == null)
            {
                InfoInterm = new List<InfoInterm5001>();
            }

            InfoInterm.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoInterm5001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoInterm</returns>
        public InfoInterm5001 GetInfoInterm(int index)
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
        /// Informações complementares contratuais do trabalhador. Evento de origem: S-1200
        /// </summary>
        [XmlElement("infoComplCont")]
        public List<InfoComplCont5001> InfoComplCont { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoComplCont(InfoComplCont5001 item)
        {
            if (InfoComplCont == null)
            {
                InfoComplCont = new List<InfoComplCont5001>();
            }

            InfoComplCont.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoComplCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoComplCont</returns>
        public InfoComplCont5001 GetInfoComplCont(int index)
        {
            if ((InfoComplCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoComplCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoComplCont
        /// </summary>
        public int GetInfoComplContCount => (InfoComplCont != null ? InfoComplCont.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas ao trabalho intermitente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoInterm5001")]
    [ComVisible(true)]
#endif
    public class InfoInterm5001 : InfoInterm1200
    {
        /// <summary>
        /// Versão do schema XML - Utilizado somente em tempo de serialização/desserialização, mas não é gerado no XML. Somente de uso interno da DLL para fazer tratamentos entre versões de schemas.
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "S_01_02_00";

        /// <summary>
        /// Retorna somente o valor inteiro da versão para facilitar comparações
        /// </summary>
        private int VersaoSchemaInt => Convert.ToInt32(VersaoSchema.Replace("S_", "").Replace("_", ""));

        /// <summary>
        /// Horas trabalhadas no dia pelo empregado com contrato de trabalho intermitente, no formato HHMM.
        /// </summary>
        [XmlElement("hrsTrab")]
        public string HrsTrab { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(HrsTrab) && VersaoSchemaInt >= 10300;

        #endregion
    }

    /// <summary>
    /// Grupo de informações da sucessão de vínculo trabalhista.
    /// Evento de origem: S-1200.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc5001")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc5001
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

        #region ShouldSerialize
        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo preenchido exclusivamente quando o evento de remuneração se referir a 
    /// trabalhador cuja categoria não estiver obrigada ao evento de início de TSVE e se não
    /// houver evento S-2300 correspondente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplCont5001")]
    [ComVisible(true)]
#endif
    public class InfoComplCont5001
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

    /// <summary>
    /// Informações sobre processos judiciais do trabalhador com decisão favorável quanto à não incidência ou alterações na incidência de contribuição previdenciária.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab5001")]
    [ComVisible(true)]
#endif
    public class ProcJudTrab5001
    {
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
    /// Cálculo da contribuição previdenciária do segurado,
    /// incidente sobre a remuneração do período de apuração e
    /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCpCalc")]
    [ComVisible(true)]
#endif
    public class InfoCpCalc
    {
        /// <summary>
        /// Código de Receita - CR da contribuição descontada do trabalhador.
        /// </summary>
        [XmlElement("tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor da contribuição do segurado, devida à Previdência Social,
        /// calculada segundo as regras da legislação em vigor, por CR.
        /// </summary>
        [XmlIgnore]
        public double VrCpSeg { get; set; }

        [XmlElement("vrCpSeg")]
        public string VrCpSegField
        {
            get => VrCpSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCpSeg = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor efetivamente descontado do segurado, corresponde a TpValor = [21] do correspondente
        /// infoCpCalc/tpCR
        /// </summary>
        [XmlIgnore]
        public double VrDescSeg { get; set; }

        [XmlElement("vrDescSeg")]
        public string VrDescSegField
        {
            get => VrDescSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescSeg = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Cálculo da contribuição previdenciária do segurado,
    /// incidente sobre a remuneração do período de apuração e
    /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCp")]
    [ComVisible(true)]
#endif
    public class InfoCp
    {
        /// <summary>
        /// Preencher com o código correspondente à classificação tributária
        /// do contribuinte, conforme Tabela08.
        /// Evento de origem: S-1000
        /// </summary>
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        /// <summary>
        /// Identificação do estabelecimento ou obra de construção civil e da lotação tributária.
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLot5001> IdeEstabLot { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLot5001 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLot5001>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLot5001 GetIdeEstabLot(int index)
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
    /// Identificação do estabelecimento ou obra de construção civil e da lotação tributária.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot5001")]
    [ComVisible(true)]
#endif
    public class IdeEstabLot5001
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
        /// Informações relativas à matrícula e categoria do trabalhador e tipos de incidências.
        /// </summary>
        [XmlElement("infoCategIncid")]
        public List<InfoCategIncid> InfoCategIncid { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCategIncid(InfoCategIncid item)
        {
            if (InfoCategIncid == null)
            {
                InfoCategIncid = new List<InfoCategIncid>();
            }

            InfoCategIncid.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCategIncid (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCategIncid</returns>
        public InfoCategIncid GetInfoCategIncid(int index)
        {
            if ((InfoCategIncid?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCategIncid[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCategIncid
        /// </summary>
        public int GetInfoCategIncidCount => (InfoCategIncid != null ? InfoCategIncid.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas à matrícula e categoria do trabalhador e tipos de incidências.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCategIncid")]
    [ComVisible(true)]
#endif
    public class InfoCategIncid
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no
        /// caso de servidor público, a matrícula constante no
        /// Sistema de Administração de Recursos Humanos do órgão.
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador,
        /// conforme Tabela 01.
        /// Validação: Se o evento de origem for S-1200, retornar o
        /// código de categoria informado nesse evento.Se o
        /// evento de origem for S-2299 ou S-2399, retornar o
        /// código de categoria existente no Registro de Eventos
        /// Trabalhistas - RET.
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Indicador de contribuição substituída.
        /// Valores válidos:
        /// 1 - Contribuição substituída integralmente
        /// 2 - Contribuição não substituída
        /// 3 - Contribuição não substituída concomitante com
        /// contribuição substituída
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("indSimples")]
#if INTEROP
        public IndSimples IndSimples { get; set; } = (IndSimples)(-1);
#else
        public IndSimples? IndSimples { get; set; }
#endif

        /// <summary>
        /// Informações sobre bases de cálculo, descontos e
        /// deduções de contribuições sociais devidas à Previdência
        /// Social e a Outras Entidades e Fundos.
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("infoBaseCS")]
        public List<InfoBaseCS> InfoBaseCS { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBaseCS(InfoBaseCS item)
        {
            if (InfoBaseCS == null)
            {
                InfoBaseCS = new List<InfoBaseCS>();
            }

            InfoBaseCS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBaseCS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBaseCS</returns>
        public InfoBaseCS GetInfoBaseCS(int index)
        {
            if ((InfoBaseCS?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoBaseCS[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoBaseCS
        /// </summary>
        public int GetInfoBaseCSCount => (InfoBaseCS != null ? InfoBaseCS.Count : 0);
#endif

        /// <summary>
        /// Cálculo das contribuições sociais devidas a Outras Entidades e Fundos.
        /// </summary>
        [XmlElement("calcTerc")]
        public List<CalcTerc> CalcTerc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCalcTerc(CalcTerc item)
        {
            if (CalcTerc == null)
            {
                CalcTerc = new List<CalcTerc>();
            }

            CalcTerc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CalcTerc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CalcTerc</returns>
        public CalcTerc GetCalcTerc(int index)
        {
            if ((CalcTerc?.Count ?? 0) == 0)
            {
                return default;
            };

            return CalcTerc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CalcTerc
        /// </summary>
        public int GetCalcTercCount => (CalcTerc != null ? CalcTerc.Count : 0);
#endif

        /// <summary>
        /// Informações de remuneração por período de referência.
        /// </summary>
        [XmlElement("infoPerRef")]
        public List<InfoPerRef> InfoPerRef { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPerRef(InfoPerRef item)
        {
            if (InfoPerRef == null)
            {
                InfoPerRef = new List<InfoPerRef>();
            }

            InfoPerRef.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPerRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPerRef</returns>
        public InfoPerRef GetInfoPerRef(int index)
        {
            if ((InfoPerRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoPerRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoPerRef
        /// </summary>
        public int GetInfoPerRefCount => (InfoPerRef != null ? InfoPerRef.Count : 0);
#endif
        #region ShouldSerialize
        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeIndSimples() => IndSimples != (IndSimples)(-1);
#else
        public bool ShouldSerializeIndSimples() => IndSimples != null;

#endif

        #endregion  ShouldSerialize
    }

    /// <summary>
    /// Informações sobre bases de cálculo, descontos e
    /// deduções de contribuições sociais devidas à Previdência
    /// Social e a Outras Entidades e Fundos.
    /// Evento de origem: S-1200, S-2299 ou S-2399.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseCS")]
    [ComVisible(true)]
#endif
    public class InfoBaseCS
    {
        /// <summary>
        /// Indicativo de 13° salário.
        /// Valores válidos:
        /// 0 - Mensal
        /// 1 - 13° salário(codIncCP em S-1010 = [12, 14, 16, 22, 26,
        /// 32, 92, 94, 96, 98])
        /// Validação: Se indApuracao = [2], preencher com[1].
        /// </summary>
        [XmlElement("ind13")]
        public string Ind13 { get; set; }

        /// <summary>
        /// Tipo de valor que influi na apuração da devida.
        /// </summary>
        [XmlElement("tpValor")]
        public string TpValor { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social devida à Previdência Social ou a Outras Entidades e Fundos, 
        /// conforme definido no campo TpValor.
        /// </summary>
        [XmlIgnore]
        public double Valor { get; set; }

        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Cálculo das contribuições sociais devidas a Outras Entidades e Fundos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CalcTerc")]
    [ComVisible(true)]
#endif
    public class CalcTerc
    {
        /// <summary>
        /// Código de Receita - CR da contribuição descontada do
        /// trabalhador.
        /// Valores válidos:
        /// 121802 - Contribuição ao SEST, descontada do transportador autônomo
        /// 122102 - Contribuição ao SENAT, descontada do transportador autônomo
        /// </summary>
        [XmlElement("tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor da contribuição social devida a Outras Entidades ou
        /// Fundos, calculada segundo a legislação em vigor, por CR.
        /// </summary>
        [XmlIgnore]
        public double VrCsSegTerc { get; set; }

        [XmlElement("vrCsSegTerc")]
        public string VrCsSegTercField
        {
            get => VrCsSegTerc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCsSegTerc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor efetivamente descontado do segurado,
        /// correspondente a tpValor = [22, 23], do correspondente calcTerc/tpCR.
        /// </summary>
        [XmlIgnore]
        public double VrDescTerc { get; set; }

        [XmlElement("vrDescTerc")]
        public string VrDescTercField
        {
            get => VrDescTerc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescTerc = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações de remuneração por período de referência.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerRef")]
    [ComVisible(true)]
#endif
    public class InfoPerRef
    {
        /// <summary>
        /// Informar o período ao qual se refere a remuneração.
        /// Origem: perApur ou campo {perRef}
        /// de S-1200/S-2299.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        /// <summary>
        /// Informar o período ao qual se refere a remuneração.
        /// Origem: perApur ou campo {perRef}
        /// de S-1200/S-2299.
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
        /// Identificação do instrumento ou situação ensejadora da
        /// remuneração relativa a períodos de apuração anteriores.
        /// Evento de origem: S-1200 ou S-2299 (exceto remunSuc,
        /// cujo evento de origem somente é S-1200).
        /// </summary>
        [XmlElement("ideADC")]
        public List<IdeADC5001> IdeADC { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeADC(IdeADC5001 item)
        {
            if (IdeADC == null)
            {
                IdeADC = new List<IdeADC5001>();
            }

            IdeADC.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeADC (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeADC</returns>
        public IdeADC5001 GetIdeADC(int index)
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

        /// <summary>
        /// Detalhamento das informações de remuneração por
        /// período de referência.Deve ser preenchido com
        /// informações de { infoPerApur }
        /// e {infoPerAnt}
        /// do S-1200 e
        /// S-2299, e de { dmDev } do S-2399, quando houver.
        /// </summary>
        [XmlElement("detInfoPerRef")]
        public List<DetInfoPerRef> DetInfoPerRef { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetInfoPerRef(DetInfoPerRef item)
        {
            if (DetInfoPerRef == null)
            {
                DetInfoPerRef = new List<DetInfoPerRef>();
            }

            DetInfoPerRef.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetInfoPerRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetInfoPerRef</returns>
        public DetInfoPerRef GetDetInfoPerRef(int index)
        {
            if ((DetInfoPerRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetInfoPerRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetInfoPerRef
        /// </summary>
        public int GetDetInfoPerRefCount => (DetInfoPerRef != null ? DetInfoPerRef.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do instrumento ou situação ensejadora da
    /// remuneração relativa a períodos de apuração anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeADC5001")]
    [ComVisible(true)]
#endif
    public class IdeADC5001
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
#if INTEROP
        public SimNaoLetra RemunSuc { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? RemunSuc { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtAcConvField() => DtAcConv > DateTimeOffset.MinValue;

#if INTEROP
        public bool ShouldSerializeRemunSuc() => RemunSuc != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeRemunSuc() => RemunSuc != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Detalhamento das informações de remuneração por
    /// período de referência.Deve ser preenchido com
    /// informações de { infoPerApur }
    /// e {infoPerAnt}
    /// do S-1200 e
    /// S-2299, e de { dmDev } do S-2399, quando houver.
    /// </summary>
    public class DetInfoPerRef
    {
        /// <summary>
        /// Indicativo de 13° salário.
        /// Valores válidos:
        /// 0 - Mensal
        /// 1 - 13° salário(codIncCP em S-1010 = [12, 14, 16, 22, 26,
        /// 32, 92, 94, 96, 98])
        /// Validação: Se indApuracao = [2], preencher com[1].
        /// </summary>
        [XmlElement("ind13")]
        public string Ind13 { get; set; }

        /// <summary>
        /// Tipo de valor que influi na apuração da contribuição devida.
        /// </summary>
        [XmlElement("tpVrPerRef")]
        public string TpVrPerRef { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social, conforme definido no campo tpVrPerRef.
        /// </summary>
        [XmlIgnore]
        public double VrPerRef { get; set; }

        [XmlElement("vrPerRef")]
        public string VrPerRefField
        {
            get => VrPerRef.ToString("F2", CultureInfo.InvariantCulture);
            set => VrPerRef = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações sobre bases de cálculo do PIS/PASEP informadas nos eventos S-1200, S-2299, S-2399 ou S-1202.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPisPasep")]
    [ComVisible(true)]
#endif
    public class InfoPisPasep
    {
        /// <summary>
        /// Identificação do estabelecimento ou obra de construção civil.
        /// </summary>
        [XmlElement("ideEstab")]
        public List<IdeEstab5001> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista.
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab5001 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab5001>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab.</returns>
        public IdeEstab5001 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            }

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstab.
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do estabelecimento ou obra de construção civil.
    /// </summary>
    public class IdeEstab5001
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05.
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações relativas à matrícula e categoria do trabalhador.
        /// </summary>
        [XmlElement("infoCategPisPasep")]
        public List<InfoCategPisPasep> InfoCategPisPasep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista.
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCategPisPasep(InfoCategPisPasep item)
        {
            if (InfoCategPisPasep == null)
            {
                InfoCategPisPasep = new List<InfoCategPisPasep>();
            }

            InfoCategPisPasep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCategPisPasep.
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado.</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCategPisPasep.</returns>
        public InfoCategPisPasep GetInfoCategPisPasep(int index)
        {
            if ((InfoCategPisPasep?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfoCategPisPasep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCategPisPasep.
        /// </summary>
        public int GetInfoCategPisPasepCount => (InfoCategPisPasep != null ? InfoCategPisPasep.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas à matrícula e categoria do trabalhador.
    /// </summary>
    public class InfoCategPisPasep
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador, conforme Tabela 01.
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Informações sobre bases de cálculo do PIS/PASEP.
        /// </summary>
        [XmlElement("infoBasePisPasep")]
        public List<InfoBasePisPasep> InfoBasePisPasep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista.
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBasePisPasep(InfoBasePisPasep item)
        {
            if (InfoBasePisPasep == null)
            {
                InfoBasePisPasep = new List<InfoBasePisPasep>();
            }

            InfoBasePisPasep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBasePisPasep.
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado.</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBasePisPasep.</returns>
        public InfoBasePisPasep GetInfoBasePisPasep(int index)
        {
            if ((InfoBasePisPasep?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfoBasePisPasep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoBasePisPasep.
        /// </summary>
        public int GetInfoBasePisPasepCount => (InfoBasePisPasep != null ? InfoBasePisPasep.Count : 0);
#endif
    }

    /// <summary>
    /// Informações sobre bases de cálculo do PIS/PASEP.
    /// </summary>
    public class InfoBasePisPasep
    {
        /// <summary>
        /// Indicativo de 13° salário.
        /// </summary>
        [XmlElement("ind13")]
        public IndicativoDecimoTerceiro Ind13 { get; set; }

        /// <summary>
        /// Tipo de valor que influi na apuração da contribuição devida.
        /// </summary>
        [XmlElement("tpValorPisPasep")]
        public TipoValorApuracaoContribuicao TpValorPisPasep { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição social devida ao PIS/PASEP.
        /// </summary>
        [XmlIgnore]
        public double ValorPisPasep { get; set; }

        [XmlElement("valorPisPasep")]
        public string ValorPisPasepField
        {
            get => ValorPisPasep.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorPisPasep = Converter.ToDouble(value);
        }
    }
}
