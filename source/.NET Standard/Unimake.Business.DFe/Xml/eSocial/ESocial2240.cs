#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2240 - Condições Ambientais do Trabalho - Agentes Nocivos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2240")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00", IsNullable = false)]
    public class ESocial2240 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Condições Ambientais do Trabalho - Agentes Nocivos
        /// </summary>
        [XmlElement("evtExpRisco")]
        public EvtExpRisco EvtExpRisco { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Condições Ambientais do Trabalho - Agentes Nocivos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtExpRisco")]
    [ComVisible(true)]
#endif
    public class EvtExpRisco
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
        public IdeEvento2240 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2240 IdeVinculo { get; set; }

        /// <summary>
        /// Ambiente de trabalho, atividades desempenhadas e exposição a agentes nocivos
        /// </summary>
        [XmlElement("infoExpRisco")]
        public InfoExpRisco InfoExpRisco { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2240")]
    [ComVisible(true)]
#endif
    public class IdeEvento2240 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2240")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2240
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Preencher com a matrícula do trabalhador
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre o ambiente de trabalho, atividades desempenhadas e exposição a agentes nocivos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoExpRisco")]
    [ComVisible(true)]
#endif
    public class InfoExpRisco
    {
        /// <summary>
        /// Informar a data em que o trabalhador iniciou as atividades nas condições descritas ou a data de 
        /// início da obrigatoriedade deste evento para o empregador no eSocial, a que for mais recente.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniCondicao { get; set; }
#else
        public DateTimeOffset DtIniCondicao { get; set; }
#endif

        [XmlElement("dtIniCondicao")]
        public string DtIniCondicaoField
        {
            get => DtIniCondicao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniCondicao = DateTime.Parse(value);
#else
            set => DtIniCondicao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informar a data em que o trabalhador terminou as atividades nas condições descritas
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtFimCondicao { get; set; }
#else
        public DateTimeOffset DtFimCondicao { get; set; }
#endif

        [XmlElement("dtFimCondicao")]
        public string DtFimCondicaoField
        {
            get => DtFimCondicao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFimCondicao = DateTime.Parse(value);
#else
            set => DtFimCondicao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informações relativas ao ambiente de trabalho. Somente no caso de trabalhador avulso 
        /// (código de categoria no RET igual a [2XX]) é possível declarar mais de um ambiente
        /// </summary>
        [XmlElement("infoAmb")]
        public List<InfoAmb> InfoAmb { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoAmb(InfoAmb item)
        {
            if (InfoAmb == null)
            {
                InfoAmb = new List<InfoAmb>();
            }

            InfoAmb.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoAmb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoAmb</returns>
        public InfoAmb GetInfoAmb(int index)
        {
            if ((InfoAmb?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoAmb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoAmb
        /// </summary>
        public int GetInfoAmbCount => (InfoAmb != null ? InfoAmb.Count : 0);
#endif

        /// <summary>
        /// Descrição das atividades desempenhadas
        /// </summary>
        [XmlElement("infoAtiv")]
        public InfoAtiv InfoAtiv { get; set; }

        /// <summary>
        /// Agente(s) nocivo(s) ao(s) qual(is) o trabalhador está exposto
        /// </summary>
        [XmlElement("agNoc")]
        public List<AgNoc> AgNoc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddAgNoc(AgNoc item)
        {
            if (AgNoc == null)
            {
                AgNoc = new List<AgNoc>();
            }

            AgNoc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista AgNoc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AgNoc</returns>
        public AgNoc GetAgNoc(int index)
        {
            if ((AgNoc?.Count ?? 0) == 0)
            {
                return default;
            };

            return AgNoc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AgNoc
        /// </summary>
        public int GetAgNocCount => (AgNoc != null ? AgNoc.Count : 0);
#endif

        /// <summary>
        /// Informações relativas ao responsável pelos registros ambientais
        /// </summary>
        [XmlElement("respReg")]
        public List<RespReg> RespReg { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRespReg(RespReg item)
        {
            if (RespReg == null)
            {
                RespReg = new List<RespReg>();
            }

            RespReg.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RespReg (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RespReg</returns>
        public RespReg GetRespReg(int index)
        {
            if ((RespReg?.Count ?? 0) == 0)
            {
                return default;
            };

            return RespReg[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RespReg
        /// </summary>
        public int GetRespRegCount => (RespReg != null ? RespReg.Count : 0);
#endif

        /// <summary>
        /// Observações relativas a registros ambientais
        /// </summary>
        [XmlElement("obs")]
        public Obs Obs { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtFimCondicaoField() => DtFimCondicao > DateTime.MinValue;

        #endregion
    }

    /// <summary>
    /// Informações relativas ao ambiente de trabalho. Somente no caso de trabalhador avulso (código de categoria no RET igual a [2XX]) é possível declarar mais de um ambiente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAmb")]
    [ComVisible(true)]
#endif
    public class InfoAmb
    {
        /// <summary>
        /// Informar o tipo de estabelecimento do ambiente de trabalho
        /// </summary>
        [XmlElement("localAmb")]
        public LocalAmb LocalAmb { get; set; }

        /// <summary>
        /// Descrição do lugar administrativo, na estrutura organizacional da empresa, onde o trabalhador exerce suas atividades laborais
        /// </summary>
        [XmlElement("dscSetor")]
        public string DscSetor { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Número de inscrição onde está localizado o ambiente
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Descrição das atividades desempenhadas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtiv")]
    [ComVisible(true)]
#endif
    public class InfoAtiv
    {
        /// <summary>
        /// Descrição das atividades, físicas ou mentais, realizadas pelo trabalhador, por força do poder de comando a que se submete.
        /// </summary>
        [XmlElement("dscAtivDes")]
        public string DscAtivDes { get; set; }
    }

    /// <summary>
    /// Agente(s) nocivo(s) ao(s) qual(is) o trabalhador está exposto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AgNoc")]
    [ComVisible(true)]
#endif
    public class AgNoc
    {
        /// <summary>
        /// Informar o código do agente nocivo ao qual o trabalhador está exposto. Preencher com números e pontos.
        /// </summary>
        [XmlElement("codAgNoc")]
        public string CodAgNoc { get; set; }

        /// <summary>
        /// Descrição do agente nocivo
        /// </summary>
        [XmlElement("dscAgNoc")]
        public string DscAgNoc { get; set; }

        /// <summary>
        /// Tipo de avaliação do agente nocivo
        /// </summary>
        [XmlElement("tpAval")]
#if INTEROP
        public TpAval TpAval { get; set; } = (TpAval)(-1);
#else
        public TpAval? TpAval { get; set; }
#endif

        /// <summary>
        /// Intensidade, concentração ou dose da exposição do trabalhador ao agente nocivo cujo critério de avaliação seja quantitativo
        /// </summary>
        [XmlIgnore]
        public double IntConc { get; set; }

        [XmlElement("intConc")]
        public string IntConcField
        {
            get => IntConc.ToString("F4", CultureInfo.InvariantCulture);
            set => IntConc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Limite de tolerância calculado para agentes específicos, conforme técnica de medição exigida na legislação
        /// </summary>
        [XmlIgnore]
        public double LimTol { get; set; }

        [XmlElement("limTol")]
        public string LimTolField
        {
            get => LimTol.ToString("F4", CultureInfo.InvariantCulture);
            set => LimTol = Converter.ToDouble(value);
        }

        /// <summary>
        /// Dose ou unidade de medida da intensidade ou concentração do agente
        /// </summary>
        [XmlElement("unMed")]
#if INTEROP
        public UnMed UnMed { get; set; } = (UnMed)(-1);
#else
        public UnMed? UnMed { get; set; }
#endif

        /// <summary>
        /// Técnica utilizada para medição da intensidade ou concentração
        /// </summary>
        [XmlElement("tecMedicao")]
        public string TecMedicao { get; set; }

        /// <summary>
        /// Em caso de agente nocivo incluído por determinação administrativa ou judicial, preencher com o número do processo
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Informações relativas a Equipamentos de Proteção Coletiva - EPC e Equipamentos de Proteção Individual - EPI.
        /// </summary>
        [XmlElement("epcEpi")]
        public EpcEpi EpcEpi { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscAgNoc() => !string.IsNullOrEmpty(DscAgNoc);

#if INTEROP
        public bool ShouldSerializeTpAval() => TpAval != (TpAval)(-1);
#else
        public bool ShouldSerializeTpAval() => TpAval != null;
#endif

        public bool ShouldSerializeIntConcField() => IntConc > 0;

        public bool ShouldSerializeLimTolField() => LimTol > 0;

#if INTEROP
        public bool ShouldSerializeUnMed() => UnMed != (UnMed)(-1);
#else
        public bool ShouldSerializeUnMed() => UnMed != null;
#endif

        public bool ShouldSerializeTecMedicao() => !string.IsNullOrEmpty(TecMedicao);

        public bool ShouldSerializeNrProcJud() => !string.IsNullOrEmpty(NrProcJud);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas a Equipamentos de Proteção Coletiva - EPC e Equipamentos de Proteção Individual - EPI.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EpcEpi")]
    [ComVisible(true)]
#endif
    public class EpcEpi
    {
        /// <summary>
        /// O empregador implementa medidas de proteção coletiva (EPC) para eliminar ou reduzir a exposição dos trabalhadores ao agente nocivo?
        /// </summary>
        [XmlElement("utilizEPC")]
        public UtilizEPC UtilizEPC { get; set; }

        /// <summary>
        /// Os EPCs são eficazes na neutralização do risco ao trabalhador?
        /// </summary>
        [XmlElement("eficEpc")]
#if INTEROP
        public SimNaoLetra EficEpc { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? EficEpc { get; set; }
#endif

        /// <summary>
        /// Utilização de EPI.
        /// </summary>
        [XmlElement("utilizEPI")]
        public UtilizEPI UtilizEPI { get; set; }

        [XmlElement("eficEpi")]
#if INTEROP
        public SimNaoLetra EficEpi { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? EficEpi { get; set; }
#endif

        /// <summary>
        /// EPI
        /// </summary>
        [XmlElement("epi")]
        public List<Epi> Epi { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEpi(Epi item)
        {
            if (Epi == null)
            {
                Epi = new List<Epi>();
            }

            Epi.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Epi (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Epi</returns>
        public Epi GetEpi(int index)
        {
            if ((Epi?.Count ?? 0) == 0)
            {
                return default;
            };

            return Epi[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Epi
        /// </summary>
        public int GetEpiCount => (Epi != null ? Epi.Count : 0);
#endif

        /// <summary>
        /// Requisitos da Norma Regulamentadora 06 - NR-06 e da Norma Regulamentadora 09 - NR-09 pelo(s) EPI(s) informado(s)
        /// </summary>
        [XmlElement("epiCompl")]
        public EpiCompl EpiCompl { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeEficEpc() => EficEpc != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeEficEpc() => EficEpc != null;
#endif

#if INTEROP
        public bool ShouldSerializeEficEpi() => EficEpi != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeEficEpi() => EficEpi != null;
#endif

        #endregion
    }

    /// <summary>
    /// EPI
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Epi")]
    [ComVisible(true)]
#endif
    public class Epi
    {
        /// <summary>
        /// Certificado de Aprovação - CA ou documento de avaliação do EPI.
        /// </summary>
        [XmlElement("docAval")]
        public string DocAval { get; set; }
    }

    /// <summary>
    /// Requisitos da Norma Regulamentadora 06 - NR-06 e da Norma Regulamentadora 09 - NR-09 pelo(s) EPI(s) informado(s).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EpiCompl")]
    [ComVisible(true)]
#endif
    public class EpiCompl
    {
        /// <summary>
        /// Foi tentada a implementação de medidas de proteção coletiva, de caráter administrativo ou de organização, 
        /// optando-se pelo EPI por inviabilidade técnica, insuficiência ou interinidade, ou ainda em caráter complementar ou emergencial?
        /// </summary>
        [XmlElement("medProtecao")]
        public SimNaoLetra MedProtecao { get; set; }

        /// <summary>
        /// Foram observadas as condições de funcionamento do EPI ao longo do tempo, 
        /// conforme especificação técnica do fabricante nacional ou importador, ajustadas às condições de campo?
        /// </summary>
        [XmlElement("condFuncto")]
        public SimNaoLetra CondFuncto { get; set; }

        /// <summary>
        /// Foi observado o uso ininterrupto do EPI ao longo do tempo, 
        /// conforme especificação técnica do fabricante nacional ou importador, ajustadas às condições de campo?
        /// </summary>
        [XmlElement("usoInint")]
        public SimNaoLetra UsoInint { get; set; }

        /// <summary>
        /// Foi observado o prazo de validade do CA no momento da compra do EPI?
        /// </summary>
        [XmlElement("przValid")]
        public SimNaoLetra PrzValid { get; set; }

        /// <summary>
        /// É observada a periodicidade de troca definida pelo fabricante nacional ou importador e/ou programas ambientais, 
        /// comprovada mediante recibo assinado pelo usuário em época própria?
        /// </summary>
        [XmlElement("periodicTroca")]
        public SimNaoLetra PeriodicTroca { get; set; }

        /// <summary>
        /// É observada a higienização conforme orientação do fabricante nacional ou importador?
        /// </summary>
        [XmlElement("higienizacao")]
        public SimNaoLetra Higienizacao { get; set; }
    }

    /// <summary>
    ///	Informações relativas ao responsável pelos registros ambientais.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RespReg")]
    [ComVisible(true)]
#endif
    public class RespReg
    {
        /// <summary>
        /// Preencher com o CPF do responsável pelos registros ambientais
        /// </summary>
        [XmlElement("cpfResp")]
        public string CpfResp { get; set; }

        /// <summary>
        /// Órgão de classe ao qual o responsável pelos registros ambientais está vinculado
        /// </summary>
        [XmlElement("ideOC")]
#if INTEROP
        public IdeOc IdeOc { get; set; } = (IdeOc)(-1);
#else
        public IdeOc? IdeOc { get; set; }
#endif

        /// <summary>
        /// Descrição (sigla) do órgão de classe ao qual o responsável pelos registros ambientais está vinculado.
        /// </summary>
        [XmlElement("dscOC")]
        public string DscOC { get; set; }

        /// <summary>
        /// Número de inscrição no órgão de classe.
        /// </summary>
        [XmlElement("nrOC")]
        public string NrOC { get; set; }

        /// <summary>
        /// Sigla da Unidade da Federação - UF do órgão de classe.
        /// </summary>
        [XmlElement("ufOC")]
#if INTEROP
        public UFBrasil UfOC { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfOC { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIdeOc() => IdeOc != (IdeOc)(-1);
#else
        public bool ShouldSerializeIdeOc() => IdeOc != null;
#endif

        public bool ShouldSerializeDscOC() => !string.IsNullOrEmpty(DscOC);

        public bool ShouldSerializeNrOC() => !string.IsNullOrEmpty(NrOC);

#if INTEROP
        public bool ShouldSerializeUfOC() => UfOC != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfOC() => UfOC != null;
#endif

        #endregion
    }

    /// <summary>
    /// Observações relativas a registros ambientais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Obs")]
    [ComVisible(true)]
#endif
    public class Obs
    {
        /// <summary>
        /// Observação(ões) complementar(es) referente(s) a registros ambientais
        /// </summary>
        [XmlElement("obsCompl")]
        public string ObsCompl { get; set; }
    }
}
