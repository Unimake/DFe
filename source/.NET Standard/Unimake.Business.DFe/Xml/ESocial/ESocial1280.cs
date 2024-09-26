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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1280")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações Complementares aos Eventos Periódicos
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoComplPer/v_S_01_02_00", IsNullable = false)]
    public class ESocial1280 : XMLBase
    {
        /// <summary>
        /// Evento Informações Complementares aos Eventos Periódicos
        /// </summary>
        [XmlElement("evtInfoComplPer")]
        public EvtInfoComplPer EvtInfoComplPer { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtInfoComplPer")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Informações Complementares aos Eventos Periódicos
    /// </summary>
    [Serializable()]
    public class EvtInfoComplPer
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
        public IdeEvento1280 IdeEvento1280 { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações complementares de cada um dos estabelecimentos/lotações constantes no evento S-1200.
        /// Grupo preenchido exclusivamente por empresa
        /// enquadrada nos arts. 7º a 9º da Lei 12.546/2011,
        /// conforme classificação tributária indicada no evento S1000.
        /// </summary>
        [XmlElement("infoSubstPatr")]
        public InfoSubstPatr InfoSubstPatr { get; set; }

        /// <summary>
        ///  Informação de substituição prevista na Lei 12.546/2011
        /// Grupo preenchido exclusivamente pelo Órgão Gestor
        /// de Mão de Obra - OGMO(classTrib em S-1000 = [09]),
        /// listando apenas seus códigos de lotação com
        /// operadores portuários enquadrados nos arts. 7º a 9º
        /// da Lei 12.546/2011.
        /// </summary>
        [XmlElement("infoSubstPatrOpPort")]
        public List<InfoSubstPatrOpPort> InfoSubstPatrOpPort { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoSubstPatrOpPort(InfoSubstPatrOpPort item)
        {
            if (InfoSubstPatrOpPort == null)
            {
                InfoSubstPatrOpPort = new List<InfoSubstPatrOpPort>();
            }

            InfoSubstPatrOpPort.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoSubstPatrOpPort (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoSubstPatrOpPort</returns>
        public InfoSubstPatrOpPort GetInfoSubstPatrOpPort(int index)
        {
            if ((InfoSubstPatrOpPort?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoSubstPatrOpPort[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoSubstPatrOpPort
        /// </summary>
        public int GetInfoSubstPatrOpPortCount => (InfoSubstPatrOpPort != null ? InfoSubstPatrOpPort.Count : 0);
#endif
        /// <summary>
        /// Empresas enquadradas no Simples Nacional - Atividades concomitantes
        ///  Grupo preenchido por empresa enquadrada no
        /// regime de tributação Simples Nacional com tributação
        /// previdenciária substituída e não substituída.
        /// </summary>
        [XmlElement("infoAtivConcom")]
        public InfoAtivConcom InfoAtiviConcom { get; set; }

        /// <summary>
        ///  Grupo preenchido por entidade que tenha se
        /// transformado em sociedade de fins lucrativos nos
        /// termos e no prazo da Lei 11.096/2005.
        /// </summary>
        [XmlElement("infoPercTransf11096")]
        public InfoPercTransf11096 InfoPercTransf11096 { get; set; }

    }

    #region IdeEvento1280

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1280")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    [Serializable()]
    public class IdeEvento1280
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
        /// (yyyy-MM)
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
        /// (yyyy-MM)
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
        /// Indicativo do tipo de guia. Valores válidos:
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
        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);
        public bool ShouldSerializeIndGuia() => !string.IsNullOrEmpty(IndGuia);
        #endregion ShouldSerialize
    }

    #endregion IdeEvento1280

    #region InfoSubstPatr

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatr")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações complementares de cada um dos estabelecimentos/lotações constantes no evento S-1200.
    /// </summary>
    [Serializable()]
    public class InfoSubstPatr
    {
        /// <summary>
        /// Identificação do estabelecimento/lotação.
        /// </summary>
        [XmlElement("indSubstPatr")]
        public IndicativoSubstituicaoPatronal IndicativoSubstituicaoPatronal { get; set; }

        /// <summary>
        /// Percentual não substituído pela contribuição prevista na Lei 12.546/2011.
        /// Informar 0 (zero) se indSubstPatr = [1]. Caso contrário, preencher com o percentual correspondente à razão entre a receita de atividades não relacionadas nos arts. 7o e 8o da Lei 12.546/2011 e a receita bruta total.
        /// Validação: Se indSubstPatr = [1], informar 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double PercRedContrib { get; set; }
        [XmlElement("percRedContrib")]
        public string PercRedContribField
        {
            get => PercRedContrib.ToString("F4", CultureInfo.InvariantCulture);
            set => PercRedContrib = Converter.ToDouble(value);
        }
    }

    #endregion InfoSubstPatr

    #region InfoSubstPatrOpPort

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatrOpPort")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações complementares de cada um dos estabelecimentos/lotações constantes no evento S-1200.
    /// </summary>
    [Serializable()]
    public class InfoSubstPatrOpPort
    {
        /// <summary>
        /// Identificação do estabelecimento/lotação.
        /// Informar o código atribuído pelo empregador para a
        ///lotação tributária.
        ///Validação: Deve ser um código válido e existente na
        ///Tabela de Lotações Tributárias (S-1020), com tpLotacao = [08]
        /// </summary>
        [XmlElement("codLotacao")]
        public TpLotacao CodLotacao { get; set; }
    }

    #endregion InfoSubstPatrOpPort

    #region InfoAtivConcom

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtivConcom")]
    [ComVisible(true)]
#endif
    /// <summary>
    ///  Grupo preenchido por empresa enquadrada no
    /// regime de tributação Simples Nacional com tributação
    /// previdenciária substituída e não substituída.
    /// </summary>
    [Serializable()]
    public class InfoAtivConcom
    {
        /// <summary>
        /// Informe o fator a ser utilizado para cálculo da
        /// contribuição patronal do mês dos trabalhadores
        /// envolvidos na execução das atividades enquadradas
        /// no Anexo IV em conjunto com as dos Anexos I a III e V
        /// da Lei Complementar 123/2006.
        /// </summary>
        [XmlIgnore]
        public double FatorMes { get; set; }

        [XmlElement("fatorMes")]
        public string FatorMesField
        {
            get => FatorMes.ToString("F4", CultureInfo.InvariantCulture);
            set => FatorMes = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informe o fator a ser utilizado para cálculo da
        /// contribuição patronal do décimo terceiro dos
        /// trabalhadores envolvidos na execução das atividades
        /// enquadradas no Anexo IV em conjunto com as dos
        /// Anexos I a III e V da Lei Complementar 123/2006.
        /// </summary>
        [XmlIgnore]
        public double Fator13 { get; set; }

        [XmlElement("fator13")]
        public string Fator13Field
        {
            get => Fator13.ToString("F4", CultureInfo.InvariantCulture);
            set => Fator13 = Converter.ToDouble(value);
        }
    }

    #endregion InfoAtivConcom

    #region InfoPercTranf11096

    /// <summary>
    /// Grupo preenchido por entidade que tenha se
    /// transformado em sociedade de fins lucrativos nos
    /// termos e no prazo da Lei 11.096/2005.
    /// </summary>
    [Serializable()]
    public class InfoPercTransf11096
    {
        /// <summary>
        /// Informe o percentual de contribuição social devida em
        /// caso de transformação em sociedade de fins lucrativos
        /// - Lei 11.096/2005.
        /// Valores válidos:
        /// 1 - 0,2000
        /// 2 - 0,4000
        /// 3 - 0,6000
        /// 4 - 0,8000
        /// 5 - 1,0000
        /// </summary>
        [XmlElement("percTranf")]
        public PercTranf PercTranf { get; set; }
    }

    #endregion InfoPercTranf11096
}
