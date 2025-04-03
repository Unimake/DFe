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
    /// S-1280 - Informações Complementares aos Eventos Periódicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1280")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoComplPer/v_S_01_03_00", IsNullable = false)]
    public class ESocial1280 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Informações Complementares aos Eventos Periódicos
        /// </summary>
        [XmlElement("evtInfoComplPer")]
        public EvtInfoComplPer EvtInfoComplPer { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Informações Complementares aos Eventos Periódicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtInfoComplPer")]
    [ComVisible(true)]
#endif
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
        public IdeEvento1280 IdeEvento { get; set; }

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
        /// Informação de substituição prevista na Lei 12.546/2011
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
        /// Grupo preenchido por empresa enquadrada no
        /// regime de tributação Simples Nacional com tributação
        /// previdenciária substituída e não substituída.
        /// </summary>
        [XmlElement("infoAtivConcom")]
        public InfoAtivConcom InfoAtiviConcom { get; set; }

        /// <summary>
        /// Grupo preenchido por entidade que tenha se
        /// transformado em sociedade de fins lucrativos nos
        /// termos e no prazo da Lei 11.096/2005.
        /// </summary>
        [XmlElement("infoPercTransf11096")]
        public InfoPercTransf11096 InfoPercTransf11096 { get; set; }

    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1280")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEvento1280 : IdeEvento1200 { }

    /// <summary>
    /// Informações complementares de cada um dos estabelecimentos/lotações constantes no evento S-1200.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatr")]
    [ComVisible(true)]
#endif
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
            get => PercRedContrib.ToString("F2", CultureInfo.InvariantCulture);
            set => PercRedContrib = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações complementares de cada um dos estabelecimentos/lotações constantes no evento S-1200.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatrOpPort")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoSubstPatrOpPort
    {
        /// <summary>
        /// Identificação do estabelecimento/lotação.
        /// Informar o código atribuído pelo empregador para a
        /// lotação tributária.
        /// Validação: Deve ser um código válido e existente na
        /// Tabela de Lotações Tributárias (S-1020), com tpLotacao = [08]
        /// </summary>
        [XmlElement("codLotacao")]
        public TpLotacao CodLotacao { get; set; }
    }

    /// <summary>
    /// Grupo preenchido por empresa enquadrada no
    /// regime de tributação Simples Nacional com tributação
    /// previdenciária substituída e não substituída.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtivConcom")]
    [ComVisible(true)]
#endif
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

    /// <summary>
    /// Grupo preenchido por entidade que tenha se
    /// transformado em sociedade de fins lucrativos nos
    /// termos e no prazo da Lei 11.096/2005.
    /// </summary>
    [Serializable()]
    public class InfoPercTransf11096
    {
        /// <summary>
        /// Informe o percentual de contribuição social devida em caso de transformação em 
        /// sociedade de fins lucrativos - Lei 11.096/2005.
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
}
