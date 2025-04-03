#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1070 - Tabela de Processos Administrativos/Judiciais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1070")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabProcesso/v_S_01_03_00", IsNullable = false)]
    public class ESocial1070 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Tabela de Processos
        /// </summary>
        [XmlElement("evtTabProcesso")]
        public EvtTabProcesso EvtTabProcesso { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Tabela de Processos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabProcesso")]
    [ComVisible(true)]
#endif
    public class EvtTabProcesso
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
        public IdeEvento1070 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do processo
        /// </summary>
        [XmlElement("infoProcesso")]
        public InfoProcessoESocial InfoProcesso { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1070")]
    [ComVisible(true)]
#endif
    public class IdeEvento1070 : IdeEvento { }

    /// <summary>
    /// Informações do processo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcessoESocial")]
    [ComVisible(true)]
#endif
    public class InfoProcessoESocial
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1070 Inclusao { get; set; }

        /// <summary>
        /// Alteração das informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1070 Alteracao { get; set; }

        /// <summary>
        /// Exclusão das informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclusao1070 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1070")]
    [ComVisible(true)]
#endif
    public class Inclusao1070
    {
        /// <summary>
        /// Identificação do processo e validade das informações
        /// </summary>
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }

        /// <summary>
        /// Dados do processo
        /// </summary>
        [XmlElement("dadosProc")]
        public DadosProc DadosProc { get; set; }
    }

    /// <summary>
    /// Identificação do processo e validade das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcesso")]
    [ComVisible(true)]
#endif
    public class IdeProcesso
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcessoESocial TpProc { get; set; }

        /// <summary>
        /// Validação: Deve ser um número de processo válido e:
        /// a) Se tpProc = [1], deve possuir 17 (dezessete) ou 21 (vinte e um) algarismos;
        /// b) Se tpProc = [2], deve possuir 20 (vinte) algarismos;
        /// c) Se tpProc = [4], deve possuir 16 (dezesseis) algarismos.
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
#else
        public DateTimeOffset IniValid { get; set; }
#endif

        [XmlElement("iniValid")]
        public string IniValidField
        {
            get => IniValid.ToString("yyyy-MM");
#if INTEROP
            set => IniValid = DateTime.Parse(value);
#else
            set => IniValid = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se 
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
#else
        public DateTimeOffset FimValid { get; set; }
#endif

        [XmlElement("fimValid")]
        public string FimValidField
        {
            get => FimValid.ToString("yyyy-MM");
#if INTEROP
            set => FimValid = DateTime.Parse(value);
#else
            set => FimValid = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize
        public bool ShouldSerializeFimValidField() => FimValid > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Dados do processo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosProc")]
    [ComVisible(true)]
#endif
    public class DadosProc
    {
        /// <summary>
        /// Indicativo da autoria da ação judicial.
        /// Valores válidos: 
        /// 1 - Próprio contribuinte
        /// 2 - Outra entidade, empresa ou empregado
        /// Validação: Preenchimento obrigatório se tpProc = [2].
        /// </summary>
        [XmlElement("indAutoria")]
#if INTEROP
        public IndicativoAutoriaAcaoJudicial IndAutoria { get; set; } = (IndicativoAutoriaAcaoJudicial)(-1);
#else
        public IndicativoAutoriaAcaoJudicial? IndAutoria { get; set; }
#endif

        /// <summary>
        /// Indicativo da matéria do processo
        /// </summary>
        [XmlElement("indMatProc")]
        public IndicativoMateriaProcesso IndMatProc { get; set; }

        /// <summary>
        /// Observações relacionadas ao processo
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Informações complementares do processo judicial
        /// </summary>
        [XmlElement("dadosProcJud")]
        public DadosProcJud1070 DadosProcJud { get; set; }

        /// <summary>
        /// Informações de suspensão de exigibilidade de tributos
        /// </summary>
        [XmlElement("infoSusp")]
        public List<InfoSusp1070> InfoSusp { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoSusp(InfoSusp1070 item)
        {
            if (InfoSusp == null)
            {
                InfoSusp = new List<InfoSusp1070>();
            }

            InfoSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoSusp1070 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoSusp</returns>
        public InfoSusp1070 GetInfoSusp(int index)
        {
            if ((InfoSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoSusp
        /// </summary>
        public int GetInfoSuspCount => (InfoSusp != null ? InfoSusp.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndAutoria() => IndAutoria != (IndicativoAutoriaAcaoJudicial)(-1);
#else
        public bool ShouldSerializeIndAutoria() => IndAutoria != null;
#endif

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

    /// <summary>
    /// Informações complementares do processo judicial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosProcJud1070")]
    [ComVisible(true)]
#endif
    public class DadosProcJud1070 : DadosProcJud { }

    /// <summary>
    /// Informações de suspensão de exigibilidade de tributos em virtude de processo administrativo ou judicial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSusp1070")]
    [ComVisible(true)]
#endif
    public class InfoSusp1070 : InfoSusp { }

    /// <summary>
    /// Alteração das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1070")]
    [ComVisible(true)]
#endif
    public class Alteracao1070
    {
        /// <summary>
        /// Identificação do processo e validade das informações
        /// </summary>
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }

        /// <summary>
        /// Dados do processo
        /// </summary>
        [XmlElement("dadosProc")]
        public DadosProc DadosProc { get; set; }

        /// <summary>
        /// Novo período de validade das informações
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1070 NovaValidade { get; set; }
    }

    /// <summary>
    /// Informação preenchida exclusivamente em caso de alteração do período de validade das informações, apresentando o novo período de validade
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1070")]
    [ComVisible(true)]
#endif
    public class NovaValidade1070 : NovaValidade1005 { }

    /// <summary>
    /// Exclusão das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1070")]
    [ComVisible(true)]
#endif
    public class Exclusao1070
    {
        /// <summary>
        /// Identificação do processo e validade das informações
        /// </summary>
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }
    }
}
