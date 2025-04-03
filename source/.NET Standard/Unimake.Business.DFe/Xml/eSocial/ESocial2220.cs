#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2220 - Monitoramento da Saúde do Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2220")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_03_00", IsNullable = false)]
    public class ESocial2220 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Monitoramento da Saúde do Trabalhador
        /// </summary>
        [XmlElement("evtMonit")]
        public EvtMonit EvtMonit { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Monitoramento da Saúde do Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtMonit")]
    [ComVisible(true)]
#endif
    public class EvtMonit
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
        public IdeEvento2220 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2220 IdeVinculo { get; set; }

        /// <summary>
        /// Informações do exame médico ocupacional
        /// </summary>
        [XmlElement("exMedOcup")]
        public ExMedOcup ExMedOcup { get; set; }
    }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2220")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2220 : IdeVinculo2206 { }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2220")]
    [ComVisible(true)]
#endif
    public class IdeEvento2220 : IdeEvento2205 { }

    /// <summary>
    /// Informações do exame médico ocupacional
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ExMedOcup")]
    [ComVisible(true)]
#endif
    public class ExMedOcup
    {
        /// <summary>
        /// Tipo do exame médico ocupacional
        /// </summary>
        [XmlElement("tpExameOcup")]
        public TpExameOcup TpExameOcup { get; set; }

        /// <summary>
        /// Detalhamento das informações do Atestado de Saúde Ocupacional - ASO
        /// </summary>
        [XmlElement("aso")]
        public Aso Aso { get; set; }

        /// <summary>
        /// Informações sobre o médico responsável/coordenador do PCMSO
        /// </summary>
        [XmlElement("respMonit")]
        public RespMonit RespMonit { get; set; }
    }

    /// <summary>
    /// Detalhamento das informações do Atestado de Saúde Ocupacional - ASO
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Aso")]
    [ComVisible(true)]
#endif
    public class Aso
    {
        /// <summary>
        /// Data de emissão do ASO.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAso { get; set; }
#else
        public DateTimeOffset DtAso { get; set; }
#endif

        [XmlElement("dtAso")]
        public string DtAsoField
        {
            get => DtAso.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAso = DateTime.Parse(value);
#else
            set => DtAso = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Resultado do ASO
        /// </summary>
        [XmlElement("resAso")]
#if INTEROP
        public ResAso ResAso { get; set; } = (ResAso)(-1);
#else
        public ResAso? ResAso { get; set; }
#endif

        /// <summary>
        /// Grupo que detalha as avaliações clínicas e os exames complementares porventura realizados pelo trabalhador 
        /// em virtude do determinado nos Anexos da NR-07, além de outros solicitados pelo médico e os referentes ao ASO
        /// </summary>
        [XmlElement("exame")]
        public List<Exame> Exame { get; set; }

        /// <summary>
        /// Informações sobre o médico emitente do ASO
        /// </summary>
        [XmlElement("medico")]
        public Medico Medico { get; set; }


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddExame(Exame item)
        {
            if (Exame == null)
            {
                Exame = new List<Exame>();
            }

            Exame.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Exame (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Exame</returns>
        public Exame GetExame(int index)
        {
            if ((Exame?.Count ?? 0) == 0)
            {
                return default;
            };

            return Exame[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Exame
        /// </summary>
        public int GetExameCount => (Exame != null ? Exame.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeResAso() => ResAso != (ResAso)(-1);
#else
        public bool ShouldSerializeResAso() => ResAso != null;
#endif

        #endregion
    }

    /// <summary>
    /// Grupo que detalha as avaliações clínicas e os exames complementares porventura realizados pelo 
    /// trabalhador em virtude do determinado nos Anexos da NR-07, além de outros solicitados pelo médico e os referentes ao ASO.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exame")]
    [ComVisible(true)]
#endif
    public class Exame
    {
        /// <summary>
        /// Data do exame realizado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtExm { get; set; }
#else
        public DateTimeOffset DtExm { get; set; }
#endif

        [XmlElement("dtExm")]
        public string DtExmField
        {
            get => DtExm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExm = DateTime.Parse(value);
#else
            set => DtExm = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Código do procedimento diagnóstico
        /// </summary>
        [XmlElement("procRealizado")]
        public string ProcRealizado { get; set; }

        /// <summary>
        /// Observação sobre o procedimento diagnóstico realizado
        /// </summary>
        [XmlElement("obsProc")]
        public string ObsProc { get; set; }

        /// <summary>
        /// Ordem do exame
        /// </summary>
        [XmlElement("ordExame")]
#if INTEROP
        public OrdExame OrdExame { get; set; } = (OrdExame)(-1);
#else
        public OrdExame? OrdExame { get; set; }
#endif

        /// <summary>
        /// Indicação dos resultados
        /// </summary>
        [XmlElement("indResult")]
#if INTEROP
        public IndResult IndResult { get; set; } = (IndResult)(-1);
#else
        public IndResult? IndResult { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeObsProc() => !string.IsNullOrEmpty(ObsProc);

#if INTEROP
        public bool ShouldSerializeOrdExame() => OrdExame != (OrdExame)(-1);
#else
        public bool ShouldSerializeOrdExame() => OrdExame != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndResult() => IndResult != (IndResult)(-1);
#else
        public bool ShouldSerializeIndResult() => IndResult != null;
#endif

        #endregion
    }

    /// <summary>
    /// Informações sobre o médico emitente do ASO.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Medico")]
    [ComVisible(true)]
#endif
    public class Medico
    {
        /// <summary>
        /// Preencher com o nome do médico emitente do ASO
        /// </summary>
        [XmlElement("nmMed")]
        public string NmMed { get; set; }

        /// <summary>
        /// Número de inscrição do médico emitente do ASO no Conselho Regional de Medicina - CRM
        /// </summary>
        [XmlElement("nrCRM")]
        public string NrCRM { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF de expedição do CRM
        /// </summary>
        [XmlElement("ufCRM")]
#if INTEROP
        public UFBrasil UfCRM { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfCRM { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNrCRM() => !string.IsNullOrEmpty(NrCRM);

#if INTEROP
        public bool ShouldSerializeUfCRM() => UfCRM != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfCRM() => UfCRM != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações sobre o médico responsável/coordenador do PCMSO
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RespMonit")]
    [ComVisible(true)]
#endif
    public class RespMonit
    {
        /// <summary>
        /// Preencher com o CPF do médico responsável/coordenador do PCMSO
        /// </summary>
        [XmlElement("cpfResp")]
        public string CpfResp { get; set; }

        /// <summary>
        /// Preencher com o nome do médico responsável/coordenador do PCMSO
        /// </summary>
        [XmlElement("nmResp")]
        public string NmResp { get; set; }

        /// <summary>
        /// Número de inscrição do médico responsável/coordenador do PCMSO no CRM
        /// </summary>
        [XmlElement("nrCRM")]
        public string NrCRM { get; set; }

        /// <summary>
        /// Preencher com a sigla da UF de expedição do CRM
        /// </summary>
        [XmlElement("ufCRM")]
        public UFBrasil UfCRM { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCpfResp() => !string.IsNullOrEmpty(CpfResp);

        #endregion ShouldSerialize
    }
}
