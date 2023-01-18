#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.SNCM
{
    /// <summary>
    /// XML para envio de todos os tipos de eventos do SNCM.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.MsgEvtIn")]
    [ComVisible(true)]
#endif
    [XmlRoot("msgEvtIn", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class MsgEvtIn : XMLBase
    {
        /// <summary>
        /// Identificador de controle da comunicação com o SNCM, gerado pelo Sistema Cliente.
        /// </summary>
        [XmlElement("docId")]
        public string DocId { get; set; }

        /// <summary>
        /// Carimbo de tempo realizado pelo Sistema Cliente no instante da comunicação com o SNCM.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CcTime { get; set; }
#else
        public DateTimeOffset CcTime { get; set; }
#endif

        [XmlElement("ccTime")]
        public string CcTimeField
        {
            get => CcTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => CcTime = DateTime.Parse(value);
#else
            set => CcTime = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Versão do Leiaute
        /// </summary>
        [XmlElement("ver")]
        public string Ver { get; set; } = "0.01";

        /// <summary>
        /// Idioma desejado para as mensagens descritivas de retorno. Padrão: pt-BR.
        /// </summary>
        [XmlElement("lc")]
        public string Lc { get; set; } = "pt-BR";

        /// <summary>
        /// Identificação do Ambiente.
        /// </summary>
        [XmlElement("env")]
        public IdentificacaoAmbiente Env { get; set; } = IdentificacaoAmbiente.Teste;

        /// <summary>
        /// Identificador do membro da cadeia.
        /// </summary>
        [XmlElement("declarant")]
        public Declarant Declarant { get; set; }

        /// <summary>
        /// Identificação do procurador que assina a comunicação (CNPJ).
        /// </summary>
        [XmlElement("mbrAgt")]
        public string MbrAgt { get; set; }

        private string UsrAgtField { get; set; }

        /// <summary>
        /// Elemento livre para o desenvolvedor do Sistema Cliente indicar seu nome, a respectiva versão do software cliente, um telefone para contato com DDD e um email.
        /// </summary>
        [XmlElement("usrAgt")]
        public string UsrAgt
        {
            get => UsrAgtField;
            set => UsrAgtField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Grupo de eventos a serem informados ao SNCM.
        /// </summary>
        [XmlElement("evts")]
        public Evts Evts { get; set; }

        /// <summary>
        /// Assinatura digital da mensagem XML.
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>
        /// Deserializar o XML msgEvtIn no objeto MsgEvtIn.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML msgEvtIn</param>
        /// <returns>Objeto do MsgEvtIn</returns>
        public MsgEvtIn LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MsgEvtIn>(doc);
        }

        /// <summary>
        /// Deserializar o XML msgEvtIn no objeto MsgEvtIn.
        /// </summary>
        /// <param name="xml">string do XML MsgEvtIn</param>
        /// <returns>Objeto do MsgEvtIn</returns>
        public MsgEvtIn LoadFromXML(string xml) => XMLUtility.Deserializar<MsgEvtIn>(xml);

        #region ShouldSerialize

        public bool ShouldSerializeMbrAgt() => !string.IsNullOrWhiteSpace(MbrAgt);

        #endregion
    }

    /// <summary>
    /// Grupo de eventos a serem informados ao SNCM.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Evts")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Evts
    {
        /// <summary>
        /// Grupo do evento de Ativação.
        /// </summary>
        [XmlElement("activ")]
        public Activ Activ { get; set; }

        /// <summary>
        /// Grupo do evento de Expedição.
        /// </summary>
        [XmlElement("shpt")]
        public Shpt Shpt { get; set; }

        /// <summary>
        /// Grupo do evento de Recepção.
        /// </summary>
        [XmlElement("rec")]
        public Rec Rec { get; set; }

        /// <summary>
        /// Grupo do evento de Finalização.
        /// </summary>
        [XmlElement("final")]
        public Final Final { get; set; }

        /// <summary>
        /// Grupo do evento de Revogação.
        /// </summary>
        [XmlElement("evtRvctn")]
        public EvtRvctn EvtRvctn { get; set; }
    }

    /// <summary>
    /// Grupo do evento de Ativação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Activ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Activ
    {
        /// <summary>
        /// Identificador da notificação da instância de evento, gerado localmente pelo sistema cliente.
        /// </summary>
        [XmlElement("evtNotifId")]
        public string EvtNotifId { get; set; }

        /// <summary>
        /// Indica que a instância de evento está sendo comunicada em tempo real. Valor informado deve sempre ser "true".
        /// </summary>
        [XmlElement("realTime")]
        public bool RealTime { get; set; }

        /// <summary>
        /// Data e horário em que a instância de evento ocorreu.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PastTime { get; set; }
#else
        public DateTimeOffset PastTime { get; set; }
#endif

        [XmlElement("pastTime")]
        public string PastTimeField
        {
            get => PastTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => PastTime = DateTime.Parse(value);
#else
            set => PastTime = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Instância de evento sendo substituída por esta instância de evento.
        /// </summary>
        [XmlElement("replacing")]
        public Replacing Replacing { get; set; }

        /// <summary>
        /// Elemento que informa se todos os DUIs dessa ativação são importados: false - Não são importados; true - São importados.
        /// </summary>
        [XmlElement("import")]
        public bool Import { get; set; }

        /// <summary>
        /// Grupo de informações que formam o Identificador Único de Medicamento(IUM).
        /// </summary>
        [XmlElement("dui")]
        public List<Dui> Dui { get; set; }

        /// <summary>
        /// Grupo de informações de uma embalagem de medicamento, que é composta por 1 ou mais IUMs, mas não é uma embalagem de transporte de medicamento. Ex.: Kit de medicamento.
        /// </summary>
        [XmlElement("compDui")]
        public List<CompDui> CompDui { get; set; }

        /// <summary>
        /// Para identificação do terceiro no caso de produção não realizada pelo detentor do registro
        /// </summary>
        [XmlElement("outsrc")]
        public Outsrc Outsrc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDui(Dui item)
        {
            if (Dui == null)
            {
                Dui = new List<Dui>();
            }

            Dui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dui</returns>
        public Dui GetDui(int index)
        {
            if ((Dui?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dui
        /// </summary>
        public int GetDuiCount => (Dui != null ? Dui.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCompDui(CompDui item)
        {
            if (CompDui == null)
            {
                CompDui = new List<CompDui>();
            }

            CompDui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CompDui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CompDui</returns>
        public CompDui GetCompDui(int index)
        {
            if ((CompDui?.Count ?? 0) == 0)
            {
                return default;
            };

            return CompDui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CompDui
        /// </summary>
        public int GetCompDuiCount => (CompDui != null ? CompDui.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializePastTimeField() => PastTime > DateTime.MinValue;

        #endregion
    }

    /// <summary>
    /// Grupo do evento de Expedição.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Shpt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Shpt
    {
        /// <summary>
        /// Identificador da notificação de instância de evento, atribuído pelo declarante.
        /// </summary>
        [XmlElement("evtNotifId")]
        public string EvtNotifId { get; set; }

        /// <summary>
        /// Elemento "realTime" é mutuamente exclusivo com a sequência de elementos "pastTime" e "replacing".
        /// </summary>
        [XmlElement("realTime")]
        public bool RealTime { get; set; }

        /// <summary>
        /// Data e hora da ocorrência da instância de evento.Sempre anterior à data e hora da comunicação.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PastTime { get; set; }
#else
        public DateTimeOffset PastTime { get; set; }
#endif

        [XmlElement("pastTime")]
        public string PastTimeField
        {
            get => PastTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => PastTime = DateTime.Parse(value);
#else
            set => PastTime = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Instância de evento sendo substituída por esta instância de evento.
        /// </summary>
        [XmlElement("replacing")]
        public Replacing Replacing { get; set; }

        /// <summary>
        /// Indicação se o medicamento está apropriado ou impróprio para o consumo. 
        /// </summary>
        [XmlElement("fit")]
        public bool Fit { get; set; }

        /// <summary>
        /// Elemento que informa o membro da cadeia parceiro da comunicação.
        /// </summary>
        [XmlElement("prtnr")]
        public Prtnr Prtnr { get; set; }

        /// <summary>
        /// Elemento que informa o transportador responsável pela movimentação do medicamento.
        /// </summary>
        [XmlElement("carrs")]
        public Carrs Carrs { get; set; }

        /// <summary>
        /// Grupo que define uma carga de IUMs, podendo conter tanto IUMs, quanto embalagens de transporte.
        /// </summary>
        [XmlElement("pld")]
        public Pld Pld { get; set; }

        /// <summary>
        /// Grupo que informa um documento relacionado com a movimentação de medicamentos.
        /// </summary>
        [XmlElement("bizTrans")]
        public BizTrans BizTrans { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePastTimeField() => PastTime > DateTime.MinValue;

        #endregion

    }

    /// <summary>
    /// Grupo que informa um documento relacionado com a movimentação de medicamentos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.BizTrans")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class BizTrans
    {
        /// <summary>
        /// Número identificador do documento ou código de série do documento.
        /// </summary>
        [XmlElement("bizTransId")]
        public string BizTransId { get; set; }

        private string BizTransTypeField { get; set; }

        /// <summary>
        /// Descrição do tipo de documento. Ex. NF-e, NFC-e, etc.
        /// </summary>
        [XmlElement("bizTransType")]
        public string BizTransType
        {
            get => BizTransTypeField;
            set => BizTransTypeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }
    }

    /// <summary>
    /// Grupo do evento de Recepção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Rec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Rec : Shpt { }

    /// <summary>
    /// Grupo do evento de Finalização.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Final")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Final
    {
        /// <summary>
        /// Identificador da notificação de instância de evento, atribuído pelo declarante.
        /// </summary>
        [XmlElement("evtNotifId")]
        public string EvtNotifId { get; set; }

        /// <summary>
        /// Elemento "realTime" é mutuamente exclusivo com a sequência de elementos "pastTime" e "replacing".
        /// </summary>
        [XmlElement("realTime")]
        public bool RealTime { get; set; }

        /// <summary>
        /// Data e hora da ocorrência da instância de evento.Sempre anterior à data e hora da comunicação.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PastTime { get; set; }
#else
        public DateTimeOffset PastTime { get; set; }
#endif

        [XmlElement("pastTime")]
        public string PastTimeField
        {
            get => PastTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => PastTime = DateTime.Parse(value);
#else
            set => PastTime = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Instância de evento sendo substituída por esta instância de evento.
        /// </summary>
        [XmlElement("replacing")]
        public Replacing Replacing { get; set; }

        /// <summary>
        /// Finalização por dispensação
        /// </summary>
        [XmlElement("dispn")]
        public Dispn Dispn { get; set; }

        /// <summary>
        /// Finalização por deslacre
        /// </summary>
        [XmlElement("sealBrk")]
        public SealBrk SealBrk { get; set; }

        /// <summary>
        /// Finalização por exportação
        /// </summary>
        [XmlElement("xport")]
        public XPort XPort { get; set; }

        /// <summary>
        /// Finalização por descarte/destruição.
        /// </summary>
        [XmlElement("destroy")]
        public Destroy Destroy { get; set; }

        /// <summary>
        /// Finalização por avaria, quando o descarte apropriado não é possível
        /// </summary>
        [XmlElement("damage")]
        public Damage Damage { get; set; }

        /// <summary>
        /// Finalização por extravio.
        /// </summary>
        [XmlElement("disapp")]
        public Disapp Disapp { get; set; }

        /// <summary>
        /// Finalização por roubo.
        /// </summary>
        [XmlElement("robry")]
        public Robry Robry { get; set; }

        /// <summary>
        /// Finalização por confisco.
        /// </summary>
        [XmlElement("seizure")]
        public Seizure Seizure { get; set; }

        /// <summary>
        /// Finalização pela vigilância sanitária.
        /// </summary>
        [XmlElement("snvs")]
        public Snvs Snvs { get; set; }

        /// <summary>
        /// Finalização para destinação às Forças Armadas.
        /// </summary>
        [XmlElement("military")]
        public Military Military { get; set; }

        /// <summary>
        /// Finalização para controle de qualidade
        /// </summary>
        [XmlElement("quality")]
        public Quality Quality { get; set; }

        /// <summary>
        /// Dados do documento de transação negocial relacionado com a movimentação, como NF-e, CF-e, NFC-e, etc.
        /// </summary>
        [XmlElement("bizTrans")]
        public BizTrans BizTrans { get; set; }
    }

    /// <summary>
    /// Grupo que define uma dispensação unitária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Dispn")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Dispn
    {
        /// <summary>
        /// Finalização de um IUM por dispensação
        /// </summary>
        [XmlElement("d")]
        public List<D> D { get; set; }

        /// <summary>
        /// 
        /// </summary>
        public List<Dwp> Dwp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddD(D item)
        {
            if (D == null)
            {
                D = new List<D>();
            }

            D.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista D (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da D</returns>
        public D GetD(int index)
        {
            if ((D?.Count ?? 0) == 0)
            {
                return default;
            };

            return D[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista D
        /// </summary>
        public int GetDCount => (D != null ? D.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDwp(Dwp item)
        {
            if (Dwp == null)
            {
                Dwp = new List<Dwp>();
            }

            Dwp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dwp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dwp</returns>
        public Dwp GetDwp(int index)
        {
            if ((Dwp?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dwp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dwp
        /// </summary>
        public int GetDwpCount => (Dwp != null ? Dwp.Count : 0);

#endif
    }

    /// <summary>
    /// Grupo que define uma dispensação unitária com prescrição
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Dwp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Dwp
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM);
        /// </summary>
        [XmlElement("dui")]
        public Dui Dui { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Elemento que informa a justificativa para a substituição.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Código de registro de profissional de saúde do prescritor.
        /// </summary>
        [XmlElement("presCode")]
        public string PresCode { get; set; }

        private DateTime PresDate { get; set; }

        /// <summary>
        /// Data da Prescrição
        /// </summary>
        [XmlElement("presDate")]
        public string PresDateField
        {
            get => PresDate.ToString("yyyy-MM-dd");
            set => PresDate = DateTime.Parse(value);
        }

        /// <summary>
        /// Cpf do responsável técnico da farmácia.
        /// </summary>
        [XmlElement("phcistMgr")]
        public string PhcistMgr { get; set; }

        private string PnnField { get; set; }

        /// <summary>
        /// Elemento que informa a justificativa para a substituição.
        /// </summary>
        [XmlElement("pnn")]
        public string Pnn
        {
            get => PnnField;
            set => PnnField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        private string PnnSNCRField { get; set; }

        /// <summary>
        /// Elemento que informa a justificativa para a substituição.
        /// </summary>
        [XmlElement("pnnSNCR")]
        public string PnnSNCR
        {
            get => PnnSNCRField;
            set => PnnSNCRField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Número do cpf do comprador
        /// </summary>
        [XmlElement("cons")]
        public string Cons { get; set; }

        /// <summary>
        /// Número do cpf do paciente
        /// </summary>
        [XmlElement("pat")]
        public string Pat { get; set; }

        /// <summary>
        /// Classificação Internacional de Doenças, mantido pela Organização Mundial de Saúde (OMS)
        /// </summary>
        [XmlElement("icd")]
        public string Icd { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "desc" se a propriedade "Desc" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "desc"</returns>
        public bool ShouldSerializeDesc() => !string.IsNullOrWhiteSpace(Desc);

        /// <summary>
        /// Só gera a tag "phcistMgr" se a propriedade "PhcistMgr" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "phcistMgr"</returns>
        public bool ShouldSerializePhcistMgr() => !string.IsNullOrWhiteSpace(PhcistMgr);

        /// <summary>
        /// Só gera a tag "pnn" se a propriedade "Pnn" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "pnn"</returns>
        public bool ShouldSerializePnn() => !string.IsNullOrWhiteSpace(Pnn);

        /// <summary>
        /// Só gera a tag "pnnSNCR" se a propriedade "PnnSNCR" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "pnnSNCR"</returns>
        public bool ShouldSerializePnnSNCR() => !string.IsNullOrWhiteSpace(PnnSNCR);

        /// <summary>
        /// Só gera a tag "cons" se a propriedade "Cons" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "cons"</returns>
        public bool ShouldSerializeCons() => !string.IsNullOrWhiteSpace(Cons);

        /// <summary>
        /// Só gera a tag "pat" se a propriedade "Pat" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "pat"</returns>
        public bool ShouldSerializePat() => !string.IsNullOrWhiteSpace(Pat);

        /// <summary>
        /// Só gera a tag "icd" se a propriedade "Icd" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "icd"</returns>
        public bool ShouldSerializeIcd() => !string.IsNullOrWhiteSpace(Icd);

        #endregion
    }

    /// <summary>
    /// Grupo que define uma dispensação unitária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.D")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class D
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM);
        /// </summary>
        [XmlElement("dui")]
        public Dui Dui { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Elemento que informa a justificativa para a substituição.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        /// <summary>
        /// Número de CPF, sem pontuação, do comprador.
        /// </summary>
        [XmlElement("cons")]
        public string Cons { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "desc" se a propriedade "Desc" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "desc"</returns>
        public bool ShouldSerializeDesc() => !string.IsNullOrWhiteSpace(Desc);

        /// <summary>
        /// Só gera a tag "cons" se a propriedade "Cons" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "cons"</returns>
        public bool ShouldSerializeCons() => !string.IsNullOrWhiteSpace(Cons);

        #endregion
    }

    /// <summary>
    /// Grupo do evento de Revogação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.EvtRvctn")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class EvtRvctn
    {
        /// <summary>
        /// Identificador da notificação da instância de evento, gerado localmente pelo sistema cliente.
        /// </summary>
        [XmlElement("evtNotifId")]
        public string EvtNotifId { get; set; }

        /// <summary>
        /// Instância de evento a ser tornada sem efeito.
        /// </summary>
        [XmlElement("revoking")]
        public Revoking Revoking { get; set; }
    }

    /// <summary>
    /// Instância de evento a ser tornada sem efeito.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Revoking")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Revoking : RevokedEventInstanceId { }

    /// <summary>
    /// Grupo que define a instância de evento a ser tornada sem efeito
    /// </summary>
    public abstract class RevokedEventInstanceId
    {
        /// <summary>
        /// Identificador global da instância de evento a ser substituída.
        /// </summary>
        [XmlElement("origEvtId")]
        public string OrigEvtId { get; set; }
        private string DescField { get; set; }

        /// <summary>
        /// Elemento que informa a justificativa para a substituição.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }
    }

    /// <summary>
    /// Instância de evento sendo substituída por esta instância de evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Replacing")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Replacing : RevokedEventInstanceId { }

    /// <summary>
    /// Grupo de informações que formam o Identificador Único de Medicamento (IUM).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Dui")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Dui
    {
        /// <summary>
        /// GTIN do medicamento
        /// </summary>
        [XmlElement("gtin")]
        public string Gtin { get; set; }

        /// <summary>
        /// Código serial do medicamento
        /// </summary>
        [XmlElement("serl")]
        public string Serl { get; set; }

        /// <summary>
        /// Formato: YYYY-MM
        /// </summary>
        [XmlElement("exp")]
        public string Exp { get; set; }

        /// <summary>
        /// Código do lote do medicamento
        /// </summary>
        [XmlElement("lot")]
        public string Lot { get; set; }
    }

    /// <summary>
    /// Grupo de informações de uma embalagem de medicamento, que é composta por 1 ou mais IUMs, mas não é uma embalagem de transporte de medicamento. Ex.: Kit de medicamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.CompDui")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class CompDui
    {
        /// <summary>
        /// IUM que identifica a embalagem de medicamento.
        /// </summary>
        [XmlElement("dui")]
        public Dui Dui { get; set; }

        /// <summary>
        /// Grupo que agrega um ou vários IUMs.
        /// </summary>
        [XmlElement("compnts")]
        public Compnts Compnts { get; set; }
    }

    /// <summary>
    /// Grupo que agrega um ou vários IUMs.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Compnts")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Compnts
    {
        /// <summary>
        /// Identificador da Unidade de Medicamento (IUM)
        /// </summary>
        [XmlElement("dui")]
        public List<Dui> Dui { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDui(Dui item)
        {
            if (Dui == null)
            {
                Dui = new List<Dui>();
            }

            Dui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dui</returns>
        public Dui GetDui(int index)
        {
            if ((Dui?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dui
        /// </summary>
        public int GetDuiCount => (Dui != null ? Dui.Count : 0);

#endif
    }

    /// <summary>
    /// Para identificação do terceiro no caso de produção não realizada pelo detentor do registro
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Outsrc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Outsrc : StakeholderId { }


    /// <summary>
    /// Identificador do expedidor
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.shipper")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Shipper : StakeholderId { }

    /// <summary>
    /// Elemento que informa o membro da cadeia parceiro da comunicação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Prtnr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Prtnr : StakeholderId { }

    /// <summary>
    /// Elemento que informa o transportador responsável pela movimentação do medicamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Carrs")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Carrs
    {
        /// <summary>
        /// Grupo que informa uma sequencia de transportadores
        /// </summary>
        [XmlElement("car")]
        public List<Car> Car { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCar(Car item)
        {
            if (Car == null)
            {
                Car = new List<Car>();
            }

            Car.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Car (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Car</returns>
        public Car GetCar(int index)
        {
            if ((Car?.Count ?? 0) == 0)
            {
                return default;
            };

            return Car[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Car
        /// </summary>
        public int GetCarCount => (Car != null ? Car.Count : 0);

#endif
    }

    /// <summary>
    /// Grupo que informa uma sequência de transportadores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Car")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Car : StakeholderId { }

    /// <summary>
    /// Grupo que define uma carga de IUMs, podendo conter tanto IUMs quanto embalagens de transporte com conteúdo
    /// </summary>
    public abstract class Payload
    {
        /// <summary>
        /// Grupo que informa uma embalagem de transporte de medicamentos.
        /// </summary>
        [XmlElement("tp")]
        public List<Tp> Tp { get; set; }

        /// <summary>
        /// Identificador Único de Medicamento (IUM)
        /// </summary>
        [XmlElement("dui")]
        public List<Dui> Dui { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTp(Tp item)
        {
            if (Tp == null)
            {
                Tp = new List<Tp>();
            }

            Tp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Tp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Tp</returns>
        public Tp GetTp(int index)
        {
            if ((Tp?.Count ?? 0) == 0)
            {
                return default;
            };

            return Tp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Tp
        /// </summary>
        public int GetTpCount => (Tp != null ? Tp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDui(Dui item)
        {
            if (Dui == null)
            {
                Dui = new List<Dui>();
            }

            Dui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dui</returns>
        public Dui GetDui(int index)
        {
            if ((Dui?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dui
        /// </summary>
        public int GetDuiCount => (Dui != null ? Dui.Count : 0);

#endif
    }

    /// <summary>
    /// Grupo que informa uma sequência de transportadores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Pld")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Pld : Payload { }

    /// <summary>
    /// Grupo que informa uma embalagem de transporte de medicamentos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Tp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Tp
    {
        /// <summary>
        /// Grupo que informa uma embalagem de transporte de medicamentos.
        /// </summary>
        [XmlElement("tpi")]
        public Tpi Tpi { get; set; }

        [XmlElement("pld")]
        public Pld Pld { get; set; }

        /// <summary>
        /// Indica que o conteúdo da embalagem de transporte não foi verificado. Informar o valor "true" quando não verificado.
        /// </summary>
        [XmlElement("unvPld")]
        public bool UnvPld { get; set; }

        /// <summary>
        /// Informa o conteúdo da embalagem de transporte não verificado. As informações contidas na hierarquia abaixo deste elemento serão ignoradas pelo SNCM.
        /// </summary>
        [XmlElement("supdPld")]
        public SupdPld SupdPld { get; set; }
    }

    /// <summary>
    /// Grupo que informa uma sequência de transportadores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.SupdPld")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class SupdPld : Payload { }

    /// <summary>
    /// Grupo que informa uma embalagem de transporte de medicamentos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Tpi")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Tpi
    {
        /// <summary>
        /// Código para identificação exclusiva e serializada de unidades logísticas
        /// </summary>
        [XmlElement("sscc")]
        public string Sscc { get; set; }

        /// <summary>
        /// Concatenação do GTIN e um serial.
        /// </summary>
        [XmlElement("gtinSn")]
        public string GtinSn { get; set; }

        /// <summary>
        /// Identificador de uma embalagem de transporte que não segue os padrões GS1.
        /// </summary>
        [XmlElement("adHocTpi")]
        public AdHocTpi AdHocTpi { get; set; }
    }

    /// <summary>
    /// Grupo para identificação de uma Embalagem de Transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.AdHocTpi")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class AdHocTpi
    {
        /// <summary>
        /// Identificador do expedidor.
        /// </summary>
        [XmlElement("shipper")]
        public Shipper Shipper { get; set; }

        /// <summary>
        /// Código de série de uma embalagem de transporte que não segue os padrões GS1.
        /// </summary>
        [XmlElement("tpSerl")]
        public string TpSerl { get; set; }
    }

    /// <summary>
    /// Grupo que define uma finalização de um ou vários IUMs por deslacre
    /// </summary>
    public abstract class SealBreak
    {
        /// <summary>
        /// Finalização de um IUM por deslacre.
        /// </summary>
        [XmlElement("s")]
        public UnitSealBreak S { get; set; }
    }

    /// <summary>
    /// Grupo que define uma finalização de um IUM por deslacre
    /// </summary>
    public abstract class UnitSealBreak
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM);
        /// </summary>
        [XmlElement("dui")]
        public Dui Dui { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Motivação para finalização
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "desc" se a propriedade "Desc" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "desc"</returns>
        public bool ShouldSerializeDesc() => !string.IsNullOrWhiteSpace(Desc);

        #endregion
    }

    /// <summary>
    /// Finalização por deslacre
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.SealBrk")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class SealBrk : SealBreak { }

    /// <summary>
    /// Grupo que define a finalização de um ou vários IUMs por descarte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.XPort")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class XPort
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM).
        /// </summary>
        [XmlElement("pld")]
        public Pld Pld { get; set; }

        private string ReceiverField { get; set; }

        /// <summary>
        /// Justificativa da Finalização por descarte.
        /// </summary>
        [XmlElement("receiver")]
        public string Receiver
        {
            get => ReceiverField;
            set => ReceiverField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }
    }

    /// <summary>
    /// Grupo que define a finalização de um ou vários IUMs por descarte
    /// </summary>
    public abstract class Disposal
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM);
        /// </summary>
        [XmlElement("dui")]
        public List<Dui> Dui { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Justificativa da Finalização por descarte.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDui(Dui item)
        {
            if (Dui == null)
            {
                Dui = new List<Dui>();
            }

            Dui.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dui (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dui</returns>
        public Dui GetDui(int index)
        {
            if ((Dui?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dui[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dui
        /// </summary>
        public int GetDuiCount => (Dui != null ? Dui.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Só gera a tag "desc" se a propriedade "Desc" tiver conteúdo.
        /// </summary>
        /// <returns>Retorna se é ou não para gerar a tag "desc"</returns>
        public bool ShouldSerializeDesc() => !string.IsNullOrWhiteSpace(Desc);

        #endregion
    }

    /// <summary>
    /// Grupo que define a finalização de um ou vários IUMs por descarte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Destroy")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Destroy : Disposal { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET
    /// </summary>
    public abstract class UncontrolledFinalization
    {
        /// <summary>
        /// Identificador Único de Medicamento(IUM).
        /// </summary>
        [XmlElement("pld")]
        public Pld Pld { get; set; }

        /// <summary>
        /// Carga de medicamentos que deverá ser desconsiderada na finalização.
        /// </summary>
        [XmlElement("but")]
        public But But { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Justificativa da Finalização não controlada.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }
    }

    /// <summary>
    /// Carga de medicamentos que deverá ser desconsiderada na finalização.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.But")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class But : Payload { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização por avaria, quando o descarte apropriado não é possível.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Damage")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Damage : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização por extravio
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Disapp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Disapp : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização por roubo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Robry")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Robry : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização por confisco.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Seizure")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Seizure : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização pela vigilância sanitária.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Snvs")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Snvs : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização para destinação às Forças Armadas.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Military")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Military : UncontrolledFinalization { }

    /// <summary>
    /// Grupo que define a finalização não controlada de IUM e/ou IET - Finalização para controle de qualidade
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Quality")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Quality : UncontrolledFinalization { }
}

