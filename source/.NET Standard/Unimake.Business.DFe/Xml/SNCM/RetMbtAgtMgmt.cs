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
    /// XML para o serviço para recepção e/ou atualização dos parâmetros do SNCM necessários para o Sistema Cliente funcionar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.RetMbtAgtMgmt")]
    [ComVisible(true)]
#endif
    [XmlRoot("retMbtAgtMgmt", Namespace = "http://sncm.anvisa.gov.br/", IsNullable = false)]
    public class RetMbtAgtMgmt : XMLBase
    {
        /// <summary>
        /// Identificador de controle da comunicação do SNCM.
        /// </summary>
        [XmlElement("docId")]
        public string DocId { get; set; }

        /// <summary>
        /// Data e horário da recepção da mensagem de entrada
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime CrTime { get; set; }
#else
        public DateTimeOffset CrTime { get; set; }
#endif

        [XmlElement("crTime")]
        public string CrTimeField
        {
            get => CrTime.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => CrTime = DateTime.Parse(value);
#else
            set => CrTime = DateTimeOffset.Parse(value);
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
        /// Código identificador da retaguarda que atendeu a solicitação
        /// </summary>
        [XmlElement("backOff")]
        public string BackOff { get; set; }

        /// <summary>
        /// Lista com os CNPJ cadastrados como procuradores
        /// </summary>
        [XmlElement("list")]
        public Lista Lista { get; set; }

        /// <summary>
        /// Grupo de mensagens de retorno do Web Service.
        /// </summary>
        [XmlElement("rets")]
        public Rets Rets { get; set; }

        /// <summary>
        /// Identificação do surgimento de anomalias pela comunicação do documento de entrada: false – Não surgiram anomalias / true – Surgiram anomalias.
        /// </summary>
        [XmlElement("anom")]
        public bool Anom { get; set; }

        /// <summary>
        /// Identificação de existência de notificações ao membro: false – Não existem notificações / true – Existem notificações
        /// </summary>
        [XmlElement("notif")]
        public bool Notif { get; set; }

        /// <summary>
        /// Identificação de existência de ações a serem desempenhadas pelo membro: false – Não existem ações / true – Existem ações do SNCM a serem desempenhados pelo membro.
        /// </summary>
        [XmlElement("action")]
        public bool Action { get; set; }

        /// <summary>
        /// Deserializar o XML retMbtAgtMgmt no objeto RetMbtAgtMgmt.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML retMbtAgtMgmt</param>
        /// <returns>Objeto do RetMbtAgtMgmt</returns>
        public RetMbtAgtMgmt LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetMbtAgtMgmt>(doc);
        }

        /// <summary>
        /// Deserializar o XML retMbtAgtMgmt no objeto RetMbtAgtMgmt.
        /// </summary>
        /// <param name="xml">string do XML retMbtAgtMgmt</param>
        /// <returns>Objeto do RetMbtAgtMgmt</returns>
        public RetMbtAgtMgmt LoadFromXML(string xml) => XMLUtility.Deserializar<RetMbtAgtMgmt>(xml);
    }

    /// <summary>
    /// Lista com os CNPJ cadastrados como procuradores
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Lista")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Lista
    {
        /// <summary>
        /// CNPJ de um procurador.
        /// </summary>
        [XmlElement("cnpj")]
        public List<string> CNPJ { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCNPJ(string item)
        {
            if (CNPJ == null)
            {
                CNPJ = new List<string>();
            }

            CNPJ.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da list CNPJ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da list)
        /// </summary>
        /// <param name="index">Índice da list a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro do CNPJ</returns>
        public string GetCNPJ(int index)
        {
            if ((CNPJ?.Count ?? 0) == 0)
            {
                return default;
            };

            return CNPJ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na list CNPJ
        /// </summary>
        public int GetCNPJCount => (CNPJ != null ? CNPJ.Count : 0);

#endif
    }

    /// <summary>
    /// Grupo para cada retorno do Web Service.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Rets")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Rets
    {
        /// <summary>
        /// Grupo para cada retorno do Web Service.
        /// </summary>
        [XmlElement("ret")]
        public List<Ret> Ret { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRet(Ret item)
        {
            if (Ret == null)
            {
                Ret = new List<Ret>();
            }

            Ret.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da list Ret (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da list)
        /// </summary>
        /// <param name="index">Índice da list a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro do Ret</returns>
        public Ret GetRet(int index)
        {
            if ((Ret?.Count ?? 0) == 0)
            {
                return default;
            };

            return Ret[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na list Ret
        /// </summary>
        public int GetRetCount => (Ret != null ? Ret.Count : 0);

#endif
    }

    /// <summary>
    /// Grupo para cada retorno do Web Service.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.SNCM.Ret")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://sncm.anvisa.gov.br/")]
    public class Ret
    {
        /// <summary>
        /// Código da mensagem de retorno do Web Service.
        /// </summary>
        [XmlElement("code")]
        public string Code { get; set; }

        /// <summary>
        /// Efeito da mensagem de retorno do Web Service.Efeitos previstos: SUC – Sucesso / WNG – Advertência / REJ – Rejeição.
        /// </summary>
        [XmlElement("effect")]
        public string Effect { get; set; }

        private string DescField { get; set; }

        /// <summary>
        /// Descrição da mensagem de retorno.
        /// </summary>
        [XmlElement("desc")]
        public string Desc
        {
            get => DescField;
            set => DescField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(140).Trim());
        }
    }
}