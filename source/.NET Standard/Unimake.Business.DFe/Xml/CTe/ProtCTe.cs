#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// XML de retorno do resultado do processamento do CTe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ProtCTe")]
    [ComVisible(true)]
#endif
    public class ProtCTe
    {
        /// <summary>
        /// Versão do leiaute do schema
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do protocolo de resposta
        /// </summary>
        [XmlElement("infProt")]
        public InfProt InfProt { get; set; }

        /// <summary>
        /// Grupo reservado para envio de mensagem do Fisco para o contribuinte
        /// </summary>
        [XmlElement("infFisco")]
        public InfFisco InfFisco { get; set; }
    }

    /// <summary>
    /// Informações do protocolo de resposta do CTe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfProt")]
    [ComVisible(true)]
#endif
    public class InfProt
    {
        /// <summary>
        /// Identificador da TAG a ser assinada, somente precisa ser informado se a UF assinar a resposta. Em caso de assinatura da resposta pela SEFAZ preencher o campo com o Nro do Protocolo, precedido com o literal “ID”
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do aplicativo que recebeu o CTe
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Chave de acesso do CT-e
        /// </summary>
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }

        /// <summary>
        /// Data e Hora do Processamento. Formato = AAAA-MM-DDTHH:MM:SS TZD. Preenchido com data e hora da gravação do CTe no Banco de Dados. Em caso de Rejeição, com data e hora do recebimento do Arquivo de CTe enviado.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar da DhRecebto (Sempre utilize a DhRecebto)
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo de autorização do CTe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Digest Value do CTe processado, utilizado para conferir a integridade com o CTe origina
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Código do status da resposta para o CTe
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta para o CTe
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
    }

    /// <summary>
    /// Grupo reservado para envio de mensagem do Fisco para o contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfFisco")]
    [ComVisible(true)]
#endif
    public class InfFisco
    {
        /// <summary>
        /// Código de status da mensagem do fisco
        /// </summary>
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        /// <summary>
        /// Mensgem do fisco para o contribuinte
        /// </summary>
        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }
}
