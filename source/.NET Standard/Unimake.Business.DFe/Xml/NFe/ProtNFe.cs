#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe do protocolo de status resultado do processamento da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProtNFe")]
    [ComVisible(true)]
#endif
    public class ProtNFe
    {
        /// <summary>
        /// Versão do schema do XML do protocolo da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados do protocolo de status
        /// </summary>
        [XmlElement("infProt")]
        public InfProt InfProt { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Classe dos dados do protocolo de status
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProt")]
    [ComVisible(true)]
#endif
    public class InfProt
    {
        /// <summary>
        /// ID.
        /// Composição: ID + nProt
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do Aplicativo que processou a NFe/NFCe
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Chave de acesso da NFe/NFCe
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Data e hora de processamento, no formato AAAA-MM-DDTHH:MM:SS (ou AAAA-MM-DDTHH:MM:SSTZD, de acordo com versão).
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhRecbto para atribuir ou resgatar o valor)
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
        /// Número do Protocolo de Status da NFe/NFCe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Digest Value da NFe/NFCe processada. Utilizado para conferir a integridade da NFe/NFCe original
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Código do status da mensagem enviada
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status do serviço solicitado
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Mensagens da SEFAZ para o emissor
        /// </summary>
        [XmlIgnore]
        public List<MensagemProtocoloNFe> Mensagem { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade Mensagem para atribuir ou resgatar o valor)
        /// </summary>
        [XmlAnyElement]
        public XmlElement[] MensagemField
        {
            get
            {
                if ((Mensagem?.Count ?? 0) == 0)
                {
                    return null;
                }

                var doc = new XmlDocument();
                var result = new List<XmlElement>();

                foreach (var item in Mensagem)
                {
                    if (!string.IsNullOrWhiteSpace(item.CMsg))
                    {
                        var cMsg = doc.CreateElement("cMsg", "http://www.portalfiscal.inf.br/nfe");
                        cMsg.InnerText = item.CMsg;
                        result.Add(cMsg);
                    }

                    if (!string.IsNullOrWhiteSpace(item.XMsg))
                    {
                        var xMsg = doc.CreateElement("xMsg", "http://www.portalfiscal.inf.br/nfe");
                        xMsg.InnerText = item.XMsg;
                        result.Add(xMsg);
                    }
                }

                return result.ToArray();
            }
            set
            {
                Mensagem = null;

                if (value == null || value.Length == 0)
                {
                    return;
                }

                Mensagem = new List<MensagemProtocoloNFe>();
                MensagemProtocoloNFe item = null;

                foreach (var element in value)
                {
                    if (element.LocalName == "cMsg")
                    {
                        item = new MensagemProtocoloNFe
                        {
                            CMsg = element.InnerText
                        };

                        Mensagem.Add(item);
                    }
                    else if (element.LocalName == "xMsg")
                    {
                        if (item == null)
                        {
                            item = new MensagemProtocoloNFe();
                            Mensagem.Add(item);
                        }

                        item.XMsg = element.InnerText;
                    }
                }
            }
        }

        /// <summary>
        /// Código da Mensagem
        /// </summary>
        [XmlIgnore]
        public string CMsg
        {
            get => Mensagem?.Count > 0 ? Mensagem[0].CMsg : null;
            set
            {
                InicializarMensagem();
                Mensagem[0].CMsg = value;
            }
        }

        /// <summary>
        /// Mensagem da SEFAZ para o emissor
        /// </summary>
        [XmlIgnore]
        public string XMsg
        {
            get => Mensagem?.Count > 0 ? Mensagem[0].XMsg : null;
            set
            {
                InicializarMensagem();
                Mensagem[0].XMsg = value;
            }
        }

        private void InicializarMensagem()
        {
            if (Mensagem == null)
            {
                Mensagem = new List<MensagemProtocoloNFe>();
            }

            if (Mensagem.Count == 0)
            {
                Mensagem.Add(new MensagemProtocoloNFe());
            }
        }
    }

    /// <summary>
    /// Classe de mensagem da SEFAZ para o emissor
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.MensagemProtocoloNFe")]
    [ComVisible(true)]
#endif
    public class MensagemProtocoloNFe
    {
        /// <summary>
        /// Código da Mensagem
        /// </summary>
        public string CMsg { get; set; }

        /// <summary>
        /// Mensagem da SEFAZ para o emissor
        /// </summary>
        public string XMsg { get; set; }
    }
}
