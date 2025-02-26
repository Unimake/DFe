#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de retorno da consulta da situação atual da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsSitNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsSitNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno da consulta situação da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do Aplicativo que processou a consulta da NFe/NFCe
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

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
        /// Código da UF de atendimento
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Data e hora do recebimento da consulta no formato AAAA-MM-DDTHH:MM:SSTZD
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
        /// Chave da NFe/NFCe consultada
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Protocolo de autorização de uso da NFe/NFCe
        /// </summary>
        [XmlElement("protNFe")]
        public ProtNFe ProtNFe { get; set; }

        /// <summary>
        /// Protocolo(s) de registro de evento da NFe/NFCe
        /// </summary>
        [XmlElement("procEventoNFe")]
        public List<ProcEventoNFe> ProcEventoNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoNFe</returns>
        public ProcEventoNFe GetProcEventoNFe(int index)
        {
            if ((ProcEventoNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoNFe
        /// </summary>
        public int GetProcEventoNFeCount => (ProcEventoNFe != null ? ProcEventoNFe.Count : 0);

#endif

        public override void ReadXml(XmlDocument document)
        {
            ProcEventoNFe.Clear();

            var nodeListProcEventoNFe = document.GetElementsByTagName("procEventoNFe");

            foreach (var item in nodeListProcEventoNFe)
            {
                ProcEventoNFe.Add(XMLUtility.Deserializar<ProcEventoNFe>(((XmlElement)item).OuterXml));
            }
        }
    }
}
