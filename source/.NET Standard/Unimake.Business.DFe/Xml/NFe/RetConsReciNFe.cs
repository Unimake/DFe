#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de retorno da consulta do recido do lote de NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsReciNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsReciNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsReciNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno da consulta recibo da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

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
        /// Número do recibo consultado
        /// </summary>
        [XmlElement("nRec")]
        public string NRec { get; set; }

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
        /// Data e hora de processamento, no formato AAAA-MM-DDTHH:MM:SSTZD. Em caso de rejeição, com data e hora do recebimento do Lote de NF-e enviado
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
            set
            {
                if (!string.IsNullOrWhiteSpace(value))
                {
#if INTEROP
                    DhRecbto = DateTime.Parse(value);
#else
                    DhRecbto = DateTimeOffset.Parse(value);
#endif
                }
            }
        }

        /// <summary>
        /// Código da mensagem
        /// </summary>
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        /// <summary>
        /// Protocolo de status resultado do processamento da NFe/NFCe
        /// </summary>
        [XmlElement("protNFe")]
        public List<ProtNFe> ProtNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProtNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista ProtNFe)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProtNFe</returns>
        public ProtNFe GetProtNFe(int index)
        {
            if ((ProtNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProtNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProtNFe
        /// </summary>
        public int GetProtNFeCount => (ProtNFe != null ? ProtNFe.Count : 0);

#endif
    }
}
