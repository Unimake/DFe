#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe da consulta do status do serviço
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsStatServ")]
    [ComVisible(true)]
#endif
    [XmlRoot("consStatServ", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsStatServ : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de consulta status do serviço da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Código da UF a ser consultada
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
        /// Descrição do serviço. Padrão = STATUS
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "STATUS";
    }
}