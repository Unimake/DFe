#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetEnviMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    [XmlRoot("retEnviMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class RetEnviMDFe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo de ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Código da UF.
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "CUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Versão do aplicativo que processou o retorno do envio do MDFe.
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do status da resposta.
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta.
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Informações do recibo.
        /// </summary>
        [XmlElement("infRec")]
        public RetEnviMDFeInfRec InfRec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetEnviMDFeInfRec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class RetEnviMDFeInfRec
    {
        /// <summary>
        /// Número do recibo.
        /// </summary>
        [XmlElement("nRec")]
        public string NRec { get; set; }

        /// <summary>
        /// Data e hora do recebimento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhRecbto" para atribuir ou resgatar o valor)
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
        /// Tempo médio de resposta (em segundos).
        /// </summary>
        [XmlElement("tMed")]
        public string TMed { get; set; }
    }
}
