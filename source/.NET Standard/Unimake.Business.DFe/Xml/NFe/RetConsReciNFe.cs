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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsReciNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsReciNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsReciNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("nRec")]
        public string NRec { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

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

        [XmlElement("cMsg")]
        public string CMsg { get; set; }

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
