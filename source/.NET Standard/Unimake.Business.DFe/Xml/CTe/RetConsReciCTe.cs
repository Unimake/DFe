#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetConsReciCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsReciCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetConsReciCTe : XMLBase
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

        [XmlElement("protCTe")]
        public List<ProtCTe> ProtCTe { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProtCTe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista ProtCTe)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProtCTe</returns>
        public ProtCTe GetProtCTe(int index)
        {
            if ((ProtCTe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProtCTe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProtCTe
        /// </summary>
        public int GetProtCTeCount => (ProtCTe != null ? ProtCTe.Count : 0);

#endif
    }
}
