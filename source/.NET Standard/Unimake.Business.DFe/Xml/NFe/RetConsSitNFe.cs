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
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsSitNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsSitNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

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
        public DateTime DhRecbto { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhRecbto = DateTime.Parse(value);
        }

        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        [XmlElement("protNFe")]
        public ProtNFe ProtNFe { get; set; }

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
    }
}
