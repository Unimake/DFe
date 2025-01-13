#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.RetConsReciNF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsReciNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class RetConsReciNF3e : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "string")]
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

        [XmlElement("protNF3e")]
        public List<ProtNF3e> ProtNF3e { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProtNF3e (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista ProtNF3e)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProtNF3e</returns>
        public ProtNF3e GetProtNF3e(int index)
        {
            if ((ProtNF3e?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProtNF3e[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProtNF3e
        /// </summary>
        public int GetProtNF3eCount => (ProtNF3e != null ? ProtNF3e.Count : 0);

#endif
    }
}
