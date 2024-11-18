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

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsSitNF3e")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class RetConsSitNF3e : XMLBase
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

        [XmlElement("protNF3e")]
        public ProtNF3e ProtNF3e { get; set; }

        [XmlElement("procEventoNF3e")]
        public List<ProcEventoNF3e> ProcEventoNF3e { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoNF3e (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoNF3e</returns>
        public ProcEventoNF3e GetProcEventoNF3e(int index)
        {
            if ((ProcEventoNF3e?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoNF3e[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoNF3e
        /// </summary>
        public int GetProcEventoNF3eCount => (ProcEventoNF3e != null ? ProcEventoNF3e.Count : 0);

#endif

        public override void ReadXml(XmlDocument document)
        {
            ProcEventoNF3e.Clear();

            var nodeListProcEventoNF3e = document.GetElementsByTagName("procEventoNF3e");

            foreach (var item in nodeListProcEventoNF3e)
            {
                ProcEventoNF3e.Add(XMLUtility.Deserializar<ProcEventoNF3e>(((XmlElement)item).OuterXml));
            }
        }
    }
}