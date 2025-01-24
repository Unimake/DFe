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

namespace Unimake.Business.DFe.Xml.NFCom
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsSitNFCom")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitNFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class RetConsSitNFCom : XMLBase
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

        [XmlElement("protNFCom")]
        public ProtNFCom ProtNFCom { get; set; }

        [XmlElement("procEventoNFCom")]
        public List<ProcEventoNFCom> ProcEventoNFCom { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoNFCom (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoNFCom</returns>
        public ProcEventoNFCom GetProcEventoNFCom(int index)
        {
            if ((ProcEventoNFCom?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoNFCom[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoNFCom
        /// </summary>
        public int GetProcEventoNFComCount => (ProcEventoNFCom != null ? ProcEventoNFCom.Count : 0);

#endif

        public override void ReadXml(XmlDocument document)
        {
            ProcEventoNFCom.Clear();

            var nodeListProcEventoNFCom = document.GetElementsByTagName("procEventoNFCom");

            foreach (var item in nodeListProcEventoNFCom)
            {
                ProcEventoNFCom.Add(XMLUtility.Deserializar<ProcEventoNFCom>(((XmlElement)item).OuterXml));
            }
        }
    }
}