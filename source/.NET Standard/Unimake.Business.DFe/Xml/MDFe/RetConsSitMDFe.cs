#pragma warning disable CS1591

#if INTEROP

using System.Runtime.InteropServices;

#endif

using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP

    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetConsSitMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class RetConsSitMDFe : XMLBase
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

        [XmlElement("protMDFe")]
        public ProtMDFe ProtMDFe { get; set; }

        [XmlElement("procEventoMDFe")]
        public List<ProcEventoMDFe> ProcEventoMDFe { get; set; }

        /// <summary>
        /// Grupo de informações do compartilhamento do MDFe com InfraSA para geração do DTe
        /// </summary>
        [XmlElement("procInfraSA")]
        public ProcInfraSA ProcInfraSA { get; set; }

        public override void ReadXml(XmlDocument document)
        {
            ProcEventoMDFe.Clear();

            var nodeListProcEventoMDFe = document.GetElementsByTagName("procEventoMDFe");

            foreach (var item in nodeListProcEventoMDFe)
            {
                ProcEventoMDFe.Add(XMLUtility.Deserializar<ProcEventoMDFe>(((XmlElement)item).OuterXml));
            }
        }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoMDFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoMDFe</returns>
        public ProcEventoMDFe GetProcEventoMDFe(int index)
        {
            if ((ProcEventoMDFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoMDFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoMDFe
        /// </summary>
        public int GetProcEventoMDFeCount => ProcEventoMDFe != null ? ProcEventoMDFe.Count : 0;

#endif
    }
}