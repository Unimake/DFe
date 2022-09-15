#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetConsSitCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetConsSitCTe : XMLBase
    {
        #region Public Properties

        [XmlElement("cStat", Order = 2)]
        public int CStat { get; set; }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF", Order = 4)]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("procEventoCTe", Order = 7)]
        public List<ProcEventoCTe> ProcEventoCTe { get; set; }

        [XmlElement("protCTe", Order = 6)]
        public ProtCTe ProtCTe { get; set; }

        [XmlElement("tpAmb", Order = 0)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic", Order = 1)]
        public string VerAplic { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("xMotivo", Order = 3)]
        public string XMotivo { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void ReadXml(XmlDocument document)
        {
            ProcEventoCTe.Clear();

            var nodeListProcEventoCTe = document.GetElementsByTagName("procEventoCTe");

            foreach (var item in nodeListProcEventoCTe)
            {
                ProcEventoCTe.Add(XMLUtility.Deserializar<ProcEventoCTe>(((XmlElement)item).OuterXml));
            }
        }

        #endregion Public Methods

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoCTe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoCTe</returns>
        public ProcEventoCTe GetProcEventoCTe(int index)
        {
            if ((ProcEventoCTe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoCTe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoCTe
        /// </summary>
        public int GetProcEventoCTeCount => (ProcEventoCTe != null ? ProcEventoCTe.Count : 0);

#endif
    }
}