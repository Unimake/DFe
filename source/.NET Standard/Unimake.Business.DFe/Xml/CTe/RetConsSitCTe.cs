#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
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

        public void AddProcEventoCTe(ProcEventoCTe procEventoCTe)
        {
            if(ProcEventoCTe == null)
            {
                ProcEventoCTe = new List<ProcEventoCTe>();
            }

            ProcEventoCTe.Add(procEventoCTe);
        }

        public override void ReadXml(XmlDocument document)
        {
            base.ReadXml(document);
            var reader = XmlReader.Create(new StringReader(document.InnerXml));

            while (reader.Read())
            {
                if(reader.NodeType != XmlNodeType.Element ||
                   reader.Name != "Signature")
                {
                    continue;
                }

                ProcEventoCTe[0].EventoCTe.Signature = reader.ToSignature();
                break;
            }
        }

        #endregion Public Methods
    }
}