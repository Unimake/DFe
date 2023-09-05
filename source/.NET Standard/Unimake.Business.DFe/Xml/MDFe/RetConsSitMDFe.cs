#pragma warning disable CS1591

#if INTEROP

using System.Runtime.InteropServices;

#endif

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

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
            base.ReadXml(document);
            var reader = XmlReader.Create(new StringReader(document.InnerXml));
            var nodes = XDocument.Parse(document.InnerXml)
                                 .DescendantNodes()
                                 .Where(w => w is XElement)
                                 .Cast<XElement>();
            var retEventos = nodes.Where(w => w.Name.LocalName.Equals(nameof(RetEventoMDFe), StringComparison.InvariantCultureIgnoreCase))
                                  .FirstOrDefault();

            if (retEventos != null)
            {
                ProcEventoMDFe[0].Versao = nodes.Where(w => w.Name.LocalName == "procEventoMDFe" &&
                                                            w.GetAttributeValue("versao") != null)
                                                .First()
                                                .GetAttributeValue("versao");
                ProcEventoMDFe[0].RetEventoMDFe = new RetEventoMDFe
                {
                    Versao = retEventos.GetAttributeValue("versao"),
                    InfEvento = new RetEventoMDFeInfEvento
                    {
                        Id = retEventos.GetElement("infEvento").GetAttributeValue("Id"),
                        TpAmb = retEventos.GetValue<TipoAmbiente>("tpAmb"),
                        VerAplic = retEventos.GetValue("verAplic"),
                        COrgao = retEventos.GetValue<UFBrasil>("cOrgao"),
                        CStat = retEventos.GetValue<int>("cStat"),
                        XMotivo = retEventos.GetValue("xMotivo"),
                        ChMDFe = retEventos.GetValue("chMDFe"),
                        TpEvento = retEventos.GetValue<TipoEventoMDFe>("tpEvento"),
                        XEvento = retEventos.GetValue("xEvento"),
                        NSeqEvento = retEventos.GetValue<int>("nSeqEvento"),
                        DhRegEvento = retEventos.GetValue<DateTime>("dhRegEvento"),
                        NProt = retEventos.GetValue("nProt")
                    }
                };
            }

            while (reader.Read())
            {
                if (reader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                if (reader.Name != "Signature")
                {
                    continue;
                }

                ProcEventoMDFe[0].EventoMDFe.Signature = reader.ToSignature();
                break;
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
            if((ProcEventoMDFe?.Count ?? 0) == 0)
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