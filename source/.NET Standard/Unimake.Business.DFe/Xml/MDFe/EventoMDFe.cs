#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoCanc")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCanc : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlElement("xJust", Order = 2)]
        public string XJust { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evCancMDFe>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <xJust>{XJust}</xJust>
            </evCancMDFe>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoIncCondutor")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoIncCondutorMDFe")]
    public class DetEventoIncCondutor : EventoDetalhe
    {
        #region Private Fields

        private EventoIncCondutor _eventoIncCondutor;

        #endregion Private Fields

        #region Internal Methods

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        #endregion Internal Methods

        #region Public Properties

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIncCondutor.DescEvento;
            set => EventoIncCondutor.DescEvento = value;
        }

        [XmlElement(ElementName = "evIncCondutorMDFe", Order = 0)]
        public EventoIncCondutor EventoIncCondutor
        {
            get => _eventoIncCondutor ?? (_eventoIncCondutor = new EventoIncCondutor());
            set => _eventoIncCondutor = value;
        }

        [XmlIgnore]
        public List<CondutorMDFe> CondutorMDFe
        {
            get => EventoIncCondutor.Condutor;
            set => EventoIncCondutor.Condutor = value;
        }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evIncCondutorMDFe>
                <descEvento>{DescEvento}</descEvento>";

            foreach (var condutorMDFe in CondutorMDFe)
            {
                writeRaw += $@"<condutor>
                               <xNome>{condutorMDFe.XNome}</xNome>
                               <CPF>{condutorMDFe.CPF}</CPF>
                               </condutor>";
            }

            writeRaw += $@"</evIncCondutorMDFe>";

            writer.WriteRaw(writeRaw);
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoIncCondutor")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoIncCondutorMDFe")]
    public class EventoIncCondutor : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Inclusao Condutor";

        [XmlElement("condutor", Order = 1)]
        public List<CondutorMDFe> Condutor { get; set; } = new List<CondutorMDFe>();

        #endregion Public Properties

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCondutor(CondutorMDFe item) =>
            (Condutor ?? (Condutor = new List<CondutorMDFe>())).Add(item ?? throw new ArgumentNullException(nameof(item)));

        /// <summary>
        /// Retorna o elemento da lista Condutor (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Condutor</returns>
        public CondutorMDFe GetCondutor(int index)
        {
            if ((Condutor?.Count ?? 0) == 0)
            {
                return default;
            };

            return Condutor[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Condutor
        /// </summary>
        public int GetCondutorCount => (Condutor != null ? Condutor.Count : 0);

#endif

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.CondutorMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class CondutorMDFe
    {
        [XmlElement("xNome", Order = 0)]
        public string XNome { get; set; }

        [XmlElement("CPF", Order = 1)]
        public string CPF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoIncDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoIncDFeMDFe")]
    public class DetEventoIncDFeMDFe : EventoDetalhe
    {
        #region Private Fields

        private EventoIncDFeMDFe _eventoIncDFeMDFe;

        #endregion Private Fields

        #region Internal Methods

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi.Name == nameof(InfDoc))
            {
                XmlReader.Read();
                InfDoc.Add(new InfDoc
                {
                    CMunDescarga = XmlReader.GetValue<string>(nameof(Xml.MDFe.InfDoc.CMunDescarga)),
                    XMunDescarga = XmlReader.GetValue<string>(nameof(Xml.MDFe.InfDoc.XMunDescarga)),
                    ChNFe = XmlReader.GetValue<string>(nameof(Xml.MDFe.InfDoc.ChNFe))
                });
                return;
            }

            base.SetValue(pi);
        }

        #endregion Internal Methods

        #region Public Properties

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIncDFeMDFe.DescEvento;
            set => EventoIncDFeMDFe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoIncDFeMDFe.NProt;
            set => EventoIncDFeMDFe.NProt = value;
        }

        [XmlIgnore]
        public string CMunCarrega
        {
            get => EventoIncDFeMDFe.CMunCarrega;
            set => EventoIncDFeMDFe.CMunCarrega = value;
        }

        [XmlIgnore]
        public string XMunCarrega
        {
            get => EventoIncDFeMDFe.XMunCarrega;
            set => EventoIncDFeMDFe.XMunCarrega = value;
        }

        [XmlElement(ElementName = "evIncDFeMDFe", Order = 0)]
        public EventoIncDFeMDFe EventoIncDFeMDFe
        {
            get => _eventoIncDFeMDFe ?? (_eventoIncDFeMDFe = new EventoIncDFeMDFe());
            set => _eventoIncDFeMDFe = value;
        }

        [XmlIgnore]
        public List<InfDoc> InfDoc
        {
            get => EventoIncDFeMDFe.InfDoc;
            set => EventoIncDFeMDFe.InfDoc = value;
        }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evIncDFeMDFe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <cMunCarrega>{CMunCarrega}</cMunCarrega>
                <xMunCarrega>{XMunCarrega}</xMunCarrega>";

            foreach (var infDoc in InfDoc)
            {
                writeRaw += $@"<infDoc>
                               <cMunDescarga>{infDoc.CMunDescarga}</cMunDescarga>
                               <xMunDescarga>{infDoc.XMunDescarga}</xMunDescarga>
                               <chNFe>{infDoc.ChNFe}</chNFe>
                               </infDoc>";
            }

            writeRaw += $@"</evIncDFeMDFe>";

            writer.WriteRaw(writeRaw);
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoIncDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoIncDFeMDFe")]
    public class EventoIncDFeMDFe : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Inclusao DF-e";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlElement("cMunCarrega", Order = 2)]
        public string CMunCarrega { get; set; }

        [XmlElement("xMunCarrega", Order = 3)]
        public string XMunCarrega { get; set; }

        [XmlElement("infDoc", Order = 4)]
        public List<InfDoc> InfDoc { get; set; } = new List<InfDoc>();

        #endregion Public Properties

        #region Public Methods

#if INTEROP
        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfDoc(InfDoc item)
        {
            if (InfDoc == null)
            {
                InfDoc = new List<InfDoc>();
            }

            InfDoc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfDoc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfDoc</returns>
        public InfDoc GetInfDoc(int index)
        {
            if ((InfDoc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfDoc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfDoc
        /// </summary>
        public int GetInfDocCount => (InfDoc != null ? InfDoc.Count : 0);


#endif

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfDoc")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "infDoc")]
    public class InfDoc : EventoDetalhe
    {
        [XmlElement("cMunDescarga", Order = 0)]
        public string CMunDescarga { get; set; }

        [XmlElement("xMunDescarga", Order = 1)]
        public string XMunDescarga { get; set; }

        [XmlElement("chNFe", Order = 2)]
        public string ChNFe { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoEncMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoEncMDFe : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Encerramento";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlIgnore]
        public DateTime DtEnc { get; set; }

        [XmlElement("DtEnc", Order = 2)]
        public string DtEncField
        {
            get => DtEnc.ToString("yyyy-MM-dd");
            set => DtEnc = DateTime.Parse(value);
        }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF", Order = 3)]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cMun", Order = 4)]
        public long CMun { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"<evEncMDFe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <dtEnc>{DtEncField}</dtEnc>
                <cUF>{CUFField}</cUF>
                <cMun>{CMun}</cMun>
            </evEncMDFe>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class EventoMDFe : XMLBase
    {
        #region Private Methods

        private void PrepararCondutor(XmlDocument xmlDoc)
        {
            var condutores = xmlDoc.GetElementsByTagName("condutor");

            if (!(condutores?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoIncCondutor detEvento)
            {
                detEvento.CondutorMDFe = new List<CondutorMDFe>();

                foreach (var elementCondutorMDFe in condutores.Cast<XmlElement>())
                {
                    detEvento.CondutorMDFe.Add(new CondutorMDFe
                    {
                        XNome = elementCondutorMDFe.GetValue<string>(nameof(PagtoOperMDFeInfPag.XNome)),
                        CPF = elementCondutorMDFe.GetValue<string>(nameof(PagtoOperMDFeInfPag.CPF))
                    });
                }
            }
        }

        private void PrepararInfViagens(XmlDocument xmlDoc)
        {
            var infViagens = xmlDoc.GetElementsByTagName("infViagens");

            if (!(infViagens?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoPagtoOperMDFe detEvento)
            {
                var infViagensElement = ((XmlElement)infViagens[0]);

                detEvento.InfViagens = new InfViagens
                {
                    QtdViagens = infViagensElement.GetValue<string>(nameof(InfViagens.QtdViagens)),
                    NroViagem = infViagensElement.GetValue<string>(nameof(InfViagens.NroViagem))
                };
            }
        }

        private void PrepararInfPag(XmlDocument xmlDoc)
        {
            var infPags = xmlDoc.GetElementsByTagName("infPag");

            if (!(infPags?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoPagtoOperMDFe detEvento)
            {
                detEvento.InfPag = new List<PagtoOperMDFeInfPag>();

                foreach (var elementInfPag in infPags.Cast<XmlElement>())
                {
                    detEvento.InfPag.Add(new PagtoOperMDFeInfPag
                    {
                        XNome = elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.XNome)),
                        CNPJ = elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.CNPJ)),
                        CPF = elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.CPF)),
                        IdEstrangeiro = elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.IdEstrangeiro)),
                        VContrato = UConvert.ToDouble(elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.VContrato)), true),
                        IndPag = elementInfPag.GetValue<IndicadorPagamento>(nameof(PagtoOperMDFeInfPag.IndPag)),
                        VAdiant = UConvert.ToDouble(elementInfPag.GetValue<string>(nameof(PagtoOperMDFeInfPag.VAdiant)), true)
                    });

                    PrepararComp(elementInfPag);
                    PrepararInfPrazo(elementInfPag);
                    PrepararInfBanc(elementInfPag);
                }
            }
        }

        private void PrepararComp(XmlElement xmlDoc)
        {
            var comps = xmlDoc.GetElementsByTagName("Comp");

            if (!(comps?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoPagtoOperMDFe detEvento)
            {
                detEvento.InfPag[detEvento.InfPag.Count - 1].Comp = new List<Comp>();

                foreach (var elComp in comps.Cast<XmlElement>())
                {
                    detEvento.InfPag[detEvento.InfPag.Count - 1].Comp.Add(new Comp
                    {
                        TpComp = elComp.GetValue<TipoComponenteMDFe>(nameof(Comp.TpComp)),
                        VCompField = elComp.GetValue<string>(nameof(Comp.VComp)),
                        XComp = elComp.GetValue<string>(nameof(Comp.XComp))
                    });
                }
            }
        }

        private void PrepararInfBanc(XmlElement xmlDoc)
        {
            var infBanc = xmlDoc.GetElementsByTagName("infBanc");

            if (!(infBanc?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoPagtoOperMDFe detEvento)
            {
                var infBancElement = ((XmlElement)infBanc[0]);

                detEvento.InfPag[detEvento.InfPag.Count - 1].InfBanc = new InfBanc
                {
                    CNPJIPEF = infBancElement.GetValue<string>(nameof(InfBanc.CNPJIPEF)),
                    CodAgencia = infBancElement.GetValue<string>(nameof(InfBanc.CodAgencia)),
                    CodBanco = infBancElement.GetValue<string>(nameof(InfBanc.CodBanco)),
                    PIX = infBancElement.GetValue<string>(nameof(InfBanc.PIX))
                };
            }
        }

        private void PrepararInfPrazo(XmlElement xmlDoc)
        {
            var infPrazos = xmlDoc.GetElementsByTagName("infPrazo");

            if (!(infPrazos?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoPagtoOperMDFe detEvento)
            {
                detEvento.InfPag[detEvento.InfPag.Count - 1].InfPrazo = new List<InfPrazo>();

                foreach (var elInfPrazo in infPrazos.Cast<XmlElement>())
                {
                    detEvento.InfPag[detEvento.InfPag.Count - 1].InfPrazo.Add(new InfPrazo
                    {
                        DVencField = elInfPrazo.GetValue<string>(nameof(InfPrazo.DVenc)),
                        NParcela = elInfPrazo.GetValue<string>(nameof(InfPrazo.NParcela)),
                        VParcelaField = elInfPrazo.GetValue<string>(nameof(InfPrazo.VParcela)),
                    });
                }
            }
        }

        private void SignEvent(EventoMDFe evento, XmlElement xmlEl)
        {
            var signature = xmlEl.GetElementsByTagName("Signature")[0];
            if (signature != null)
            {
                var signatureEvento = new XmlDocument();

                signatureEvento.LoadXml(signature.OuterXml);
                evento.Signature = XMLUtility.Deserializar<Signature>(signatureEvento);
            }
        }

        #endregion Private Methods

        #region Public Properties

        [XmlElement("infEvento", Order = 0)]
        public InfEvento InfEvento { get; set; }

        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#", Order = 1)]
        public Signature Signature { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();

            #region Adicionar o atributo de namespace que falta nas tags "evento"

            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();

            for (var i = 0; i < xmlDocument.GetElementsByTagName("evento").Count; i++)
            {
                var xmlElement = (XmlElement)xmlDocument.GetElementsByTagName("evento")[i];
                xmlElement.SetAttribute("xmlns", attribute.Namespace);
            }

            #endregion Adicionar o atributo de namespace que falta nas tags "evento"

            return xmlDocument;
        }

        public override void ReadXml(XmlDocument document)
        {
            base.ReadXml(document);

            switch (document.GetElementsByTagName("tpEvento")[0].InnerText)
            {
                case "110116":
                    PrepararInfViagens(document);
                    PrepararInfPag(document);
                    break;

                case "110114":
                    PrepararCondutor(document);
                    break;
            }
        }

        public override T LerXML<T>(XmlDocument doc)
        {
            if (typeof(T) != typeof(EventoMDFe))
            {
                throw new InvalidCastException($"Cannot cast type '{typeof(T).Name}' into type '{typeof(EventoMDFe).Name}'.");
            }

            var retornar = base.LerXML<T>(doc) as EventoMDFe;
            var eventos = doc.GetElementsByTagName("eventoMDFe");

            if (eventos?.Count > 0)
            {
                var xmlEl = (XmlElement)eventos[0];

                var xml = new StringBuilder();
                xml.Append("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                xml.Append($"<eventoMDFe versao=\"3.00\" xmlns=\"{xmlEl.NamespaceURI}\">");
                xml.Append($"{xmlEl.InnerXml}</eventoMDFe>");

                var envEvt = XMLUtility.Deserializar<EventoMDFe>(xml.ToString());
                var evt = envEvt;
                SignEvent(evt, xmlEl);
                retornar = evt;
            }

            return (T)(object)retornar;
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoDetalhe")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(DetEventoCanc))]
    [XmlInclude(typeof(DetEventoIncCondutor))]
    [XmlInclude(typeof(DetEventoIncDFeMDFe))]
    public class EventoDetalhe : IXmlSerializable
    {
        #region Private Fields

        private static readonly BindingFlags bindingFlags = BindingFlags.Public |
                                                            BindingFlags.Instance |
                                                            BindingFlags.IgnoreCase;

        private static readonly List<string> hasField = new List<string>
        {
            "DtEnc",
            "CUF"
        };

        #endregion Private Fields

        #region Private Methods

        private bool SetLocalValue(Type type)
        {
            var pi = GetPropertyInfo(type);
            if (pi == null)
            {
                return false;
            }

            SetValue(pi);
            return true;
        }

        private void SetPropertyValue(string attributeName)
        {
            PropertyInfo pi;

            if (XmlReader.GetAttribute(attributeName) != "")
            {
                pi = GetType().GetProperty(attributeName, bindingFlags);
                if (!(pi?.CanWrite ?? false))
                {
                    return;
                }

                pi?.SetValue(this, XmlReader.GetAttribute(attributeName));
            }
        }

        #endregion Private Methods

        #region Protected Internal Methods

        protected internal PropertyInfo GetPropertyInfo(Type type)
        {
            var pi = hasField.Exists(w => w.ToLower() == XmlReader.Name.ToLower()) ?
                                type.GetProperty(XmlReader.Name + "Field", bindingFlags) :
                                type.GetProperty(XmlReader.Name, bindingFlags);
            return pi;
        }

        #endregion Protected Internal Methods

        #region Internal Properties

        internal XmlReader XmlReader { get; set; }

        #endregion Internal Properties

        #region Internal Methods

        internal virtual void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }
            var type = GetType();

            SetPropertyValue("versao");
            SetPropertyValue("versaoEvento");

            while (XmlReader.Read())
            {
                if (XmlReader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                if (SetLocalValue(type) && XmlReader.NodeType == XmlNodeType.Element)
                {
                    SetLocalValue(type);
                }
            }
        }

        internal virtual void SetValue(PropertyInfo pi) => pi?.SetValue(this, XmlReader.GetValue<object>(XmlReader.Name, pi));

        #endregion Internal Methods

        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public virtual string DescEvento { get; set; }

        [XmlAttribute(AttributeName = "versaoEvento", DataType = "token")]
        public virtual string VersaoEvento { get; set; }

        #endregion Public Properties

        #region Public Methods

        public XmlSchema GetSchema() => default;

        public void ReadXml(XmlReader reader) => XmlReader = reader;

        public virtual void WriteXml(XmlWriter writer) => writer.WriteAttributeString("versaoEvento", VersaoEvento);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfEvento
    {
        #region Private Fields

        private EventoDetalhe _detEvento;

        #endregion Private Fields

        #region Public Properties

        [XmlElement("chMDFe", Order = 4)]
        public string ChMDFe { get; set; }

        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao", Order = 0)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("detEvento", Order = 9)]
        public EventoDetalhe DetEvento
        {
            get => _detEvento;
            set
            {
                switch (TpEvento)
                {
                    case 0:
                        _detEvento = value;
                        break;

                    case TipoEventoMDFe.Cancelamento:
                        _detEvento = value is DetEventoCanc ? value : new DetEventoCanc();
                        break;

                    case TipoEventoMDFe.InclusaoCondutor:
                        _detEvento = value is DetEventoIncCondutor ? value : new DetEventoIncCondutor();
                        break;

                    case TipoEventoMDFe.Encerramento:
                        _detEvento = value is DetEventoEncMDFe ? value : new DetEventoEncMDFe();
                        break;

                    case TipoEventoMDFe.InclusaoDFe:
                        _detEvento = value is DetEventoIncDFeMDFe ? value : new DetEventoIncDFeMDFe();
                        break;

                    case TipoEventoMDFe.PagamentoOperacao:
                        _detEvento = value is DetEventoPagtoOperMDFe ? value : new DetEventoPagtoOperMDFe();
                        break;

                    case TipoEventoMDFe.RegistroPassagem:
                        _detEvento = value is DetEventoMDFeRegPassagem ? value : new DetEventoMDFeRegPassagem();
                        break;

                    case TipoEventoMDFe.ConfirmacaoServicoTransporte:
                        _detEvento = value is DetEventoConfirmaServMDFe ? value : new DetEventoConfirmaServMDFe();
                        break;

                    case TipoEventoMDFe.AlteracaoPagamentoServico:
                        _detEvento = value is DetEventoAlteracaoPagtoServMDFe ? value : new DetEventoAlteracaoPagtoServMDFe();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                _detEvento.XmlReader = value.XmlReader;
                _detEvento.ProcessReader();
            }
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        [XmlElement("dhEvento", Order = 6)]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEvento = DateTime.Parse(value);
#else
            set => DhEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlAttribute(DataType = "ID")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChMDFe + NSeqEvento.ToString("000");
            set => _ = value;
        }

        [XmlElement("nSeqEvento", Order = 8)]
        public int NSeqEvento { get; set; }

        [XmlElement("tpAmb", Order = 1)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("tpEvento", Order = 7)]
        public TipoEventoMDFe TpEvento { get; set; }

        #endregion Public Properties

        #region Public Constructors

        public InfEvento()
        {
        }

        public InfEvento(EventoDetalhe detEvento) => DetEvento = detEvento ?? throw new ArgumentNullException(nameof(detEvento));

        #endregion Public Constructors

        #region Public Methods

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoPagtoOperMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoPagtoOperMDFe")]
    public class DetEventoPagtoOperMDFe : EventoDetalhe
    {
        private EventoPagtoOperMDFe _eventoPagtoOperMDFe;

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        [XmlElement(ElementName = "evPagtoOperMDFe", Order = 0)]
        public EventoPagtoOperMDFe EventoPagtoOperMDFe
        {
            get => _eventoPagtoOperMDFe ?? (_eventoPagtoOperMDFe = new EventoPagtoOperMDFe());
            set => _eventoPagtoOperMDFe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoPagtoOperMDFe.DescEvento;
            set => EventoPagtoOperMDFe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoPagtoOperMDFe.NProt;
            set => EventoPagtoOperMDFe.NProt = value;
        }

        [XmlIgnore]
        public InfViagens InfViagens
        {
            get => EventoPagtoOperMDFe.InfViagens;
            set => EventoPagtoOperMDFe.InfViagens = value;
        }

        [XmlIgnore]
        public List<PagtoOperMDFeInfPag> InfPag
        {
            get => EventoPagtoOperMDFe.InfPag;
            set => EventoPagtoOperMDFe.InfPag = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evPagtoOperMDFe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <infViagens>
                <qtdViagens>{InfViagens.QtdViagens}</qtdViagens>
                <nroViagem>{InfViagens.NroViagem}</nroViagem>
                </infViagens>";

            foreach (var infPag in InfPag)
            {
                writeRaw += $@"<infPag>
                               <xNome>{infPag.XNome}</xNome>";

                if (!string.IsNullOrWhiteSpace(infPag.CNPJ))
                {
                    writeRaw += $@"<CNPJ>{infPag.CNPJ}</CNPJ>";
                }
                else if (!string.IsNullOrWhiteSpace(infPag.CPF))
                {
                    writeRaw += $@"<CPF>{infPag.CPF}</CPF>";
                }
                else if (!string.IsNullOrWhiteSpace(infPag.IdEstrangeiro))
                {
                    writeRaw += $@"<idEstrangeiro>{infPag.IdEstrangeiro}</idEstrangeiro>";
                }

                foreach (var comp in infPag.Comp)
                {
                    writeRaw += $@"<Comp>
                                   <tpComp>{((int)comp.TpComp).ToString("00")}</tpComp>
                                   <vComp>{comp.VCompField}</vComp>";

                    if (comp.TpComp == TipoComponenteMDFe.Outros && !string.IsNullOrWhiteSpace(comp.XComp))
                    {
                        writeRaw += $@"<xComp>{comp.XComp}</xComp>";
                    }

                    writeRaw += $@"</Comp>";
                }

                writeRaw += $@"<vContrato>{infPag.VContratoField}</vContrato>
                               <indPag>{infPag.IndPagField}</indPag>
                               <vAdiant>{infPag.VAdiantField}</vAdiant>";

                foreach (var infPrazo in infPag.InfPrazo)
                {
                    writeRaw += $@"<infPrazo>
                                   <nParcela>{infPrazo.NParcela}</nParcela>
                                   <dVenc>{infPrazo.DVencField}</dVenc>
                                   <vParcela>{infPrazo.VParcelaField}</vParcela>
                                   </infPrazo>";
                }

                writeRaw += $@"<infBanc>";

                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CodAgencia))
                {
                    writeRaw += $@"<codBanco>{infPag.InfBanc.CodAgencia}</codBanco>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CodBanco))
                {
                    writeRaw += $@"<codAgencia>{infPag.InfBanc.CodBanco}</codAgencia>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CNPJIPEF))
                {
                    writeRaw += $@"<CNPJIPEF>{infPag.InfBanc.CNPJIPEF}</CNPJIPEF>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.PIX))
                {
                    writeRaw += $@"<PIX>{infPag.InfBanc.PIX}</PIX>";
                }

                writeRaw += $@"</infBanc>";

                writeRaw += $@"</infPag>";
            }

            writeRaw += $@"</evPagtoOperMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoPagtoOperMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoPagtoOperMDFe")]
    public class EventoPagtoOperMDFe : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Pagamento Operacao MDF-e";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlElement("infViagens", Order = 2)]
        public InfViagens InfViagens { get; set; }

        [XmlElement("infPag", Order = 3)]
        public List<PagtoOperMDFeInfPag> InfPag { get; set; } = new List<PagtoOperMDFeInfPag>();

        #endregion Public Properties

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infPag">Elemento</param>
        public void AddInfPag(PagtoOperMDFeInfPag infPag)
        {
            if (InfPag == null)
            {
                InfPag = new List<PagtoOperMDFeInfPag>();
            }

            InfPag.Add(infPag);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPag</returns>
        public PagtoOperMDFeInfPag GetInfPag(int index)
        {
            if ((InfPag?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPag
        /// </summary>
        public int GetInfPagCount => (InfPag != null ? InfPag.Count : 0);

#endif

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfViagens")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "infViagens")]
    public class InfViagens
    {
        [XmlElement("qtdViagens", Order = 0)]
        public string QtdViagens { get; set; }

        [XmlElement("nroViagem", Order = 1)]
        public string NroViagem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.PagtoOperMDFeInfPag")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "infPag")]
    public class PagtoOperMDFeInfPag : InfContratante
    {
        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

        [XmlIgnore]
        public double VContrato { get; set; }

        [XmlElement("vContrato")]
        public string VContratoField
        {
            get => VContrato.ToString("F2", CultureInfo.InvariantCulture);
            set => VContrato = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public IndicadorPagamento IndPag { get; set; }

        [XmlElement("indPag")]
        public int IndPagField
        {
            get => (int)IndPag;
            set => IndPag = (IndicadorPagamento)Enum.Parse(typeof(IndicadorPagamento), value.ToString());
        }

        [XmlIgnore]
        public double VAdiant { get; set; }

        [XmlElement("vAdiant")]
        public string VAdiantField
        {
            get => VAdiant.ToString("F2", CultureInfo.InvariantCulture);
            set => VAdiant = Converter.ToDouble(value);
        }

        [XmlElement("infPrazo")]
        public List<InfPrazo> InfPrazo { get; set; }


        [XmlElement("infBanc")]
        public InfBanc InfBanc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comp">Elemento</param>
        public void AddComp(Comp comp)
        {
            if (Comp == null)
            {
                Comp = new List<Comp>();
            }

            Comp.Add(comp);
        }

        /// <summary>
        /// Retorna o elemento da lista Comp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comp</returns>
        public Comp GetComp(int index)
        {
            if ((Comp?.Count ?? 0) == 0)
            {
                return default;
            };

            return Comp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comp
        /// </summary>
        public int GetCompCount => (Comp != null ? Comp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infPrazo">Elemento</param>
        public void AddInfPrazo(InfPrazo infPrazo)
        {
            if (InfPrazo == null)
            {
                InfPrazo = new List<InfPrazo>();
            }

            InfPrazo.Add(infPrazo);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPrazo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPrazo</returns>
        public InfPrazo GetInfPrazo(int index)
        {
            if ((InfPrazo?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPrazo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPrazo
        /// </summary>
        public int GetInfPrazoCount => (InfPrazo != null ? InfPrazo.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoMDFeRegPassagem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoMDFeRegPassagem")]
    public class DetEventoMDFeRegPassagem : EventoDetalhe
    {
        private EventoMDFeRegPassagem _eventoMDFeRegPassagem;

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        [XmlElement(ElementName = "evMDFeRegPassagem", Order = 0)]
        public EventoMDFeRegPassagem EventoMDFeRegPassagem
        {
            get => _eventoMDFeRegPassagem ?? (_eventoMDFeRegPassagem = new EventoMDFeRegPassagem());
            set => _eventoMDFeRegPassagem = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoMDFeRegPassagem.DescEvento;
            set => EventoMDFeRegPassagem.DescEvento = value;
        }

        [XmlIgnore]
        public string CUFTransito
        {
            get => EventoMDFeRegPassagem.CUFTransito;
            set => EventoMDFeRegPassagem.CUFTransito = value;
        }

        [XmlIgnore]
        public string CUnidFiscal
        {
            get => EventoMDFeRegPassagem.CUnidFiscal;
            set => EventoMDFeRegPassagem.CUnidFiscal = value;

        }
        [XmlIgnore]
        public string XUnidFiscal
        {
            get => EventoMDFeRegPassagem.XUnidFiscal;
            set => EventoMDFeRegPassagem.XUnidFiscal = value;
        }

        [XmlIgnore]
        public string DhPass
        {
            get => EventoMDFeRegPassagem.DhPass;
            set => EventoMDFeRegPassagem.DhPass = value;
        }

        [XmlIgnore]
        public string CPFFunc
        {
            get => EventoMDFeRegPassagem.CPFFunc;
            set => EventoMDFeRegPassagem.CPFFunc = value;
        }

        [XmlIgnore]
        public string XFunc
        {
            get => EventoMDFeRegPassagem.XFunc;
            set => EventoMDFeRegPassagem.XFunc = value;
        }

        [XmlIgnore]
        public string TpTransm
        {
            get => EventoMDFeRegPassagem.TpTransm;
            set => EventoMDFeRegPassagem.TpTransm = value;
        }

        [XmlIgnore]
        public string TpSentido
        {
            get => EventoMDFeRegPassagem.TpSentido;
            set => EventoMDFeRegPassagem.TpSentido = value;
        }

        [XmlIgnore]
        public string Placa
        {
            get => EventoMDFeRegPassagem.Placa;
            set => EventoMDFeRegPassagem.Placa = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evMDFeRegPassagem>
                <descEvento>{DescEvento}</descEvento>
                <cUFTransito>{CUFTransito}</cUFTransito>
                <cUnidFiscal>{CUnidFiscal}</cUnidFiscal>
                <xUnidFiscal>{XUnidFiscal}</xUnidFiscal>
                <dhPass>{DhPass}</dhPass>
                <CPFFunc>{CPFFunc}</CPFFunc>
                <xFunc>{XFunc}</xFunc>
                <tpTransm>{TpTransm}</tpTransm>
                <tpSentido>{TpSentido}</tpSentido>
                <placa>{Placa}</placa>
                </evMDFeRegPassagem>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoMDFeRegPassagem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoMDFeRegPassagem")]
    public class EventoMDFeRegPassagem : EventoDetalhe
    {
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Registro de Passagem";

        [XmlElement("cUFTransito", Order = 1)]
        public string CUFTransito { get; set; }

        [XmlElement("cUnidFiscal", Order = 1)]
        public string CUnidFiscal { get; set; }

        [XmlElement("xUnidFiscal", Order = 1)]
        public string XUnidFiscal { get; set; }

        [XmlElement("dhPass", Order = 1)]
        public string DhPass { get; set; }

        [XmlElement("CPFFunc", Order = 1)]
        public string CPFFunc { get; set; }

        [XmlElement("xFunc", Order = 1)]
        public string XFunc { get; set; }

        [XmlElement("tpTransm", Order = 1)]
        public string TpTransm { get; set; }

        [XmlElement("tpSentido", Order = 1)]
        public string TpSentido { get; set; }

        [XmlElement("placa", Order = 1)]
        public string Placa { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoAlteracaoPagtoServMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoAlteracaoPagtoServMDFe")]
    public class DetEventoAlteracaoPagtoServMDFe : EventoDetalhe
    {
        private EventoAlteracaoPagtoServMDFe _eventoAlteracaoPagtoServMDFe;

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        [XmlElement(ElementName = "evAlteracaoPagtoServMDFe", Order = 0)]
        public EventoAlteracaoPagtoServMDFe EventoAlteracaoPagtoServMDFe
        {
            get => _eventoAlteracaoPagtoServMDFe ?? (_eventoAlteracaoPagtoServMDFe = new EventoAlteracaoPagtoServMDFe());
            set => _eventoAlteracaoPagtoServMDFe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoAlteracaoPagtoServMDFe.DescEvento;
            set => EventoAlteracaoPagtoServMDFe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoAlteracaoPagtoServMDFe.NProt;
            set => EventoAlteracaoPagtoServMDFe.NProt = value;
        }

        [XmlIgnore]
        public List<AlteracaoPagtoServMDFeInfPag> InfPag
        {
            get => EventoAlteracaoPagtoServMDFe.InfPag;
            set => EventoAlteracaoPagtoServMDFe.InfPag = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evAlteracaoPagtoServMDFe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>";

            foreach (PagtoOperMDFeInfPag infPag in InfPag)
            {
                writeRaw += $@"<infPag>
                               <xNome>{infPag.XNome}</xNome>";

                if (!string.IsNullOrWhiteSpace(infPag.CNPJ))
                {
                    writeRaw += $@"<CNPJ>{infPag.CNPJ}</CNPJ>";
                }
                else if (!string.IsNullOrWhiteSpace(infPag.CPF))
                {
                    writeRaw += $@"<CPF>{infPag.CPF}</CPF>";
                }
                else if (!string.IsNullOrWhiteSpace(infPag.IdEstrangeiro))
                {
                    writeRaw += $@"<idEstrangeiro>{infPag.IdEstrangeiro}</idEstrangeiro>";
                }

                foreach (var comp in infPag.Comp)
                {
                    writeRaw += $@"<Comp>
                                   <tpComp>{((int)comp.TpComp).ToString("00")}</tpComp>
                                   <vComp>{comp.VCompField}</vComp>";

                    if (comp.TpComp == TipoComponenteMDFe.Outros && !string.IsNullOrWhiteSpace(comp.XComp))
                    {
                        writeRaw += $@"<xComp>{comp.XComp}</xComp>";
                    }

                    writeRaw += $@"</Comp>";
                }

                writeRaw += $@"<vContrato>{infPag.VContratoField}</vContrato>
                               <indPag>{infPag.IndPagField}</indPag>
                               <vAdiant>{infPag.VAdiantField}</vAdiant>";

                foreach (var infPrazo in infPag.InfPrazo)
                {
                    writeRaw += $@"<infPrazo>
                                   <nParcela>{infPrazo.NParcela}</nParcela>
                                   <dVenc>{infPrazo.DVencField}</dVenc>
                                   <vParcela>{infPrazo.VParcelaField}</vParcela>
                                   </infPrazo>";
                }

                writeRaw += $@"<infBanc>";

                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CodAgencia))
                {
                    writeRaw += $@"<codBanco>{infPag.InfBanc.CodAgencia}</codBanco>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CodBanco))
                {
                    writeRaw += $@"<codAgencia>{infPag.InfBanc.CodBanco}</codAgencia>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.CNPJIPEF))
                {
                    writeRaw += $@"<CNPJIPEF>{infPag.InfBanc.CNPJIPEF}</CNPJIPEF>";
                }
                if (!string.IsNullOrWhiteSpace(infPag.InfBanc.PIX))
                {
                    writeRaw += $@"<PIX>{infPag.InfBanc.PIX}</PIX>";
                }

                writeRaw += $@"</infBanc>";

                writeRaw += $@"</infPag>";
            }

            writeRaw += $@"</evAlteracaoPagtoServMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoPagtoOperMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoAlteracaoPagtoServMDFe")]
    public class EventoAlteracaoPagtoServMDFe : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Alteracao Pagamento Servico MDFe";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlElement("infPag", Order = 3)]
        public List<AlteracaoPagtoServMDFeInfPag> InfPag { get; set; } = new List<AlteracaoPagtoServMDFeInfPag>();

        #endregion Public Properties

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infPag">Elemento</param>
        public void AddInfPag(AlteracaoPagtoServMDFeInfPag infPag)
        {
            if (InfPag == null)
            {
                InfPag = new List<AlteracaoPagtoServMDFeInfPag>();
            }

            InfPag.Add(infPag);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPag</returns>
        public AlteracaoPagtoServMDFeInfPag GetInfPag(int index)
        {
            if ((InfPag?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPag
        /// </summary>
        public int GetInfPagCount => (InfPag != null ? InfPag.Count : 0);

#endif

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.AlteracaoPagtoServMDFeInfPag")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "infPag")]
    public class AlteracaoPagtoServMDFeInfPag : PagtoOperMDFeInfPag { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoConfirmaServMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoConfirmaServMDFe")]
    public class DetEventoConfirmaServMDFe : EventoDetalhe
    {
        private EventoConfirmaServMDFe _eventoConfirmaServMDFe;

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        [XmlElement(ElementName = "evConfirmaServMDFe", Order = 0)]
        public EventoConfirmaServMDFe EventoConfirmaServMDFe
        {
            get => _eventoConfirmaServMDFe ?? (_eventoConfirmaServMDFe = new EventoConfirmaServMDFe());
            set => _eventoConfirmaServMDFe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoConfirmaServMDFe.DescEvento;
            set => EventoConfirmaServMDFe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoConfirmaServMDFe.NProt;
            set => EventoConfirmaServMDFe.NProt = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evConfirmaServMDFe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>";

            writeRaw += $@"</evConfirmaServMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoConfirmaServMDFe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoConfirmaServMDFe")]
    public class EventoConfirmaServMDFe : EventoDetalhe
    {
        #region Public Properties

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Confirmacao Servico Transporte";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        #endregion Public Properties
    }
}