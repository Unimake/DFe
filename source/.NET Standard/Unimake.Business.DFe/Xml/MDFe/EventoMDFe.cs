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
using Unimake.Business.DFe.Xml.NFCom;

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

        /// <summary>
        /// Descrição do Evento: Cancelamento
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento";

        /// <summary>
        /// Número do Protocolo
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Justificativa
        /// </summary>
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

        /// <summary>
        /// Descrição do Evento (obtida de EventoIncCondutor)
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIncCondutor.DescEvento;
            set => EventoIncCondutor.DescEvento = value;
        }

        /// <summary>
        /// Evento de Inclusão de Condutor do MDF-e
        /// </summary>
        [XmlElement(ElementName = "evIncCondutorMDFe", Order = 0)]
        public EventoIncCondutor EventoIncCondutor
        {
            get => _eventoIncCondutor ?? (_eventoIncCondutor = new EventoIncCondutor());
            set => _eventoIncCondutor = value;
        }

        /// <summary>
        /// Lista de Condutores do MDF-e (obtida de EventoIncCondutor)
        /// </summary>
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

        /// <summary>
        /// Descrição do Evento: Inclusão Condutor
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Inclusao Condutor";

        /// <summary>
        /// Lista de Condutores
        /// </summary>
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
        /// <summary>
        /// Nome do Condutor
        /// </summary>
        [XmlElement("xNome", Order = 0)]
        public string XNome { get; set; }

        /// <summary>
        /// CPF do Condutor
        /// </summary>
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

        /// <summary>
        /// Descrição do Evento (obtida de EventoIncDFeMDFe)
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIncDFeMDFe.DescEvento;
            set => EventoIncDFeMDFe.DescEvento = value;
        }

        /// <summary>
        /// Número do Protocolo (obtido de EventoIncDFeMDFe)
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoIncDFeMDFe.NProt;
            set => EventoIncDFeMDFe.NProt = value;
        }

        /// <summary>
        /// Código do Município de Carregamento (obtido de EventoIncDFeMDFe)
        /// </summary>
        [XmlIgnore]
        public string CMunCarrega
        {
            get => EventoIncDFeMDFe.CMunCarrega;
            set => EventoIncDFeMDFe.CMunCarrega = value;
        }

        /// <summary>
        /// Nome do Município de Carregamento (obtido de EventoIncDFeMDFe)
        /// </summary>
        [XmlIgnore]
        public string XMunCarrega
        {
            get => EventoIncDFeMDFe.XMunCarrega;
            set => EventoIncDFeMDFe.XMunCarrega = value;
        }

        /// <summary>
        /// Evento de Inclusão de DF-e no MDF-e
        /// </summary>
        [XmlElement(ElementName = "evIncDFeMDFe", Order = 0)]
        public EventoIncDFeMDFe EventoIncDFeMDFe
        {
            get => _eventoIncDFeMDFe ?? (_eventoIncDFeMDFe = new EventoIncDFeMDFe());
            set => _eventoIncDFeMDFe = value;
        }

        /// <summary>
        /// Lista de Informações do Documento (obtida de EventoIncDFeMDFe)
        /// </summary>
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

        /// <summary>
        /// Descrição do Evento: Inclusão DF-e
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Inclusao DF-e";

        /// <summary>
        /// Número do Protocolo
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Código do Município de Carregamento
        /// </summary>
        [XmlElement("cMunCarrega", Order = 2)]
        public string CMunCarrega { get; set; }

        /// <summary>
        /// Nome do Município de Carregamento
        /// </summary>
        [XmlElement("xMunCarrega", Order = 3)]
        public string XMunCarrega { get; set; }

        /// <summary>
        /// Informações do Documento
        /// </summary>
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
        /// <summary>
        /// Código do Município de Descarregamento
        /// </summary>
        [XmlElement("cMunDescarga", Order = 0)]
        public string CMunDescarga { get; set; }

        /// <summary>
        /// Nome do Município de Descarregamento
        /// </summary>
        [XmlElement("xMunDescarga", Order = 1)]
        public string XMunDescarga { get; set; }

        /// <summary>
        /// Chave de Acesso da NF-e
        /// </summary>
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

        /// <summary>
        /// Descrição do Evento: Encerramento
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Encerramento";

        /// <summary>
        /// Número do Protocolo
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Data do Encerramento
        /// </summary>
        [XmlIgnore]
        public DateTime DtEnc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DtEnc" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("DtEnc", Order = 2)]
        public string DtEncField
        {
            get => DtEnc.ToString("yyyy-MM-dd");
            set => DtEnc = DateTime.Parse(value);
        }

        /// <summary>
        /// Código da UF
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "CUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF", Order = 3)]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Código do Município
        /// </summary>
        [XmlElement("cMun", Order = 4)]
        public long CMun { get; set; }

        /// <summary>
        /// Indicador de Encerramento por Terceiro
        /// </summary>
        [XmlElement("indEncPorTerceiro", Order = 5)]
        public int IndEncPorTerceiro { get; set; } = 0;

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
                <cMun>{CMun}</cMun>" +
                (IndEncPorTerceiro.Equals(1) ? $@"<indEncPorTerceiro>1</indEncPorTerceiro>" : "") +
                $@"</evEncMDFe>");
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

        private void PrepararInfPag2(XmlDocument xmlDoc)
        {
            var infPags = xmlDoc.GetElementsByTagName("infPag");

            if (!(infPags?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoAlteracaoPagtoServMDFe detEvento)
            {
                detEvento.InfPag = new List<AlteracaoPagtoServMDFeInfPag>();

                foreach (var elementInfPag in infPags.Cast<XmlElement>())
                {
                    detEvento.InfPag.Add(new AlteracaoPagtoServMDFeInfPag
                    {
                        XNome = elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.XNome)),
                        CNPJ = elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.CNPJ)),
                        CPF = elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.CPF)),
                        IdEstrangeiro = elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.IdEstrangeiro)),
                        VContrato = UConvert.ToDouble(elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.VContrato)), true),
                        IndPag = elementInfPag.GetValue<IndicadorPagamento>(nameof(AlteracaoPagtoServMDFeInfPag.IndPag)),
                        VAdiant = UConvert.ToDouble(elementInfPag.GetValue<string>(nameof(AlteracaoPagtoServMDFeInfPag.VAdiant)), true)
                    });

                    PrepararComp2(elementInfPag);
                    PrepararInfPrazo2(elementInfPag);
                    PrepararInfBanc2(elementInfPag);
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

        private void PrepararComp2(XmlElement xmlDoc)
        {
            var comps = xmlDoc.GetElementsByTagName("Comp");

            if (!(comps?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoAlteracaoPagtoServMDFe detEvento)
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

        private void PrepararInfBanc2(XmlElement xmlDoc)
        {
            var infBanc = xmlDoc.GetElementsByTagName("infBanc");

            if (!(infBanc?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoAlteracaoPagtoServMDFe detEvento)
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

        private void PrepararInfPrazo2(XmlElement xmlDoc)
        {
            var infPrazos = xmlDoc.GetElementsByTagName("infPrazo");

            if (!(infPrazos?.Count > 0))
            {
                return;
            }

            if (InfEvento.DetEvento is DetEventoAlteracaoPagtoServMDFe detEvento)
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

        /// <summary>
        /// Informações do Evento
        /// </summary>
        [XmlElement("infEvento", Order = 0)]
        public InfEvento InfEvento { get; set; }

        /// <summary>
        /// Assinatura XML
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#", Order = 1)]
        public Signature Signature { get; set; }

        /// <summary>
        /// Versão do leiaute
        /// </summary>
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

                case "110118":
                    PrepararInfPag2(document);
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
    [Serializable()]
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

                //Encerrou o detalhe do evento
                if (XmlReader.Name == "detEvento")
                {
                    break;
                }

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

        /// <summary>
        /// Descrição do Evento
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public virtual string DescEvento { get; set; }

        /// <summary>
        /// Versão do Evento
        /// </summary>
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

        /// <summary>
        /// Chave de Acesso do MDF-e
        /// </summary>
        [XmlElement("chMDFe", Order = 4)]
        public string ChMDFe { get; set; }

        /// <summary>
        /// CNPJ do Emitente
        /// </summary>
        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Emitente
        /// </summary>
        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        /// <summary>
        /// Código do Órgão
        /// </summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>
        /// Campo para serialização do Código do Órgão
        /// </summary>
        [XmlElement("cOrgao", Order = 0)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Detalhe do Evento
        /// </summary>
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

                    case TipoEventoMDFe.RegistroPassagemBRId:
                        _detEvento = value is DetEventoMDFeRegPassagemAuto ? value : new DetEventoMDFeRegPassagemAuto();
                        break;

                    case TipoEventoMDFe.ConfirmacaoServicoTransporte:
                        _detEvento = value is DetEventoConfirmaServMDFe ? value : new DetEventoConfirmaServMDFe();
                        break;

                    case TipoEventoMDFe.AlteracaoPagamentoServico:
                        _detEvento = value is DetEventoAlteracaoPagtoServMDFe ? value : new DetEventoAlteracaoPagtoServMDFe();
                        break;

                    case TipoEventoMDFe.EncerramentoFisco:
                        _detEvento = value is DetEventoEncerramentoFisco ? value : new DetEventoEncerramentoFisco();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                _detEvento.XmlReader = value.XmlReader;
                _detEvento.ProcessReader();
            }
        }

        /// <summary>
        /// Data e Hora do Evento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        /// <summary>
        /// Campo para serialização da Data e Hora do Evento
        /// </summary>
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

        /// <summary>
        /// Identificador do Evento
        /// </summary>
        [XmlAttribute(DataType = "ID")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChMDFe + NSeqEvento.ToString("000");
            set => _ = value;
        }

        /// <summary>
        /// Número Sequencial do Evento
        /// </summary>
        [XmlElement("nSeqEvento", Order = 8)]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Tipo de Ambiente
        /// </summary>
        [XmlElement("tpAmb", Order = 1)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Tipo do Evento
        /// </summary>
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

        /// <summary>
        /// evPagtoOperMDFe - Evento Pagamento da Operação do MDF-e
        /// </summary>
        [XmlElement(ElementName = "evPagtoOperMDFe", Order = 0)]
        public EventoPagtoOperMDFe EventoPagtoOperMDFe
        {
            get => _eventoPagtoOperMDFe ?? (_eventoPagtoOperMDFe = new EventoPagtoOperMDFe());
            set => _eventoPagtoOperMDFe = value;
        }

        /// <summary>
        /// Descrição do Evento
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoPagtoOperMDFe.DescEvento;
            set => EventoPagtoOperMDFe.DescEvento = value;
        }

        /// <summary>
        /// Número do Protocolo
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoPagtoOperMDFe.NProt;
            set => EventoPagtoOperMDFe.NProt = value;
        }

        /// <summary>
        /// Informações das Viagens
        /// </summary>
        [XmlIgnore]
        public InfViagens InfViagens
        {
            get => EventoPagtoOperMDFe.InfViagens;
            set => EventoPagtoOperMDFe.InfViagens = value;
        }

        /// <summary>
        /// Informações de Pagamento
        /// </summary>
        [XmlIgnore]
        public List<PagtoOperMDFeInfPag> InfPag
        {
            get => EventoPagtoOperMDFe.InfPag;
            set => EventoPagtoOperMDFe.InfPag = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoPagtoOperMDFe em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
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
                                 <indPag>{infPag.IndPagField}</indPag>";

                if (infPag.IndPag == IndicadorPagamento.PagamentoPrazo)
                {
                    writeRaw += $@"<vAdiant>{infPag.VAdiantField}</vAdiant>";
                }

                if (infPag.InfPrazo != null)
                {
                    foreach (var infPrazo in infPag.InfPrazo)
                    {
                        writeRaw += $@"<infPrazo>
                                     <nParcela>{infPrazo.NParcela}</nParcela>
                                     <dVenc>{infPrazo.DVencField}</dVenc>
                                     <vParcela>{infPrazo.VParcelaField}</vParcela>
                                     </infPrazo>";
                    }
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
        /// <summary>
        /// Quantidade de viagens.
        /// </summary>
        [XmlElement("qtdViagens", Order = 0)]
        public string QtdViagens { get; set; }

        /// <summary>
        /// Número da viagem.
        /// </summary>
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
        private int IndAntecipaAdiantField;

        /// <summary>
        /// Lista de componentes de pagamento.
        /// </summary>
        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

        /// <summary>
        /// Valor do contrato.
        /// </summary>
        [XmlIgnore]
        public double VContrato { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VContrato" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vContrato")]
        public string VContratoField
        {
            get => VContrato.ToString("F2", CultureInfo.InvariantCulture);
            set => VContrato = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Indicador de pagamento.
        /// </summary>
        [XmlIgnore]
        public IndicadorPagamento IndPag { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "IndPag" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("indPag")]
        public int IndPagField
        {
            get => (int)IndPag;
            set => IndPag = (IndicadorPagamento)Enum.Parse(typeof(IndicadorPagamento), value.ToString());
        }

        /// <summary>
        /// Valor do adiantamento.
        /// </summary>
        [XmlIgnore]
        public double VAdiant { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VAdiant" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAdiant")]
        public string VAdiantField
        {
            get => VAdiant.ToString("F2", CultureInfo.InvariantCulture);
            set => VAdiant = Converter.ToDouble(value);
        }

        /// <summary>
        /// Indicador para declarar concordância em antecipar o adiantamento (Informar a tag somente se for autorizado antecipar o adiantamento)
        /// </summary>
        [XmlElement("indAntecipaAdiant")]
        public int IndAntecipaAdiant
        {
            get => IndAntecipaAdiantField;
            set
            {
                if (value != 1)
                {
                    throw new Exception("Conteúdo da TAG <indAntecipaAdiant> inválido! Valores aceitos: 1 ou não informe a TAG.");
                }

                IndAntecipaAdiantField = value;
            }
        }

        /// <summary>
        /// Informações de prazo de pagamento.
        /// </summary>
        [XmlElement("infPrazo")]
        public List<InfPrazo> InfPrazo { get; set; }

        /// <summary>
        /// Tipo de Permissão em relação a antecipação das parcelas
        /// </summary>
        [XmlElement("tpAntecip")]
#if INTEROP
        public TipoPermissaoAtencipacaoParcela TpAntecip { get; set; } = (TipoPermissaoAtencipacaoParcela)(-1);
#else
        public TipoPermissaoAtencipacaoParcela? TpAntecip { get; set; }
#endif

        /// <summary>
        /// Informações bancárias.
        /// </summary>
        [XmlElement("infBanc")]
        public InfBanc InfBanc { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Indica se a propriedade IndAntecipaAdiant deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIndAntecipaAdiant() => IndAntecipaAdiant == 1;

#if INTEROP
        /// <summary>
        /// Indica se a propriedade TpAntecip deve ser serializada.
        /// </summary>
        public bool ShouldSerializeTpAntecip() => TpAntecip != (TipoPermissaoAtencipacaoParcela)(-1);
#else
        /// <summary>
        /// Indica se a propriedade TpAntecip deve ser serializada.
        /// </summary>
        public bool ShouldSerializeTpAntecip() => TpAntecip != null;
#endif

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista de componentes de pagamento.
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
        /// Retorna o elemento da lista de componentes de pagamento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
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
        /// Retorna a quantidade de elementos existentes na lista de componentes de pagamento.
        /// </summary>
        public int GetCompCount => (Comp != null ? Comp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista de informações de prazo de pagamento.
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
        /// Retorna o elemento da lista de informações de prazo de pagamento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
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
        /// Retorna a quantidade de elementos existentes na lista de informações de prazo de pagamento.
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

        /// <summary>
        /// Evento de Registro de Passagem do MDF-e.
        /// </summary>
        [XmlElement(ElementName = "evMDFeRegPassagem", Order = 0)]
        public EventoMDFeRegPassagem EventoMDFeRegPassagem
        {
            get => _eventoMDFeRegPassagem ?? (_eventoMDFeRegPassagem = new EventoMDFeRegPassagem());
            set => _eventoMDFeRegPassagem = value;
        }

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoMDFeRegPassagem.DescEvento;
            set => EventoMDFeRegPassagem.DescEvento = value;
        }

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlIgnore]
        public string CUFTransito
        {
            get => EventoMDFeRegPassagem.CUFTransito;
            set => EventoMDFeRegPassagem.CUFTransito = value;
        }

        /// <summary>
        /// Código da unidade fiscal.
        /// </summary>
        [XmlIgnore]
        public string CUnidFiscal
        {
            get => EventoMDFeRegPassagem.CUnidFiscal;
            set => EventoMDFeRegPassagem.CUnidFiscal = value;
        }

        /// <summary>
        /// Descrição da unidade fiscal.
        /// </summary>
        [XmlIgnore]
        public string XUnidFiscal
        {
            get => EventoMDFeRegPassagem.XUnidFiscal;
            set => EventoMDFeRegPassagem.XUnidFiscal = value;
        }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlIgnore]
        public string DhPass
        {
            get => EventoMDFeRegPassagem.DhPass;
            set => EventoMDFeRegPassagem.DhPass = value;
        }

        /// <summary>
        /// CPF do funcionário.
        /// </summary>
        [XmlIgnore]
        public string CPFFunc
        {
            get => EventoMDFeRegPassagem.CPFFunc;
            set => EventoMDFeRegPassagem.CPFFunc = value;
        }

        /// <summary>
        /// Nome do funcionário.
        /// </summary>
        [XmlIgnore]
        public string XFunc
        {
            get => EventoMDFeRegPassagem.XFunc;
            set => EventoMDFeRegPassagem.XFunc = value;
        }

        /// <summary>
        /// Tipo de transmissão.
        /// </summary>
        [XmlIgnore]
        public string TpTransm
        {
            get => EventoMDFeRegPassagem.TpTransm;
            set => EventoMDFeRegPassagem.TpTransm = value;
        }

        /// <summary>
        /// Tipo de sentido.
        /// </summary>
        [XmlIgnore]
        public string TpSentido
        {
            get => EventoMDFeRegPassagem.TpSentido;
            set => EventoMDFeRegPassagem.TpSentido = value;
        }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlIgnore]
        public string Placa
        {
            get => EventoMDFeRegPassagem.Placa;
            set => EventoMDFeRegPassagem.Placa = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoMDFeRegPassagem em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
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
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Registro de Passagem";

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlElement("cUFTransito", Order = 1)]
        public string CUFTransito { get; set; }

        /// <summary>
        /// Código da unidade fiscal.
        /// </summary>
        [XmlElement("cUnidFiscal", Order = 1)]
        public string CUnidFiscal { get; set; }

        /// <summary>
        /// Descrição da unidade fiscal.
        /// </summary>
        [XmlElement("xUnidFiscal", Order = 1)]
        public string XUnidFiscal { get; set; }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlElement("dhPass", Order = 1)]
        public string DhPass { get; set; }

        /// <summary>
        /// CPF do funcionário.
        /// </summary>
        [XmlElement("CPFFunc", Order = 1)]
        public string CPFFunc { get; set; }

        /// <summary>
        /// Nome do funcionário.
        /// </summary>
        [XmlElement("xFunc", Order = 1)]
        public string XFunc { get; set; }

        /// <summary>
        /// Tipo de transmissão.
        /// </summary>
        [XmlElement("tpTransm", Order = 1)]
        public string TpTransm { get; set; }

        /// <summary>
        /// Tipo de sentido.
        /// </summary>
        [XmlElement("tpSentido", Order = 1)]
        public string TpSentido { get; set; }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlElement("placa", Order = 1)]
        public string Placa { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoMDFeRegPassagemAuto")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEventoMDFeRegPassagemAuto")]
    public class DetEventoMDFeRegPassagemAuto : EventoDetalhe
    {
        private EventoMDFeRegPassagemAuto _eventoMDFeRegPassagemAuto;

        /// <summary>
        /// Define o valor da propriedade usando informações de reflexão.
        /// </summary>
        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        /// <summary>
        /// Evento de Registro de Passagem Automático do MDF-e.
        /// </summary>
        [XmlElement(ElementName = "evMDFeRegPassagemAuto", Order = 0)]
        public EventoMDFeRegPassagemAuto EventoMDFeRegPassagemAuto
        {
            get => _eventoMDFeRegPassagemAuto ?? (_eventoMDFeRegPassagemAuto = new EventoMDFeRegPassagemAuto());
            set => _eventoMDFeRegPassagemAuto = value;
        }

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoMDFeRegPassagemAuto.DescEvento;
            set => EventoMDFeRegPassagemAuto.DescEvento = value;
        }

        /// <summary>
        /// Tipo de transmissão.
        /// </summary>
        [XmlIgnore]
        public string TpTransm
        {
            get => EventoMDFeRegPassagemAuto.TpTransm;
            set => EventoMDFeRegPassagemAuto.TpTransm = value;
        }

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlIgnore]
        public string CUFTransito
        {
            get => EventoMDFeRegPassagemAuto.CUFTransito;
            set => EventoMDFeRegPassagemAuto.CUFTransito = value;
        }

        /// <summary>
        /// Código de identificação do equipamento.
        /// </summary>
        [XmlIgnore]
        public string CIdEquip
        {
            get => EventoMDFeRegPassagemAuto.CIdEquip;
            set => EventoMDFeRegPassagemAuto.CIdEquip = value;
        }

        /// <summary>
        /// Descrição do equipamento.
        /// </summary>
        [XmlIgnore]
        public string XIdEquip
        {
            get => EventoMDFeRegPassagemAuto.XIdEquip;
            set => EventoMDFeRegPassagemAuto.XIdEquip = value;
        }

        /// <summary>
        /// Tipo do equipamento.
        /// </summary>
        [XmlIgnore]
        public string TpEquip
        {
            get => EventoMDFeRegPassagemAuto.TpEquip;
            set => EventoMDFeRegPassagemAuto.TpEquip = value;
        }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlIgnore]
        public string Placa
        {
            get => EventoMDFeRegPassagemAuto.Placa;
            set => EventoMDFeRegPassagemAuto.Placa = value;
        }

        /// <summary>
        /// Tipo de sentido.
        /// </summary>
        [XmlIgnore]
        public string TpSentido
        {
            get => EventoMDFeRegPassagemAuto.TpSentido;
            set => EventoMDFeRegPassagemAuto.TpSentido = value;
        }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlIgnore]
        public string DhPass
        {
            get => EventoMDFeRegPassagemAuto.DhPass;
            set => EventoMDFeRegPassagemAuto.DhPass = value;
        }

        /// <summary>
        /// Latitude da passagem.
        /// </summary>
        [XmlIgnore]
        public string Latitude
        {
            get => EventoMDFeRegPassagemAuto.Latitude;
            set => EventoMDFeRegPassagemAuto.Latitude = value;
        }

        /// <summary>
        /// Longitude da passagem.
        /// </summary>
        [XmlIgnore]
        public string Longitude
        {
            get => EventoMDFeRegPassagemAuto.Longitude;
            set => EventoMDFeRegPassagemAuto.Longitude = value;
        }

        /// <summary>
        /// Número Sequencial Único.
        /// </summary>
        [XmlIgnore]
        public string NSU
        {
            get => EventoMDFeRegPassagemAuto.NSU;
            set => EventoMDFeRegPassagemAuto.NSU = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoMDFeRegPassagemAuto em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evMDFeRegPassagemAuto>
                    <descEvento>{DescEvento}</descEvento>
                    <tpTransm>{TpTransm}</tpTransm>
                    <infPass>
                    <cUFTransito>{CUFTransito}</cUFTransito>
                    <cIdEquip>{CIdEquip}</cIdEquip>
                    <xIdEquip>{XIdEquip}</xIdEquip>
                    <tpEquip>{TpEquip}</tpEquip>
                    <placa>{Placa}</placa>
                    <tpSentido>{TpSentido}</tpSentido>
                    <dhPass>{DhPass}</dhPass>
                    <latitude>{Latitude}</latitude>
                    <longitude>{Longitude}</longitude>
                    <NSU>{NSU}</NSU>
                    </infPass>
                    </evMDFeRegPassagemAuto>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoMDFeRegPassagemAuto")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "evMDFeRegPassagemAuto")]
    public class EventoMDFeRegPassagemAuto : EventoDetalhe
    {
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Registro de Passagem Automático";

        /// <summary>
        /// Tipo de transmissão.
        /// </summary>
        [XmlElement("tpTransm", Order = 1)]
        public string TpTransm { get; set; }

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlElement("cUFTransito", Order = 1)]
        public string CUFTransito { get; set; }

        /// <summary>
        /// Código de identificação do equipamento.
        /// </summary>
        [XmlElement("cIdEquip", Order = 1)]
        public string CIdEquip { get; set; }

        /// <summary>
        /// Descrição do equipamento.
        /// </summary>
        [XmlElement("xIdEquip", Order = 1)]
        public string XIdEquip { get; set; }

        /// <summary>
        /// Tipo do equipamento.
        /// </summary>
        [XmlElement("tpEquip", Order = 1)]
        public string TpEquip { get; set; }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlElement("placa", Order = 1)]
        public string Placa { get; set; }

        /// <summary>
        /// Tipo de sentido.
        /// </summary>
        [XmlElement("tpSentido", Order = 1)]
        public string TpSentido { get; set; }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlElement("dhPass", Order = 1)]
        public string DhPass { get; set; }

        /// <summary>
        /// Latitude da passagem.
        /// </summary>
        [XmlElement("latitude", Order = 1)]
        public string Latitude { get; set; }

        /// <summary>
        /// Longitude da passagem.
        /// </summary>
        [XmlElement("longitude", Order = 1)]
        public string Longitude { get; set; }

        /// <summary>
        /// Número Sequencial Único.
        /// </summary>
        [XmlElement("NSU", Order = 1)]
        public string NSU { get; set; }
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

        /// <summary>
        /// Define o valor da propriedade usando informações de reflexão.
        /// </summary>
        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        /// <summary>
        /// Evento de Alteração do Pagamento do Serviço do MDF-e.
        /// </summary>
        [XmlElement(ElementName = "evAlteracaoPagtoServMDFe", Order = 0)]
        public EventoAlteracaoPagtoServMDFe EventoAlteracaoPagtoServMDFe
        {
            get => _eventoAlteracaoPagtoServMDFe ?? (_eventoAlteracaoPagtoServMDFe = new EventoAlteracaoPagtoServMDFe());
            set => _eventoAlteracaoPagtoServMDFe = value;
        }

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoAlteracaoPagtoServMDFe.DescEvento;
            set => EventoAlteracaoPagtoServMDFe.DescEvento = value;
        }

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoAlteracaoPagtoServMDFe.NProt;
            set => EventoAlteracaoPagtoServMDFe.NProt = value;
        }

        /// <summary>
        /// Informações de Pagamento.
        /// </summary>
        [XmlIgnore]
        public List<AlteracaoPagtoServMDFeInfPag> InfPag
        {
            get => EventoAlteracaoPagtoServMDFe.InfPag;
            set => EventoAlteracaoPagtoServMDFe.InfPag = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoAlteracaoPagtoServMDFe em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
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
                                 <indPag>{infPag.IndPagField}</indPag>";

                if (infPag.IndPag == IndicadorPagamento.PagamentoPrazo)
                {
                    writeRaw += $"<vAdiant>{infPag.VAdiantField}</vAdiant>";
                }

                if (infPag.InfPrazo != null)
                {
                    foreach (var infPrazo in infPag.InfPrazo)
                    {
                        writeRaw += $@"<infPrazo>
                                     <nParcela>{infPrazo.NParcela}</nParcela>
                                     <dVenc>{infPrazo.DVencField}</dVenc>
                                     <vParcela>{infPrazo.VParcelaField}</vParcela>
                                     </infPrazo>";
                    }
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

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Alteracao Pagamento Servico MDFe";

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Informações de Pagamento.
        /// </summary>
        [XmlElement("infPag", Order = 3)]
        public List<AlteracaoPagtoServMDFeInfPag> InfPag { get; set; } = new List<AlteracaoPagtoServMDFeInfPag>();

        #endregion Public Properties

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista de informações de pagamento.
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
        /// Retorna o elemento da lista de informações de pagamento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
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
        /// Retorna a quantidade de elementos existentes na lista de informações de pagamento.
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

        /// <summary>
        /// Define o valor da propriedade usando informações de reflexão.
        /// </summary>
        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        /// <summary>
        /// Evento de Confirmação do Serviço do MDF-e.
        /// </summary>
        [XmlElement(ElementName = "evConfirmaServMDFe", Order = 0)]
        public EventoConfirmaServMDFe EventoConfirmaServMDFe
        {
            get => _eventoConfirmaServMDFe ?? (_eventoConfirmaServMDFe = new EventoConfirmaServMDFe());
            set => _eventoConfirmaServMDFe = value;
        }

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoConfirmaServMDFe.DescEvento;
            set => EventoConfirmaServMDFe.DescEvento = value;
        }

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoConfirmaServMDFe.NProt;
            set => EventoConfirmaServMDFe.NProt = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoConfirmaServMDFe em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
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

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Confirmacao Servico Transporte";

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        #endregion Public Properties
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.DetEventoEncerramentoFisco")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "evMDFeEncFisco")]
    public class DetEventoEncerramentoFisco : EventoDetalhe
    {
        private EventoEncerramentoFisco _eventoEncerramentoEvento;

        /// <summary>
        /// Define o valor da propriedade usando informações de reflexão.
        /// </summary>
        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        /// <summary>
        /// Evento de Encerramento Fiscal do MDF-e.
        /// </summary>
        [XmlElement(ElementName = "evMDFeEncFisco", Order = 0)]
        public EventoEncerramentoFisco EventoEncerramentoFisco
        {
            get => _eventoEncerramentoEvento ?? (_eventoEncerramentoEvento = new EventoEncerramentoFisco());
            set => _eventoEncerramentoEvento = value;
        }

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoEncerramentoFisco.DescEvento;
            set => EventoEncerramentoFisco.DescEvento = value;
        }

        /// <summary>
        /// Tipo de Encerramento.
        /// </summary>
        [XmlIgnore]
        public string TpEnc
        {
            get => EventoEncerramentoFisco.TpEnc;
            set => EventoEncerramentoFisco.TpEnc = value;
        }

        /// <summary>
        /// Justificativa do Encerramento.
        /// </summary>
        [XmlIgnore]
        public string XJust
        {
            get => EventoEncerramentoFisco.XJust;
            set => EventoEncerramentoFisco.XJust = value;
        }

        /// <summary>
        /// Serializa o objeto DetEventoEncerramentoFisco em XML.
        /// </summary>
        /// <param name="writer">O XmlWriter usado para serialização.</param>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evMDFeEncFisco>
                    <descEvento>{DescEvento}</descEvento>
                    <tpEnc>{TpEnc}</tpEnc>
                    <xJust>{XJust}</xJust>";

            writeRaw += $@"</evMDFeEncFisco>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EventoEncerramentoFisco")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "evMDFeEncFisco")]
    public class EventoEncerramentoFisco : EventoDetalhe
    {
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Encerramento Fisco";

        /// <summary>
        /// Tipo de Encerramento.
        /// </summary>
        [XmlElement("tpEnc", Order = 1)]
        public string TpEnc { get; set; }

        /// <summary>
        /// Justificativa do Encerramento.
        /// </summary>
        [XmlElement("xJust", Order = 2)]
        public string XJust { get; set; }
    }
}