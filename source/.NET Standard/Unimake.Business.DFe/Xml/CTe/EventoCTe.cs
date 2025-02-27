#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCanc")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCanc : EventoDetalhe
    {
        #region Public Properties

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento";

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Justificativa do Cancelamento.
        /// </summary>
        [XmlElement("xJust", Order = 2)]
        public string XJust { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evCancCTe>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <xJust>{XJust}</xJust>
            </evCancCTe>");
        }

        #endregion Public Methods
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoPrestDesacordo")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoPrestDesacordo : EventoDetalhe
    {
        #region Public Properties

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Prestacao do Servico em Desacordo";

        /// <summary>
        /// Indicador de Desacordo na Operação.
        /// </summary>
        [XmlElement("indDesacordoOper", Order = 1)]
        public string IndDesacordoOper { get; set; }

        /// <summary>
        /// Observações.
        /// </summary>
        [XmlElement("xObs", Order = 2)]
        public string XObs { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
                <evPrestDesacordo>
                <descEvento>{DescEvento}</descEvento>
                <indDesacordoOper>{IndDesacordoOper}</indDesacordoOper>
                <xObs>{XObs}</xObs>
                </evPrestDesacordo>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCancelamentoPrestDesacordo")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCancelamentoPrestDesacordo : EventoDetalhe
    {
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento Prestacao do Servico em Desacordo";

        /// <summary>
        /// Número do Protocolo do Evento de Prestação em Desacordo.
        /// </summary>
        [XmlElement("nProtEvPrestDes", Order = 1)]
        public string NProtEvPrestDes { get; set; }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
                <evCancPrestDesacordo>
                <descEvento>{DescEvento}</descEvento>
                <nProtEvPrestDes>{NProtEvPrestDes}</nProtEvPrestDes>
                </evCancPrestDesacordo>");
        }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCancCompEntrega")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCancCompEntrega : EventoDetalhe
    {
        #region Public Properties

        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento do Comprovante de Entrega do CT-e";

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Número do Protocolo do Comprovante de Entrega.
        /// </summary>
        [XmlElement("nProtCE", Order = 2)]
        public string NProtCE { get; set; }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
                <evCancCECTe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <nProtCE>{NProtCE}</nProtCE>
                </evCancCECTe>");
        }

        #endregion Public Methods
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCCE")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "detEvento")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class DetEventoCCE : EventoDetalhe
    {
        #region Private Fields

        private EventoCCeCTe _eventoCCeCTe;

        #endregion Private Fields

        #region Internal Methods

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi?.Name == nameof(InfCorrecao))
            {
                XmlReader.Read();
                var grupoAlterado = XmlReader.GetValue<string>(nameof(Xml.CTe.InfCorrecao.GrupoAlterado));
                var campoAlterado = XmlReader.GetValue<string>(nameof(Xml.CTe.InfCorrecao.CampoAlterado));
                var valorAlterado = XmlReader.GetValue<string>(nameof(Xml.CTe.InfCorrecao.ValorAlterado));
                var nroItemAlterado = XmlReader.GetValue<string>(nameof(Xml.CTe.InfCorrecao.NroItemAlterado));

                InfCorrecao.Add(new InfCorrecao
                {
                    GrupoAlterado = grupoAlterado,
                    CampoAlterado = campoAlterado,
                    ValorAlterado = valorAlterado,
                    NroItemAlterado = nroItemAlterado
                });

                return;
            }

            base.SetValue(pi);
        }

        #endregion Internal Methods

        #region Public Properties

        /// <summary>
        /// Obtém ou define a descrição do evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoCCeCTe.DescEvento;
            set => EventoCCeCTe.DescEvento = value;
        }

        /// <summary>
        /// Obtém ou define o evento de carta de correção do CT-e.
        /// </summary>
        [XmlElement(ElementName = "evCCeCTe", Order = 0)]
        public EventoCCeCTe EventoCCeCTe
        {
            get => _eventoCCeCTe ?? (_eventoCCeCTe = new EventoCCeCTe());
            set => _eventoCCeCTe = value;
        }

        /// <summary>
        /// Obtém ou define a lista de informações de correção.
        /// </summary>
        [XmlIgnore]
        public List<InfCorrecao> InfCorrecao
        {
            get => EventoCCeCTe.InfCorrecao;
            set => EventoCCeCTe.InfCorrecao = value;
        }

        /// <summary>
        /// Obtém ou define as condições de uso da carta de correção.
        /// </summary>
        [XmlIgnore]
        public string XCondUso
        {
            get => EventoCCeCTe.XCondUso;
            set => EventoCCeCTe.XCondUso = value;
        }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCCeCTe>" +
                $@"<descEvento>{DescEvento}</descEvento>";

            foreach (var infCorrecao in InfCorrecao)
            {
                writeRaw += $@"<infCorrecao>" +
                    $@"<grupoAlterado>{infCorrecao.GrupoAlterado}</grupoAlterado>" +
                    $@"<campoAlterado>{infCorrecao.CampoAlterado}</campoAlterado>" +
                    $@"<valorAlterado>{infCorrecao.ValorAlterado}</valorAlterado>" +
                    $@"</infCorrecao>";
            }

            writeRaw += $@"<xCondUso>{XCondUso}</xCondUso>" +
                $@"</evCCeCTe>";

            writer.WriteRaw(writeRaw);
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoEPEC")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoEPEC : EventoDetalhe
    {
        private EvEPECCTe _evEPECCTe;

        /// <summary>
        /// Obtém ou define o evento de EPEC do CT-e.
        /// </summary>
        [XmlElement(ElementName = "evEPECCTe", Order = 0)]
        public EvEPECCTe EvEPECCTe
        {
            get => _evEPECCTe ?? (_evEPECCTe = new EvEPECCTe());
            set => _evEPECCTe = value;
        }

        /// <summary>
        /// Obtém ou define a descrição do evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvEPECCTe.DescEvento;
            set => EvEPECCTe.DescEvento = value;
        }

        /// <summary>
        /// Obtém ou define a justificativa do EPEC.
        /// </summary>
        [XmlIgnore]
        public string XJust
        {
            get => EvEPECCTe.XJust;
            set => EvEPECCTe.XJust = value;
        }

        /// <summary>
        /// Obtém ou define o valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS
        {
            get => EvEPECCTe.VICMS;
            set => EvEPECCTe.VICMS = value;
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string VICMSField
        {
            get => EvEPECCTe.VICMSField;
            set => EvEPECCTe.VICMSField = value;
        }

        /// <summary>
        /// Obtém ou define o valor do ICMS ST.
        /// </summary>
        [XmlIgnore]
        public double VICMSST
        {
            get => EvEPECCTe.VICMSST;
            set => EvEPECCTe.VICMSST = value;
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSST" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string VICMSSTField
        {
            get => EvEPECCTe.VICMSSTField;
            set => EvEPECCTe.VICMSSTField = value;
        }

        /// <summary>
        /// Obtém ou define o valor total da prestação.
        /// </summary>
        [XmlIgnore]
        public double VTPrest
        {
            get => EvEPECCTe.VTPrest;
            set => EvEPECCTe.VTPrest = value;
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTPrest" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string VTPrestField
        {
            get => EvEPECCTe.VTPrestField;
            set => EvEPECCTe.VTPrestField = value;
        }

        /// <summary>
        /// Obtém ou define o valor da carga.
        /// </summary>
        [XmlIgnore]
        public double VCarga
        {
            get => EvEPECCTe.VCarga;
            set => EvEPECCTe.VCarga = value;
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCarga" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string VCargaField
        {
            get => EvEPECCTe.VCargaField;
            set => EvEPECCTe.VCargaField = value;
        }

        /// <summary>
        /// Obtém ou define as informações do tomador do serviço (tipo 4).
        /// </summary>
        [XmlIgnore]
        public EvEPECCTeToma4 Toma4 { get; set; }

        /// <summary>
        /// Obtém ou define a modalidade do transporte.
        /// </summary>
        [XmlIgnore]
        public ModalidadeTransporteCTe Modal
        {
            get => EvEPECCTe.Modal;
            set => EvEPECCTe.Modal = value;
        }

        /// <summary>
        /// Obtém ou define a UF de início da prestação.
        /// </summary>
        [XmlIgnore]
        public UFBrasil UFIni
        {
            get => EvEPECCTe.UFIni;
            set => EvEPECCTe.UFIni = value;
        }

        /// <summary>
        /// Obtém ou define a UF de fim da prestação.
        /// </summary>
        [XmlIgnore]
        public UFBrasil UFFim
        {
            get => EvEPECCTe.UFFim;
            set => EvEPECCTe.UFFim = value;
        }

        /// <summary>
        /// Obtém ou define o tipo do CT-e.
        /// </summary>
        [XmlIgnore]
        public TipoCTe TpCTe
        {
            get => EvEPECCTe.TpCTe;
            set => EvEPECCTe.TpCTe = value;
        }

        /// <summary>
        /// Obtém ou define a data e hora de emissão do CT-e.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi
        {
            get => EvEPECCTe.DhEmi;
            set => EvEPECCTe.DhEmi = value;
        }
#else
        public DateTimeOffset DhEmi
        {
            get => EvEPECCTe.DhEmi;
            set => EvEPECCTe.DhEmi = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhEmiField
        {
            get => EvEPECCTe.DhEmiField;
            set => EvEPECCTe.DhEmiField = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evEPECCTe>
                <descEvento>{DescEvento}</descEvento>
                <xJust>{XJust}</xJust>
                <vICMS>{VICMSField}</vICMS>";

            if (VICMSST > 0)
            {
                writeRaw += $@"<vICMSST>{VICMSSTField}</vICMSST>";
            }

            writeRaw += $@"<vTPrest>{VTPrestField}</vTPrest>
                <vCarga>{VCargaField}</vCarga>";

            writeRaw += $@"<toma4>
                <toma>{(int)Toma4.Toma}</toma>
                <UF>{Toma4.UF.ToString()}</UF>";

            if (!string.IsNullOrWhiteSpace(Toma4.CNPJ))
            {
                writeRaw += $@"<CNPJ>{Toma4.CNPJ}</CNPJ>";
            }

            if (!string.IsNullOrWhiteSpace(Toma4.CPF))
            {
                writeRaw += $@"<CPF>{Toma4.CPF}</CPF>";
            }

            if (!string.IsNullOrWhiteSpace(Toma4.IE))
            {
                writeRaw += $@"<IE>{Toma4.IE}</IE>";
            }

            writeRaw += $@"</toma4>
                <modal>{((int)Modal).ToString().PadLeft(2, '0')}</modal>
                <UFIni>{UFIni.ToString()}</UFIni>
                <UFFim>{UFFim.ToString()}</UFFim>
                <tpCTe>{(int)TpCTe}</tpCTe>
                <dhEmi>{DhEmiField}</dhEmi>";

            writeRaw += $@"</evEPECCTe>";

            writer.WriteRaw(writeRaw);
        }


        internal override void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }

            var xml = new XmlDocument();
            xml.Load(XmlReader);

            if (xml.GetElementsByTagName("detEvento")[0].Attributes.GetNamedItem("versaoEvento") != null)
            {
                VersaoEvento = xml.GetElementsByTagName("detEvento")[0].Attributes.GetNamedItem("versaoEvento").Value;
            }

            if (xml.GetElementsByTagName("evEPECCTe").Count > 0)
            {
                var evEPECCTe = (XmlElement)xml.GetElementsByTagName("evEPECCTe")[0];

                if (evEPECCTe.GetElementsByTagName("descEvento").Count > 0)
                {
                    DescEvento = evEPECCTe.GetElementsByTagName("descEvento")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("xJust").Count > 0)
                {
                    XJust = evEPECCTe.GetElementsByTagName("xJust")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("vICMS").Count > 0)
                {
                    VICMSField = evEPECCTe.GetElementsByTagName("vICMS")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("vICMSST").Count > 0)
                {
                    VICMSSTField = evEPECCTe.GetElementsByTagName("vICMSST")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("vTPrest").Count > 0)
                {
                    VTPrestField = evEPECCTe.GetElementsByTagName("vTPrest")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("vCarga").Count > 0)
                {
                    VCargaField = evEPECCTe.GetElementsByTagName("vCarga")[0].InnerText;
                }

                if (evEPECCTe.GetElementsByTagName("toma4").Count > 0)
                {
                    Toma4 = new EvEPECCTeToma4();

                    var toma4 = (XmlElement)evEPECCTe.GetElementsByTagName("toma4")[0];

                    if (toma4.GetElementsByTagName("toma").Count > 0)
                    {
                        if (toma4.GetElementsByTagName("toma").Count > 0)
                        {
                            Toma4.Toma = (TomadorServicoCTe)Convert.ToInt32(toma4.GetElementsByTagName("toma")[0].InnerText);
                        }

                        if (toma4.GetElementsByTagName("UF").Count > 0)
                        {
                            Toma4.UF = (UFBrasil)Enum.Parse(typeof(UFBrasil), toma4.GetElementsByTagName("UF")[0].InnerText);
                        }

                        if (toma4.GetElementsByTagName("CNPJ").Count > 0)
                        {
                            Toma4.CNPJ = toma4.GetElementsByTagName("CNPJ")[0].InnerText;
                        }

                        if (toma4.GetElementsByTagName("CPF").Count > 0)
                        {
                            Toma4.CPF = toma4.GetElementsByTagName("CPF")[0].InnerText;
                        }

                        if (toma4.GetElementsByTagName("IE").Count > 0)
                        {
                            Toma4.IE = toma4.GetElementsByTagName("IE")[0].InnerText;
                        }
                    }
                }

                if (evEPECCTe.GetElementsByTagName("modal").Count > 0)
                {
                    Modal = (ModalidadeTransporteCTe)Convert.ToInt32(evEPECCTe.GetElementsByTagName("modal")[0].InnerText);
                }

                if (evEPECCTe.GetElementsByTagName("UFIni").Count > 0)
                {
                    UFIni = (UFBrasil)Enum.Parse(typeof(UFBrasil), evEPECCTe.GetElementsByTagName("UFIni")[0].InnerText);
                }

                if (evEPECCTe.GetElementsByTagName("UFFim").Count > 0)
                {
                    UFFim = (UFBrasil)Enum.Parse(typeof(UFBrasil), evEPECCTe.GetElementsByTagName("UFFim")[0].InnerText);
                }

                if (evEPECCTe.GetElementsByTagName("tpCTe").Count > 0)
                {
                    TpCTe = (TipoCTe)Convert.ToInt32(evEPECCTe.GetElementsByTagName("tpCTe")[0].InnerText);
                }

                if (evEPECCTe.GetElementsByTagName("dhEmi").Count > 0)
                {
                    DhEmiField = evEPECCTe.GetElementsByTagName("dhEmi")[0].InnerText;
                }
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvEPECCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evEPECCTe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvEPECCTe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "EPEC";

        /// <summary>
        /// Justificativa do EPEC.
        /// </summary>
        [XmlElement("xJust", Order = 1)]
        public string XJust { get; set; }

        /// <summary>
        /// Valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS", Order = 2)]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST.
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSST" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST", Order = 3)]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total da Prestação.
        /// </summary>
        [XmlIgnore]
        public double VTPrest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTPrest" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTPrest", Order = 4)]
        public string VTPrestField
        {
            get => VTPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VTPrest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Carga.
        /// </summary>
        [XmlIgnore]
        public double VCarga { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCarga" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCarga", Order = 5)]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informações do Tomador do Serviço (tipo 4).
        /// </summary>
        [XmlElement("toma4", Order = 6)]
        public EvEPECCTeToma4 Toma4 { get; set; }

        /// <summary>
        /// Modalidade do Transporte.
        /// </summary>
        [XmlElement("modal", Order = 7)]
        public ModalidadeTransporteCTe Modal { get; set; }

        /// <summary>
        /// UF de Início da Prestação.
        /// </summary>
        [XmlElement("UFIni", Order = 8)]
        public UFBrasil UFIni { get; set; }

        /// <summary>
        /// UF de Fim da Prestação.
        /// </summary>
        [XmlElement("UFFim", Order = 9)]
        public UFBrasil UFFim { get; set; }

        /// <summary>
        /// Tipo do CT-e.
        /// </summary>
        [XmlElement("tpCTe", Order = 10)]
        public TipoCTe TpCTe { get; set; }

        /// <summary>
        /// Data e Hora de Emissão.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif


        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEmi", Order = 11)]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {

        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {

        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvEPECCTeToma4")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvEPECCTeToma4
    {
        private TomadorServicoCTe TomaField;

        /// <summary>
        /// Tipo do tomador do serviço.
        /// </summary>
        [XmlElement("toma", Order = 0)]
        public TomadorServicoCTe Toma
        {
            get => TomaField;
            set => TomaField = value;
        }

        /// <summary>
        /// UF do tomador do serviço.
        /// </summary>
        [XmlElement("UF", Order = 1)]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// CNPJ do tomador do serviço.
        /// </summary>
        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do tomador do serviço.
        /// </summary>
        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do tomador do serviço.
        /// </summary>
        [XmlElement("IE", Order = 4)]
        public string IE { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCompEntrega")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCompEntrega : EventoDetalhe
    {
        #region Private Fields

        private EventoCECTe _eventoCECTe;

        #endregion Private Fields

        #region Internal Methods

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi.Name == nameof(InfEntrega))
            {
                XmlReader.Read();
                InfEntrega.Add(new InfEntrega
                {
                    ChNFe = XmlReader.GetValue<string>(nameof(Xml.CTe.InfEntrega.ChNFe))
                });
                return;
            }

            base.SetValue(pi);
        }

        #endregion Internal Methods

        #region Public Properties

        /// <summary>
        /// Obtém ou define a descrição do evento.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoCECTe.DescEvento;
            set => EventoCECTe.DescEvento = value;
        }

        /// <summary>
        /// Obtém ou define a data e hora da entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEntrega
        {
            get => EventoCECTe.DhEntrega;
            set => EventoCECTe.DhEntrega = value;
        }
#else
        public DateTimeOffset DhEntrega
        {
            get => EventoCECTe.DhEntrega;
            set => EventoCECTe.DhEntrega = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhEntregaField
        {
            get => EventoCECTe.DhEntregaField;
            set => EventoCECTe.DhEntregaField = value;
        }

        /// <summary>
        /// Obtém ou define a data e hora do hash da entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhHashEntrega
        {
            get => EventoCECTe.DhHashEntrega;
            set => EventoCECTe.DhHashEntrega = value;
        }
#else
        public DateTimeOffset DhHashEntrega
        {
            get => EventoCECTe.DhHashEntrega;
            set => EventoCECTe.DhHashEntrega = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhHashEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhHashEntregaField
        {
            get => EventoCECTe.DhHashEntregaField;
            set => EventoCECTe.DhHashEntregaField = value;
        }

        /// <summary>
        /// Obtém ou define o evento de comprovante de entrega do CT-e.
        /// </summary>
        [XmlElement(ElementName = "evCECTe", Order = 0)]
        public EventoCECTe EventoCECTe
        {
            get => _eventoCECTe ?? (_eventoCECTe = new EventoCECTe());
            set => _eventoCECTe = value;
        }

        /// <summary>
        /// Obtém ou define o hash da entrega.
        /// </summary>
        [XmlIgnore]
        public string HashEntrega
        {
            get => EventoCECTe.HashEntrega;
            set => EventoCECTe.HashEntrega = value;
        }

        /// <summary>
        /// Obtém ou define a lista de informações de entrega.
        /// </summary>
        [XmlIgnore]
        public List<InfEntrega> InfEntrega
        {
            get => EventoCECTe.InfEntrega;
            set => EventoCECTe.InfEntrega = value;
        }

        /// <summary>
        /// Obtém ou define a latitude da entrega.
        /// </summary>
        [XmlIgnore]
        public string Latitude
        {
            get => EventoCECTe.Latitude;
            set => EventoCECTe.Latitude = value;
        }

        /// <summary>
        /// Obtém ou define a longitude da entrega.
        /// </summary>
        [XmlIgnore]
        public string Longitude
        {
            get => EventoCECTe.Longitude;
            set => EventoCECTe.Longitude = value;
        }

        /// <summary>
        /// Obtém ou define o número do documento.
        /// </summary>
        [XmlIgnore]
        public string NDoc
        {
            get => EventoCECTe.NDoc;
            set => EventoCECTe.NDoc = value;
        }

        /// <summary>
        /// Obtém ou define o número do protocolo.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoCECTe.NProt;
            set => EventoCECTe.NProt = value;
        }

        /// <summary>
        /// Obtém ou define o nome da pessoa que recebeu a entrega.
        /// </summary>
        [XmlIgnore]
        public string XNome
        {
            get => EventoCECTe.XNome;
            set => EventoCECTe.XNome = value;
        }

        #endregion Public Properties

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCECTe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <dhEntrega>{DhEntregaField}</dhEntrega>
                <nDoc>{NDoc}</nDoc>
                <xNome>{XNome}</xNome>
                <latitude>{Latitude}</latitude>
                <longitude>{Longitude}</longitude>
                <hashEntrega>{HashEntrega}</hashEntrega>
                <dhHashEntrega>{DhHashEntregaField}</dhHashEntrega>";

            foreach (var infEntrega in InfEntrega)
            {
                writeRaw += $@"<infEntrega><chNFe>{infEntrega.ChNFe}</chNFe></infEntrega>";
            }

            writeRaw += $@"</evCECTe>";

            writer.WriteRaw(writeRaw);
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EventoCCeCTe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot(ElementName = "detEvento")]
    public class EventoCCeCTe : EventoDetalhe
    {
        #region Private Fields

        private string XCondUsoField = "A Carta de Correcao e disciplinada pelo Art. 58-B do CONVENIO/SINIEF 06/89: Fica permitida a utilizacao de carta de correcao, para regularizacao de erro ocorrido na emissao de documentos fiscais relativos a prestacao de servico de transporte, desde que o erro nao esteja relacionado com: I - as variaveis que determinam o valor do imposto tais como: base de calculo, aliquota, diferenca de preco, quantidade, valor da prestacao;II - a correcao de dados cadastrais que implique mudanca do emitente, tomador, remetente ou do destinatario;III - a data de emissao ou de saida.";

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Descrição do evento da carta de correção.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Carta de Correcao";

        /// <summary>
        /// Lista de informações de correção.
        /// </summary>
        [XmlElement("infCorrecao", Order = 1)]
        public List<InfCorrecao> InfCorrecao { get; set; } = new List<InfCorrecao>();

        /// <summary>
        /// Condições de uso da carta de correção.
        /// </summary>
        [XmlElement("xCondUso", Order = 2)]
        public string XCondUso
        {
            get => XCondUsoField;
            set => XCondUsoField = value;
        }

        #endregion Public Properties

#if INTEROP

        /// <summary>
        /// Adiciona um novo elemento à lista de informações de correção.
        /// </summary>
        /// <param name="infcorrecao">Elemento a ser adicionado.</param>
        public void AddInfCorrecao(InfCorrecao infcorrecao)
        {
            if (InfCorrecao == null)
            {
                InfCorrecao = new List<InfCorrecao>();
            }

            InfCorrecao.Add(infcorrecao);
        }

        /// <summary>
        /// Retorna o elemento da lista de informações de correção no índice especificado.
        /// </summary>
        /// <param name="index">Índice do elemento a ser retornado.</param>
        /// <returns>Elemento da lista no índice especificado.</returns>
        public InfCorrecao GetInfCorrecao(int index)
        {
            if ((InfCorrecao?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCorrecao[index];
        }

        /// <summary>
        /// Retorna o número de elementos na lista de informações de correção.
        /// </summary>
        public int GetInfCorrecaoCount => (InfCorrecao != null ? InfCorrecao.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EventoCECTe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCECTe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EventoCECTe : EventoDetalhe
    {
        #region Public Properties

        /// <summary>
        /// Descrição do evento de comprovante de entrega do CT-e.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Comprovante de Entrega do CT-e";

        /// <summary>
        /// Data e hora da entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEntrega { get; set; }
#else
        public DateTimeOffset DhEntrega { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEntrega", Order = 2)]
        public string DhEntregaField
        {
            get => DhEntrega.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEntrega = DateTime.Parse(value);
#else
            set => DhEntrega = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data e hora do hash da entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhHashEntrega { get; set; }
#else
        public DateTimeOffset DhHashEntrega { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhHashEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhHashEntrega", Order = 8)]
        public string DhHashEntregaField
        {
            get => DhHashEntrega.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhHashEntrega = DateTime.Parse(value);
#else
            set => DhHashEntrega = DateTimeOffset.Parse(value);
#endif
        }

        private string HashEntregaField;

        /// <summary>
        /// Hash da entrega (SHA1 Base64).
        /// </summary>
        [XmlElement("hashEntrega", Order = 7)]
        public string HashEntrega
        {
            get => HashEntregaField;
            set
            {
                if (Converter.IsSHA1Base64(value))
                {
                    HashEntregaField = value;
                }
                else
                {
                    HashEntregaField = Converter.CalculateSHA1Hash(value);
                }
            }
        }

        /// <summary>
        /// Lista de informações de entrega.
        /// </summary>
        [XmlElement("infEntrega", Order = 9)]
        public List<InfEntrega> InfEntrega { get; set; } = new List<InfEntrega>();

        /// <summary>
        /// Latitude da entrega.
        /// </summary>
        [XmlElement("latitude", Order = 5)]
        public string Latitude { get; set; }

        /// <summary>
        /// Longitude da entrega.
        /// </summary>
        [XmlElement("longitude", Order = 6)]
        public string Longitude { get; set; }

        /// <summary>
        /// Número do documento.
        /// </summary>
        [XmlElement("nDoc", Order = 3)]
        public string NDoc { get; set; }

        /// <summary>
        /// Número do protocolo.
        /// </summary>
        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        /// <summary>
        /// Nome da pessoa que recebeu a entrega.
        /// </summary>
        [XmlElement("xNome", Order = 4)]
        public string XNome { get; set; }

        #endregion Public Properties

#if INTEROP

        /// <summary>
        /// Adiciona um novo elemento à lista de informações de entrega.
        /// </summary>
        /// <param name="infentrega">Elemento a ser adicionado.</param>
        public void AddInfEntrega(InfEntrega infentrega)
        {
            if (InfEntrega == null)
            {
                InfEntrega = new List<InfEntrega>();
            }

            InfEntrega.Add(infentrega);
        }

        /// <summary>
        /// Retorna o elemento da lista de informações de entrega no índice especificado.
        /// </summary>
        /// <param name="index">Índice do elemento a ser retornado (começa em 0).</param>
        /// <returns>Elemento da lista no índice especificado.</returns>
        public InfEntrega GetInfEntrega(int index)
        {
            if ((InfEntrega?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfEntrega[index];
        }

        /// <summary>
        /// Retorna o número de elementos na lista de informações de entrega.
        /// </summary>
        public int GetInfEntregaCount => (InfEntrega != null ? InfEntrega.Count : 0);

#endif

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EventoCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class EventoCTe : XMLBase
    {
        #region Private Methods

        private void SignEvent(EventoCTe evento, XmlElement xmlEl)
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
        /// Informações do evento.
        /// </summary>
        [XmlElement("infEvento", Order = 0)]
        public InfEvento InfEvento { get; set; }

        /// <summary>
        /// Assinatura digital do evento.
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#", Order = 1)]
        public Signature Signature { get; set; }

        /// <summary>
        /// Versão do leiaute do evento.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Gera o XML do evento.
        /// </summary>
        /// <returns>Documento XML do evento.</returns>
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

        /// <summary>
        /// Lê o XML do evento e o deserializa para o objeto EventoCTe.
        /// </summary>
        /// <typeparam name="T">Tipo do objeto a ser deserializado.</typeparam>
        /// <param name="doc">Documento XML do evento.</param>
        /// <returns>Objeto EventoCTe deserializado.</returns>
        public override T LerXML<T>(XmlDocument doc)
        {
            if (typeof(T) != typeof(EventoCTe))
            {
                throw new InvalidCastException($"Cannot cast type '{typeof(T).Name}' into type '{typeof(EventoCTe).Name}'.");
            }

            var retornar = base.LerXML<T>(doc) as EventoCTe;
            var eventos = doc.GetElementsByTagName("eventoCTe");

            if ((eventos?.Count ?? 0) > 0)
            {
                var xmlEl = (XmlElement)eventos[0];

                var xml = new StringBuilder();
                xml.Append("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                xml.Append($"<eventoCTe versao=\"{eventos[0].Attributes["versao"].Value}\" xmlns=\"{xmlEl.NamespaceURI}\">");
                xml.Append($"{xmlEl.InnerXml}</eventoCTe>");

                var envEvt = XMLUtility.Deserializar<EventoCTe>(xml.ToString());
                var evt = envEvt;
                SignEvent(evt, xmlEl);
                retornar = evt;
            }

            return (T)(object)retornar;
        }

        /// <summary>
        /// Desserializa o XML do eventoCTe a partir de um arquivo.
        /// </summary>
        /// <param name="filename">Caminho do arquivo XML do eventoCTe.</param>
        /// <returns>Objeto EventoCTe deserializado.</returns>
        public EventoCTe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EventoCTe>(doc);
        }

        /// <summary>
        /// Desserializa o XML do eventoCTe a partir de uma string XML.
        /// </summary>
        /// <param name="xml">String XML do eventoCTe.</param>
        /// <returns>Objeto EventoCTe deserializado.</returns>
        public EventoCTe LoadFromXML(string xml) => XMLUtility.Deserializar<EventoCTe>(xml);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EventoDetalhe")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(DetEventoCanc))]
    [XmlInclude(typeof(DetEventoCCE))]
    [XmlInclude(typeof(DetEventoCancCompEntrega))]
    [XmlInclude(typeof(DetEventoCompEntrega))]
    [XmlInclude(typeof(DetEventoInsucessoEntrega))]
    [XmlInclude(typeof(EventoCECTe))]
    [XmlInclude(typeof(DetEventoPrestDesacordo))]
    [XmlInclude(typeof(DetEventoFiscoMDFeCancelado))]
    [XmlInclude(typeof(DetEventoFiscoMDFeAutorizado))]
    public class EventoDetalhe : System.Xml.Serialization.IXmlSerializable
    {
        #region Private Fields

        private static readonly BindingFlags bindingFlags = BindingFlags.Public |
                                                                BindingFlags.Instance |
                                                                BindingFlags.IgnoreCase;

        private static readonly List<string> hasField = new List<string>
        {
            "DhEntrega",
            "DhHashEntrega",
            "DhTentativaEntrega",
            "DhHashTentativaEntrega",
            "NTentativa",
            "VICMS",
            "VICMSST",
            "VTPrest",
            "VCarga",
            "Modal",
            "UFIni",
            "UFFim",
            "TpCTe",
            "DhEmi",
            "DhPass",
            "DhRecbto"
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

                if (SetLocalValue(type) &&
                    XmlReader.NodeType == XmlNodeType.Element)
                {
                    SetLocalValue(type);
                }
            }
        }

        internal virtual void SetValue(PropertyInfo pi) => pi?.SetValue(this, XmlReader.GetValue<object>(XmlReader.Name));

        #endregion Internal Methods

        #region Public Properties

        /// <summary>
        /// Obtém ou define a descrição do evento.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public virtual string DescEvento { get; set; }

        /// <summary>
        /// Obtém ou define a versão do evento.
        /// </summary>
        [XmlAttribute(AttributeName = "versaoEvento")]
        public virtual string VersaoEvento { get; set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Obtém o esquema XML.
        /// </summary>
        /// <returns>O esquema XML.</returns>
        public XmlSchema GetSchema() => default;

        /// <summary>
        /// Lê o XML do leitor especificado.
        /// </summary>
        /// <param name="reader">O leitor XML.</param>
        public void ReadXml(XmlReader reader) => XmlReader = reader;

        /// <summary>
        /// Escreve o XML para o escritor especificado.
        /// </summary>
        /// <param name="writer">O escritor XML.</param>
        public virtual void WriteXml(XmlWriter writer) => writer.WriteAttributeString("versaoEvento", VersaoEvento);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCorrecao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCorrecao
    {
        #region Public Properties

        /// <summary>
        /// Obtém ou define o campo alterado.
        /// </summary>
        [XmlElement("campoAlterado", Order = 1)]
        public string CampoAlterado { get; set; }

        /// <summary>
        /// Obtém ou define o grupo alterado.
        /// </summary>
        [XmlElement("grupoAlterado", Order = 0)]
        public string GrupoAlterado { get; set; }

        /// <summary>
        /// Obtém ou define o número do item alterado.
        /// </summary>
        [XmlElement("nroItemAlterado", Order = 3)]
        public string NroItemAlterado { get; set; }

        /// <summary>
        /// Obtém ou define o valor alterado.
        /// </summary>
        [XmlElement("valorAlterado", Order = 2)]
        public string ValorAlterado { get; set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Verifica se a propriedade NroItemAlterado deve ser serializada.
        /// </summary>
        /// <returns>True se deve ser serializada, false caso contrário.</returns>
        public bool ShouldSerializeNroItemAlterado() => !string.IsNullOrWhiteSpace(NroItemAlterado);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfEntrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfEntrega
    {
        private string ChNFeField;

        /// <summary>
        /// Obtém ou define a chave da NF-e.
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe
        {
            get => ChNFeField;
            set
            {
                if (value.Length != 44)
                {
                    throw new Exception("Conteúdo da tag <chNFe> filha da tag <infEntrega> inválido! O conteúdo da tag deve ter 44 dígitos.");
                }

                ChNFeField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfEvento
    {
        #region Private Fields

        private EventoDetalhe _detEvento;

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Obtém ou define o órgão emissor.
        /// </summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "COrgao" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cOrgao", Order = 0)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Obtém ou define o ambiente de processamento.
        /// </summary>
        [XmlElement("tpAmb", Order = 1)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Obtém ou define o CNPJ do emitente.
        /// </summary>
        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        /// <summary>
        /// Obtém ou define o CPF do emitente.
        /// </summary>
        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        /// <summary>
        /// Obtém ou define a chave do CT-e.
        /// </summary>
        [XmlElement("chCTe", Order = 4)]
        public string ChCTe { get; set; }

        /// <summary>
        /// Obtém ou define a data e hora do evento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEvento" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEvento", Order = 5)]
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
        /// Obtém ou define o tipo do evento.
        /// </summary>
        [XmlElement("tpEvento", Order = 6)]
        public TipoEventoCTe TpEvento { get; set; }

        /// <summary>
        /// Obtém ou define o número sequencial do evento.
        /// </summary>
        [XmlElement("nSeqEvento", Order = 7)]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Obtém ou define os detalhes do evento.
        /// </summary>
        [XmlElement("detEvento", Order = 8)]
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

                    case TipoEventoCTe.Cancelamento:
                        _detEvento = new DetEventoCanc();
                        break;

                    case TipoEventoCTe.ComprovanteEntrega:
                        _detEvento = new DetEventoCompEntrega();
                        break;

                    case TipoEventoCTe.CancelamentoComprovanteEntrega:
                        _detEvento = new DetEventoCancCompEntrega();
                        break;

                    case TipoEventoCTe.CartaCorrecao:
                        _detEvento = new DetEventoCCE();
                        break;

                    case TipoEventoCTe.PrestDesacordo:
                        _detEvento = new DetEventoPrestDesacordo();
                        break;

                    case TipoEventoCTe.CancelamentoPrestDesacordo:
                        _detEvento = new DetEventoCancelamentoPrestDesacordo();
                        break;

                    case TipoEventoCTe.EPEC:
                        _detEvento = new DetEventoEPEC();
                        break;

                    case TipoEventoCTe.MDFeCancelado:
                        _detEvento = new DetEventoFiscoMDFeCancelado();
                        break;

                    case TipoEventoCTe.MDFeAutorizado:
                        _detEvento = new DetEventoFiscoMDFeAutorizado();
                        break;

                    case TipoEventoCTe.InsucessoEntrega:
                        _detEvento = new DetEventoInsucessoEntrega();
                        break;

                    case TipoEventoCTe.CancelamentoInsucessoEntrega:
                        _detEvento = new DetEventoCancelamentoInsucessoEntrega();
                        break;

                    case TipoEventoCTe.RegistroPassagem:
                        _detEvento = new DetEventoRegistroPassagem();
                        break;

                    case TipoEventoCTe.RegistroPassagemAutomatico:
                        _detEvento = new DetEventoRegistroPassagemAutomatico();
                        break;

                    case TipoEventoCTe.RegistoPassagemAutomaticoOriginadoMDFe:
                        _detEvento = new DetEventoRegistroPassagemAutomaticoMDFe();
                        break;

                    case TipoEventoCTe.AutorizadoCTeComplementar:
                        _detEvento = new DetEventoAutorizadoCTeComplementar();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                _detEvento.XmlReader = value.XmlReader;
                _detEvento.ProcessReader();
            }
        }

        /// <summary>
        /// Obtém ou define as informações da solicitação da NFF.
        /// </summary>
        [XmlElement("infSolicNFF", Order = 9)]
        public InfSolicNFF InfSolicNFF { get; set; }

        /// <summary>
        /// Obtém ou define o ID do evento.
        /// </summary>
        [XmlAttribute(DataType = "ID", AttributeName = "Id")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChCTe + NSeqEvento.ToString((DetEvento.VersaoEvento == "3.00" ? "00" : "000"));
            set => _ = value;
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Inicializa uma nova instância da classe <see cref="InfEvento"/>.
        /// </summary>
        public InfEvento() { }

        /// <summary>
        /// Inicializa uma nova instância da classe <see cref="InfEvento"/> com os detalhes do evento especificados.
        /// </summary>
        /// <param name="detEvento">Os detalhes do evento.</param>
        public InfEvento(EventoDetalhe detEvento) => DetEvento = detEvento ?? throw new ArgumentNullException(nameof(detEvento));

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        /// <returns>True se deve ser serializada, false caso contrário.</returns>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoInsucessoEntrega")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoInsucessoEntrega : EventoDetalhe
    {
        private EvIECTe _eventoIECTe;

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi.Name == nameof(InfEntrega))
            {
                XmlReader.Read();
                InfEntrega.Add(new InfEntrega
                {
                    ChNFe = XmlReader.GetValue<string>(nameof(Xml.CTe.InfEntrega.ChNFe))
                });

                return;
            }

            if (pi.Name == nameof(TpMotivo))
            {
                TpMotivo = XmlReader.GetValue<TipoMotivoInsucessoEntrega>(nameof(TpMotivo));

                return;
            }

            base.SetValue(pi);
        }

        /// <summary>
        /// Descrição do evento de insucesso de entrega.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIECTe.DescEvento;
            set => EventoIECTe.DescEvento = value;
        }

        /// <summary>
        /// Número do protocolo do evento.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EventoIECTe.NProt;
            set => EventoIECTe.NProt = value;
        }

        /// <summary>
        /// Data e hora da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhTentativaEntrega
        {
            get => EventoIECTe.DhTentativaEntrega;
            set => EventoIECTe.DhTentativaEntrega = value;
        }
#else
        public DateTimeOffset DhTentativaEntrega
        {
            get => EventoIECTe.DhTentativaEntrega;
            set => EventoIECTe.DhTentativaEntrega = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhTentativaEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhTentativaEntregaField
        {
            get => EventoIECTe.DhTentativaEntregaField;
            set => EventoIECTe.DhTentativaEntregaField = value;
        }

        /// <summary>
        /// Número da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
        public int NTentativa
        {
            get => EventoIECTe.NTentativa;
            set => EventoIECTe.NTentativa = value;
        }

        /// <summary>
        /// Tipo do motivo do insucesso da entrega.
        /// </summary>
        [XmlIgnore]
        public TipoMotivoInsucessoEntrega TpMotivo
        {
            get => EventoIECTe.TpMotivo;
            set => EventoIECTe.TpMotivo = value;
        }

        /// <summary>
        /// Justificativa do motivo do insucesso da entrega (quando o motivo for "Outros").
        /// </summary>
        [XmlIgnore]
        public string XJustMotivo
        {
            get => EventoIECTe.XJustMotivo;
            set => EventoIECTe.XJustMotivo = value;
        }

        /// <summary>
        /// Latitude da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
        public string Latitude
        {
            get => EventoIECTe.Latitude;
            set => EventoIECTe.Latitude = value;
        }

        /// <summary>
        /// Longitude da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
        public string Longitude
        {
            get => EventoIECTe.Longitude;
            set => EventoIECTe.Longitude = value;
        }

        /// <summary>
        /// Hash da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
        public string HashTentativaEntrega
        {
            get => EventoIECTe.HashTentativaEntrega;
            set => EventoIECTe.HashTentativaEntrega = value;
        }

        /// <summary>
        /// Data e hora do hash da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhHashTentativaEntrega
        {
            get => EventoIECTe.DhHashTentativaEntrega;
            set => EventoIECTe.DhHashTentativaEntrega = value;
        }
#else
        public DateTimeOffset DhHashTentativaEntrega
        {
            get => EventoIECTe.DhHashTentativaEntrega;
            set => EventoIECTe.DhHashTentativaEntrega = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhHashTentativaEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhHashTentativaEntregaField
        {
            get => EventoIECTe.DhHashTentativaEntregaField;
            set => EventoIECTe.DhHashTentativaEntregaField = value;
        }

        /// <summary>
        /// Lista de informações de entrega.
        /// </summary>
        [XmlIgnore]
        public List<InfEntrega> InfEntrega
        {
            get => EventoIECTe.InfEntrega;
            set => EventoIECTe.InfEntrega = value;
        }

        /// <summary>
        /// Evento de insucesso de entrega do CT-e.
        /// </summary>
        [XmlElement(ElementName = "evIECTe")]
        public EvIECTe EventoIECTe
        {
            get => _eventoIECTe ?? (_eventoIECTe = new EvIECTe());
            set => _eventoIECTe = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evIECTe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <dhTentativaEntrega>{DhTentativaEntregaField}</dhTentativaEntrega>";

            if (NTentativa > 0)
            {
                writeRaw += $@"<nTentativa>{NTentativa}</nTentativa>";
            }

            writeRaw += $@"<tpMotivo>{((int)TpMotivo).ToString()}</tpMotivo>";

            if (TpMotivo == TipoMotivoInsucessoEntrega.Outros)
            {
                writeRaw += $@"<xJustMotivo>{XJustMotivo}</xJustMotivo>";
            }

            if (!string.IsNullOrWhiteSpace(Latitude))
            {
                writeRaw += $@"<latitude>{Latitude}</latitude>";
            }

            if (!string.IsNullOrWhiteSpace(Longitude))
            {
                writeRaw += $@"<longitude>{Longitude}</longitude>";
            }

            writeRaw += $@"<hashTentativaEntrega>{HashTentativaEntrega}</hashTentativaEntrega>
                    <dhHashTentativaEntrega>{DhHashTentativaEntregaField}</dhHashTentativaEntrega>";

            foreach (var infEntrega in InfEntrega)
            {
                writeRaw += $@"<infEntrega><chNFe>{infEntrega.ChNFe}</chNFe></infEntrega>";
            }

            writeRaw += $@"</evIECTe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoCancelamentoInsucessoEntrega")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoCancelamentoInsucessoEntrega : EventoDetalhe
    {
        private EvCancIECTe _eventoCancIECTe;

        internal override void SetValue(PropertyInfo pi) => base.SetValue(pi);

        /// <summary>
        /// Descrição do evento de cancelamento de insucesso de entrega.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCancIECTe.DescEvento;
            set => EvCancIECTe.DescEvento = value;
        }

        /// <summary>
        /// Número do protocolo do evento de insucesso de entrega a ser cancelado.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EvCancIECTe.NProt;
            set => EvCancIECTe.NProt = value;
        }

        /// <summary>
        /// Número do protocolo do evento de insucesso de entrega.
        /// </summary>
        [XmlIgnore]
        public string NProtIE
        {
            get => EvCancIECTe.NProtIE;
            set => EvCancIECTe.NProtIE = value;
        }

        /// <summary>
        /// Evento de cancelamento de insucesso de entrega do CT-e.
        /// </summary>
        [XmlElement(ElementName = "evCancIECTe")]
        public EvCancIECTe EvCancIECTe
        {
            get => _eventoCancIECTe ?? (_eventoCancIECTe = new EvCancIECTe());
            set => _eventoCancIECTe = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCancIECTe>
                <descEvento>{DescEvento}</descEvento>
                <nProt>{NProt}</nProt>
                <nProtIE>{NProtIE}</nProtIE>
                </evCancIECTe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvIECTe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evIECTe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvIECTe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de insucesso na entrega do CT-e.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Insucesso na Entrega do CT-e";

        /// <summary>
        /// Número do protocolo do evento.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Data e hora da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhTentativaEntrega { get; set; }
#else
        public DateTimeOffset DhTentativaEntrega { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhTentativaEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhTentativaEntrega")]
        public string DhTentativaEntregaField
        {
            get => DhTentativaEntrega.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhTentativaEntrega = DateTime.Parse(value);
#else
            set => DhTentativaEntrega = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número da tentativa de entrega.
        /// </summary>
        [XmlElement("nTentativa")]
        public int NTentativa { get; set; }

        /// <summary>
        /// Tipo do motivo do insucesso da entrega.
        /// </summary>
        [XmlElement("tpMotivo")]
        public TipoMotivoInsucessoEntrega TpMotivo { get; set; }

        /// <summary>
        /// Justificativa do motivo do insucesso da entrega (quando o motivo for "Outros").
        /// </summary>
        [XmlElement("xJustMotivo")]
        public string XJustMotivo { get; set; }

        /// <summary>
        /// Latitude da tentativa de entrega.
        /// </summary>
        [XmlElement("latitude")]
        public string Latitude { get; set; }

        /// <summary>
        /// Longitude da tentativa de entrega.
        /// </summary>
        [XmlElement("longitude")]
        public string Longitude { get; set; }

        private string HashTentativaEntregaField;

        /// <summary>
        /// Hash da tentativa de entrega (SHA1 Base64).
        /// </summary>
        [XmlElement("hashTentativaEntrega")]
        public string HashTentativaEntrega
        {
            get => HashTentativaEntregaField;
            set
            {
                if (Converter.IsSHA1Base64(value))
                {
                    HashTentativaEntregaField = value;
                }
                else
                {
                    HashTentativaEntregaField = Converter.CalculateSHA1Hash(value);
                }
            }
        }

        /// <summary>
        /// Data e hora do hash da tentativa de entrega.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhHashTentativaEntrega { get; set; }
#else
        public DateTimeOffset DhHashTentativaEntrega { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhHashTentativaEntrega" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhHashTentativaEntrega")]
        public string DhHashTentativaEntregaField
        {
            get => DhHashTentativaEntrega.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhHashTentativaEntrega = DateTime.Parse(value);
#else
            set => DhHashTentativaEntrega = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Lista de informações de entrega.
        /// </summary>
        [XmlElement("infEntrega")]
        public List<InfEntrega> InfEntrega { get; set; } = new List<InfEntrega>();

#if INTEROP

        /// <summary>
        /// Adiciona um novo elemento à lista de informações de entrega.
        /// </summary>
        /// <param name="infentrega">Elemento a ser adicionado.</param>
        public void AddInfEntrega(InfEntrega infentrega)
        {
            if (InfEntrega == null)
            {
                InfEntrega = new List<InfEntrega>();
            }

            InfEntrega.Add(infentrega);
        }

        /// <summary>
        /// Retorna o elemento da lista de informações de entrega no índice especificado.
        /// </summary>
        /// <param name="index">Índice do elemento a ser retornado (começa em 0).</param>
        /// <returns>Elemento da lista no índice especificado.</returns>
        public InfEntrega GetInfEntrega(int index)
        {
            if ((InfEntrega?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfEntrega[index];
        }

        /// <summary>
        /// Retorna o número de elementos na lista de informações de entrega.
        /// </summary>
        public int GetInfEntregaCount => (InfEntrega != null ? InfEntrega.Count : 0);

#endif

        /// <summary>
        /// Lê o XML do documento especificado.
        /// </summary>
        /// <param name="document">Documento XML a ser lido.</param>
        public void ReadXml(XmlDocument document)
        {
            
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização
        /// </summary>
        ///<param name="writer">string XML recebido durante o processo de serialização</param>
        public void WriteXml(System.IO.StringWriter writer)
        {

        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCancIECTe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCancIECTe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCancIECTe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de cancelamento do insucesso de entrega do CT-e.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento do Insucesso de Entrega do CT-e";

        /// <summary>
        /// Número do protocolo do evento de insucesso de entrega a ser cancelado.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Número do protocolo do evento de insucesso de entrega.
        /// </summary>
        [XmlElement("nProtIE")]
        public string NProtIE { get; set; }

        /// <summary>
        /// Lê o XML do documento especificado.
        /// </summary>
        /// <param name="document">Documento XML a ser lido.</param>
        public void ReadXml(XmlDocument document) { }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização
        /// </summary>
        ///<param name="writer">string XML recebido durante o processo de serialização</param>
        public void WriteXml(System.IO.StringWriter writer) { }
    }

    #region Eventos exclusivos do fisco (Gerados pelo fisco)

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoFiscoMDFeCancelado")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoFiscoMDFeCancelado : EventoDetalhe
    {
        private EvCTeCanceladoMDFe _evCTeCanceladoMDFe;

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi?.Name == nameof(MDFe))
            {
                XmlReader.Read();

                MDFe = new EvCTeCanceladoMDFeMDFe();
                MDFe.ChMDFe = XmlReader.GetValue<string>(nameof(MDFe.ChMDFe));
                MDFe.NProtCanc = XmlReader.GetValue<string>(nameof(MDFe.NProtCanc));

                return;
            }

            base.SetValue(pi);
        }

        /// <summary>
        /// Evento de cancelamento do MDFe relacionado ao CTe.
        /// </summary>
        [XmlElement(ElementName = "evCTeCanceladoMDFe", Order = 0)]
        public EvCTeCanceladoMDFe EvCTeCanceladoMDFe
        {
            get => _evCTeCanceladoMDFe ?? (_evCTeCanceladoMDFe = new EvCTeCanceladoMDFe());
            set => _evCTeCanceladoMDFe = value;
        }

        /// <summary>
        /// Descrição do evento de cancelamento do MDFe.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeCanceladoMDFe.DescEvento;
            set => EvCTeCanceladoMDFe.DescEvento = value;
        }

        /// <summary>
        /// Informações do MDFe cancelado.
        /// </summary>
        [XmlIgnore]
        public EvCTeCanceladoMDFeMDFe MDFe
        {
            get => EvCTeCanceladoMDFe.MDFe;
            set => EvCTeCanceladoMDFe.MDFe = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeCanceladoMDFe>";

            writeRaw += $@"<descEvento>{DescEvento}</descEvento>";
            writeRaw += $@"<MDFe>";
            writeRaw += $@"<chMDFe>{EvCTeCanceladoMDFe.MDFe.ChMDFe}</chMDFe>";
            writeRaw += $@"<nProtCanc>{EvCTeCanceladoMDFe.MDFe.NProtCanc}</nProtCanc>";
            writeRaw += $@"</MDFe>";

            writeRaw += $@"</evCTeCanceladoMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeCanceladoMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeCanceladoMDFe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeCanceladoMDFe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de MDFe cancelado.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "MDF-e Cancelado";

        /// <summary>
        /// Informações do MDFe cancelado.
        /// </summary>
        [XmlElement("MDFe", Order = 6)]
        public EvCTeCanceladoMDFeMDFe MDFe { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeCanceladoMDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeCanceladoMDFeMDFe
    {
        /// <summary>
        /// Chave de acesso do MDFe cancelado.
        /// </summary>
        [XmlElement("chMDFe", Order = 0)]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Número do protocolo de cancelamento do MDFe.
        /// </summary>
        [XmlElement("nProtCanc", Order = 1)]
        public string NProtCanc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoFiscoMDFeAutorizado")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoFiscoMDFeAutorizado : EventoDetalhe
    {
        private EvCTeAutorizadoMDFe _evCTeAutorizadoMDFe;

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi?.Name == nameof(MDFe))
            {
                XmlReader.Read();

                MDFe = new EvCTeAutorizadoMDFeMDFe();
                MDFe.ChMDFe = XmlReader.GetValue<string>(nameof(MDFe.ChMDFe));
                MDFe.Modal = XmlReader.GetValue<ModalidadeTransporteCTe>(nameof(MDFe.Modal));
#if INTEROP
                MDFe.DhEmi = XmlReader.GetValue<DateTime>(nameof(MDFe.DhEmi));
#else
                MDFe.DhEmi = XmlReader.GetValue<DateTimeOffset>(nameof(MDFe.DhEmi));
#endif
                MDFe.NProt = XmlReader.GetValue<string>(nameof(MDFe.NProt));

#if INTEROP
                MDFe.DhRecbto = XmlReader.GetValue<DateTime>(nameof(MDFe.DhRecbto));
#else
                MDFe.DhRecbto = XmlReader.GetValue<DateTimeOffset>(nameof(MDFe.DhRecbto));
#endif

                return;
            }

            if (pi?.Name == nameof(Emit))
            {
                XmlReader.Read();

                Emit = new EvCTeAutorizadoMDFeEmit();
                Emit.CNPJ = XmlReader.GetValue<string>(nameof(Emit.CNPJ));
                Emit.IE = XmlReader.GetValue<string>(nameof(Emit.IE));
                Emit.XNome = XmlReader.GetValue<string>(nameof(Emit.XNome));

                return;
            }

            base.SetValue(pi);
        }

        /// <summary>
        /// Evento de autorização do MDFe relacionado ao CTe.
        /// </summary>
        [XmlElement(ElementName = "evCTeAutorizadoMDFe", Order = 0)]
        public EvCTeAutorizadoMDFe EvCTeAutorizadoMDFe
        {
            get => _evCTeAutorizadoMDFe ?? (_evCTeAutorizadoMDFe = new EvCTeAutorizadoMDFe());
            set => _evCTeAutorizadoMDFe = value;
        }

        /// <summary>
        /// Descrição do evento de autorização do MDFe.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeAutorizadoMDFe.DescEvento;
            set => EvCTeAutorizadoMDFe.DescEvento = value;
        }

        /// <summary>
        /// Informações do MDFe autorizado.
        /// </summary>
        [XmlIgnore]
        public EvCTeAutorizadoMDFeMDFe MDFe
        {
            get => EvCTeAutorizadoMDFe.MDFe;
            set => EvCTeAutorizadoMDFe.MDFe = value;
        }

        /// <summary>
        /// Informações do emitente do MDFe.
        /// </summary>
        [XmlIgnore]
        public EvCTeAutorizadoMDFeEmit Emit
        {
            get => EvCTeAutorizadoMDFe.Emit;
            set => EvCTeAutorizadoMDFe.Emit = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeAutorizadoMDFe>";

            writeRaw += $@"<descEvento>{DescEvento}</descEvento>";

            writeRaw += $@"<MDFe>";
            writeRaw += $@"<chMDFe>{MDFe.ChMDFe}</chMDFe>";
            writeRaw += $@"<modal>{(int)MDFe.Modal:00}</modal>";
            writeRaw += $@"<dhEmi>{MDFe.DhEmiField}</dhEmi>";
            writeRaw += $@"<nProt>{MDFe.NProt}</nProt>";
            writeRaw += $@"<dhRecbto>{MDFe.DhRecbtoField}</dhRecbto>";
            writeRaw += $@"</MDFe>";

            writeRaw += $@"<emit>";
            writeRaw += $@"<CNPJ>{Emit.CNPJ}</CNPJ>";
            writeRaw += $@"<IE>{Emit.IE}</IE>";
            writeRaw += $@"<xNome>{Emit.XNome}</xNome>";
            writeRaw += $@"</emit>";

            writeRaw += $@"</evCTeAutorizadoMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeAutorizadoMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeAutorizadoMDFe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeAutorizadoMDFe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de MDFe autorizado.
        /// </summary>
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "MDF-e Autorizado";

        /// <summary>
        /// Informações do MDFe autorizado.
        /// </summary>
        [XmlElement("MDFe", Order = 1)]
        public EvCTeAutorizadoMDFeMDFe MDFe { get; set; }

        /// <summary>
        /// Informações do emitente do MDFe.
        /// </summary>
        [XmlElement("emit", Order = 2)]
        public EvCTeAutorizadoMDFeEmit Emit { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeAutorizadoMDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeAutorizadoMDFeMDFe
    {
        /// <summary>
        /// Chave de acesso do MDFe autorizado.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Modalidade de transporte do MDFe.
        /// </summary>
        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal { get; set; }

        /// <summary>
        /// Data e hora de emissão do MDFe.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo de autorização do MDFe.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Data e hora de recebimento da autorização do MDFe.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhRecbto" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeAutorizadoMDFeEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeAutorizadoMDFeEmit
    {
        /// <summary>
        /// CNPJ do emitente do MDFe.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Inscrição Estadual do emitente do MDFe.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Nome ou razão social do emitente do MDFe.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoRegistroPassagemAutomatico")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoRegistroPassagemAutomatico : EventoDetalhe
    {
        private EvCTeRegPassagemAuto _evCTeRegPassagemAuto;

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi?.Name == nameof(InfPass))
            {
                XmlReader.Read();
                InfPass = new EvCTeRegPassagemAutoInfPass();
                InfPass.CUFTransito = XmlReader.GetValue<string>(nameof(InfPass.CUFTransito));
                InfPass.CIdEquip = XmlReader.GetValue<string>(nameof(InfPass.CIdEquip));
                InfPass.XIdEquip = XmlReader.GetValue<string>(nameof(InfPass.XIdEquip));
                InfPass.TpEquip = XmlReader.GetValue<string>(nameof(InfPass.TpEquip));
                InfPass.Placa = XmlReader.GetValue<string>(nameof(InfPass.Placa));
                InfPass.TpSentido = XmlReader.GetValue<string>(nameof(InfPass.TpSentido));
#if INTEROP
                InfPass.DhPass = XmlReader.GetValue<DateTime>(nameof(InfPass.DhPass));
#else
                InfPass.DhPass = XmlReader.GetValue<DateTimeOffset>(nameof(InfPass.DhPass));
#endif
                InfPass.Latitude = XmlReader.GetValue<string>(nameof(InfPass.Latitude));
                InfPass.Longitude = XmlReader.GetValue<string>(nameof(InfPass.Longitude));
                InfPass.NSU = XmlReader.GetValue<string>(nameof(InfPass.NSU));

                return;
            }

            base.SetValue(pi);
        }

        /// <summary>
        /// Descrição do evento de registro de passagem automático.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeRegPassagemAuto.DescEvento;
            set => EvCTeRegPassagemAuto.DescEvento = value;
        }

        /// <summary>
        /// Tipo de transmissão do registro de passagem.
        /// </summary>
        [XmlIgnore]
        public string TpTransm
        {
            get => EvCTeRegPassagemAuto.TpTransm;
            set => EvCTeRegPassagemAuto.TpTransm = value;
        }

        /// <summary>
        /// Informações do registro de passagem.
        /// </summary>
        [XmlIgnore]
        public EvCTeRegPassagemAutoInfPass InfPass
        {
            get => EvCTeRegPassagemAuto.InfPass;
            set => EvCTeRegPassagemAuto.InfPass = value;
        }

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem.
        /// </summary>
        [XmlIgnore]
        public string ChMDFe
        {
            get => EvCTeRegPassagemAuto.ChMDFe;
            set => EvCTeRegPassagemAuto.ChMDFe = value;
        }

        /// <summary>
        /// Evento de registro de passagem automático do CT-e.
        /// </summary>
        [XmlIgnore]
        public EvCTeRegPassagemAuto EvCTeRegPassagemAuto
        {
            get => _evCTeRegPassagemAuto ?? (_evCTeRegPassagemAuto = new EvCTeRegPassagemAuto());
            set => _evCTeRegPassagemAuto = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeRegPassagemAuto>
                <descEvento>{DescEvento}</descEvento>
                <tpTransm>{TpTransm}</tpTransm>";

            writeRaw += "<infPass>";

            writeRaw += $@"<cUFTransito>{InfPass.CUFTransito}</cUFTransito>
                <cIdEquip>{InfPass.CIdEquip}</cIdEquip>
                <xIdEquip>{InfPass.XIdEquip}</xIdEquip>
                <tpEquip>{InfPass.TpEquip}</tpEquip>
                <placa>{InfPass.Placa}</placa>
                <tpSentido>{InfPass.TpSentido}</tpSentido>
                <dhPass>{InfPass.DhPassField}</dhPass>
                <latitude>{InfPass.Latitude}</latitude>
                <longitude>{InfPass.Longitude}</longitude>
                <NSU>{InfPass.NSU}</NSU>";

            writeRaw += "</infPass>";

            writeRaw += $@"<chMDFe>{ChMDFe}</chMDFe>";

            writeRaw += "</evCTeRegPassagemAuto>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeRegPassagemAuto")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeRegPassagemAuto")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeRegPassagemAuto : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de registro de passagem automático.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Registro de Passagem Automático";

        /// <summary>
        /// Tipo de transmissão do registro de passagem.
        /// </summary>
        [XmlElement("tpTransm")]
        public string TpTransm { get; set; }

        /// <summary>
        /// Informações do registro de passagem.
        /// </summary>
        [XmlElement("infPass")]
        public EvCTeRegPassagemAutoInfPass InfPass { get; set; }

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeRegPassagemAutoInfPass")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeRegPassagemAutoInfPass
    {
        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlElement("cUFTransito")]
        public string CUFTransito { get; set; }

        /// <summary>
        /// Código de identificação do equipamento.
        /// </summary>
        [XmlElement("cIdEquip")]
        public string CIdEquip { get; set; }

        /// <summary>
        /// Descrição do equipamento.
        /// </summary>
        [XmlElement("xIdEquip")]
        public string XIdEquip { get; set; }

        /// <summary>
        /// Tipo do equipamento.
        /// </summary>
        [XmlElement("tpEquip")]
        public string TpEquip { get; set; }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlElement("placa")]
        public string Placa { get; set; }

        /// <summary>
        /// Tipo de sentido do registro de passagem.
        /// </summary>
        [XmlElement("tpSentido")]
        public string TpSentido { get; set; }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhPass { get; set; }
#else
        public DateTimeOffset DhPass { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhPass" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhPass")]
        public string DhPassField
        {
            get => DhPass.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhPass = DateTime.Parse(value);
#else
            set => DhPass = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Latitude da passagem.
        /// </summary>
        [XmlElement("latitude")]
        public string Latitude { get; set; }

        /// <summary>
        /// Longitude da passagem.
        /// </summary>
        [XmlElement("longitude")]
        public string Longitude { get; set; }

        /// <summary>
        /// Número Sequencial Único do registro de passagem.
        /// </summary>
        [XmlElement("NSU")]
        public string NSU { get; set; }
    }

    #endregion

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoRegistroPassagemAutomaticoMDFe")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoRegistroPassagemAutomaticoMDFe : EventoDetalhe
    {
        private EvCTeRegPassagemAutoMDFe _evCTeRegPassagemAutoMDFe;

        /// <summary>
        /// Descrição do evento de registro de passagem automático originado no MDFe.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeRegPassagemAutoMDFe.DescEvento;
            set => EvCTeRegPassagemAutoMDFe.DescEvento = value;
        }

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem automático.
        /// </summary>
        [XmlIgnore]
        public string ChMDFe
        {
            get => EvCTeRegPassagemAutoMDFe.ChMDFe;
            set => EvCTeRegPassagemAutoMDFe.ChMDFe = value;
        }

        /// <summary>
        /// Evento de registro de passagem automático originado no MDFe do CT-e.
        /// </summary>
        [XmlIgnore]
        public EvCTeRegPassagemAutoMDFe EvCTeRegPassagemAutoMDFe
        {
            get => _evCTeRegPassagemAutoMDFe ?? (_evCTeRegPassagemAutoMDFe = new EvCTeRegPassagemAutoMDFe());
            set => _evCTeRegPassagemAutoMDFe = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeRegPassagemAutoMDFe>
                <descEvento>{DescEvento}</descEvento>
                <chMDFe>{ChMDFe}</chMDFe>";

            writeRaw += "</evCTeRegPassagemAutoMDFe>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeRegPassagemAutoMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeRegPassagemAutoMDFe")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeRegPassagemAutoMDFe : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de registro de passagem automático originado no MDFe.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Registro de Passagem Automatico Originado no MDFe";

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem automático.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoRegistroPassagem")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoRegistroPassagem : EventoDetalhe
    {
        private EvCTeRegPassagem _evCTeRegPassagem;

        /// <summary>
        /// Descrição do evento de registro de passagem.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeRegPassagem.DescEvento;
            set => EvCTeRegPassagem.DescEvento = value;
        }

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlIgnore]
        public string CUFTransito
        {
            get => EvCTeRegPassagem.CUFTransito;
            set => EvCTeRegPassagem.CUFTransito = value;
        }

        /// <summary>
        /// Código da unidade fiscal.
        /// </summary>
        [XmlIgnore]
        public string CUnidFiscal
        {
            get => EvCTeRegPassagem.CUnidFiscal;
            set => EvCTeRegPassagem.CUnidFiscal = value;
        }

        /// <summary>
        /// Descrição da unidade fiscal.
        /// </summary>
        [XmlIgnore]
        public string XUnidFiscal
        {
            get => EvCTeRegPassagem.XUnidFiscal;
            set => EvCTeRegPassagem.XUnidFiscal = value;
        }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhPass
        {
            get => EvCTeRegPassagem.DhPass;
            set => EvCTeRegPassagem.DhPass = value;
        }
#else
        public DateTimeOffset DhPass
        {
            get => EvCTeRegPassagem.DhPass;
            set => EvCTeRegPassagem.DhPass = value;
        }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhPass" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public string DhPassField
        {
            get => EvCTeRegPassagem.DhPassField;
            set => EvCTeRegPassagem.DhPassField = value;
        }

        /// <summary>
        /// CPF do funcionário que registrou a passagem.
        /// </summary>
        [XmlIgnore]
        public string CPFFunc
        {
            get => EvCTeRegPassagem.CPFFunc;
            set => EvCTeRegPassagem.CPFFunc = value;
        }

        /// <summary>
        /// Nome do funcionário que registrou a passagem.
        /// </summary>
        [XmlIgnore]
        public string XFunc
        {
            get => EvCTeRegPassagem.XFunc;
            set => EvCTeRegPassagem.XFunc = value;
        }

        /// <summary>
        /// Tipo de transmissão do registro de passagem.
        /// </summary>
        [XmlIgnore]
        public string TpTransm
        {
            get => EvCTeRegPassagem.TpTransm;
            set => EvCTeRegPassagem.TpTransm = value;
        }

        /// <summary>
        /// Tipo de sentido do registro de passagem.
        /// </summary>
        [XmlIgnore]
        public string TpSentido
        {
            get => EvCTeRegPassagem.TpSentido;
            set => EvCTeRegPassagem.TpSentido = value;
        }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlIgnore]
        public string Placa
        {
            get => EvCTeRegPassagem.Placa;
            set => EvCTeRegPassagem.Placa = value;
        }

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem.
        /// </summary>
        [XmlIgnore]
        public string ChMDFe
        {
            get => EvCTeRegPassagem.ChMDFe;
            set => EvCTeRegPassagem.ChMDFe = value;
        }

        /// <summary>
        /// Evento de registro de passagem do CT-e.
        /// </summary>
        [XmlIgnore]
        public EvCTeRegPassagem EvCTeRegPassagem
        {
            get => _evCTeRegPassagem ?? (_evCTeRegPassagem = new EvCTeRegPassagem());
            set => _evCTeRegPassagem = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeRegPassagem>
                <descEvento>{DescEvento}</descEvento>
                <cUFTransito>{CUFTransito}</cUFTransito>
                <cUnidFiscal>{CUnidFiscal}</cUnidFiscal>
                <xUnidFiscal>{XUnidFiscal}</xUnidFiscal>
                <dhPass>{DhPassField}</dhPass>
                <CPFFunc>{CPFFunc}</CPFFunc>
                <xFunc>{XFunc}</xFunc>
                <tpTransm>{TpTransm}</tpTransm>
                <tpSentido>{TpSentido}</tpSentido>
                <placa>{Placa}</placa>
                <chMDFe>{ChMDFe}</chMDFe>
                </evCTeRegPassagem>";

            writer.WriteRaw(writeRaw);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeRegPassagem")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeRegPassagem")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeRegPassagem : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do evento de registro de passagem.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Registro de Passagem";

        /// <summary>
        /// Código da UF de trânsito.
        /// </summary>
        [XmlElement("cUFTransito")]
        public string CUFTransito { get; set; }

        /// <summary>
        /// Código da unidade fiscal.
        /// </summary>
        [XmlElement("cUnidFiscal")]
        public string CUnidFiscal { get; set; }

        /// <summary>
        /// Descrição da unidade fiscal.
        /// </summary>
        [XmlElement("xUnidFiscal")]
        public string XUnidFiscal { get; set; }

        /// <summary>
        /// Data e hora da passagem.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhPass { get; set; }
#else
        public DateTimeOffset DhPass { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhPass" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhPass")]
        public string DhPassField
        {
            get => DhPass.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhPass = DateTime.Parse(value);
#else
            set => DhPass = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// CPF do funcionário que registrou a passagem. 
        /// </summary>
        [XmlElement("CPFFunc")]
        public string CPFFunc { get; set; }

        /// <summary>
        /// Nome do funcionário que registrou a passagem.
        /// </summary>
        [XmlElement("xFunc")]
        public string XFunc { get; set; }

        /// <summary>
        /// Tipo de transmissão do registro de passagem.
        /// </summary>
        [XmlElement("tpTransm")]
        public string TpTransm { get; set; }

        /// <summary>
        /// Tipo de sentido do registro de passagem.
        /// </summary>
        [XmlElement("tpSentido")]
        public string TpSentido { get; set; }

        /// <summary>
        /// Placa do veículo.
        /// </summary>
        [XmlElement("placa")]
        public string Placa { get; set; }

        /// <summary>
        /// Chave de acesso do MDFe relacionado ao registro de passagem.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização.
        /// </summary>
        /// <param name="document">XmlDocument recebido durante o processo de desserialização.</param>
        public void ReadXml(XmlDocument document)
        {
        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização.
        /// </summary>
        /// <param name="writer">string XML recebido durante o processo de serialização.</param>
        public void WriteXml(System.IO.StringWriter writer)
        {
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetEventoAutorizadoCTeComplementar")]
    [ComVisible(true)]
#endif
    [XmlInclude(typeof(EventoDetalhe))]
    [XmlRoot(ElementName = "detEvento")]
    public class DetEventoAutorizadoCTeComplementar : EventoDetalhe
    {
        private EvCTeComplementar _evCTeComplementar;

        /// <summary>
        /// Descrição do evento de autorização do CT-e complementar.
        /// </summary>
        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeComplementar.DescEvento;
            set => EvCTeComplementar.DescEvento = value;
        }

        /// <summary>
        /// Chave de acesso do CT-e complementar.
        /// </summary>
        [XmlIgnore]
        public string ChCTeCompl
        {
            get => EvCTeComplementar.ChCTeCompl;
            set => EvCTeComplementar.ChCTeCompl = value;
        }

        /// <summary>
        /// Número do protocolo de autorização do CT-e complementar.
        /// </summary>
        [XmlIgnore]
        public string NProt
        {
            get => EvCTeComplementar.NProt;
            set => EvCTeComplementar.NProt = value;
        }

        /// <summary>
        /// Data e hora de recebimento da autorização do CT-e complementar.
        /// </summary>
        [XmlIgnore]
        public string DhRecbto
        {
            get => EvCTeComplementar.DhRecbto;
            set => EvCTeComplementar.DhRecbto = value;
        }

        /// <summary>
        /// Evento de autorização do CT-e complementar.
        /// </summary>
        [XmlIgnore]
        public EvCTeComplementar EvCTeComplementar
        {
            get => _evCTeComplementar ?? (_evCTeComplementar = new EvCTeComplementar());
            set => _evCTeComplementar = value;
        }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            var writeRaw = $@"<evCTeComplementar>
                <descEvento>{DescEvento}</descEvento>
                <chCTeCompl>{ChCTeCompl}</chCTeCompl>
                <dhRecbto>{DhRecbto}</dhRecbto>
                <nProt>{NProt}</nProt>";

            writeRaw += "</evCTeComplementar>";

            writer.WriteRaw(writeRaw);
        }
    }

    /// <summary>
    /// Evento CTe Complementar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeComplementar")]
    [ComVisible(true)]
#endif
    [XmlRoot(ElementName = "evCTeComplementar")]
    [XmlInclude(typeof(EventoDetalhe))]
    public class EvCTeComplementar : Contract.Serialization.IXmlSerializable
    {
        /// <summary>
        /// Descrição do Evento.
        /// </summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Autorizado CT-e Complementar";

        /// <summary>
        /// Chave de acesso do CT-e Complementar.
        /// </summary>
        [XmlElement("chCTeCompl")]
        public string ChCTeCompl { get; set; }

        /// <summary>
        /// Data e Hora do Recebimento.
        /// </summary>
        [XmlElement("dhRecbto")]
        public string DhRecbto { get; set; }

        /// <summary>
        /// Número do Protocolo.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na desserialização
        /// </summary>
        ///<param name="document">XmlDocument recebido durante o processo de desserialização</param>
        public void ReadXml(XmlDocument document)
        {

        }

        /// <summary>
        /// Executa o processamento do XMLReader recebido na serialização
        /// </summary>
        ///<param name="writer">string XML recebido durante o processo de serialização</param>
        public void WriteXml(System.IO.StringWriter writer)
        {

        }
    }
}