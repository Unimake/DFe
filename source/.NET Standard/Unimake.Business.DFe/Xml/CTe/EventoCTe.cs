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

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Prestacao do Servico em Desacordo";

        [XmlElement("indDesacordoOper", Order = 1)]
        public string IndDesacordoOper { get; set; }

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
        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento Prestacao do Servico em Desacordo";

        [XmlElement("nProtEvPrestDes", Order = 1)]
        public string NProtEvPrestDes { get; set; }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evPrestDesacordo>
            <descEvento>{DescEvento}</descEvento>
            <nProtEvPrestDes>{NProtEvPrestDes}</nProtEvPrestDes>
            </evPrestDesacordo>");
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

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Cancelamento do Comprovante de Entrega do CT-e";

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

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

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoCCeCTe.DescEvento;
            set => EventoCCeCTe.DescEvento = value;
        }

        [XmlElement(ElementName = "evCCeCTe", Order = 0)]
        public EventoCCeCTe EventoCCeCTe
        {
            get => _eventoCCeCTe ?? (_eventoCCeCTe = new EventoCCeCTe());
            set => _eventoCCeCTe = value;
        }

        [XmlIgnore]
        public List<InfCorrecao> InfCorrecao
        {
            get => EventoCCeCTe.InfCorrecao;
            set => EventoCCeCTe.InfCorrecao = value;
        }

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

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi.Name == nameof(InfEntrega))
            {
                XmlReader.Read();

                Toma4 = new EvEPECCTeToma4();
                Toma4.UF = XmlReader.GetValue<UFBrasil>(nameof(Toma4.UF));
                Toma4.CNPJ = XmlReader.GetValue<string>(nameof(Toma4.CNPJ));
                Toma4.CPF = XmlReader.GetValue<string>(nameof(Toma4.CPF));
                Toma4.IE = XmlReader.GetValue<string>(nameof(Toma4.IE));

                return;
            }

            base.SetValue(pi);
        }

        [XmlElement(ElementName = "evEPECCTe", Order = 0)]
        public EvEPECCTe EvEPECCTe
        {
            get => _evEPECCTe ?? (_evEPECCTe = new EvEPECCTe());
            set => _evEPECCTe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EvEPECCTe.DescEvento;
            set => EvEPECCTe.DescEvento = value;
        }

        [XmlIgnore]
        public string XJust
        {
            get => EvEPECCTe.XJust;
            set => EvEPECCTe.XJust = value;
        }

        [XmlIgnore]
        public double VICMS
        {
            get => EvEPECCTe.VICMS;
            set => EvEPECCTe.VICMS = value;
        }

        [XmlIgnore]
        public string VICMSField
        {
            get => EvEPECCTe.VICMSField;
            set => EvEPECCTe.VICMSField = value;
        }

        [XmlIgnore]
        public double VICMSST
        {
            get => EvEPECCTe.VICMSST;
            set => EvEPECCTe.VICMSST = value;
        }

        [XmlIgnore]
        public string VICMSSTField
        {
            get => EvEPECCTe.VICMSSTField;
            set => EvEPECCTe.VICMSSTField = value;
        }

        [XmlIgnore]
        public double VTPrest
        {
            get => EvEPECCTe.VTPrest;
            set => EvEPECCTe.VTPrest = value;
        }

        [XmlIgnore]
        public string VTPrestField
        {
            get => EvEPECCTe.VTPrestField;
            set => EvEPECCTe.VTPrestField = value;
        }

        [XmlIgnore]
        public double VCarga
        {
            get => EvEPECCTe.VCarga;
            set => EvEPECCTe.VCarga = value;
        }

        [XmlIgnore]
        public string VCargaField
        {
            get => EvEPECCTe.VCargaField;
            set => EvEPECCTe.VCargaField = value;
        }

        [XmlIgnore]
        public EvEPECCTeToma4 Toma4 { get; set; }

        [XmlIgnore]
        public ModalidadeTransporteCTe Modal
        {
            get => EvEPECCTe.Modal;
            set => EvEPECCTe.Modal = value;
        }

        [XmlIgnore]
        public UFBrasil UFIni
        {
            get => EvEPECCTe.UFIni;
            set => EvEPECCTe.UFIni = value;
        }

        [XmlIgnore]
        public UFBrasil UFFim
        {
            get => EvEPECCTe.UFFim;
            set => EvEPECCTe.UFFim = value;
        }

        [XmlIgnore]
        public TipoCTe TpCTe
        {
            get => EvEPECCTe.TpCTe;
            set => EvEPECCTe.TpCTe = value;
        }

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

            if (VICMS > 0)
            {
                writeRaw += $@"<vICMSST>{VICMSSTField}</vICMSST>";
            }

            writeRaw += $@"<vTPrest>{VTPrestField}</vTPrest>
                <vCarga>{VCargaField}</vCarga>";

            writeRaw += $@"<toma4>
                <toma>{(int)Toma4.Toma}</toma>
                <UF>{Toma4.UF}</UF>";

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
                <UFIni>{UFIni}</UFIni>
                <UFFim>{UFFim}</UFFim>
                <tpCTe>{(int)TpCTe}</tpCTe>
                <dhEmi>{DhEmiField}</dhEmi>";

            writeRaw += $@"</evEPECCTe>";

            writer.WriteRaw(writeRaw);
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
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "EPEC";

        [XmlElement("xJust", Order = 1)]
        public string XJust { get; set; }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS", Order = 2)]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST", Order = 3)]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTPrest { get; set; }

        [XmlElement("vTPrest", Order = 4)]
        public string VTPrestField
        {
            get => VTPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VTPrest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCarga { get; set; }

        [XmlElement("vCarga", Order = 5)]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Converter.ToDouble(value);
        }

        [XmlElement("toma4", Order = 6)]
        public EvEPECCTeToma4 Toma4 { get; set; }

        [XmlElement("modal", Order = 7)]
        public ModalidadeTransporteCTe Modal { get; set; }

        [XmlElement("UFIni", Order = 8)]
        public UFBrasil UFIni { get; set; }

        [XmlElement("UFFim", Order = 9)]
        public UFBrasil UFFim { get; set; }

        [XmlElement("tpCTe", Order = 10)]
        public TipoCTe TpCTe { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif


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

        [XmlElement("toma", Order = 0)]
        public TomadorServicoCTe Toma
        {
            get => TomadorServicoCTe.Outros;
            set => TomaField = value;
        }

        [XmlElement("UF", Order = 1)]
        public UFBrasil UF { get; set; }

        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        [XmlElement("IE", Order = 4)]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

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

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoCECTe.DescEvento;
            set => EventoCECTe.DescEvento = value;
        }

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

        [XmlIgnore]
        public string DhEntregaField
        {
            get => EventoCECTe.DhEntregaField;
            set => EventoCECTe.DhEntregaField = value;
        }

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

        [XmlIgnore]
        public string DhHashEntregaField
        {
            get => EventoCECTe.DhHashEntregaField;
            set => EventoCECTe.DhHashEntregaField = value;
        }

        [XmlElement(ElementName = "evCECTe", Order = 0)]
        public EventoCECTe EventoCECTe
        {
            get => _eventoCECTe ?? (_eventoCECTe = new EventoCECTe());
            set => _eventoCECTe = value;
        }

        [XmlIgnore]
        public string HashEntrega
        {
            get => EventoCECTe.HashEntrega;
            set => EventoCECTe.HashEntrega = value;
        }

        [XmlIgnore]
        public List<InfEntrega> InfEntrega
        {
            get => EventoCECTe.InfEntrega;
            set => EventoCECTe.InfEntrega = value;
        }

        [XmlIgnore]
        public string Latitude
        {
            get => EventoCECTe.Latitude;
            set => EventoCECTe.Latitude = value;
        }

        [XmlIgnore]
        public string Longitude
        {
            get => EventoCECTe.Longitude;
            set => EventoCECTe.Longitude = value;
        }

        [XmlIgnore]
        public string NDoc
        {
            get => EventoCECTe.NDoc;
            set => EventoCECTe.NDoc = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoCECTe.NProt;
            set => EventoCECTe.NProt = value;
        }

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

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Carta de Correcao";

        [XmlElement("infCorrecao", Order = 1)]
        public List<InfCorrecao> InfCorrecao { get; set; } = new List<InfCorrecao>();

        [XmlElement("xCondUso", Order = 2)]
        public string XCondUso
        {
            get => XCondUsoField;
            set => XCondUsoField = value;
        }

        #endregion Public Properties

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infcorrecao">Elemento</param>
        public void AddInfCorrecao(InfCorrecao infcorrecao)
        {
            if (InfCorrecao == null)
            {
                InfCorrecao = new List<InfCorrecao>();
            }

            InfCorrecao.Add(infcorrecao);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCorrecao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCorrecao</returns>
        public InfCorrecao GetInfCorrecao(int index)
        {
            if ((InfCorrecao?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCorrecao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCorrecao
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

        [XmlElement("descEvento", Order = 0)]
        public override string DescEvento { get; set; } = "Comprovante de Entrega do CT-e";

        [XmlIgnore]
#if INTEROP
        public DateTime DhEntrega { get; set; }
#else
        public DateTimeOffset DhEntrega { get; set; }
#endif

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

        [XmlIgnore]
#if INTEROP
        public DateTime DhHashEntrega { get; set; }
#else
        public DateTimeOffset DhHashEntrega { get; set; }
#endif

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

        [XmlElement("infEntrega", Order = 9)]
        public List<InfEntrega> InfEntrega { get; set; } = new List<InfEntrega>();

        [XmlElement("latitude", Order = 5)]
        public string Latitude { get; set; }

        [XmlElement("longitude", Order = 6)]
        public string Longitude { get; set; }

        [XmlElement("nDoc", Order = 3)]
        public string NDoc { get; set; }

        [XmlElement("nProt", Order = 1)]
        public string NProt { get; set; }

        [XmlElement("xNome", Order = 4)]
        public string XNome { get; set; }

        #endregion Public Properties

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infentrega">Elemento</param>
        public void AddInfEntrega(InfEntrega infentrega)
        {
            if (InfEntrega == null)
            {
                InfEntrega = new List<InfEntrega>();
            }

            InfEntrega.Add(infentrega);
        }

        /// <summary>
        /// Retorna o elemento da lista InfEntrega (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfEntrega</returns>
        public InfEntrega GetInfEntrega(int index)
        {
            if ((InfEntrega?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfEntrega[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfEntrega
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
        /// Desserializar o XML no objeto EventoCTe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML do eventoCTe</param>
        /// <returns>Objeto do EventoCTe</returns>
        public EventoCTe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EventoCTe>(doc);
        }

        /// <summary>
        /// Desserializar o XML eventoCTe no objeto EventoCTe
        /// </summary>
        /// <param name="xml">string do XML eventoCTe</param>
        /// <returns>Objeto da EventoCTe</returns>
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

        [XmlElement("descEvento", Order = 0)]
        public virtual string DescEvento { get; set; }

        [XmlAttribute(AttributeName = "versaoEvento")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCorrecao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCorrecao
    {
        #region Public Properties

        [XmlElement("campoAlterado", Order = 1)]
        public string CampoAlterado { get; set; }

        [XmlElement("grupoAlterado", Order = 0)]
        public string GrupoAlterado { get; set; }

        [XmlElement("nroItemAlterado", Order = 3)]
        public string NroItemAlterado { get; set; }

        [XmlElement("valorAlterado", Order = 2)]
        public string ValorAlterado { get; set; }

        #endregion Public Properties

        #region Public Methods

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

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao", Order = 0)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("tpAmb", Order = 1)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("CNPJ", Order = 2)]
        public string CNPJ { get; set; }

        [XmlElement("CPF", Order = 3)]
        public string CPF { get; set; }

        [XmlElement("chCTe", Order = 4)]
        public string ChCTe { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

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

        [XmlElement("tpEvento", Order = 6)]
        public TipoEventoCTe TpEvento { get; set; }

        [XmlElement("nSeqEvento", Order = 7)]
        public int NSeqEvento { get; set; }

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


                    case TipoEventoCTe.RegistroPassagemAutomatico:
                        _detEvento = new DetEventoRegistroPassagemAutomatico();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                _detEvento.XmlReader = value.XmlReader;
                _detEvento.ProcessReader();
            }
        }

        [XmlElement("infSolicNFF", Order = 9)]
        public InfSolicNFF InfSolicNFF { get; set; }

        [XmlAttribute(DataType = "ID", AttributeName = "Id")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChCTe + NSeqEvento.ToString((DetEvento.VersaoEvento == "3.00" ? "00" : "000"));
            set => _ = value;
        }

        #endregion Public Properties

        #region Public Constructors

        public InfEvento() { }

        public InfEvento(EventoDetalhe detEvento) => DetEvento = detEvento ?? throw new ArgumentNullException(nameof(detEvento));

        #endregion Public Constructors

        #region Public Methods

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

        [XmlIgnore]
        public override string DescEvento
        {
            get => EventoIECTe.DescEvento;
            set => EventoIECTe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EventoIECTe.NProt;
            set => EventoIECTe.NProt = value;
        }

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

        [XmlIgnore]
        public string DhTentativaEntregaField
        {
            get => EventoIECTe.DhTentativaEntregaField;
            set => EventoIECTe.DhTentativaEntregaField = value;
        }


        [XmlIgnore]
        public int NTentativa
        {
            get => EventoIECTe.NTentativa;
            set => EventoIECTe.NTentativa = value;
        }


        [XmlIgnore]
        public TipoMotivoInsucessoEntrega TpMotivo
        {
            get => EventoIECTe.TpMotivo;
            set => EventoIECTe.TpMotivo = value;
        }

        [XmlIgnore]
        public string XJustMotivo
        {
            get => EventoIECTe.XJustMotivo;
            set => EventoIECTe.XJustMotivo = value;
        }

        [XmlIgnore]
        public string Latitude
        {
            get => EventoIECTe.Latitude;
            set => EventoIECTe.Latitude = value;
        }

        [XmlIgnore]
        public string Longitude
        {
            get => EventoIECTe.Longitude;
            set => EventoIECTe.Longitude = value;
        }

        [XmlIgnore]
        public string HashTentativaEntrega
        {
            get => EventoIECTe.HashTentativaEntrega;
            set => EventoIECTe.HashTentativaEntrega = value;
        }

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

        [XmlIgnore]
        public string DhHashTentativaEntregaField
        {
            get => EventoIECTe.DhHashTentativaEntregaField;
            set => EventoIECTe.DhHashTentativaEntregaField = value;
        }


        [XmlIgnore]
        public List<InfEntrega> InfEntrega
        {
            get => EventoIECTe.InfEntrega;
            set => EventoIECTe.InfEntrega = value;
        }

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

        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCancIECTe.DescEvento;
            set => EvCancIECTe.DescEvento = value;
        }

        [XmlIgnore]
        public string NProt
        {
            get => EvCancIECTe.NProt;
            set => EvCancIECTe.NProt = value;
        }

        [XmlIgnore]
        public string NProtIE
        {
            get => EvCancIECTe.NProtIE;
            set => EvCancIECTe.NProtIE = value;
        }

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
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Insucesso na Entrega do CT-e";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhTentativaEntrega { get; set; }
#else
        public DateTimeOffset DhTentativaEntrega { get; set; }
#endif

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

        [XmlElement("nTentativa")]
        public int NTentativa { get; set; }

        [XmlElement("tpMotivo")]
        public TipoMotivoInsucessoEntrega TpMotivo { get; set; }

        [XmlElement("xJustMotivo")]
        public string XJustMotivo { get; set; }

        [XmlElement("latitude")]
        public string Latitude { get; set; }

        [XmlElement("longitude")]
        public string Longitude { get; set; }

        private string HashTentativaEntregaField;

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

        [XmlIgnore]
#if INTEROP
        public DateTime DhHashTentativaEntrega { get; set; }
#else
        public DateTimeOffset DhHashTentativaEntrega { get; set; }
#endif

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

        [XmlElement("infEntrega")]
        public List<InfEntrega> InfEntrega { get; set; } = new List<InfEntrega>();


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infentrega">Elemento</param>
        public void AddInfEntrega(InfEntrega infentrega)
        {
            if (InfEntrega == null)
            {
                InfEntrega = new List<InfEntrega>();
            }

            InfEntrega.Add(infentrega);
        }

        /// <summary>
        /// Retorna o elemento da lista InfEntrega (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfEntrega</returns>
        public InfEntrega GetInfEntrega(int index)
        {
            if ((InfEntrega?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfEntrega[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfEntrega
        /// </summary>
        public int GetInfEntregaCount => (InfEntrega != null ? InfEntrega.Count : 0);

#endif
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
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Cancelamento do Insucesso de Entrega do CT-e";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("nProtIE")]
        public string NProtIE { get; set; }

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

        [XmlElement(ElementName = "evCTeCanceladoMDFe", Order = 0)]
        public EvCTeCanceladoMDFe EvCTeCanceladoMDFe
        {
            get => _evCTeCanceladoMDFe ?? (_evCTeCanceladoMDFe = new EvCTeCanceladoMDFe());
            set => _evCTeCanceladoMDFe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeCanceladoMDFe.DescEvento;
            set => EvCTeCanceladoMDFe.DescEvento = value;
        }

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
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "MDF-e Cancelado";

        [XmlElement("MDFe", Order = 6)]
        public EvCTeCanceladoMDFeMDFe MDFe { get; set; }

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeCanceladoMDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeCanceladoMDFeMDFe
    {
        [XmlElement("chMDFe", Order = 0)]
        public string ChMDFe { get; set; }

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

        [XmlElement(ElementName = "evCTeAutorizadoMDFe", Order = 0)]
        public EvCTeAutorizadoMDFe EvCTeAutorizadoMDFe
        {
            get => _evCTeAutorizadoMDFe ?? (_evCTeAutorizadoMDFe = new EvCTeAutorizadoMDFe());
            set => _evCTeAutorizadoMDFe = value;
        }

        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeAutorizadoMDFe.DescEvento;
            set => EvCTeAutorizadoMDFe.DescEvento = value;
        }

        [XmlIgnore]
        public EvCTeAutorizadoMDFeMDFe MDFe
        {
            get => EvCTeAutorizadoMDFe.MDFe;
            set => EvCTeAutorizadoMDFe.MDFe = value;
        }

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
        [XmlElement("descEvento", Order = 0)]
        public string DescEvento { get; set; } = "MDF-e Autorizado";

        [XmlElement("MDFe", Order = 1)]
        public EvCTeAutorizadoMDFeMDFe MDFe { get; set; }

        [XmlElement("emit", Order = 2)]
        public EvCTeAutorizadoMDFeEmit Emit { get; set; }

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeAutorizadoMDFeMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeAutorizadoMDFeMDFe
    {
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

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

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

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
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

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

        [XmlIgnore]
        public override string DescEvento
        {
            get => EvCTeRegPassagemAuto.DescEvento;
            set => EvCTeRegPassagemAuto.DescEvento = value;
        }

        [XmlIgnore]
        public string TpTransm
        {
            get => EvCTeRegPassagemAuto.TpTransm;
            set => EvCTeRegPassagemAuto.TpTransm = value;
        }

        [XmlIgnore]
        public EvCTeRegPassagemAutoInfPass InfPass
        {
            get => EvCTeRegPassagemAuto.InfPass;
            set => EvCTeRegPassagemAuto.InfPass = value;
        }

        [XmlIgnore]
        public string ChMDFe
        {
            get => EvCTeRegPassagemAuto.ChMDFe;
            set => EvCTeRegPassagemAuto.ChMDFe = value;
        }

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
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Registro de Passagem Automático";

        [XmlElement("tpTransm")]
        public string TpTransm { get; set; }

        [XmlElement("infPass")]
        public EvCTeRegPassagemAutoInfPass InfPass { get; set; }

        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EvCTeRegPassagemAutoInfPass")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EvCTeRegPassagemAutoInfPass
    {
        [XmlElement("cUFTransito")]
        public string CUFTransito { get; set; }

        [XmlElement("cIdEquip")]
        public string CIdEquip { get; set; }

        [XmlElement("xIdEquip")]
        public string XIdEquip { get; set; }

        [XmlElement("tpEquip")]
        public string TpEquip { get; set; }

        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("tpSentido")]
        public string TpSentido { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhPass { get; set; }
#else
        public DateTimeOffset DhPass { get; set; }
#endif

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

        [XmlElement("latitude")]
        public string Latitude { get; set; }

        [XmlElement("longitude")]
        public string Longitude { get; set; }

        [XmlElement("NSU")]
        public string NSU { get; set; }
    }

    #endregion
}