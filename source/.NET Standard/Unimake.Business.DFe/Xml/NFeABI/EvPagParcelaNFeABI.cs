#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Código do evento de pagamento de parcela.</summary>
    public enum CodigoEventoPagamentoNFeABI
    {
        /// <summary>01 - Pagamento de parcela em incorporação, loteamento ou desmembramento.</summary>
        [XmlEnum("01")]
        ParcelaIncorporacaoLoteamento = 1,

        /// <summary>02 - Pagamento complementar nas demais alienações.</summary>
        [XmlEnum("02")]
        PagamentoComplementar = 2
    }

    /// <summary>Detalhe do evento de pagamento de parcela da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.EvPagParcelaNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("evPagParcelaNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class EvPagParcelaNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute do detalhe.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Descrição fixa do evento.</summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Pagamento de Parcela";

        /// <summary>Código da modalidade de pagamento informada pelo evento.</summary>
        [XmlElement("cdEventoPag")]
        public CodigoEventoPagamentoNFeABI CdEventoPag { get; set; }

        /// <summary>Valor principal da parcela.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VParcela { get; set; }

        /// <summary>Campo auxiliar para serialização de vParcela.</summary>
        [XmlElement("vParcela")]
        public string VParcelaField { get => NFeABIFormat.Money(VParcela); set => VParcela = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor dos acréscimos da série de parcelas.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VAcrescimos { get; set; }

        /// <summary>Campo auxiliar para serialização de vAcrescimos.</summary>
        [XmlElement("vAcrescimos")]
        public string VAcrescimosField { get => NFeABIFormat.Money(VAcrescimos); set => VAcrescimos = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor total da série de parcelas.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VTotalParcela { get; set; }

        /// <summary>Campo auxiliar para serialização de vTotalParcela.</summary>
        [XmlElement("vTotalParcela")]
        public string VTotalParcelaField { get => NFeABIFormat.Money(VTotalParcela); set => VTotalParcela = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor original da parcela ou prestação.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VOrigParcela { get; set; }

        /// <summary>Campo auxiliar para serialização de vOrigParcela.</summary>
        [XmlElement("vOrigParcela")]
        public string VOrigParcelaField { get => NFeABIFormat.Money(VOrigParcela); set => VOrigParcela = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor dos acréscimos da parcela ou prestação.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VAcrescParcela { get; set; }

        /// <summary>Campo auxiliar para serialização de vAcrescParcela.</summary>
        [XmlElement("vAcrescParcela")]
        public string VAcrescParcelaField { get => NFeABIFormat.Money(VAcrescParcela); set => VAcrescParcela = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Data do pagamento.</summary>
        [XmlIgnore]
        public DateTime DataPag { get; set; }

        /// <summary>Campo auxiliar para serialização de dataPag.</summary>
        [XmlElement("dataPag")]
        public string DataPagField { get => DataPag.ToString("yyyy-MM-dd"); set => DataPag = DateTime.Parse(value, CultureInfo.InvariantCulture); }

        /// <summary>Código de situação tributária do IBS/CBS.</summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>Código de classificação tributária do IBS/CBS.</summary>
        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        /// <summary>Tributação IBS/CBS do pagamento.</summary>
        [XmlElement("gIBSCBS")]
        public GIBSCBSEventoPagParcela GIBSCBS { get; set; }

        /// <summary>Indica se vParcela deve ser serializado.</summary>
        public bool ShouldSerializeVParcelaField() => VParcela.HasValue;

        /// <summary>Indica se vAcrescimos deve ser serializado.</summary>
        public bool ShouldSerializeVAcrescimosField() => VAcrescimos.HasValue;

        /// <summary>Indica se vTotalParcela deve ser serializado.</summary>
        public bool ShouldSerializeVTotalParcelaField() => VTotalParcela.HasValue;

        /// <summary>Indica se vOrigParcela deve ser serializado.</summary>
        public bool ShouldSerializeVOrigParcelaField() => VOrigParcela.HasValue;

        /// <summary>Indica se vAcrescParcela deve ser serializado.</summary>
        public bool ShouldSerializeVAcrescParcelaField() => VAcrescParcela.HasValue;

        /// <summary>Valida os campos condicionais da modalidade de pagamento.</summary>
        public void Validar()
        {
            if (GIBSCBS == null)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O grupo gIBSCBS do evento de pagamento de parcela deve ser informado."));
            }

            if (CdEventoPag == CodigoEventoPagamentoNFeABI.ParcelaIncorporacaoLoteamento)
            {
                if (!VParcela.HasValue || !VTotalParcela.HasValue || !GIBSCBS.VOperacIndiv.HasValue)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para cdEventoPag=01, vParcela, vTotalParcela, gIBSCBS e vOperacIndiv são obrigatórios."));
                }
                if (VOrigParcela.HasValue || VAcrescParcela.HasValue)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para cdEventoPag=01, vOrigParcela e vAcrescParcela não devem ser informados."));
                }
                return;
            }

            if (CdEventoPag == CodigoEventoPagamentoNFeABI.PagamentoComplementar)
            {
                if (!VOrigParcela.HasValue || !VAcrescParcela.HasValue)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para cdEventoPag=02, vOrigParcela e vAcrescParcela são obrigatórios."));
                }
                if (VParcela.HasValue || VAcrescimos.HasValue || VTotalParcela.HasValue || (GIBSCBS != null && (GIBSCBS.VOperacIndiv.HasValue || GIBSCBS.VRedAjusteIndiv.HasValue || GIBSCBS.VRedSocialIndiv.HasValue)))
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para cdEventoPag=02, os campos exclusivos de cdEventoPag=01 não devem ser informados."));
                }
                if (Math.Abs(GIBSCBS.VBC - VAcrescParcela.Value) > 0.005)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para cdEventoPag=02, a tag vBC deve ser igual a vAcrescParcela."));
                }
                return;
            }

            ThrowHelper.Instance.Throw(new ValidatorDFeException("A tag cdEventoPag deve ser 01 ou 02."));
        }
    }

    /// <summary>Tributação IBS/CBS do evento de pagamento de parcela.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIBSCBSEventoPagParcela")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GIBSCBSEventoPagParcela
    {
        /// <summary>Valor individual da operação.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VOperacIndiv { get; set; }

        /// <summary>Campo auxiliar para serialização de vOperacIndiv.</summary>
        [XmlElement("vOperacIndiv")]
        public string VOperacIndivField { get => NFeABIFormat.Money(VOperacIndiv); set => VOperacIndiv = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor do redutor de ajuste individual.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VRedAjusteIndiv { get; set; }

        /// <summary>Campo auxiliar para serialização de vRedAjusteIndiv.</summary>
        [XmlElement("vRedAjusteIndiv")]
        public string VRedAjusteIndivField { get => NFeABIFormat.Money(VRedAjusteIndiv); set => VRedAjusteIndiv = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Valor do redutor social individual.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VRedSocialIndiv { get; set; }

        /// <summary>Campo auxiliar para serialização de vRedSocialIndiv.</summary>
        [XmlElement("vRedSocialIndiv")]
        public string VRedSocialIndivField { get => NFeABIFormat.Money(VRedSocialIndiv); set => VRedSocialIndiv = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Base de cálculo.</summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>Campo auxiliar para serialização de vBC.</summary>
        [XmlElement("vBC")]
        public string VBCField { get => NFeABIFormat.Money(VBC); set => VBC = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Tributação estadual do IBS.</summary>
        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }

        /// <summary>Tributação municipal do IBS.</summary>
        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }

        /// <summary>Tributação da CBS.</summary>
        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        /// <summary>Tributação de compra governamental.</summary>
        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }

        /// <summary>Indica se vOperacIndiv deve ser serializado.</summary>
        public bool ShouldSerializeVOperacIndivField() => VOperacIndiv.HasValue;

        /// <summary>Indica se vRedAjusteIndiv deve ser serializado.</summary>
        public bool ShouldSerializeVRedAjusteIndivField() => VRedAjusteIndiv.HasValue;

        /// <summary>Indica se vRedSocialIndiv deve ser serializado.</summary>
        public bool ShouldSerializeVRedSocialIndivField() => VRedSocialIndiv.HasValue;
    }
}
