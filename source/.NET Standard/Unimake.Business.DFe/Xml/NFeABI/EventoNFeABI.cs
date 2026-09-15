#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Códigos dos eventos disponíveis para a NFeABI.</summary>
    public enum TipoEventoNFeABI
    {
        /// <summary>110111 - Cancelamento.</summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>112110 - Pagamento de parcela.</summary>
        [XmlEnum("112110")]
        PagamentoParcela = 112110,

        /// <summary>112120 - Apropriação de créditos individuais.</summary>
        [XmlEnum("112120")]
        ApropriacaoCreditosIndividuais = 112120
    }

    /// <summary>Pedido de registro de evento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.EventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("evento", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class EventoNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute do evento.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Informações do evento.</summary>
        [XmlElement("infEvento")]
        public InfEventoNFeABI InfEvento { get; set; }

        /// <summary>Assinatura digital do evento.</summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>Valida as invariantes do envelope e do detalhe do evento.</summary>
        public void Validar()
        {
            if (InfEvento == null)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O grupo infEvento da NFeABI deve ser informado."));
            }

            InfEvento.Validar();
        }
    }

    /// <summary>Informações do evento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfEventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class InfEventoNFeABI
    {
        /// <summary>Código do órgão de recepção do evento.</summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>Campo auxiliar para serialização de cOrgao.</summary>
        [XmlElement("cOrgao")]
        public int COrgaoField { get => (int)COrgao; set => COrgao = (UFBrasil)value; }

        /// <summary>Ambiente de recepção do evento.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>CNPJ do autor do evento.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>CPF do autor do evento.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>Chave de acesso da NFeABI vinculada ao evento.</summary>
        [XmlElement("chNFeABI")]
        public string ChNFeABI { get; set; }

        /// <summary>Data e hora do evento.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        /// <summary>Campo auxiliar para serialização de dhEvento.</summary>
        [XmlElement("dhEvento")]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEvento = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhEvento = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Tipo do evento.</summary>
        [XmlElement("tpEvento")]
        public TipoEventoNFeABI TpEvento { get; set; }

        /// <summary>Número sequencial do evento.</summary>
        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; } = 1;

        /// <summary>Versão do detalhe do evento.</summary>
        [XmlElement("verEvento")]
        public string VerEvento { get; set; }

        /// <summary>Detalhe específico do evento.</summary>
        [XmlElement("detEvento")]
        public DetEventoNFeABI DetEvento { get; set; }

        /// <summary>Identificador calculado da tag assinada.</summary>
        [XmlAttribute("Id", DataType = "ID")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString("000000") + ChNFeABI + NSeqEvento;
            set => _ = value;
        }

        /// <summary>Indica se o CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>Indica se o CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);

        /// <summary>Valida código, sequência, identificador e detalhe do evento.</summary>
        public void Validar()
        {
            if (string.IsNullOrWhiteSpace(CNPJ) == string.IsNullOrWhiteSpace(CPF))
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O evento da NFeABI deve informar exatamente um autor: CNPJ ou CPF."));
            }

            if (NSeqEvento != 1)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("A tag nSeqEvento da NFeABI deve ser igual a 1. Valor informado: " + NSeqEvento + "."));
            }

            if (string.IsNullOrWhiteSpace(ChNFeABI) || ChNFeABI.Length != 44)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("A tag chNFeABI do evento deve conter 44 caracteres."));
            }

            if (DetEvento == null)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O grupo detEvento da NFeABI deve ser informado."));
            }

            DetEvento.Validar(TpEvento);
        }
    }

    /// <summary>Detalhe específico do evento da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.DetEventoNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class DetEventoNFeABI
    {
        /// <summary>Versão do detalhe do evento.</summary>
        [XmlAttribute("versaoEvento", DataType = "token")]
        public string VersaoEvento { get; set; }

        /// <summary>Detalhe do cancelamento.</summary>
        [XmlElement("evCancNFeABI")]
        public EvCancNFeABI EvCancNFeABI { get; set; }

        /// <summary>Detalhe do pagamento de parcela.</summary>
        [XmlElement("evPagParcelaNFeABI")]
        public EvPagParcelaNFeABI EvPagParcelaNFeABI { get; set; }

        /// <summary>Detalhe da apropriação de créditos individuais.</summary>
        [XmlElement("evApropCredIndNFeABI")]
        public EvApropCredIndNFeABI EvApropCredIndNFeABI { get; set; }

        /// <summary>Valida a correspondência entre tpEvento e o detalhe informado.</summary>
        public void Validar(TipoEventoNFeABI tipoEvento)
        {
            var quantidade = (EvCancNFeABI == null ? 0 : 1) + (EvPagParcelaNFeABI == null ? 0 : 1) + (EvApropCredIndNFeABI == null ? 0 : 1);
            if (quantidade != 1)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O grupo detEvento deve conter exatamente um detalhe de evento da NFeABI."));
            }

            if (tipoEvento == TipoEventoNFeABI.Cancelamento && EvCancNFeABI != null) return;
            if (tipoEvento == TipoEventoNFeABI.PagamentoParcela && EvPagParcelaNFeABI != null)
            {
                EvPagParcelaNFeABI.Validar();
                return;
            }
            if (tipoEvento == TipoEventoNFeABI.ApropriacaoCreditosIndividuais && EvApropCredIndNFeABI != null)
            {
                EvApropCredIndNFeABI.Validar();
                return;
            }

            ThrowHelper.Instance.Throw(new ValidatorDFeException("A tag tpEvento não corresponde ao detalhe informado na NFeABI. Valor informado: " + (int)tipoEvento + "."));
        }
    }
}
