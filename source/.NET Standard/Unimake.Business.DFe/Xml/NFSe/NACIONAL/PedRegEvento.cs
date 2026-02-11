#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Namespace da NFSe Ambiente Nacional
    /// </summary>
    public static class NfseNs
    {
        public const string Ns = "http://www.sped.fazenda.gov.br/nfse";
    }

    /// <summary>
    /// Pedido de Cancelamento de NFS-e (Padrão Nacional).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.PedRegEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = NfseNs.Ns)]
    [XmlRoot("pedRegEvento", Namespace = NfseNs.Ns, IsNullable = false)]
    public class PedRegEvento : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de Cancelamento da NFS-e.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações do Pedido de Cancelamento da NFS-e.
        /// </summary>
        [XmlElement("infPedReg", Namespace = NfseNs.Ns)]
        public InfPedReg InfPedReg { get; set; } = new InfPedReg();

        /// <summary>
        /// Assinatura XML-DSig (se exigida pelo provedor).
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        public override System.Xml.XmlDocument GerarXML()
        {
            InfPedReg?.ValidarRegrasAutor();
            return base.GerarXML();
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfPedReg")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infPedReg", Namespace = NfseNs.Ns)]
    public class InfPedReg
    {
        private string IdField;

        /// <summary>
        /// Identificador do Pedido de Cancelamento da NFS-e.
        /// Formato:
        /// "PRE" + "chNFSe (50)" + "Código do Evento (6)"
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get
            {
                if (string.IsNullOrWhiteSpace(IdField))
                {
                    IdField = MontarIdPedRegEvento();
                }
                return IdField;
            }

            set => IdField = value;
        }

        /// <summary>
        /// Tipo de Ambiente - Produção ou Homologação.
        /// </summary>
        [XmlElement("tpAmb", Namespace = NfseNs.Ns)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão da Aplicação.
        /// </summary>
        [XmlElement("verAplic", Namespace = NfseNs.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Data e hora do evento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        [XmlElement("dhEvento", Namespace = NfseNs.Ns)]
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
        /// CNPJ do autor do evento (opcional se houver CPF).
        /// </summary>
        [XmlElement("CNPJAutor", Namespace = NfseNs.Ns)]
        public string CNPJAutor { get; set; }

        /// <summary>
        /// CPF do autor do evento (opcional se houver CNPJ).
        /// </summary>
        [XmlElement("CPFAutor", Namespace = NfseNs.Ns)]
        public string CPFAutor { get; set; }

        /// <summary>
        /// Chave da NFS-e a ser cancelada.
        /// </summary>
        [XmlElement("chNFSe", Namespace = NfseNs.Ns)]
        public string ChNFSe { get; set; }

        /// <summary>
        /// Número do pedido/registro do evento.
        /// </summary>
        [XmlElement("nPedRegEvento", Namespace = NfseNs.Ns)]
        public string NPedRegEvento { get; set; }

        /// <summary>
        /// Evento de Cancelamento (código 101101).
        /// </summary>
        [XmlElement("e101101", Namespace = NfseNs.Ns)]
        public E101101 E101101 { get; set; }

        /// <summary>
        /// Solicitação de Análise Fiscal para Cancelamento (código 101103).
        /// </summary>
        [XmlElement("e101103", Namespace = NfseNs.Ns)]
        public E101103 E101103 { get; set; }

        /// <summary>
        /// Evento de Cancelamento Deferido por Análise Fiscal (código 105104).
        /// </summary>
        [XmlElement("e105104", Namespace = NfseNs.Ns)]
        public E105104 E105104 { get; set; }

        /// <summary>
        /// Evento de Cancelamento por Substituição (código 105102).
        /// </summary>
        [XmlElement("e105102", Namespace = NfseNs.Ns)]
        public E105102 E105102 { get; set; }

        /// <summary>
        /// Evento de Cancelamento de NFS-e Indeferida por Análise Fiscal (código 105105).
        /// </summary>
        [XmlElement("e105105", Namespace = NfseNs.Ns)]
        public E105105 E105105 { get; set; }

        /// <summary>
        /// Evento de Confirmação do Prestador (código 202201).
        /// </summary>
        [XmlElement("e202201", Namespace = NfseNs.Ns)]
        public E202201 E202201 { get; set; }

        /// <summary>
        /// Evento de Rejeição do Prestador (código 202205).
        /// </summary>
        [XmlElement("e202205", Namespace = NfseNs.Ns)]
        public E202205 E202205 { get; set; }

        /// <summary>
        /// Evento de Rejeição do Tomador (código 203206).
        /// </summary>
        [XmlElement("e203206", Namespace = NfseNs.Ns)]
        public E203206 E203206 { get; set; }

        /// <summary>
        /// Evento de Confirmação do Tomador (código 203202).
        /// </summary>
        [XmlElement("e203202", Namespace = NfseNs.Ns)]
        public E203202 E203202 { get; set; }

        ///<summary>
        ///Evento de Confirmação do Intermediário (código 204203). 
        /// </summary>
        [XmlElement("e204203", Namespace = NfseNs.Ns)]
        public E204203 E204203 { get; set; }

        /// <summary>
        /// Evento da Rejeição do Intermediário (código 204207).
        /// </summary>
        [XmlElement("e204207", Namespace = NfseNs.Ns)]
        public E204207 E204207 { get; set; }
        /// <summary>
        /// Evento de Confirmação Tácita (código 205204).
        /// </summary>
        [XmlElement("e205204", Namespace = NfseNs.Ns)]
        public E205204 E205204 { get; set; }

        /// <summary>
        /// Evento de Anulação de Manifestação de Rejeição (código 205208).
        /// </summary>
        [XmlElement("e205208", Namespace = NfseNs.Ns)]
        public E205208 E205208 { get; set; }

        /// <summary>
        /// Evento de Cancelamento por Ofício (código 305101).
        /// </summary>
        [XmlElement("e305101", Namespace = NfseNs.Ns)]
        public E305101 E305101 { get; set; }

        /// <summary>
        /// Evento de Bloqueio por Ofício (código 305102).
        /// </summary>
        [XmlElement("e305102", Namespace = NfseNs.Ns)]
        public E305102 E305102 { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeCNPJAutor() => !string.IsNullOrWhiteSpace(CNPJAutor);
        public bool ShouldSerializeCPFAutor() => !string.IsNullOrWhiteSpace(CPFAutor);
        public bool ShouldSerializeNPedRegEvento() => !string.IsNullOrWhiteSpace(NPedRegEvento);
        #endregion

        public void ValidarRegrasAutor()
        {
            var temCnpj = !string.IsNullOrWhiteSpace(CNPJAutor);
            var temCpf = !string.IsNullOrWhiteSpace(CPFAutor);
            if (temCnpj == temCpf)
            {
                throw new Exception("Informe exatamente um identificador do autor: CNPJAutor OU CPFAutor.");
            }
        }

        #region Geração de ID

        private void ValidacaoDeDados()
        {
            if(string.IsNullOrWhiteSpace(ChNFSe))
            {
                throw new Exception("A Chave da NFS-e (chNFSe) é obrigatória para gerar o ID do Pedido de Registro de Evento.");
            }

            if(ChNFSe.Length != 50)
            {
                throw new Exception("A chave da NFS-e (chNFSe) deve conter exatamente 50 caracteres.");
            }

            var codigoEvento = ObterCodigoEvento();
            if (string.IsNullOrWhiteSpace(codigoEvento))
            {
                throw new Exception("Nenhum evento foi informado. Informe um dos eventos disponíveis (e101101, e101103, etc.) antes de gerar o Id.");
            }
        }

        private string ObterCodigoEvento()
        {
            if (E101101 != null) return "101101";
            if (E101103 != null) return "101103";
            if (E105104 != null) return "105104";
            if (E105102 != null) return "105102";
            if (E105105 != null) return "105105";
            if (E202201 != null) return "202201";
            if (E202205 != null) return "202205";
            if (E203206 != null) return "203206";
            if (E203202 != null) return "203202";
            if (E204203 != null) return "204203";
            if (E204207 != null) return "204207";
            if (E205204 != null) return "205204";
            if (E205208 != null) return "205208";
            if (E305101 != null) return "305101";
            if (E305102 != null) return "305102";
            return null;
        }

        private string MontarIdPedRegEvento()
        {
            ValidacaoDeDados();

            var codigoEvento = ObterCodigoEvento();

            return $"PRE{ChNFSe}{codigoEvento}{NPedRegEvento}";
        }

        #endregion Geração de ID
    }
}