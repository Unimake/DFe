#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    /// <summary>Detalhe do evento de apropriação de créditos individuais da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.EvApropCredIndNFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("evApropCredIndNFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class EvApropCredIndNFeABI : XMLBase
    {
        /// <summary>Versão do leiaute do detalhe.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Descrição fixa do evento.</summary>
        [XmlElement("descEvento")]
        public string DescEvento { get; set; } = "Apropriação de créditos individuais";

        /// <summary>Adquirentes e suas participações individuais.</summary>
        [XmlElement("adquirente")]
        public List<AdquirApropCredInd> Adquirente { get; set; }

#if INTEROP
        /// <summary>Adiciona um adquirente ao evento.</summary>
        public void AddAdquirente(AdquirApropCredInd item) { if (Adquirente == null) Adquirente = new List<AdquirApropCredInd>(); Adquirente.Add(item); }

        /// <summary>Obtém um adquirente pelo índice.</summary>
        public AdquirApropCredInd GetAdquirente(int index) => Adquirente[index];

        /// <summary>Obtém a quantidade de adquirentes.</summary>
        public int GetAdquirenteCount => Adquirente?.Count ?? 0;
#endif

        /// <summary>Valida a cardinalidade dos adquirentes.</summary>
        public void Validar()
        {
            var quantidade = Adquirente?.Count ?? 0;
            if (quantidade < 2 || quantidade > 99)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O evento evApropCredIndNFeABI deve conter de 2 a 99 adquirentes. Quantidade informada: " + quantidade + "."));
            }

            var totalParticipacao = 0.0;
            for (var i = 0; i < quantidade; i++)
            {
                var adquirente = Adquirente[i];
                if (adquirente == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O adquirente da posição " + (i + 1) + " não pode ser nulo."));
                }

                adquirente.Validar(i + 1);
                totalParticipacao += adquirente.PParticip;
            }

            if (Math.Abs(totalParticipacao - 100.0) > 0.00005)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("A soma da tag pParticip dos adquirentes deve ser 100.0000%. Soma informada: " + totalParticipacao.ToString("F4", System.Globalization.CultureInfo.InvariantCulture) + "%."));
            }
        }
    }

    /// <summary>Participação de um adquirente na apropriação de créditos individuais.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.AdquirApropCredInd")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class AdquirApropCredInd
    {
        /// <summary>Número sequencial do adquirente.</summary>
        [XmlAttribute("nAdquir")]
        public int NAdquir { get; set; }

        /// <summary>CNPJ do adquirente.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>CPF do adquirente.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>Nome ou razão social do adquirente.</summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>Percentual de participação do adquirente.</summary>
        [XmlIgnore]
        public double PParticip { get; set; }

        /// <summary>Campo auxiliar para serialização de pParticip.</summary>
        [XmlElement("pParticip")]
        public string PParticipField { get => NFeABIFormat.Rate(PParticip); set => PParticip = NFeABIFormat.ParseDecimal(value); }

        /// <summary>Indica se o CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>Indica se o CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);

        /// <summary>Valida identificação, sequência e participação do adquirente.</summary>
        public void Validar(int sequenciaEsperada)
        {
            if (NAdquir != sequenciaEsperada || NAdquir < 1 || NAdquir > 99)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O atributo nAdquir deve ser sequencial de 1 a 99. Esperado: " + sequenciaEsperada + "; informado: " + NAdquir + "."));
            }

            if (string.IsNullOrWhiteSpace(CNPJ) == string.IsNullOrWhiteSpace(CPF))
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O adquirente " + NAdquir + " deve informar exatamente um identificador: CNPJ ou CPF."));
            }
        }
    }
}
