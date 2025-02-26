#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de retorno do pedido de consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsCad")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsCad : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno da consulta cadastro de contribuinte
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados do resultado do dados do pedido de consulta de cadastro de contribuintes
        /// </summary>
        [XmlElement("infCons")]
        public InfConsRetorno InfCons { get; set; }
    }

    /// <summary>
    /// Classe dos dados do resultado do dados do pedido de consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfConsRetorno")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfConsRetorno
    {
        /// <summary>
        /// Versão do aplicativo que processou o pedido de consulta de cadastro
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do status da mensagem enviada
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status do serviço solicitado
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Sigla da UF consultada, utilizar SU para SUFRAMA
        /// </summary>
        [XmlIgnore]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade UF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("UF")]
        public string UFField
        {
            get => UF.ToString();
            set => UF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value);
        }

        /// <summary>
        /// CNPJ do contribuinte
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do contribuinte
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Data da Consulta no formato AAAA-MM-DDTHH:MM:SSTZD
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhCons { get; set; }
#else
        public DateTimeOffset DhCons { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhCons para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhCons")]
        public string DhConsField
        {
            get => DhCons.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhCons = DateTime.Parse(value);
#else
            set => DhCons = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Código da UF de atendimento
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Informações cadastrais do contribuinte consultado
        /// </summary>
        [XmlElement("infCad")]
        public List<InfCad> InfCad { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfCad(InfCad item)
        {
            if (InfCad == null)
            {
                InfCad = new List<InfCad>();
            }

            InfCad.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCad (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCad</returns>
        public InfCad GetInfCad(int index)
        {
            if ((InfCad?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCad[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCad
        /// </summary>
        public int GetInfCadCount => (InfCad != null ? InfCad.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações cadastrais do contribuinte consultado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfCad")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfCad
    {
        /// <summary>
        /// Número da inscrição estadual do contribuinte
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// CNPJ do contribuinte
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do contribuinte
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Sigla da UF de localização do contribuinte
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Situação cadastral do contribuinte:
        /// 0 - Não habilitado, 1 - Habilitado
        /// </summary>
        [XmlElement("cSit")]
        public int CSit { get; set; }

        /// <summary>
        /// Indicador de contribuinte credenciado a emitir NFe/NFCe:
        /// 0 - Não credenciado para emissão da NFe/NFCe;
        /// 1 - Credenciado;
        /// 2 - Credenciado com obrigatoriedade para todas operações;
        /// 3 - Credenciado com obrigatoriedade parcial;
        /// 4 – a SEFAZ não fornece a informação. Este indicador significa apenas que o contribuinte é credenciado para emitir NFe/NFCe na SEFAZ consultada.
        /// </summary>
        [XmlElement("indCredNFe")]
        public int IndCredNFe { get; set; }

        /// <summary>
        /// Indicador de contribuinte credenciado a emitir CTe:
        /// 0 - Não credenciado para emissão da CTe;
        /// 1 - Credenciado;
        /// 2 - Credenciado com obrigatoriedade para todas operações;
        /// 3 - Credenciado com obrigatoriedade parcial;
        /// 4 – a SEFAZ não fornece a informação. Este indicador significa apenas que o contribuinte é credenciado para emitir CTe na SEFAZ consultada.
        /// </summary>
        [XmlElement("indCredCTe")]
        public int IndCredCTe { get; set; }

        /// <summary>
        /// Razão social ou nome do contribuinte
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Razão social ou nome do contribuinte
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Regime de apuração do ICMS
        /// </summary>
        [XmlElement("xRegApur")]
        public string XRegApur { get; set; }

        /// <summary>
        /// CNAE Fiscal do contribuinte
        /// </summary>
        [XmlElement("CNAE")]
        public string CNAE { get; set; }

        /// <summary>
        /// Data de início de atividades do contribuinte no formato AAAA-MM-DD
        /// </summary>
        [XmlIgnore]
        public DateTime DIniAtiv { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DIniAtiv para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dIniAtiv")]
        public string DIniAtivField
        {
            get => DIniAtiv.ToString("yyyy-MM-dd");
            set => DIniAtiv = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DUltSit { get; set; }

        /// <summary>
        /// Data da última modificação da situação cadastral do contribuinte no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("dUltSit")]
        public string DUltSitField
        {
            get => DUltSit.ToString("yyyy-MM-dd");
            set => DUltSit = DateTime.Parse(value);
        }

        /// <summary>
        /// Data de ocorrência da baixa do contribuinte no formato AAAA-MM-DD
        /// </summary>
        [XmlIgnore]
        public DateTime DBaixa { get; set; }

        [XmlElement("dBaixa")]
        public string DBaixaField
        {
            get => DBaixa.ToString("yyyy-MM-dd");
            set => DBaixa = DateTime.Parse(value);
        }

        /// <summary>
        /// Inscrição estadual Única
        /// </summary>
        [XmlElement("IEUnica")]
        public string IEUnica { get; set; }

        /// <summary>
        /// Inscrição estadual atual
        /// </summary>
        [XmlElement("IEAtual")]
        public string IEAtual { get; set; }

        /// <summary>
        /// Endereço do contribuinte
        /// </summary>
        [XmlElement("ender")]
        public Ender Ender { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

    /// <summary>
    /// Classe de endereço do contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Ender")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Ender
    {
        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código IBGE do município
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }
    }
}