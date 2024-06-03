#pragma warning disable CS1591

#if INTEROP
 using System.Runtime.InteropServices;
 #endif

using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    #region IdeEvento

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
    [Serializable()]
    public class IdeEvento
    {
        /// <summary>
        ///Valores válidos:
        ///1 - Produção;
        ///2 - Produção restrita;
        ///7 - Validação(uso interno);
        ///8 - Teste(uso interno);
        ///9 - Desenvolvimento(uso interno);
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        ///        Valores válidos:
        ///1 - Aplicativo do empregador; 
        ///2 - Aplicativo governamental - Simplificado Pessoa Física;
        ///3 - Aplicativo governamental - Web Geral;
        ///4 - Aplicativo governamental - Simplificado Pessoa Jurídica;
        ///8 - Aplicativo governamental para envio de eventos pelo Judiciário;
        ///9 - Aplicativo governamental - Integração com a Junta Comercial;
        ///22 - Aplicativo governamental para dispositivos móveis - Empregador Doméstico;
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

    #endregion IdeEvento

    #region IdeEmpregador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do empregador
    /// </summary>
    [Serializable()]
    public class IdeEmpregador
    {
        /// <summary>
        /// Valores válidos:
        ///1 - CNPJ
        ///2 - CPF
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo tpInsc.
        ///Validação: Se tpInsc for igual a [1], deve ser um número de CNPJ válido. Neste caso, deve ser informada apenas a raiz/base (8 posições), exceto se a
        ///natureza jurídica do declarante for igual a 101-5, 104-
        ///0, 107-4, 116-3 ou 134-1, situação em que o campo
        ///deve ser preenchido com o CNPJ completo(14
        ///posições).
        ///Se tpInsc for igual a[2], deve ser um CPF válido.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    #endregion

    #region CultureInfoESocial
    /// <summary>
    /// Configurações Cultura para o ESocial
    /// </summary>
    public static class CultureInfoESocial
    {
        private static CultureInfo InfoField = new CultureInfo("pt-BR");

        /// <summary>
        /// Info
        /// </summary>
        public static CultureInfo Info
        {
            get
            {
                InfoField.NumberFormat.NumberDecimalSeparator = ",";

                return InfoField;
            }
        }
    }
    #endregion

    #region ItensRemun

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemun")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Rubricas que compõem a remuneração do trabalhador
    /// </summary>
    [Serializable()]
    public abstract class ItensRemun
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador que identifica a rubrica em sua 
        /// folha de pagamento ou ocódigo da rubrica constante da Tabela de Rubricas Padrão.
        /// </summary>
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        /// <summary>
        /// Preencher com o identificador da Tabela de Rubricas para a rubrica definida em codRubr.
        /// </summary>
        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        /// <summary>
        /// Informar a quantidade de referência para apuração (em horas, cotas, meses, etc.). 
        /// Ex.: Quantidade de horas extras trabalhadas relacionada com uma rubrica de hora extra,
        /// quantidade de dias trabalhados relacionada com uma rubrica de salário, etc.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("qtdRubr")]
        public double QtdRubr { get; set; }

        ///// <summary>
        ///// Informar a quantidade de referência para apuração (em horas, cotas, meses, etc.). 
        ///// Ex.: Quantidade de horas extras trabalhadas relacionada com uma rubrica de hora extra,
        ///// quantidade de dias trabalhados relacionada com uma rubrica de salário, etc.
        ///// Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("qtdRubr")]
        //public string QtdRubrField
        //{
        //    get => QtdRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => QtdRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}

        /// <summary>
        /// Informar o fator, percentual, etc. da rubrica, quando necessário.
        /// Ex.: Adicional de horas extras 50%, relacionado a uma rubrica de horas extras: Fator = 50.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("fatorRubr")]
        public double FatorRubr { get; set; }

        ///// <summary>
        ///// Informar o fator, percentual, etc. da rubrica, quando necessário.
        ///// Ex.: Adicional de horas extras 50%, relacionado a uma rubrica de horas extras: Fator = 50.
        ///// Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("fatorRubr")]
        //public string fatorRubrField
        //{
        //    get => FatorRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => FatorRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}

        /// <summary>
        /// Valor total da rubrica. Validação: Deve ser maior que 0 (zero).
        /// </summary>
        //[XmlIgnore]
        [XmlElement("vrRubr")]
        public double VrRubr { get; set; }

        ///// <summary>
        ///// Valor total da rubrica. Validação: Deve ser maior que 0 (zero).
        ///// </summary>
        //[XmlElement("vrRubr")]
        //public string VrRubrField
        //{
        //    get => VrRubr.ToString("F2", CultureInfoESocial.Info);
        //    set => VrRubr = double.Parse(value.ToString(), CultureInfoESocial.Info);
        //}
    }

    #endregion ItensRemun
}
