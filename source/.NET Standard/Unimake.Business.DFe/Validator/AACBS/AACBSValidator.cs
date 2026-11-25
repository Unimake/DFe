using System;
using System.Globalization;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.AACBS
{
    /// <summary>Regras condicionais dos contratos XML da apuração assistida da CBS, complementares ao XSD.</summary>
    public class AACBSValidator : XmlValidatorBase
    {
        private static readonly XNamespace NS = AACBSFormat.Namespace;

        /// <summary>Registra as regras de conteúdo do domínio.</summary>
        public AACBSValidator() => ValidateTag(CanValidate, ValidateContent);

        /// <summary>Prepara a validação de conteúdo de um documento AACBS.</summary>
        /// <param name="xml">Documento XML a validar.</param>
        public AACBSValidator(XmlDocument xml) : this()
        {
            Xml = Unimake.Business.DFe.Utility.XMLUtility.ObterTextoXmlAACBS(xml ?? throw new ArgumentNullException(nameof(xml)));
        }

        /// <summary>Reconhece somente raízes do catálogo no namespace AACBS.</summary>
        /// <param name="element">Raiz a identificar.</param>
        /// <returns>Verdadeiro para um documento do domínio.</returns>
        public override bool CanValidate(XElement element) => element != null && element.Parent == null &&
            element.Name.Namespace == NS && AACBSFormat.IsRoot(element.Name.LocalName);

        /// <summary>Valida o conteúdo AACBS preservando os espaços do XML.</summary>
        /// <returns>Verdadeiro quando a validação é concluída.</returns>
        public override bool Validate() => base.Validate(LoadOptions.PreserveWhitespace);

        private static void Fail(XElement element, string rule) =>
            throw new ValidatorDFeException("AACBS: tag <" + element.Name.LocalName + "> no grupo <" +
                (element.Parent?.Name.LocalName ?? element.Name.LocalName) + ">: " + rule);

        private static void ValidateContent(XElement root)
        {
            foreach (var element in root.Descendants().Where(e => e.Name.Namespace == NS))
            {
                var name = element.Name.LocalName;
                switch (name)
                {
                    case "geradoEm":
                    case "emissao":
                    case "registro":
                    case "atualizacao":
                    case "expiracao":
                    case "expiracaoLink":
                    case "urlAssinadaExpiraEm":
                        ValidateInstant(element);
                        break;
                    case "dataArrecadacao":
                    case "dataMaximaPagamento":
                    case "dataVencimento":
                        ValidateDate(element);
                        break;
                    case "vencimento":
                        if (element.Value.Contains("T")) ValidateInstant(element);
                        else ValidateDate(element);
                        break;
                    case "urlRetorno":
                        if (element.Value.Length != 0) ValidateUrl(element, true);
                        break;
                    case "url":
                    case "linkDownload":
                    case "urlAssinada":
                        ValidateUrl(element, true, root.Name.LocalName == "baixarArquivoRetorno");
                        break;
                    case "linkDarfPDF":
                        ValidateUrl(element, false);
                        break;
                    case "token_type":
                        if (!string.Equals(element.Value, "Bearer", StringComparison.OrdinalIgnoreCase)) Fail(element, "o tipo de token deve ser Bearer.");
                        break;
                    case "opcaoPagamento":
                        if (element.Value == "PAGAMENTO_PARCIAL" && element.Parent.Element(NS + "valorDarfParcial") == null)
                            Fail(element, "PAGAMENTO_PARCIAL exige <valorDarfParcial>, inclusive quando zero.");
                        break;
                    case "chaveDFE":
                        if (root.Name.LocalName == "gerarDarfAdquirente" && string.IsNullOrWhiteSpace(element.Value))
                            Fail(element, "informe uma chave DF-e que não seja apenas espaço em branco.");
                        ValidateKeyLength(element);
                        break;
                    case "chave":
                        ValidateKeyLength(element);
                        break;
                    case "situacao":
                        var needsError = element.Value == "ERRO" || element.Value == "EXPIRADA";
                        var code = element.Parent.Element(NS + "codigoErro");
                        var message = element.Parent.Element(NS + "mensagemErro");
                        if (needsError != (code != null && message != null) || (!needsError && (code != null || message != null)))
                            Fail(element, "codigoErro e mensagemErro devem aparecer juntos, somente em ERRO ou EXPIRADA.");
                        break;
                    case "cdRetorno":
                        ValidateDarfResult(element);
                        break;
                }
            }

            foreach (var payment in root.Descendants(NS + "pagamentos")
                .Where(e => e.Ancestors(NS + "retArquivoPagamentos").Any()).Elements(NS + "item"))
            {
                int.TryParse(payment.Element(NS + "tipo")?.Value, NumberStyles.Integer, CultureInfo.InvariantCulture, out var type);
                var rad = type == 1;
                var pcont = type == 2;
                if (!rad && !pcont) continue; // O domínio de tipo é verificado no XSD.
                if (rad && (payment.Element(NS + "niAdquirente") == null || payment.Element(NS + "niContribuinte") != null))
                    Fail(payment, "tipo=1 exige apenas niAdquirente na identificação do pagamento.");
                if (pcont && (payment.Element(NS + "niContribuinte") == null || payment.Element(NS + "niAdquirente") != null))
                    Fail(payment, "tipo=2 exige apenas niContribuinte na identificação do pagamento.");
                foreach (var part in payment.Elements(NS + "composicao").Elements(NS + "item"))
                {
                    if (pcont && (part.Element(NS + "niContribuinte") != null || part.Element(NS + "chaveDFE") != null))
                        Fail(part, "a composição PCONT não admite niContribuinte nem chaveDFE.");
                    if (pcont && part.Element(NS + "vencimento") is XElement due) ValidateDate(due);
                }
            }
        }

        private static void ValidateKeyLength(XElement element)
        {
            // O contrato limita caracteres Unicode; um par substituto UTF-16 conta uma vez.
            var value = element.Value;
            var characters = 0;
            for (var index = 0; index < value.Length; index++)
            {
                if (char.IsHighSurrogate(value[index]) && index + 1 < value.Length &&
                    char.IsLowSurrogate(value[index + 1]))
                {
                    index++;
                }
                characters++;
                if (characters > 100)
                {
                    Fail(element, "informe uma chave com no máximo 100 caracteres Unicode.");
                }
            }
        }

        private static void ValidateDarfResult(XElement code)
        {
            var parent = code.Parent;
            var success = code.Value == "DARF_GERADO";
            var darf = parent.Element(NS + "darf");
            var payment = parent.Element(NS + "informacoesPagamento");
            var message = parent.Element(NS + "mensagem");
            var invalid = parent.Element(NS + "dadosInvalidos");
            if (success ? (darf == null || payment == null || message != null || invalid != null) :
                (darf != null || payment != null || message == null))
                Fail(code, "o código de retorno deve corresponder exclusivamente aos grupos de sucesso ou erro do DARF.");
            if (code.Value == "DADOS_DE_ENTRADA_INVALIDOS" && invalid == null)
                Fail(code, "DADOS_DE_ENTRADA_INVALIDOS exige dadosInvalidos.");
        }

        private static void ValidateDate(XElement element)
        {
            if (!DateTime.TryParseExact(element.Value, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out _))
                Fail(element, "informe uma data de calendário válida yyyy-MM-dd, sem hora ou fuso.");
        }

        private static void ValidateInstant(XElement element)
        {
            try { AACBSFormat.ParseInstant(element.Value); }
            catch (FormatException) { Fail(element, "informe um instante válido com fuso explícito e projeção CLR representável."); }
        }

        private static void ValidateUrl(XElement element, bool https, bool download = false)
        {
            if (!Uri.TryCreate(element.Value, UriKind.Absolute, out var uri) ||
                !uri.IsWellFormedOriginalString() ||
                (https && uri.Scheme != Uri.UriSchemeHttps) ||
                (download && (uri.UserInfo.Length != 0 || uri.Fragment.Length != 0)))
                Fail(element, download ? "informe uma URL HTTPS absoluta, sem credencial ou fragmento." :
                    https ? "informe uma URL HTTPS absoluta." : "informe uma URI absoluta.");
        }
    }
}
