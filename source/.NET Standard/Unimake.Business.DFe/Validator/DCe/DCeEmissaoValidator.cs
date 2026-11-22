using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.DCe
{
    // O tipo TDCe é compartilhado com consulta/distribuição. As exigências abaixo
    // se aplicam à emissão, sem restringir o documento histórico distribuído.
    internal sealed class DCeEmissaoValidator : XmlValidatorBase
    {
        private static readonly XNamespace NamespaceDCe = "http://www.portalfiscal.inf.br/dce";

        internal DCeEmissaoValidator() => ValidateTag(CanValidate, documento =>
        {
            var infDCe = documento.Element(NamespaceDCe + "infDCe");
            var tipoEmitente = infDCe?.Element(NamespaceDCe + "ide")?.Element(NamespaceDCe + "tpEmit");
            if (documento.Element(NamespaceDCe + "infSolicDCe") != null &&
                tipoEmitente != null && tipoEmitente.Value != "0")
            {
                throw new ValidarXMLException("O grupo <infSolicDCe> é permitido somente na emissão da DC-e pelo Fisco (tpEmit=0).");
            }

            var endereco = infDCe?.Element(NamespaceDCe + "dest")?.Element(NamespaceDCe + "enderDest");
            // A ausência dos grupos estruturais continua sendo diagnosticada pelo XSD.
            if (endereco == null) return;
            foreach (var nome in new[] { "cPais", "xPais" })
            {
                if (string.IsNullOrWhiteSpace(endereco.Element(NamespaceDCe + nome)?.Value))
                {
                    throw new ValidarXMLException("A tag obrigatória <" + nome +
                        "> do grupo <DCe><infDCe><dest><enderDest> não foi informada para emissão da DC-e.");
                }
            }
        });

        public override bool CanValidate(XElement element) =>
            element.Parent == null && element.Name == NamespaceDCe + "DCe";
    }
}
