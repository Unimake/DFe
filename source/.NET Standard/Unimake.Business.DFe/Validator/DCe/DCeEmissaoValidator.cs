using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.DCe
{
    // O tipo TDCe compartilhado com consulta/distribuição permite omitir o país.
    // A entrada de emissão exige os dois campos, sem modificar esse contrato histórico.
    internal sealed class DCeEmissaoValidator : XmlValidatorBase
    {
        private static readonly XNamespace NamespaceDCe = "http://www.portalfiscal.inf.br/dce";

        internal DCeEmissaoValidator() => ValidateTag(CanValidate, documento =>
        {
            var endereco = documento.Element(NamespaceDCe + "infDCe")?
                .Element(NamespaceDCe + "dest")?.Element(NamespaceDCe + "enderDest");
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
