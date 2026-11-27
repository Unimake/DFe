using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.NFSe
{
    internal sealed class NFSeNacionalComercioExteriorValidator : XmlValidatorBase
    {
        private static readonly XNamespace NamespaceNFSe = Unimake.Business.DFe.Xml.NFSe.NACIONAL.Nfse.Ns;

        internal NFSeNacionalComercioExteriorValidator() => ValidateTag(CanValidate, ValidarComercioExterior);

        public override bool CanValidate(XElement element) =>
            element.Parent == null &&
            (element.Name == NamespaceNFSe + "DPS" || element.Name == NamespaceNFSe + "NFSe");

        private static void ValidarComercioExterior(XElement raiz)
        {
            var dps = raiz.Name == NamespaceNFSe + "DPS"
                ? raiz
                : raiz.Element(NamespaceNFSe + "infNFSe")?.Element(NamespaceNFSe + "DPS");
            var infDps = dps?.Element(NamespaceNFSe + "infDPS");
            var grupo = infDps?.Element(NamespaceNFSe + "serv")?.Element(NamespaceNFSe + "comExt");
            if (grupo == null)
            {
                return;
            }

            var valorServico = grupo.Element(NamespaceNFSe + "vServMoeda");
            var tagLegada = valorServico ?? grupo.Element(NamespaceNFSe + "nDI") ?? grupo.Element(NamespaceNFSe + "nRE");
            if (tagLegada == null)
            {
                return;
            }

            var tagAtual = grupo.Element(NamespaceNFSe + "nFatura") ?? grupo.Element(NamespaceNFSe + "vFatura") ??
                grupo.Element(NamespaceNFSe + "nDUIMP") ?? grupo.Element(NamespaceNFSe + "nDUE") ??
                grupo.Element(NamespaceNFSe + "nContCambio") ??
                infDps.Element(NamespaceNFSe + "valores")?.Element(NamespaceNFSe + "vAjusteBC")
                    ?.Element(NamespaceNFSe + "vAjusteBCIBSCBSComExt");
            if (tagAtual != null)
            {
                throw new ValidatorDFeException($"A DPS informa a tag legada <{tagLegada.Name.LocalName}> do grupo <comExt> junto com a tag <{tagAtual.Name.LocalName}> do leiaute atual.");
            }

            if (valorServico == null)
            {
                throw new ValidatorDFeException($"A tag <vServMoeda> é obrigatória no leiaute legado do grupo <comExt> quando <{tagLegada.Name.LocalName}> é informada.");
            }

            if (grupo.Element(NamespaceNFSe + "mdPrestacao")?.Value == "5")
            {
                throw new ValidatorDFeException("A tag <mdPrestacao> do grupo <comExt> aceita o valor 5 somente no leiaute atual.");
            }
        }
    }
}
