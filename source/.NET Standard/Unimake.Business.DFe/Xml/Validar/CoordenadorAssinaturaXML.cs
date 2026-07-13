using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.Validar
{
    internal delegate void AssinarEventoEmLote(
        XmlDocument xml,
        XmlNode servico,
        TipoAmbiente? ambienteSemAssinatura,
        X509Certificate2 certificado,
        TipoAmbiente tipoAmbiente);

    internal delegate void AssinarElementoXML(
        XmlDocument xml,
        string tagAssinatura,
        string tagID,
        TipoAmbiente? ambienteSemAssinatura,
        bool usaCertificado,
        bool canonicalizacaoExclusiva,
        AlgorithmType algoritmo,
        bool gerarQrCode,
        X509Certificate2 certificado,
        TipoAmbiente tipoAmbiente,
        TipoDFe tipoDFe,
        Configuracao configuracao);

    /// <summary>
    /// Define quais assinaturas devem ser aplicadas ao XML.
    /// </summary>
    internal static class CoordenadorAssinaturaXML
    {
        internal static void AssinarSeNecessario(
            XmlDocument xml,
            XmlNode servico,
            ValidarEstruturaXML.InformacaoXML informacao,
            X509Certificate2 certificado,
            Configuracao configuracao,
            TipoAmbiente tipoAmbiente,
            TipoDFe tipoDFe,
            AssinarEventoEmLote assinarEFDReinf,
            AssinarEventoEmLote assinarESocial,
            AssinarElementoXML assinarElemento)
        {
            if (tipoDFe == TipoDFe.EFDReinf && xml.GetElementsByTagName("envioLoteEventos").Count > 0)
            {
                assinarEFDReinf(xml, servico, informacao.NaoAssina, certificado, tipoAmbiente);
                return;
            }

            if (tipoDFe == TipoDFe.ESocial && xml.GetElementsByTagName("envioLoteEventos").Count > 0)
            {
                assinarESocial(xml, servico, informacao.NaoAssina, certificado, tipoAmbiente);
                return;
            }

            AssinarSeConfigurado(xml, informacao.TagAssinatura, informacao.TagAtributoID, informacao, certificado, configuracao, tipoAmbiente, tipoDFe, assinarElemento);
            AssinarSeConfigurado(xml, informacao.TagLoteAssinatura, informacao.TagLoteAtributoID, informacao, certificado, configuracao, tipoAmbiente, tipoDFe, assinarElemento);
            AssinarSeConfigurado(xml, informacao.TagExtraAssinatura, informacao.TagExtraAtributoID, informacao, certificado, configuracao, tipoAmbiente, tipoDFe, assinarElemento);
        }

        private static void AssinarSeConfigurado(
            XmlDocument xml,
            string tagAssinatura,
            string tagID,
            ValidarEstruturaXML.InformacaoXML informacao,
            X509Certificate2 certificado,
            Configuracao configuracao,
            TipoAmbiente tipoAmbiente,
            TipoDFe tipoDFe,
            AssinarElementoXML assinarElemento)
        {
            if (string.IsNullOrEmpty(tagAssinatura))
            {
                return;
            }

            assinarElemento(
                xml,
                tagAssinatura,
                tagID,
                informacao.NaoAssina,
                informacao.UsaCertificadoDigital,
                informacao.AssinaCanonicalizacaoExclusiva,
                informacao.SignatureAlgorithmType,
                informacao.GerarQRCode,
                certificado,
                tipoAmbiente,
                tipoDFe,
                configuracao);
        }
    }
}
