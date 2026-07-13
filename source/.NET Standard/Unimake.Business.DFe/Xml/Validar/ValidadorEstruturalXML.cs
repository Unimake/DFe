using System;
using System.Xml;
using Unimake.Business.DFe.Isoladores;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Vinculadores;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Coordena a validação do XML contra os schemas geral e específicos.
    /// </summary>
    internal static class ValidadorEstruturalXML
    {
        internal static void Validar(XmlDocument xml, XmlNode servico, ValidarEstruturaXML.InformacaoXML informacao, TipoDFe tipoDFe, PadraoNFSe padraoNFSe)
        {
            if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") == null)
            {
                ValidarGeral(xml, informacao, tipoDFe, padraoNFSe);
                return;
            }

            var isEvento = servico.SelectSingleNode(".//*[local-name()='TagEvento']") != null;
            var vinculador = VinculadorFactory.Criar(tipoDFe, isEvento);
            var vinculacoes = vinculador.Vincular(servico, xml);
            var geralValidado = false;

            foreach (var vinculacao in vinculacoes)
            {
                var informacaoEspecifica = Copiar(informacao);
                LeitorConfiguracaoValidacao.AplicarInformacaoEspecifica(vinculacao.Item1, informacaoEspecifica);

                if (!geralValidado)
                {
                    ValidarGeral(xml, informacaoEspecifica, tipoDFe, padraoNFSe);
                    geralValidado = true;
                }

                ValidarEspecifico(vinculacao.Item2, isEvento, informacaoEspecifica, tipoDFe);
            }
        }

        internal static void ValidarGeral(XmlDocument xml, ValidarEstruturaXML.InformacaoXML informacao, TipoDFe tipoDFe, PadraoNFSe padraoNFSe)
        {
            if (string.IsNullOrEmpty(informacao.SchemaArquivo))
            {
                return;
            }

            var tipoSchema = tipoDFe == TipoDFe.NFCe ? TipoDFe.NFe : tipoDFe;
            var schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoSchema}.{informacao.SchemaArquivo}"
                : $"{tipoSchema}.{padraoNFSe}.{informacao.SchemaArquivo}";
            var validar = new ValidarSchema();
            validar.Validar(xml, schema, informacao.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema geral: {validar.ErrorMessage}.");
            }
        }

        internal static void ValidarEspecifico(XmlNode node, bool isEvento, ValidarEstruturaXML.InformacaoXML informacao, TipoDFe tipoDFe)
        {
            if (string.IsNullOrEmpty(informacao.SchemaArquivoEspecifico))
            {
                return;
            }

            var isolador = IsoladorFactory.CriarIsolador(tipoDFe, isEvento);
            var xmlEspecifico = isolador.Isolar(node);

            if (xmlEspecifico == null)
            {
                return;
            }

            var validar = new ValidarSchema();
            validar.Validar(xmlEspecifico, $"{tipoDFe}.{informacao.SchemaArquivoEspecifico}", informacao.TargetNSEspecifico);

            if (!validar.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema específico: {validar.ErrorMessage}.");
            }
        }

        private static ValidarEstruturaXML.InformacaoXML Copiar(ValidarEstruturaXML.InformacaoXML origem)
        {
            return new ValidarEstruturaXML.InformacaoXML
            {
                TagRaiz = origem.TagRaiz,
                Descricao = origem.Descricao,
                Versao = origem.Versao,
                SchemaArquivo = origem.SchemaArquivo,
                SchemaArquivoEspecifico = origem.SchemaArquivoEspecifico,
                TagEvento = origem.TagEvento,
                TargetNS = origem.TargetNS,
                TargetNSEspecifico = origem.TargetNSEspecifico,
                TagAssinatura = origem.TagAssinatura,
                TagAtributoID = origem.TagAtributoID,
                TagLoteAssinatura = origem.TagLoteAssinatura,
                TagLoteAtributoID = origem.TagLoteAtributoID,
                TagExtraAssinatura = origem.TagExtraAssinatura,
                TagExtraAtributoID = origem.TagExtraAtributoID,
                AssinaCanonicalizacaoExclusiva = origem.AssinaCanonicalizacaoExclusiva,
                UsaCertificadoDigital = origem.UsaCertificadoDigital,
                SignatureAlgorithmType = origem.SignatureAlgorithmType,
                NaoAssina = origem.NaoAssina,
                GerarQRCode = origem.GerarQRCode
            };
        }
    }
}
