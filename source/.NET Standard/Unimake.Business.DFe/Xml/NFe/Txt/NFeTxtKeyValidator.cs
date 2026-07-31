using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Monta a chave de acesso e valida o digito verificador informado no TXT.
    /// </summary>
    internal sealed class NFeTxtKeyValidator
    {
        internal string MontarEValidar(DFeNFe.InfNFe infNFe, bool cDvInformado, int cDvInformadoNoTxt, string chaveInformada)
        {
            var cnpjcpfEmissor = string.IsNullOrWhiteSpace(infNFe.Emit.CNPJ)
                ? infNFe.Emit.CPF.PadLeft(14, '0')
                : infNFe.Emit.CNPJ.PadLeft(14, '0');

            if (infNFe.Avulsa != null && !string.IsNullOrWhiteSpace(infNFe.Avulsa.CNPJ))
            {
                cnpjcpfEmissor = infNFe.Avulsa.CNPJ.PadLeft(14, '0');
            }

            var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
            {
                UFEmissor = (UFBrasil)(int)infNFe.Ide.CUF,
                AnoEmissao = infNFe.Ide.DhEmi.ToString("yy"),
                MesEmissao = infNFe.Ide.DhEmi.ToString("MM"),
                CNPJCPFEmissor = cnpjcpfEmissor,
                Modelo = (ModeloDFe)(int)infNFe.Ide.Mod,
                Serie = infNFe.Ide.Serie,
                NumeroDoctoFiscal = infNFe.Ide.NNF,
                TipoEmissao = (TipoEmissao)(int)infNFe.Ide.TpEmis,
                CodigoNumerico = infNFe.Ide.CNF
            };
            var chave = XMLUtility.MontarChaveNFe(ref conteudoChaveDFe);

            var documentoMascarado = IdentificadorMascarado(cnpjcpfEmissor);
            var digitoChaveInformada = -1;
            var chaveCompletaInformada = !string.IsNullOrWhiteSpace(chaveInformada) &&
                chaveInformada.Length == 44 &&
                int.TryParse(chaveInformada.Substring(43, 1), out digitoChaveInformada);
            var chaveCompletaConfirmaDigitoDoTxt = chaveCompletaInformada &&
                cDvInformado &&
                cDvInformadoNoTxt == digitoChaveInformada;
            var tolerarDocumentoMascarado = documentoMascarado && chaveCompletaConfirmaDigitoDoTxt;
            var digitoDoTxtDiverge = cDvInformado && cDvInformadoNoTxt != conteudoChaveDFe.DigitoVerificador;
            var digitoDaChaveDiverge = chaveCompletaInformada && digitoChaveInformada != conteudoChaveDFe.DigitoVerificador;
            if (!tolerarDocumentoMascarado && (digitoDoTxtDiverge || digitoDaChaveDiverge))
            {
                throw new InvalidOperationException("Dígito verificador informado no TXT diverge da chave de acesso calculada.");
            }

            infNFe.Ide.CDV = conteudoChaveDFe.DigitoVerificador;
            return chave;
        }

        private static bool IdentificadorMascarado(string identificador)
        {
            if (string.IsNullOrWhiteSpace(identificador))
            {
                return false;
            }

            var primeiro = identificador[0];
            for (var indice = 1; indice < identificador.Length; indice++)
            {
                if (identificador[indice] != primeiro)
                {
                    return false;
                }
            }

            return true;
        }
    }
}
