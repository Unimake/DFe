using System;
using System.Text;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe.Txt;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Converte arquivos TXT no layout 4.00 para XML de NFe ou NFCe.
    /// A conversão apenas lê o arquivo informado e mantém o resultado em memória.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFeTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class NFeTxtConverter
    {
        /// <summary>
        /// Converte todas as NFe ou NFCe presentes no arquivo TXT informado.
        /// </summary>
        /// <param name="caminhoArquivo">Caminho completo do arquivo TXT de origem.</param>
        /// <returns>Resultado com os XMLs produzidos ou a mensagem de erro de compatibilidade.</returns>
        public NFeTxtConversaoResultado Converter(string caminhoArquivo)
        {
            return Converter(caminhoArquivo, VersaoLeiauteMonofasia.Legado);
        }

        /// <summary>
        /// Converte todas as NFe ou NFCe presentes no arquivo TXT informado usando a versão escolhida do leiaute monofásico.
        /// </summary>
        /// <param name="caminhoArquivo">Caminho completo do arquivo TXT de origem.</param>
        /// <param name="versaoLeiauteMonofasia">Versão do leiaute dos grupos de tributação monofásica.</param>
        /// <returns>Resultado com os XMLs produzidos ou a mensagem de erro de compatibilidade.</returns>
        public NFeTxtConversaoResultado Converter(string caminhoArquivo, VersaoLeiauteMonofasia versaoLeiauteMonofasia)
        {
            return ConverterInterno(caminhoArquivo, versaoLeiauteMonofasia, null);
        }

        /// <summary>
        /// Converte o TXT respeitando o BOM e priorizando UTF-8 estrito antes de usar Windows-1252.
        /// </summary>
        /// <param name="caminhoArquivo">Caminho completo do arquivo TXT de origem.</param>
        /// <param name="codificacaoWindows1252">Codificação explícita de fallback para arquivos sem BOM.</param>
        /// <param name="versaoLeiauteMonofasia">Versão do leiaute dos grupos de tributação monofásica.</param>
        /// <returns>Resultado da conversão de NFe ou NFCe.</returns>
#if INTEROP
        [ComVisible(false)]
#endif
        public NFeTxtConversaoResultado ConverterComDeteccaoCodificacao(string caminhoArquivo, Encoding codificacaoWindows1252,
            VersaoLeiauteMonofasia versaoLeiauteMonofasia = VersaoLeiauteMonofasia.Legado)
        {
            if (codificacaoWindows1252 == null)
                throw new ArgumentNullException(nameof(codificacaoWindows1252));

            return ConverterInterno(caminhoArquivo, versaoLeiauteMonofasia, codificacaoWindows1252);
        }

        private static NFeTxtConversaoResultado ConverterInterno(string caminhoArquivo,
            VersaoLeiauteMonofasia versaoLeiauteMonofasia, Encoding codificacaoWindows1252)
        {
            if (string.IsNullOrWhiteSpace(caminhoArquivo))
            {
                throw new ArgumentException("O caminho do arquivo TXT deve ser informado.", nameof(caminhoArquivo));
            }

            return new NFeTxtConversionEngine(versaoLeiauteMonofasia).Converter(caminhoArquivo, codificacaoWindows1252);
        }

        /// <summary>
        /// Converte o retorno bruto da autorização de NFe/NFCe para o layout TXT legado do UniNFe.
        /// </summary>
        /// <param name="xmlRetorno">XML bruto retornado pelo serviço de autorização.</param>
        /// <returns>Conteúdo TXT compatível com o UniNFe.</returns>
        public string ConverterRetorno(string xmlRetorno)
        {
            return Txt.LegacyTxtRetornoConverter.Autorizacao(xmlRetorno);
        }
    }
}
