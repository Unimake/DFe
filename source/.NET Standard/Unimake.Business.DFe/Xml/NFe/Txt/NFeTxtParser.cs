using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Coordena o processamento sequencial dos segmentos TXT e a apresentacao de erros de leitura.
    /// </summary>
    internal sealed class NFeTxtParser
    {
        internal bool Processar(NFeTxtConversionContext context, List<string> conteudoNota, Action<NFeTxtSegment> processarRegistro, Dictionary<string, string> layouts, Func<decimal> obterVersao, Func<string> obterLayout, Action<string> adicionarMensagem, string prefixo)
        {
            var houveErro = false;
            foreach (string conteudoRegistro in conteudoNota)
            {
                houveErro = false;
                ++context.LinhaLida;
                try
                {
                    var segmento = new NFeTxtSegment(conteudoRegistro);
                    if (!segmento.Ignorar)
                    {
                        segmento.DefinirLayout(NFeTxtLayoutResolver.Resolver(layouts, segmento.Codigo, obterVersao(), segmento.QuantidadePipes));
                    }

                    processarRegistro(segmento);
                }
                catch (Exception ex)
                {
                    houveErro = true;
                    var layout = obterLayout();
                    if (!string.IsNullOrEmpty(layout))
                    {
                        adicionarMensagem("Layout: " + layout.Replace(prefixo, "") + Environment.NewLine);
                    }

                    adicionarMensagem("Linha lida: " + (context.LinhaLida + 1).ToString() + Environment.NewLine +
                                      "Conteudo: " + conteudoRegistro.Substring(1) + Environment.NewLine +
                                      ex.Message + Environment.NewLine);
                }
            }

            return houveErro;
        }
    }
}
