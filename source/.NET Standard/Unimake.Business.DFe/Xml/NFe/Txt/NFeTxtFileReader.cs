using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal static class NFeTxtFileReader
    {
        internal static Dictionary<int, List<string>> Carregar(string arquivo, string prefixo, out string mensagemErro, out bool possuiCabecalho)
        {
            mensagemErro = string.Empty;
            possuiCabecalho = false;
            var conteudoPorNota = new Dictionary<int, List<string>>();

            if (!File.Exists(arquivo))
            {
                mensagemErro = "Arquivo [" + arquivo + "] nÃ£o encontrado";
                return conteudoPorNota;
            }

            try
            {
                using (var leitor = new StreamReader(arquivo, Encoding.Default, true))
                {
                    var numeroNota = -1;
                    var linha = leitor.ReadLine();
                    if (linha != null)
                    {
                        possuiCabecalho = true;
                        if (!linha.StartsWith("NOTAFISCAL") && !linha.StartsWith("NOTA FISCAL"))
                        {
                            mensagemErro = " ConteÃºdo da primeira linha do arquivo deve ser 'NOTAFISCAL'";
                        }

                        linha = leitor.ReadLine();
                    }

                    while (linha != null)
                    {
                        if (linha.Trim().Length > 0)
                        {
                            if (linha.StartsWith("A|"))
                            {
                                ++numeroNota;
                                conteudoPorNota.Add(numeroNota, new List<string>());
                            }

                            List<string> linhasNota;
                            conteudoPorNota.TryGetValue(numeroNota, out linhasNota);
                            if (linhasNota == null)
                            {
                                mensagemErro = "O primeiro registro da nota deve ser o segmento A";
                                return conteudoPorNota;
                            }

                            var linhaNormalizada = linha.Trim();
                            linhasNota.Add(prefixo + linhaNormalizada + (!linha.TrimEnd().EndsWith("|") ? "|" : string.Empty));
                        }

                        linha = leitor.ReadLine();
                    }
                }
            }
            catch (IOException ex)
            {
                mensagemErro += ex.Message;
            }
            catch (Exception ex)
            {
                mensagemErro += ex.Message;
            }

            return conteudoPorNota;
        }
    }
}
