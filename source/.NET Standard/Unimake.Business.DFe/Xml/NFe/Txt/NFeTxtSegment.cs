namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Representa um registro TXT já identificado para processamento.
    /// </summary>
    internal sealed class NFeTxtSegment
    {
        internal NFeTxtSegment(string conteudo)
        {
            Conteudo = conteudo;
            Ignorar = conteudo.StartsWith("*");
            if (Ignorar)
            {
                Codigo = string.Empty;
                QuantidadePipes = 0;
                return;
            }

            QuantidadePipes = conteudo.Split(new char[] { '|' }).Length - 1;
            Codigo = conteudo.Substring(1, conteudo.IndexOf("|") - 1).ToUpperInvariant();
        }

        internal string Codigo { get; private set; }

        internal string Conteudo { get; private set; }

        internal bool Ignorar { get; private set; }

        internal int QuantidadePipes { get; private set; }

        internal string Layout { get; private set; }

        internal void DefinirLayout(string layout)
        {
            Layout = layout;
        }
    }
}
