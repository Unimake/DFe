using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Representa o resultado da conversão de um arquivo TXT de NFe ou NFCe.
    /// </summary>
    public sealed class NFeTxtConversaoResultado
    {
        /// <summary>
        /// Inicializa uma nova instância do resultado da conversão.
        /// </summary>
        public NFeTxtConversaoResultado()
        {
            Documentos = new List<NFeTxtDocumento>();
        }

        /// <summary>
        /// Obtém ou define a mensagem de erro da conversão.
        /// </summary>
        public string MensagemErro { get; set; }

        /// <summary>
        /// Obtém os documentos XML produzidos para as notas presentes no TXT.
        /// </summary>
        public List<NFeTxtDocumento> Documentos { get; private set; }

        /// <summary>
        /// Obtém um valor que indica se a conversão foi concluída sem erros.
        /// </summary>
        public bool Sucesso { get { return string.IsNullOrEmpty(MensagemErro); } }
    }

    /// <summary>
    /// Representa o XML de uma NFe ou NFCe convertido a partir de um TXT.
    /// </summary>
    public sealed class NFeTxtDocumento
    {
        internal NFeTxtDocumento(string xml, string chave, int numero, int serie)
        {
            Xml = xml;
            Chave = chave;
            Numero = numero;
            Serie = serie;
        }

        /// <summary>
        /// Obtém o XML da NFe ou NFCe, sem qualquer persistência em disco.
        /// </summary>
        public string Xml { get; private set; }

        /// <summary>
        /// Obtém a chave de acesso do documento.
        /// </summary>
        public string Chave { get; private set; }

        /// <summary>
        /// Obtém o número da nota fiscal.
        /// </summary>
        public int Numero { get; private set; }

        /// <summary>
        /// Obtém a série da nota fiscal.
        /// </summary>
        public int Serie { get; private set; }
    }
}
