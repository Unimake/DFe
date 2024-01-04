using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;

namespace Unimake.Business.DFe.Security
{
    internal static class IIBRASIL
    {
        public static string cabecalho
        {
            get { return "<cabecalho xmlns=\"http://www.abrasf.org.br/nfse.xsd\" versao=\"2.04\"><versaoDados>2.04</versaoDados></cabecalho>"; }
        }
        public static string GerarIntegridade(string xmlDoc, string token)
        {
            string conteudo = xmlDoc;
            conteudo = Regex.Replace(conteudo, "/[^\x20-\x7E]+/","");
            conteudo = Regex.Replace(conteudo, "/[ ]+/", "");
            string integridade = Criptografia.GerarRSASHA512(conteudo + token);

            return integridade;
        }
    }
}
