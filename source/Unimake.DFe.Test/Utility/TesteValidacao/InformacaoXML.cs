using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Unimake.DFe.Test.Utility.TesteValidacao
{
    public class InformacaoXML
    {
        public string TagRaiz { get; set; }
        public string Versao { get; set; }
        public string SchemaArquivo { get; set; }
        public string SchemasEspecifico { get; set; }
        public string SchemaArquivoEspecifico { get; set; }
        public string TagEvento { get; set; }
        public string TargetNS { get; set; }
        public string TagAssinatura { get; set; }
        public string TagAtributoID { get; set; }
        public string TagLoteAssinatura { get; set; }
        public string TagLoteAtributoID { get; set; }
        public string TagExtraAssinatura { get; set; }
        public string TagExtraAtributoID { get; set; }

        public override string ToString()
        {
            return
                $"TagRaiz: {TagRaiz}\n" +
                $"Versão: {Versao}\n" +
                $"SchemaArquivo: {SchemaArquivo}\n" +
                $"SchemasEspecifico: {SchemaArquivoEspecifico}\n" +
                $"TargetNS: {TargetNS}\n" +
                $"TagAssinatura: {TagAssinatura}\n" +
                $"TagAtributoID: {TagAtributoID}\n" +
                $"TagLoteAssinatura: {TagLoteAssinatura}\n" +
                $"TagLoteAtributoID: {TagLoteAtributoID}\n" +
                $"TagExtraAssinatura: {TagExtraAssinatura}\n" +
                $"TagExtraAtributoID: {TagExtraAtributoID}";
        }

    }
}
