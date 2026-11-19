using System.Text;
using System.Xml.Linq;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class ConsultaSituacaoTxtRetornoTest
{
    [Theory]
    [InlineData("55", false, false)]
    [InlineData("55", false, true)]
    [InlineData("55", true, false)]
    [InlineData("55", true, true)]
    [InlineData("65", false, false)]
    [InlineData("65", false, true)]
    [InlineData("65", true, false)]
    [InlineData("65", true, true)]
    public void PreservaBytesDoResumoEDosBlocosDeEvento(string modelo, bool temProtocolo, bool temEvento)
    {
        XNamespace ns = "http://www.portalfiscal.inf.br/nfe";
        const string data = "2026-09-24T12:05:00-03:00";
        const string protocoloNota = "141260000000099";
        const string protocoloRegistro = "141260000000101";
        var chave = $"41260999999999000191{modelo}0590000000501000000050";
        var status = temEvento ? "101" : "526";
        var raiz = new XElement(ns + "retConsSitNFe", new XAttribute("versao", "4.00"),
            new XElement(ns + "tpAmb", "2"), new XElement(ns + "verAplic", "TESTE"),
            new XElement(ns + "cStat", status), new XElement(ns + "xMotivo", "Situação sintética"),
            new XElement(ns + "cUF", "41"), new XElement(ns + "dhRecbto", data),
            new XElement(ns + "chNFe", chave));

        if (temProtocolo)
            raiz.Add(new XElement(ns + "protNFe", new XElement(ns + "infProt",
                new XElement(ns + "tpAmb", "2"), new XElement(ns + "cStat", "100"),
                new XElement(ns + "xMotivo", "Autorização sintética"), new XElement(ns + "cUF", "41"),
                new XElement(ns + "dhRecbto", data), new XElement(ns + "nProt", protocoloNota))));

        if (temEvento)
            raiz.Add(new XElement(ns + "procEventoNFe",
                new XElement(ns + "evento", new XElement(ns + "infEvento",
                    new XElement(ns + "chNFe", chave),
                    new XElement(ns + "detEvento", new XElement(ns + "descEvento", "Cancelamento"),
                        new XElement(ns + "nProt", protocoloNota),
                        new XElement(ns + "xJust", "Justificativa sintética")))),
                new XElement(ns + "retEvento", new XElement(ns + "infEvento",
                    new XElement(ns + "cStat", "135"), new XElement(ns + "xMotivo", "Registro concluído"),
                    new XElement(ns + "nProt", protocoloRegistro)))));

        var esperado = temProtocolo
            ? $"2;100;Autorização sintética;41;{data};{protocoloNota};\r\n"
            : temEvento ? "" : $"2;526;Situação sintética;41;{data};0;\r\n";
        if (temEvento)
            esperado += $"[evento]\r\n{chave};Cancelamento;{protocoloNota};Justificativa sintética;\r\n"
                + $"[retEvento]\r\n135;Registro concluído;{protocoloRegistro};\r\n";

        var retorno = new ConsultaSituacaoTxtConverter().ConverterRetorno(raiz.ToString(SaveOptions.DisableFormatting));
        Assert.Equal(Encoding.UTF8.GetBytes(esperado), Encoding.UTF8.GetBytes(retorno));
    }
}
