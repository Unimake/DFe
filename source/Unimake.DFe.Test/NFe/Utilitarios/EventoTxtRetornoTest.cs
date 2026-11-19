#nullable enable

using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class EventoTxtRetornoTest
{
    public static IEnumerable<object[]> CasosLegados()
    {
        foreach (var modelo in new[] { "55", "65" })
        {
            const string data = "2026-09-24T12:05:00-03:00";
            var chave = "41260899999999000191" + modelo + "0010000000501100000509";
            const string protocolo = "141260000000101";
            const string cpf = "99999999999";
            const string cnpj = "99999999000191";
            XNamespace ns = "http://www.portalfiscal.inf.br/nfe";
            string Cabeca(string codigo, string motivo, string tipo) => $"2;41;{codigo};{motivo};{chave};{tipo};";
            XElement Inf(string tipo, string cod, string motivo, string? nome, string? prot, string? destinatarioTag = null, string? destinatario = null) =>
                new(ns + "infEvento", new XElement(ns + "tpAmb", 2), new XElement(ns + "cOrgao", 41), new XElement(ns + "cStat", cod),
                    new XElement(ns + "xMotivo", motivo), new XElement(ns + "chNFe", chave), new XElement(ns + "tpEvento", tipo),
                    nome is null ? null : new XElement(ns + "xEvento", nome), new XElement(ns + "nSeqEvento", 1),
                    destinatarioTag is null ? null : new XElement(ns+destinatarioTag, destinatario),
                    new XElement(ns + "dhRegEvento", data), prot is null ? null : new XElement(ns + "nProt", prot));
            XElement Lote(params XElement[] eventos) => new(ns + "retEnvEvento", eventos.Select(e => new XElement(ns + "retEvento", e)));
            var epec = Inf("110140", "136", "Evento registrado", "EPEC", protocolo);
            epec.Add(new XElement(ns + "cOrgaoAutor", "41"), new XElement(ns + "chNFePend", chave), new XElement(ns + "chNFePend", new string('9', 44)));
            var epecSemOrgao = new XElement(epec);
            epecSemOrgao.Element(ns + "cOrgaoAutor")!.Remove();
            var prorrogacao = Inf("111500", "135", "Evento registrado", "Prorrogação", protocolo);
            var prorrogacaoComEmail = new XElement(prorrogacao);
            prorrogacaoComEmail.Add(new XElement(ns + "emailDest", "teste@example.com"));
            var aceito = Inf("110111", "135", "Evento registrado", "Cancelamento", protocolo);
            var rejeitado = Inf("110111", "573", "Rejeição; sintética", null, null);
            var cenarios = new (string nome, XElement xml, string legado)[]
            {
                ("aceito-sem-destinatario", Lote(aceito), Cabeca("135", "Evento registrado", "110111") + $"Cancelamento;1;;{data};{protocolo};\r\n"),
                ("separador-interno-com-todos-os-campos", Lote(Inf("110111", "135", "Registro; sintético", "Cancelamento", protocolo)),
                    Cabeca("135", "Registro  sintético", "110111") + $"Cancelamento;1;;{data};{protocolo};\r\n"),
                ("rejeitado-opcionais-ausentes", Lote(Inf("110111", "573", "Rejeição sintética", null, null)),
                    Cabeca("573", "Rejeição sintética", "110111") + $"1;;{data};\r\n"),
                ("opcional-presente-vazio", Lote(Inf("110111", "573", "Rejeição sintética", "", null)),
                    Cabeca("573", "Rejeição sintética", "110111") + $";1;;{data};\r\n"),
                ("destinatario-cnpj", Lote(Inf("110111", "135", "Evento registrado", "Cancelamento", protocolo, "CNPJDest", cnpj)),
                    Cabeca("135", "Evento registrado", "110111") + $"Cancelamento;1;{cnpj};{data};{protocolo};\r\n"),
                ("destinatario-cpf", Lote(Inf("110111", "135", "Evento registrado", "Cancelamento", protocolo, "CPFDest", cpf)),
                    Cabeca("135", "Evento registrado", "110111") + $"Cancelamento;1;{cpf};{data};{protocolo};\r\n"),
                ("epec-com-orgao-e-lista", Lote(epec), Cabeca("136", "Evento registrado", "110140") + $"EPEC;1;;{data};{protocolo};41;{chave};{new string('9', 44)};\r\n"),
                ("epec-sem-orgao-com-lista", Lote(epecSemOrgao), Cabeca("136", "Evento registrado", "110140") + $"EPEC;1;;{data};{protocolo};{chave};{new string('9', 44)};\r\n"),
                ("prorrogacao-com-email", Lote(prorrogacaoComEmail), Cabeca("135", "Evento registrado", "111500") + $"Prorrogação;1;;{data};{protocolo};teste@example.com;\r\n"),
                ("prorrogacao-sem-email", Lote(prorrogacao), Cabeca("135", "Evento registrado", "111500") + $"Prorrogação;1;;{data};{protocolo};\r\n"),
                ("lote-aceite-e-rejeicao", Lote(aceito, rejeitado),
                    Cabeca("135", "Evento registrado", "110111") + $"Cancelamento;1;;{data};{protocolo};\r\n"
                    + Cabeca("573", "Rejeição  sintética", "110111") + $"1;;{data};\r\n"),
                ("lote-sem-retEvento", Lote(), ""),
            };

            foreach (var caso in cenarios)
                yield return new object[] { modelo, caso.nome, caso.xml.ToString(SaveOptions.DisableFormatting), caso.legado };
        }
    }

    [Theory]
    [MemberData(nameof(CasosLegados))]
    public void Retorno_DevePreservarBytesDoLegado(string modelo, string cenario, string xml, string esperado)
    {
        Assert.Contains(modelo, new[] { "55", "65" });
        Assert.NotEmpty(cenario);
        var atual = new EventoTxtConverter().ConverterRetorno(xml);
        Assert.Equal(Encoding.Default.GetBytes(esperado), Encoding.Default.GetBytes(atual));
    }
}
