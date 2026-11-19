using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

[Trait("DFe", "NFe")]
public class ConsultaSituacaoTxtRetornoTest
{
    private static readonly XNamespace Ns = "http://www.portalfiscal.inf.br/nfe";
    private const string Data = "2026-09-24T12:05:00-03:00";
    private const string ProtocoloNota = "141260000000099";
    private const string ProtocoloRegistro = "141260000000101";

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
        var chave = $"41260999999999000191{modelo}0590000000501000000050";
        var status = temEvento ? "101" : temProtocolo ? "100" : "526";
        var raiz = new XElement(Ns + "retConsSitNFe", new XAttribute("versao", "4.00"),
            new XElement(Ns + "tpAmb", "2"), new XElement(Ns + "verAplic", "TESTE"),
            new XElement(Ns + "cStat", status), new XElement(Ns + "xMotivo", "Situação sintética"),
            new XElement(Ns + "cUF", "41"), new XElement(Ns + "dhRecbto", Data),
            new XElement(Ns + "chNFe", chave));

        if (temProtocolo)
        {
            raiz.Add(new XElement(Ns + "protNFe", new XAttribute("versao", "4.00"),
                new XElement(Ns + "infProt", new XElement(Ns + "tpAmb", "2"),
                    new XElement(Ns + "verAplic", "TESTE"), new XElement(Ns + "chNFe", chave),
                    new XElement(Ns + "dhRecbto", Data), new XElement(Ns + "nProt", ProtocoloNota),
                    new XElement(Ns + "cStat", "100"), new XElement(Ns + "xMotivo", "Autorização sintética"))));
        }

        if (temEvento)
        {
            raiz.Add(new XElement(Ns + "procEventoNFe", new XAttribute("versao", "1.00"),
                CriarEventoAssinado(chave),
                new XElement(Ns + "retEvento", new XAttribute("versao", "1.00"),
                    new XElement(Ns + "infEvento", new XElement(Ns + "tpAmb", "2"),
                        new XElement(Ns + "verAplic", "TESTE"), new XElement(Ns + "cOrgao", "41"),
                        new XElement(Ns + "cStat", "135"), new XElement(Ns + "xMotivo", "Registro concluído; motivo sintético"),
                        new XElement(Ns + "chNFe", chave), new XElement(Ns + "tpEvento", "110111"),
                        new XElement(Ns + "xEvento", "Cancelamento"), new XElement(Ns + "nSeqEvento", "1"),
                        new XElement(Ns + "dhRegEvento", Data), new XElement(Ns + "nProt", ProtocoloRegistro)))));
        }

        var xml = new XmlDocument { XmlResolver = null };
        xml.LoadXml(raiz.ToString(SaveOptions.DisableFormatting));
        var schema = new ValidarSchema();
        schema.Validar(xml, "NFe.retConsSitNFe_v4.00.xsd");
        Assert.True(schema.Success, schema.ErrorMessage);

        var esperado = temProtocolo
            ? $"2;100;Autorização sintética;{Data};{ProtocoloNota};\r\n"
            : temEvento ? "" : $"2;526;Situação sintética;41;{Data};0;\r\n";
        if (temEvento)
        {
            esperado += $"[evento]\r\n41;2;99999999000191;{chave};{Data};110111;1;1.00;Cancelamento;{ProtocoloNota};Cancelamento  pedido em duplicidade;\r\n"
                + $"[retEvento]\r\n2;TESTE;41;135;Registro concluído  motivo sintético;{chave};110111;Cancelamento;1;{Data};{ProtocoloRegistro};\r\n";
        }

        var retorno = new ConsultaSituacaoTxtConverter().ConverterRetorno(xml.OuterXml);
        Assert.Equal(Encoding.UTF8.GetBytes(esperado), Encoding.UTF8.GetBytes(retorno));
    }

    private static XElement CriarEventoAssinado(string chave)
    {
        var evento = new XElement(Ns + "evento", new XAttribute("versao", "1.00"),
            new XElement(Ns + "infEvento", new XAttribute("Id", "ID110111" + chave + "01"),
                new XElement(Ns + "cOrgao", "41"), new XElement(Ns + "tpAmb", "2"),
                new XElement(Ns + "CNPJ", "99999999000191"), new XElement(Ns + "chNFe", chave),
                new XElement(Ns + "dhEvento", Data), new XElement(Ns + "tpEvento", "110111"),
                new XElement(Ns + "nSeqEvento", "1"), new XElement(Ns + "verEvento", "1.00"),
                new XElement(Ns + "detEvento", new XAttribute("versao", "1.00"),
                    new XElement(Ns + "descEvento", "Cancelamento"), new XElement(Ns + "nProt", ProtocoloNota),
                    new XElement(Ns + "xJust", "Cancelamento; pedido em duplicidade"))));
        var xml = new XmlDocument { XmlResolver = null };
        xml.LoadXml(evento.ToString(SaveOptions.DisableFormatting));
        using var chaveRsa = RSA.Create(2048);
        var pedido = new CertificateRequest("CN=Situacao TXT sintetico", chaveRsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
        using var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
        AssinaturaDigital.Assinar(xml, "evento", "infEvento", certificado);
        return XElement.Parse(xml.DocumentElement.OuterXml, LoadOptions.PreserveWhitespace);
    }
}
