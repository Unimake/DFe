using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Utilitarios;

/// <summary>
/// Regressão de ICMS70 sem desoneração em TXT de quatro itens.
/// </summary>
[Trait("DFe", "NFe")]
public class NFeTxtIcms70MotivoAusenteTest
{
    [Fact]
    public void N09SemDesoneracaoDeveOmitirValorEMotivoEmTodosOsItens()
    {
        var origem = Path.Combine(Environment.CurrentDirectory, @"NFe\Resources\Txt",
            "000034435-icms70-motivo-ausente-nfe.txt");
        var resultado = new NFeTxtConverter().Converter(origem);

        Assert.True(resultado.Sucesso, resultado.MensagemErro);
        var xml = new XmlDocument();
        xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
        var grupos = xml.SelectNodes("//*[local-name()='ICMS70']");
        Assert.Equal(4, grupos.Count);
        foreach (XmlNode icms70 in grupos)
        {
            Assert.Null(icms70.SelectSingleNode("*[local-name()='vICMSDeson']"));
            Assert.Null(icms70.SelectSingleNode("*[local-name()='motDesICMS']"));
        }

        var validacao = new ValidarSchema();
        validacao.Validar(xml, "NFe.nfe_v4.00.xsd", "http://www.portalfiscal.inf.br/nfe");
        Assert.False(validacao.Success);
        Assert.Contains("Signature", validacao.ErrorMessage);
        Assert.Equal(1, validacao.ErrorMessage.Split(new[] { "Linha:" }, StringSplitOptions.None).Length - 1);
    }
}
