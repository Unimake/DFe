using System.Diagnostics;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.Security;
using Xunit;
using Xunit.Abstractions;

public class TesteCertificadoDigital
{
    private readonly ITestOutputHelper _output;

    public TesteCertificadoDigital(ITestOutputHelper output)
    {
        _output = output;
    }

    [Fact]
    public void TesteCertificadoVelocidade()
    {
        var CertificadoCaminho = @"C:\Projetos\Unimake_PV.pfx";
        var CertificadoSenha = "12345678";

        // Método original
        var sw1 = Stopwatch.StartNew();
        for (int i = 0; i < 50; i++)
        {
            var certificado = new CertificadoDigital()
                .CarregarCertificadoDigitalA1(CertificadoCaminho, CertificadoSenha);
        }
        sw1.Stop();
        _output.WriteLine($"Tempo do método original: {sw1.ElapsedMilliseconds} ms");


        // Método com Configuracao 
        var configuracao = new Configuracao
        {
            CertificadoArquivo = CertificadoCaminho,
            CertificadoSenha = CertificadoSenha,
            TipoDFe = TipoDFe.NFSe
        };

        var sw2 = Stopwatch.StartNew();
        for (int i = 0; i < 50; i++)
        {
            var certRapido = configuracao.CertificadoDigital;
        }
        sw2.Stop();
        _output.WriteLine($"Tempo do método com Configuração única: {sw2.ElapsedMilliseconds} ms");
    }
}
