using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Validator.CTeOS;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.CTeOS.Validacao
{
    [Trait("DFe", "CTeOS")]
    [Trait("Categoria", "Offline")]
    public class CTeOSModalValidatorTest
    {
        public static IEnumerable<object[]> Cenarios()
        {
            foreach (var tpCTe in new[] { 0, 3 })
            foreach (var ufFim in new[] { "PR", "SP" })
            foreach (var estrutura in new[] { "SemNorm", "SemModal", "SemRodo", "ObrigatorioVazio", "ProibidoPresente", "Completo" })
            {
                yield return new object[] { tpCTe, "6", "01", "PR", ufFim, estrutura };
            }

            foreach (var tpCTe in new[] { 0, 3 })
            foreach (var estrutura in new[] { "SemModal", "SemRodo" })
            {
                yield return new object[] { tpCTe, "6", "01", "EX", "PR", estrutura };
                yield return new object[] { tpCTe, "6", "01", "PR", "EX", estrutura };
            }

            yield return new object[] { 1, "6", "01", "PR", "PR", "SemModal" };
            yield return new object[] { 0, "7", "01", "PR", "PR", "SemModal" };
            yield return new object[] { 0, "6", "02", "PR", "PR", "SemModal" };
        }

        [Theory]
        [MemberData(nameof(Cenarios))]
        public void ValidarDeveDiagnosticarGrupoAusenteEPreservarRegrasEExclusoes(
            int tpCTe, string tpServ, string modal, string ufIni, string ufFim, string estrutura)
        {
            var xml = CriarXml(tpCTe, tpServ, modal, ufIni, ufFim, estrutura);
            var validator = Assert.IsType<CTeOSValidator>(new ValidatorFactory().BuidValidator(xml));
            var regraAplicavel = (tpCTe == 0 || tpCTe == 3) && tpServ == "6" && modal == "01";
            var exterior = ufIni == "EX" || ufFim == "EX";
            if (!regraAplicavel || exterior || estrutura == "Completo")
            {
                Assert.True(validator.Validate());
                return;
            }

            var erro = Assert.Throws<ValidatorDFeException>(() => validator.Validate());
            Assert.Contains(estrutura == "ProibidoPresente" ? "não pode" : "obrigatório", erro.Message);
            switch (estrutura)
            {
                case "SemNorm":
                    Assert.Contains("<infCTeNorm>", erro.Message);
                    Assert.Contains("<tpCTe> = 0 ou 3", erro.Message);
                    break;
                case "SemModal":
                    Assert.Contains("<infModal>", erro.Message);
                    Assert.Contains("<infCte><infCTeNorm>", erro.Message);
                    break;
                case "SemRodo":
                    Assert.Contains("<rodoOS>", erro.Message);
                    Assert.Contains("<infCte><infCTeNorm><infModal>", erro.Message);
                    break;
                case "ObrigatorioVazio":
                    Assert.Contains(ufIni == ufFim ? "<NroRegEstadual>" : "<TAF>", erro.Message);
                    break;
                case "ProibidoPresente":
                    Assert.Contains(ufIni == ufFim ? "<TAF>" : "<NroRegEstadual>", erro.Message);
                    break;
            }
        }

        [Theory]
        [InlineData(0, "PR", "SemModal", "infModal")]
        [InlineData(0, "SP", "SemModal", "infModal")]
        [InlineData(3, "PR", "SemModal", "infModal")]
        [InlineData(3, "SP", "SemModal", "infModal")]
        [InlineData(0, "PR", "SemRodo", "rodoOS")]
        [InlineData(0, "SP", "SemRodo", "rodoOS")]
        [InlineData(3, "PR", "SemRodo", "rodoOS")]
        [InlineData(3, "SP", "SemRodo", "rodoOS")]
        public void ConstruirAutorizacaoDeveRecusarGrupoAusenteAntesDePrepararEnvio(
            int tpCTe, string ufFim, string estrutura, string grupo)
        {
            var documento = new XmlDocument { XmlResolver = null };
            documento.LoadXml(CriarXml(tpCTe, "6", "01", "PR", ufFim, estrutura));
            var cteOS = Unimake.Business.DFe.Utility.XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTeOS.CTeOS>(documento);
            cteOS.InfCTe.Emit = new Unimake.Business.DFe.Xml.CTeOS.Emit { CNPJ = "99999999000191" };
            cteOS.InfCTe.Ide.CUF = UFBrasil.PR;
            cteOS.InfCTe.Ide.DhEmi = new DateTimeOffset(2026, 11, 11, 12, 0, 0, TimeSpan.FromHours(-3));
            cteOS.InfCTe.Ide.Serie = 59;
            cteOS.InfCTe.Ide.NCT = 50;
            cteOS.InfCTe.Ide.TpEmis = TipoEmissao.Normal;
            cteOS.InfCTe.Ide.TpImp = (FormatoImpressaoDACTE)1;
            cteOS.InfCTe.Ide.TpAmb = TipoAmbiente.Homologacao;
            cteOS.InfCTe.Ide.UFEnv = UFBrasil.PR;
            cteOS.InfCTe.Ide.IndIEToma = (IndicadorIEDestinatario)1;
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                TipoAmbiente = TipoAmbiente.Homologacao,
                HasProxy = false
            };

            // O erro de conteúdo deve anteceder a preparação fiscal; não há certificado nem chamada Executar.
            var erro = Assert.Throws<ValidatorDFeException>(() =>
                new Unimake.Business.DFe.Servicos.CTeOS.Autorizacao(cteOS, configuracao));
            Assert.Contains("<" + grupo + ">", erro.Message);
            Assert.Contains("obrigatório", erro.Message);
            Assert.False(configuracao.Definida);
        }

        private static string CriarXml(int tpCTe, string tpServ, string modal, string ufIni, string ufFim, string estrutura)
        {
            // Massa mínima para isolar a regra de conteúdo; não representa documento completo de autorização.
            XNamespace ns = "http://www.portalfiscal.inf.br/cte";
            var ide = new XElement(ns + "ide",
                new XElement(ns + "mod", "67"), new XElement(ns + "tpCTe", tpCTe),
                new XElement(ns + "tpServ", tpServ), new XElement(ns + "modal", modal),
                new XElement(ns + "UFIni", ufIni), new XElement(ns + "UFFim", ufFim));
            var inf = new XElement(ns + "infCte", new XAttribute("versao", "4.00"), ide);
            if (estrutura != "SemNorm")
            {
                var normal = new XElement(ns + "infCTeNorm");
                inf.Add(normal);
                if (estrutura != "SemModal")
                {
                    var infModal = new XElement(ns + "infModal", new XAttribute("versaoModal", "4.00"));
                    normal.Add(infModal);
                    if (estrutura != "SemRodo")
                    {
                        var rodo = new XElement(ns + "rodoOS");
                        infModal.Add(rodo);
                        var obrigatorio = ufIni == ufFim ? "NroRegEstadual" : "TAF";
                        var proibido = ufIni == ufFim ? "TAF" : "NroRegEstadual";
                        if (estrutura != "ObrigatorioVazio")
                            rodo.Add(new XElement(ns + obrigatorio, "189439"));
                        if (estrutura == "ProibidoPresente")
                            rodo.Add(new XElement(ns + proibido, "189439"));
                    }
                }
            }
            return new XElement(ns + "CTeOS", inf).ToString(SaveOptions.DisableFormatting);
        }
    }
}
