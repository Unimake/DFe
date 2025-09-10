using System;
using System.IO;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Validator;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    public class NFeValidatorTest
    {
        /// <summary>
        /// Testa o XML da NFe com tags da RTC com CSTs 000, 550, 200, 410, 510, 620 e 800
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST000_CST550.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST200_CST410.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST510_CST620.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFe_CST800.xml")]
        public void ValidarNFeComIBSCBS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            Assert.True(ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());
        }

        /// <summary>
        /// Testa o XML da NFCe com tags da RTC com CSTs que não devem ser utilizadas para esse modelo: 550 e 800
        /// </summary>
        /// <param name="arqXML">Caminho do arquivo XML que será testado</param>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFCe_CST550.xml")]
        [InlineData(@"..\..\..\NFe\Resources\RTC\NFCe_CST800.xml")]
        public void ValidarNFCeComIBSCBS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da validação.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var excecao = Assert.Throws<ValidatorDFeException>(() => ValidatorFactory.BuidValidator(doc.InnerXml)?.Validate());

            Assert.Contains($"Para o modelo {ModeloDFe.NFCe.ToString()}, o CST", excecao.Message);
        }

        /// <summary>
        /// Testa o XML da NFe com CST 020 no ICMS, que pode ou não ter a base legal informada. Lançar apenas um aviso, não interromper.
        /// </summary>
        /// <param name="arqXml"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST20_sem_base_legal.xml")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST20_com_base_legal.xml")]
        public void ValidarNFeComCST20(string arqXml)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator
            {
                Xml = doc.InnerXml
            };

            // Valida (não deve lançar exceção nos dois cenários)
            Assert.True(validator!.Validate());

            // Warnings é protegido em XmlValidatorBase → usar reflection pra ler
            var prop = validator.GetType().GetProperty("Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (arqXml.Contains("sem_base_legal", StringComparison.OrdinalIgnoreCase))
            {
                Assert.Single(warnings);
                Assert.StartsWith("Nota Fiscal possui produto utilizando CST 020", warnings[0].Message);
            }
            else // com_base_legal
            {
                Assert.Empty(warnings);
            }
        }

        /// <summary>
        /// Testa o XML da NFe com CST 040 no ICMS, que pode ou não ter a base legal informada. Lançar apenas um aviso, não interromper.
        /// </summary>
        /// <param name="arqXml"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST40_sem_base_legal.xml")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST40_com_base_legal.xml")]
        public void ValidarNFeComCST40(string arqXml)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator
            {
                Xml = doc.InnerXml
            };

            // Valida (não deve lançar exceção nos dois cenários)
            Assert.True(validator!.Validate());

            // Warnings é protegido em XmlValidatorBase → usar reflection pra ler
            var prop = validator.GetType().GetProperty("Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (arqXml.Contains("sem_base_legal", StringComparison.OrdinalIgnoreCase))
            {
                Assert.Single(warnings);
                Assert.StartsWith("Nota Fiscal possui produto utilizando CST 040", warnings[0].Message);
            }
            else // com_base_legal
            {
                Assert.Empty(warnings);
            }
        }

        /// <summary>
        /// Testa o XML da NFe com CST 051 no ICMS, que pode ou não ter a base legal informada. Lançar apenas um aviso, não interromper.
        /// </summary>
        /// <param name="arqXml"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST51_sem_base_legal.xml")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CST51_com_base_legal.xml")]
        public void ValidarNFeComCST51(string arqXml)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator
            {
                Xml = doc.InnerXml
            };

            // Valida (não deve lançar exceção nos dois cenários)
            Assert.True(validator!.Validate());

            // Warnings é protegido em XmlValidatorBase → usar reflection pra ler
            var prop = validator.GetType().GetProperty("Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (arqXml.Contains("sem_base_legal", StringComparison.OrdinalIgnoreCase))
            {
                Assert.Single(warnings);
                Assert.StartsWith("Nota Fiscal possui produto utilizando CST 051", warnings[0].Message);
            }
            else // com_base_legal
            {
                Assert.Empty(warnings);
            }
        }

        /// <summary>
        /// Testa o XML da NFe com CFOP 6.101 e CST 00 ou 10 no ICMS. Deve lançar um aviso (não-impeditivo) apenas se for CST 00.
        /// </summary>
        /// <param name="arqXml"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP6101_CST00.xml")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP6101_CST10.xml")]
        public void ValidarNfe_CFOP6101_CST00(string arqXml)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator
            {
                Xml = doc.InnerXml
            };

            // validação deve passar (aviso é não-impeditivo)
            Assert.True(validator!.Validate());

            // Warnings é protegido em XmlValidatorBase → reflection
            var prop = validator.GetType().GetProperty(
                "Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic
            );
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            var deveAvisar = arqXml.Contains("CST00", StringComparison.OrdinalIgnoreCase);

            if (deveAvisar)
            {
                Assert.Single(warnings);
                Assert.Contains("CFOP 6.101", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
                Assert.Contains("CST 00", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
            }
            else
            {
                Assert.Empty(warnings);
            }

        }

        /// <summary>
        /// Testa o XML da NFe com CFOP 6.102 e CST 10 no ICMS. Deve lançar um aviso (não-impeditivo) apenas se a operação for de "CFe com entrega em outro estado" (venda interestadual com entrega em outro estado, CFOP 6.102).
        /// </summary>
        /// <param name="arqXml"></param>
        /// <param name="deveAvisar"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP6102_CST10_CFInterestadual.xml", true)]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP6102_CST10_VendaNormal.xml", false)]
        public void ValidarNfe_CFOP6102_CST10(string arqXml, bool deveAvisar)
        {
            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator
            {
                Xml = doc.InnerXml
            };

            // validação deve passar (aviso é não-impeditivo)
            Assert.True(validator!.Validate());

            // Warnings é protegido em XmlValidatorBase → reflection
            var prop = validator.GetType().GetProperty(
                "Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic
            );
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (deveAvisar)
            {
                Assert.Single(warnings);
                Assert.Contains("CFOP 6.102", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
                Assert.Contains("CST 10", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
            }
            else
            {
                Assert.Empty(warnings);
            }
        }

        /// <summary>
        /// Testa o XML da NFe com CFOP 5.102 e CST 40 no ICMS. Deve lançar um aviso (não-impeditivo)
        /// </summary>
        /// <param name="arqXml"></param>
        /// <param name="deveAvisar"></param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP5102_CST40.xml", true)]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP5102_CST00.xml", false)]
        public void ValidarNFe_CFOP5102_CST40(string arqXml, bool deveAvisar)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator { Xml = doc.InnerXml };

            // aviso não impede
            Assert.True(validator.Validate());

            // ler Warnings (reflection)
            var prop = validator.GetType().GetProperty("Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (deveAvisar)
            {
                Assert.Single(warnings);
                Assert.Contains("CFOP 5.102", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
                Assert.Contains("CST 40", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
            }
            else
            {
                Assert.Empty(warnings);
            }
        }

        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP5949_CST00.xml", true)]
        [InlineData(@"..\..\..\NFe\Resources\Warnings\NFe_CFOP5949_CST10.xml", false)]
        public void ValidarNFe_CFOP5949_CST00(string arqXml, bool deveAvisar)
        {
            Assert.True(File.Exists(arqXml), "Arquivo " + arqXml + " não foi localizado para a validação.");

            var doc = new XmlDocument();
            doc.Load(arqXml);

            var validator = new Unimake.Business.DFe.Validator.NFe.NFeValidator { Xml = doc.InnerXml };

            // aviso não impede
            Assert.True(validator.Validate());

            // ler Warnings (reflection)
            var prop = validator.GetType().GetProperty("Warnings",
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            var getter = prop!.GetGetMethod(nonPublic: true);
            var warnings = (System.Collections.Generic.List<ValidatorDFeException>)getter!.Invoke(validator, null)!;

            if (deveAvisar)
            {
                Assert.Single(warnings);
                Assert.Contains("CFOP 5.949", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
                Assert.Contains("CST 00", warnings[0].Message, StringComparison.OrdinalIgnoreCase);
            }
            else
            {
                Assert.Empty(warnings);
            }
        }

    }
}
