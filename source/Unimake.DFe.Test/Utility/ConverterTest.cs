using System;
using System.Security.Cryptography;
using System.Text;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility
{
    public class ConverterTest
    {
        [Fact]
        [Trait("Utility", "Converter")]
        public void TestValidSHA1Hash()
        {
            var validHash = "2ef7bde608ce5404e97d5f042f95f89f1c232871";
            var isValid = Converter.IsSHA1Hash(validHash);

            Assert.True(isValid, "A string deveria ser um hash SHA-1 válido.");
        }

        [Fact]
        [Trait("Utility", "Converter")]
        public void TestInvalidSHA1Hash()
        {
            var invalidHash = "invalidhash12345";
            var isValid = Converter.IsSHA1Hash(invalidHash);
            Assert.False(isValid, "A string não deveria ser um hash SHA-1 válido.");
        }

        [Fact]
        [Trait("Utility", "Converter")]
        public void TestValidSHA1Base64()
        {
            var validBase64 = "RX2tvfSSZWhNsjSwmWQOzZM71hI=";
            var isValid = Converter.IsSHA1Base64(validBase64);
            Assert.True(isValid, "A string deveria ser um hash SHA-1 com padrão Base64 válido.");
        }

        [Fact]
        [Trait("Utility", "Converter")]
        public void TestInvalidSHA1Base64()
        {
            var invalidBase64 = "invalidbase64";
            var isValid = Converter.IsSHA1Base64(invalidBase64);
            Assert.False(isValid, "A string não deveria ser um hash SHA-1 com padrão Base64 válido.");
        }

        [Fact]
        [Trait("Utility", "Converter")]
        public void TestCalculateSHA1Hash()
        {
            var input = "example_input";
            var expectedHash = CalculateExpectedSHA1Hash(input);

            var calculatedHash = Converter.CalculateSHA1Hash(input);

            Assert.Equal(expectedHash, calculatedHash);
        }

        private string CalculateExpectedSHA1Hash(string input)
        {
            using (var sha1 = SHA1.Create())
            {
                var inputBytes = Encoding.UTF8.GetBytes(input);
                var hashBytes = sha1.ComputeHash(inputBytes);

                return Convert.ToBase64String(hashBytes);
            }
        }
    }
}
