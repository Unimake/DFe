using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.BugFixes
{
    public class Bug140668
    {
        #region Public Methods

        [Fact]
        [Trait("Bug", "140668")]
        public void SetXCplToNullThrowsArgumentNullException()
        {
            var enderEmit = new EnderEmit
            {
                XLgr = "lorem ipsum dolor",
                Nro = "123",
                XCpl = null,
                XBairro = "lorem ipsum",
                CMun = 9999999,
                XMun = "lorem",
                UF = Business.DFe.Servicos.UFBrasil.PR,
                CEP = "00000-000",
                CPais = 1058,
                XPais = "BRASIL"
            };

            Assert.Null(enderEmit.XCpl);
        }

        #endregion Public Methods
    }
}