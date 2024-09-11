using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;

namespace Treinamento
{
    public partial class Form1 : Form
    {
        public Form1() => InitializeComponent();

        private void btnMDFeSincrono_Click(object sender, EventArgs e)
        {
            MessageBox.Show(AppContext.TargetFrameworkName);

            var certificadoDigital = (new Unimake.Business.Security.CertificadoDigital()).CarregarCertificadoDigitalA1(@"D:\projetos\UnimakePV.pfx", "12345678");

            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"C:\Users\Wandrey\Downloads\Telegram Desktop\31240901687693000103580010000001212000001211_MDFe.xml");

            var xml = XMLUtility.Deserializar<MDFe>(xmlDoc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = certificadoDigital
            };

            var autorizacao = new Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc(xml, configuracao);
            MessageBox.Show(autorizacao.ConteudoXMLAssinado.OuterXml);
        }
    }
}
