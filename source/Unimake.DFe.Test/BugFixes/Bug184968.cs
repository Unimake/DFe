using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.BugFixes
{
    public class Bug184968
    {
        [Theory]
        [InlineData(@"..\..\..\BugFixes\XML\184968ComplementoCTe.xml")]
        [InlineData(@"..\..\..\BugFixes\XML\184968CTe_evento.xml")]
        [InlineData(@"..\..\..\BugFixes\XML\184968CTeModal.xml")]
        public void ValidarXMLCTe(string xml) 
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(xml);

            var config = new Unimake.Business.DFe.Servicos.Configuracao
            {
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = 35,
                PadraoNFSe = PadraoNFSe.None,
                CertificadoArquivo = @"C:\Projetos\Unimake_PV.pfx",
                CertificadoSenha = "12345678"
            };


            var validar = new ValidarEstruturaXML();
            var result = validar.ValidarServico(xmlDoc, config);

            if (!result.Validado) 
            {
                throw new Exception($"Validação falhou: {result.MensagemRetorno}");
            }
        }
        

    }
}
