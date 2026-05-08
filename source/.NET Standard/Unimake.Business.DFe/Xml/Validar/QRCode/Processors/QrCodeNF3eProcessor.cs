using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.QRCode.Processors
{
    internal class QrCodeNF3eProcessor : IQrCodeProcessor
    {
        public void GerarQrCode(XmlDocument xml, Configuracao configuracao) 
        {
            QrCodeXmlHelper.MontarQrCodeNF3e(xml, configuracao);
        }
    }
}
