using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.QRCode.Processors
{
    internal class QrCodeNFComProcessor : IQrCodeProcessor
    {
        public void GerarQrCode(XmlDocument xml, Configuracao configuracao) 
        {
            QrCodeXmlHelper.MontarQrCodeNFCom(xml, configuracao);
        }
    }
}
