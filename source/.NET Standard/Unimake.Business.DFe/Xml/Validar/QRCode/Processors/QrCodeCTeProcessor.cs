using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.QRCode.Processors
{
    internal class QrCodeCTeProcessor: IQrCodeProcessor
    {
        public void GerarQrCode(XmlDocument xml, Configuracao configuracao) 
        {
            var tagIdentificadora = DefinirTagIdentificadora(xml);

            switch (tagIdentificadora) 
            {
                case "CTeOS":
                    QrCodeXmlHelper.MontarQrCodeCTeOS(xml, configuracao);
                    break;

                case "CTeSimp":
                    QrCodeXmlHelper.MontarQrCodeCTeSimp(xml, configuracao);
                    break;

                case "CTe":
                    QrCodeXmlHelper.MontarQrCodeCTe(xml, configuracao);     
                    break;

            }

        }



        private string DefinirTagIdentificadora(XmlDocument xml) 
        {
            if (xml.GetElementsByTagName("CTeOS").Count > 0) 
                 return "CTeOS";

            if (xml.GetElementsByTagName("CTeSimp").Count > 0)
                return "CTeSimp";

            if (xml.GetElementsByTagName("CTe").Count > 0)
                return "CTe";

            throw new Exception("Não foi possível identificar a tag principal do CTe no XML para formar o QR code.");
        }
    }
}
