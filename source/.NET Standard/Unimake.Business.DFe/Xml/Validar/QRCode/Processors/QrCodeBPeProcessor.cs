using System;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.QRCode.Processors
{
    internal class QrCodeBPeProcessor : IQrCodeProcessor
    {
        public void GerarQrCode(XmlDocument xml, Configuracao configuracao)
        {
            var tagIdentificadora = DefinirTagIdentificadora(xml);

            switch (tagIdentificadora)
            {
                case "BPeTM":
                    break;

                case "BPeTA":
                    QrCodeXmlHelper.MontarQrCodeBPeTA(xml, configuracao);
                    break;

                case "BPe":
                    QrCodeXmlHelper.MontarQrCodeBPe(xml, configuracao);
                    break;
            }
        }

        private string DefinirTagIdentificadora(XmlDocument xml)
        {
            if (xml.GetElementsByTagName("BPeTM").Count > 0)
            {
                return "BPeTM";
            }

            if (xml.GetElementsByTagName("BPeTA").Count > 0)
            {
                return "BPeTA";
            }

            if (xml.GetElementsByTagName("BPe").Count > 0)
            {
                return "BPe";
            }

            throw new Exception("Não foi possível identificar a tag principal do BPe no XML para formar o QR code.");
        }
    }
}
