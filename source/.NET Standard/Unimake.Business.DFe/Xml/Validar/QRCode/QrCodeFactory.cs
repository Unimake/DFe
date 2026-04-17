using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.Validar.Interfaces;
using Unimake.Business.DFe.Xml.Validar.QRCode.Processors;
using static Unimake.Business.DFe.ValidarEstruturaXML;


namespace Unimake.Business.DFe.Xml.Validar.QRCode
{
    internal class QrCodeFactory
    {
        public static IQrCodeProcessor Criar(Configuracao configuracao, bool gerarQrCode, TipoDFe tipoDFe)
        {
            switch (tipoDFe)
            {
                case TipoDFe.NFCe:
                    if (gerarQrCode)
                    {
                        return new QrCodeNFCeProcessor();
                    }
                    return null;

                case TipoDFe.MDFe:
                    if (gerarQrCode)
                    {
                        return new QrCodeMDFeProcessor();
                    }
                    return null;

                case TipoDFe.CTe:
                    if (gerarQrCode) 
                    {
                        return new QrCodeCTeProcessor();
                    }
                    return null;
                case TipoDFe.NF3e:
                    if (gerarQrCode) 
                    {
                        return new QrCodeCTeProcessor();
                    }
                    return null;
                case TipoDFe.NFCom:
                    if (gerarQrCode) 
                    {
                        return new QrCodeCTeProcessor();
                    }
                    return null;

                default:
                    return null;
            }

        }

    }
}
