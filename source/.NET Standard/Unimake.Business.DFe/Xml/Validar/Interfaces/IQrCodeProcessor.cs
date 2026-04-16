using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using static Unimake.Business.DFe.ValidarEstruturaXML;

namespace Unimake.Business.DFe.Xml.Validar.Interfaces
{

    /// <summary>
    /// Interface que define o contrato para geração de Qr code de cada tipo de DFe.
    /// </summary>
    internal interface IQrCodeProcessor
    {
        /// <summary>
        /// Gerar o Qr code para o tipo de DFe, utilizando as informações do XML e a configuração do serviço. 
        /// </summary>
        /// <param name="xml"></param>
        /// <param name="configuracao"></param>
        void GerarQrCode(XmlDocument xml, Configuracao configuracao);
    }
}
