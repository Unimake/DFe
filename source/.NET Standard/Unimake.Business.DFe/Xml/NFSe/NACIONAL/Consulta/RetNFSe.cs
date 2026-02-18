using System;
using System.Collections.Generic;
using System.Text;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta
{
    /// <summary>
    /// Retorno da Consulta de NFSe - Padrão NACIONAL
    /// Contém a NFSe completa em caso de sucesso
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.RetNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFSe")]
    public class RetNFSe : NFSe
    {
        // Herda toda a estrutura da NFSe
        // Pode adicionar propriedades específicas de retorno se necessário
    }
}
