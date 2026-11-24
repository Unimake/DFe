using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Solicitação de autenticação da AACBS. As credenciais pertencem à configuração do serviço.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.Autenticar")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "AutenticarTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("autenticar", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class Autenticar : AACBSBase
    {
    }
}
