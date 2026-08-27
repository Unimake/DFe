#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta
{
    /// <summary>
    /// Consulta de eventos da NFSe por chave de acesso (Padrão NACIONAL).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("consultaEventosNFSeChaveAcesso")]
    public class ConsultaEventosNFSeChaveAcesso : XMLBase
    {
        private string ChaveNFSeField;

        /// <summary>
        /// Chave de acesso da NFSe para consulta de eventos.
        /// </summary>
        [XmlElement("ChaveNFSe")]
        public string ChaveNFSe 
        {             
            get { return ChaveNFSeField; }
            set 
            { 
                if (value.Length != 50)
                {
                    throw new ArgumentException("A Chave de Acesso da NFSe deve conter 50 caracteres.");
                }

                ChaveNFSeField = value;
            }
        }
    }
}
