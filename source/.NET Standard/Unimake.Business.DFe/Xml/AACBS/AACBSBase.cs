#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>Base dos documentos XML de apuração assistida da CBS.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.AACBSBase")]
    [ComVisible(true)]
#endif
    public abstract class AACBSBase : XMLBase
    {
#if INTEROP
        /// <summary>Serializa em texto XML preservando retornos de carro nos dados AACBS.</summary>
        /// <returns>Texto XML sem normalização das quebras de linha dos campos.</returns>
        public override string GerarXMLString() => Utility.XMLUtility.ObterTextoXmlAACBS(GerarXML());
#endif

        /// <summary>Versão do contrato XML Unimake, independente da versão HTTP.</summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";
    }

    /// <summary>Base dos resultados de serviço, com alternativa local sem resposta remota.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetornoAACBSBase")]
    [ComVisible(true)]
#endif
    public abstract class RetornoAACBSBase : AACBSBase
    {
        /// <summary>Erro local exclusivo; não integra o contrato JSON da Receita.</summary>
        [XmlElement("erroLocal")]
        public ErroLocal ErroLocal { get; set; }
    }

    /// <summary>Ausência de retorno da operação, sem atribuir código à Receita.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ErroLocal")]
    [ComVisible(true)]
#endif
    [XmlType(Namespace = "http://www.unimake.com.br/dfe/aacbs")]
    public class ErroLocal
    {
        /// <summary>Código local SEM_RETORNO.</summary>
        [XmlElement("codigo")]
        public string Codigo { get; set; } = "SEM_RETORNO";

        /// <summary>Descrição local para o consumidor.</summary>
        [XmlElement("mensagem")]
        public string Mensagem { get; set; } = "Não há retorno disponível para este serviço.";
    }
}
