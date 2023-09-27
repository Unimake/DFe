#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using System;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta/Download do PDF da NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePDF")]
    [ComVisible(true)]
#endif
    public class ConsultarNfsePDF : ConsultarNfse
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarNfsePDF() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarNfsePDF(XmlDocument conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao)
        { }

        /// <summary>
        /// Extrair o PDF do retorno obtido em Base64
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o PDF</param>
        /// <param name="nomePDF">Nome do arquivo PDF a ser gravado</param>
        /// <param name="nomeTagPDF">Nome da tag que tem o conteúdo do PDF em BASE64</param>
        public void ExtrairPDF(string pasta, string nomePDF, string nomeTagPDF)
        {
            try
            {
                //Esta comparação está sendo utilizada apenas para o padrão NACIONAL. É o único com comunicação por API. RetornoStream só está sendo preenchido na classe "TratarRetorno" no consumo por API.
                if (RetornoStream != null)
                {
                    using (FileStream fs = new FileStream(Path.Combine (pasta, nomePDF), FileMode.Create))
                    {
                        RetornoStream.CopyTo(fs);
                        fs.Close();
                    }
                    return;
                }

                if (RetornoWSXML.GetElementsByTagName(nomeTagPDF)[0] != null)
                {
                    Converter.Base64ToPDF(RetornoWSXML.GetElementsByTagName("Base64Pdf")[0].InnerText, Path.Combine(pasta, nomePDF));
                }
                else
                {
                    throw new Exception("Não foi possível localizar a TAG com o conteúdo do PDF no XML retornado pela prefeitura.");
                }
            }
            catch (Exception ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}