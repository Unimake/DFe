﻿#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Validator;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Utilitários diversos para trabalhar com XML
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public static class XMLUtility
    {
        #region Public Structs

        /// <summary>
        /// Estrutura para recuperar o conteúdo separadamente da chave do DFe (NFe, CTe, NFCe, MDfe, etc...)
        /// </summary>
#if INTEROP
        public class ConteudoChaveDFe
#else
        public struct ConteudoChaveDFe
#endif
        {
            #region Public Properties

            /// <summary>
            /// Ano de emissão do documento fiscal
            /// </summary>
            public string AnoEmissao { get; set; }

            /// <summary>
            /// CNPJ do emissor do documento fiscal
            /// </summary>
            [Obsolete("Agora utilize a propriedade CNPJCPFEmissor. Em futuras versões esta propriedade será excluída.")]
            public string CNPJEmissor
            {
                get => CNPJCPFEmissor;
                set => CNPJCPFEmissor = value;
            }

            /// <summary>
            /// CNPJ ou CPF do emissor do documento fiscal
            /// </summary>
            public string CNPJCPFEmissor { get; set; }

            /// <summary>
            /// Código numérico do documento fiscal
            /// </summary>
            public string CodigoNumerico { get; set; }

            /// <summary>
            /// Digito verificador da chave do documento fiscal
            /// </summary>
            public int DigitoVerificador { get; set; }

            /// <summary>
            /// Mês de emissão do documento fiscal
            /// </summary>
            public string MesEmissao { get; set; }

            /// <summary>
            /// Modelo do documento fiscal
            /// </summary>
            public ModeloDFe Modelo { get; set; }

            /// <summary>
            /// Número do documento fiscal
            /// </summary>
            public int NumeroDoctoFiscal { get; set; }

            /// <summary>
            /// Série do documento fiscal
            /// </summary>
            public int Serie { get; set; }

            /// <summary>
            /// Tipo de emissão do documento fiscal
            /// </summary>
            public TipoEmissao TipoEmissao { get; set; }

            /// <summary>
            /// UF do emissor do documento fiscal
            /// </summary>
            public UFBrasil UFEmissor { get; set; }

            /// <summary>
            /// Site do Autorizador que recepcionou a NF3e 
            /// </summary>
            public string NSiteAutoriz { get; set; }

            #endregion Public Properties
        }

        #endregion Public Structs

        #region Public Classes

        /// <summary>
        /// Tipo Namespace
        /// </summary>
        public class TNameSpace
        {
            #region Public Properties

            /// <summary>
            /// Conteúdo do Namespace
            /// </summary>
            public string NS { get; set; }

            /// <summary>
            /// Prefixo do Namespace
            /// </summary>
            public string Prefix { get; set; }

            #endregion Public Properties
        }

        /// <summary>
        /// Implementa um StringWriter para gravar informações em uma cadeia de caracteres. As informações são armazenadas em um StringBuilder subjacente.
        /// </summary>
        public class Utf8StringWriter : StringWriter
        {
            #region Public Properties

            /// <summary>
            /// Sobrescrever o Encoding para deixar como padrão o UTF8
            /// </summary>
            public override Encoding Encoding => Encoding.UTF8;

            #endregion Public Properties
        }

        #endregion Public Classes

        #region Private Methods

        private static bool Validate(string xml) => ValidatorFactory.BuidValidator(xml)?.Validate() ?? true;

        #endregion

        #region Public Methods

        /// <summary>
        /// Gerar o dígito da chave da NFe, CTe, MDFe ou NFCe
        /// </summary>
        /// <param name="chave">Chave do DFe (sem o dígito) que deve ser calculado o dígito verificador.</param>
        /// <returns>Dígito verificador</returns>
        public static int CalcularDVChave(string chave)
        {
            if(chave is null)
            {
                throw new ArgumentNullException(nameof(chave));
            }

            int i, j, Digito;
            const string PESO = "4329876543298765432987654329876543298765432";

            chave = chave.Replace("NFe", "").Replace("CTe", "").Replace("MDFe", "");

            if(chave.Length != 43)
            {
                throw new Exception(string.Format("Erro na composição da chave [{0}] para obter o dígito verificador.", chave) + Environment.NewLine);
            }
            else
            {
                j = 0;
                try
                {
                    for(i = 0; i < 43; ++i)
                    {
                        j += Convert.ToInt32(chave.Substring(i, 1)) * Convert.ToInt32(PESO.Substring(i, 1));
                    }

                    Digito = 11 - (j % 11);
                    if((j % 11) < 2)
                    {
                        Digito = 0;
                    }
                }
                catch
                {
                    Digito = -1;
                }

                return Digito == -1
                    ? throw new Exception(string.Format("Erro no cálculo do dígito verificador da chave [{0}].", chave) + Environment.NewLine)
                    : Digito;
            }
        }

        /// <summary>
        /// Executa uma verificação simples para garantir que a chave do DFe (NFe, CTe, MDfe, NFCe, CTeOS) é valida, se tiver erros retorna exceção.
        /// </summary>
        /// <param name="chave">Chave do DFe a ser verificada</param>
        /// <example>
        /// try
        /// {
        ///     XMLUtility.ChecarChaveDFe("41201280568835000181570010000004841004185096");
        /// }
        /// catch(Exception ex)
        /// {
        ///     //Se chave tiver algum erro, vai retornar uma exceção.
        ///     MessageBox.Show(ex.Message);
        /// }
        /// </example>
        public static void ChecarChaveDFe(string chave)
        {
            #region Verificar o tamanho da chave

            if(chave.Length != 44)
            {
                throw new Exception("Tamanho da chave do documento fiscal eletrônico está diferente de 44 dígitos. Chave deve ter exatamente 44 dígitos.");
            }

            #endregion Verificar o tamanho da chave

            #region Verificar se o mês da emissão da nota da chave é válida

            var mes = Convert.ToInt32(chave.Substring(4, 2));

            if(mes < 1 || mes > 12)
            {
                throw new Exception("Mês da data de emissão, do documento fiscal eletrônico, que compõe a chave, está incorreto. Mês informado: " + mes.ToString() + ". Meses permitidos: 01 a 12.");
            }

            #endregion Verificar se o mês da emissão da nota da chave é válida

            #region Verificar se o modelo da chave é válido

            var modeloDFe = Convert.ToInt32(chave.Substring(20, 2));
            var tipoEnum = typeof(ModeloDFe);
            if(!Enum.IsDefined(tipoEnum, modeloDFe))
            {
                var modeloPermitido = string.Empty;

                foreach(var item in tipoEnum.GetFields())
                {
                    if(Attribute.GetCustomAttribute(item, typeof(XmlEnumAttribute)) is XmlEnumAttribute attribute)
                    {
                        if(!string.IsNullOrEmpty(modeloPermitido))
                        {
                            modeloPermitido += ", ";
                        }

                        modeloPermitido += attribute.Name;
                    }
                }

                throw new Exception("Modelo, do documento fiscal eletrônico, que compõe a chave, está incorreto. Modelo informado: " + modeloDFe.ToString() + ". Modelos permitidos: " + modeloPermitido + ".");
            }

            #endregion Verificar se o modelo da chave é válido

            #region Verificar se o tipo de emissão da chave é válido

            var tpEmis = Convert.ToInt32(chave.Substring(34, 1));
            tipoEnum = typeof(TipoEmissao);
            if(!Enum.IsDefined(tipoEnum, tpEmis))
            {
                var tipoPermitido = string.Empty;

                foreach(var item in tipoEnum.GetFields())
                {
                    if(Attribute.GetCustomAttribute(item, typeof(XmlEnumAttribute)) is XmlEnumAttribute attribute)
                    {
                        if(!string.IsNullOrEmpty(tipoPermitido))
                        {
                            tipoPermitido += ", ";
                        }

                        tipoPermitido += attribute.Name;
                    }
                }

                throw new Exception("Tipo de emissão, do documento fiscal eletrônico, que compõe a chave, está incorreto. Tipo informado: " + tpEmis.ToString() + ". Tipos permitidos: " + tipoPermitido + ".");
            }

            #endregion Verificar se o tipo de emissão da chave é válido

            #region Verificar se a UF da chave é válida

            var cUF = Convert.ToInt32(chave.Substring(0, 2));
            tipoEnum = typeof(UFBrasil);

            if(!Enum.IsDefined(tipoEnum, cUF) || cUF >= 90 || cUF == 0)
            {
                var cufPermitido = string.Empty;

                foreach(var item in Enum.GetValues(tipoEnum))
                {
                    var ufBrasil = (UFBrasil)Enum.Parse(typeof(UFBrasil), item.ToString());
                    var uf = (int)ufBrasil;

                    if(uf > 0 && uf < 90)
                    {
                        if(!string.IsNullOrEmpty(cufPermitido))
                        {
                            cufPermitido += ", ";
                        }

                        cufPermitido += uf.ToString();
                    }
                }

                throw new Exception("Código da UF, do documento fiscal eletrônico, que compõe a chave, está incorreto. Código informado: " + cUF.ToString() + ". Códigos permitidos: " + cufPermitido + ".");
            }

            #endregion Verificar se a UF da chave é válida

            #region Verificar se o dígito verificador está correto

            var digitoCalc = CalcularDVChave(chave.Substring(0, 43));
            var digitoInf = chave.Substring(43, 1);
            if(digitoCalc != Convert.ToInt32(digitoInf))
            {
                throw new Exception("Dígito verificador, do documento fiscal eletrônico, que compõe a chave, está incorreto. Dígito informado: " + digitoInf + ". Dígito calculado: " + digitoCalc.ToString() + ".");
            }

            #endregion Verificar se o dígito verificador está correto
        }

        /// <summary>
        /// Limpar espaços desnecessários da string, por exemplo: Espaços duplos no meio da sentença, espaços no inicio ou final da sentença.
        /// </summary>
        /// <returns>Retorna string sem os espaços desnecessários</returns>
        /// <example>
        /// var texto = " Eu    vou   ao    supermercado comprar alimentos.    ";
        /// texto = XMLUtility.LimparEspacoDesnecessario(texto);
        /// MessageBox.Show(texto); // Retorno será: "Eu vou ao supermercado comprar alimentos."
        /// </example>
        /// <seealso cref="UnescapeReservedCharacters(string)"/>
        public static string ClearExtraSpaces(string content)
        {
            if(string.IsNullOrWhiteSpace(content))
            {
                return content;
            }

            var regex = new Regex(@"\s{2,}");
            content = regex.Replace(content, " ");
            content = content.Trim();

            return content;
        }

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="xml">String do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
        public static T Deserializar<T>(string xml)
            where T : new()
        {
            if(!Validate(xml))
            {
                return default;
            }

            try
            {
                xml = TratarFalhaXML(xml);

                var result = XmlHelper.Deserialize<T>(xml);

                if(result is Contract.Serialization.IXmlSerializable serializable)
                {
                    var xmlDoc = new XmlDocument();
                    xmlDoc.LoadXml(xml);
                    serializable.ReadXml(xmlDoc);
                }

                return result;
            }
            catch(Exception ex)
            {
                if(ex.GetLastException() is XmlException)
                {
                    var exception = (XmlException)ex.GetLastException();

                    ImproveInvalidCharacterExceptionInXML(xml, exception);
                }
                else if(ex is InvalidOperationException)
                {
                    var conteudo = ExtrairLinhaColuna(ex.Message);

                    var linha = conteudo[0];
                    var coluna = conteudo[1];

                    if(coluna > 0)
                    {
                        var message = ExtrairParteXMLComFalha(xml, coluna, true);

                        throw new XmlException(message, ex.InnerException, linha, coluna);
                    }
                    else
                    {
                        ThrowHelper.Instance.Throw(new Exception(ex.GetAllMessages()));
                        throw; //Desnecessário, mas se eu tiro esta linha o compilador gera falha, mas dentro do ThrowHelper já tem este cara.
                    }
                }

                ThrowHelper.Instance.Throw(ex.GetLastException());
                throw; //Desnecessário, mas se eu tiro esta linha o compilador gera falha, mas dentro do ThrowHelper já tem este cara.
            }
        }

        /// <summary>
        /// Tratar erros no XML antes de desserializar para evitar erro na desserialização
        /// </summary>
        /// <param name="xml">String do XML que deve sofrer a correção</param>
        /// <returns>XML já corrigido</returns>
        private static string TratarFalhaXML(string xml) =>
            //Falta de namespace na tag principal gera erro. Caso abaixo é no XML de distribuição de eventos
            xml.Replace("<procEventoNFe>", "<procEventoNFe versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfe\">")
               .Replace("<procEventoNFe versao=\"1.00\">", "<procEventoNFe versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfe\">")
               .Replace("<NFe xmlns=\"\">", "<NFe xmlns=\"http://www.portalfiscal.inf.br/nfe\">")
               .Replace("<SignedInfo xmlns=\"\">", "<SignedInfo>")
               .Replace("<SignatureValue xmlns=\"\">", "<SignatureValue>")
               .Replace("<KeyInfo xmlns=\"\">", "<KeyInfo>")
               .Replace("<protNFe xmlns=\"\" versao=\"4.00\">", "<protNFe versao=\"4.00\" xmlns=\"http://www.portalfiscal.inf.br/nfe\">")
            ;

        /// <summary>
        /// Extrair a linha e coluna da string da exceção referente a problema de desserialização do XML
        /// </summary>
        /// <param name="input">mensagem de exceção para analisar e extrair o conteúdo</param>
        private static int[] ExtrairLinhaColuna(string input)
        {
            // Padrão regex para encontrar números entre parênteses
            var pattern = @"\((\d+), (\d+)\)";

            // Aplica o padrão regex à string de entrada
            var match = Regex.Match(input, pattern);

            var linha = 0;
            var coluna = 0;

            // Verifica se houve uma correspondência
            if(match.Success)
            {
                // Os números estarão nos grupos de captura 1 e 2
                linha = Convert.ToInt32(match.Groups[1].Value);
                coluna = Convert.ToInt32(match.Groups[2].Value) - 3; //Tenho que voltar 3 caracteres para pegar a tag correta, não consegui entender a logica ainda, mas funciona. Wandrey 29/12/2023
            }

            return new int[] { linha, coluna };
        }

        /// <summary>
        /// Melhorar a mensagem de exceção quando é um erro de caracteres inválidos no XML
        /// </summary>
        /// <param name="xml">Conteúdo do XML</param>
        /// <param name="ex">Exceção gerada inicialmente</param>
        public static void ImproveInvalidCharacterExceptionInXML(string xml, XmlException ex)
        {
            if(!string.IsNullOrWhiteSpace(xml))
            {
                if(ex.LinePosition > 0)
                {
                    try
                    {
                        var message = ExtrairParteXMLComFalha(xml, ex.LinePosition);

                        throw new XmlException(message, ex.InnerException, ex.LineNumber, ex.LinePosition);
                    }
                    catch(XmlException newException)
                    {
                        ThrowHelper.Instance.Throw(newException);
                    }
                }
            }
        }

        /// <summary>
        /// Extrair do XML a parte que gerou a exceção para retornar uma mensagem mais clara para o ERP.
        /// </summary>
        /// <param name="xml">Conteúdo do XML</param>
        /// <param name="linePosition">Posição da linha do XML que gerou a exceção</param>
        /// <param name="voltarUmaTag">Booleano para voltar uma tag</param>
        private static string ExtrairParteXMLComFalha(string xml, int linePosition, bool voltarUmaTag = false)
        {
            var message = string.Empty;

            if(!string.IsNullOrWhiteSpace(xml))
            {
                if(linePosition > 0)
                {
                    var positionStart = xml.LastIndexOf("<", linePosition);

                    if(voltarUmaTag)
                    {
                        positionStart = xml.LastIndexOf("<", positionStart - 1);
                    }

                    var positionFinal = xml.IndexOf(">", linePosition) - positionStart + 1;

                    message = "TAG com caracteres inválidos: " + xml.Substring(positionStart, positionFinal) + ".";
                }
            }

            return message;
        }

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="doc">Conteúdo do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
        public static T Deserializar<T>(XmlDocument doc)
        where T : new() => Deserializar<T>(doc.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de documento fiscal eletrônico do XML
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do documento eletrônico</returns>
        public static TipoDFe DetectDFeType(XmlDocument xml) => DetectDFeType(xml.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de documento fiscal eletrônico do XML
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do documento eletrônico</returns>
        public static TipoDFe DetectDFeType(string xml)
        {
            var tipoDFe = TipoDFe.Desconhecido;

            if(xml.Contains("<mod>55</mod>"))
            {
                tipoDFe = TipoDFe.NFe;
            }
            else if(xml.Contains("<mod>65</mod>"))
            {
                tipoDFe = TipoDFe.NFCe;
            }
            else if(xml.Contains("<mod>57</mod>"))
            {
                tipoDFe = TipoDFe.CTe;
            }
            else if(xml.Contains("<mod>67</mod>"))
            {
                tipoDFe = TipoDFe.CTeOS;
            }
            else if(xml.Contains("infMDFe"))
            {
                tipoDFe = TipoDFe.MDFe;
            }
            else if(xml.Contains("infCFe"))
            {
                tipoDFe = TipoDFe.CFe;
            }
            else if (xml.Contains("<mod>66</mod>"))
            {
                tipoDFe = TipoDFe.NF3e;
            }

            return tipoDFe;
        }

        /// <summary>
        /// Retorna o tipo de documento fiscal com base no XML de evento.
        /// </summary>
        /// <param name="xml">XML válido de evento.</param>
        /// <returns></returns>
        public static TipoDFe DetectEventByDFeType(string xml)
        {
            var tipoDFe = TipoDFe.Desconhecido;
            var tagId = "<infEvento Id=\"ID";

            if(!xml.Contains(tagId))
            {
                return tipoDFe;
            }

            var pos = xml.IndexOf(tagId);
            pos += tagId.Length + 26;
            var modelo = xml.Substring(pos, 2);

            switch(modelo)
            {
                case "55":
                    tipoDFe = TipoDFe.NFe;
                    break;

                case "57":
                    tipoDFe = TipoDFe.CTe;
                    break;

                case "58":
                    tipoDFe = TipoDFe.MDFe;
                    break;

                case "65":
                    tipoDFe = TipoDFe.NFCe;
                    break;

                case "66":
                    tipoDFe = TipoDFe.NF3e;
                    break;

                case "67":
                    tipoDFe = TipoDFe.CTeOS;
                    break;
            }

            return tipoDFe;
        }

        /// <summary>
        /// Detectar qual o tipo de evento do CT-e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do CT-e</returns>
        public static TipoEventoCTe DetectEventoCTeType(XmlDocument xml) => DetectEventoCTeType(xml.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de evento do CT-e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do CT-e</returns>
        public static TipoEventoCTe DetectEventoCTeType(string xml)
        {
            var tipoEventoCTe = TipoEventoCTe.Desconhecido;

            if(DetectEventByDFeType(xml) == TipoDFe.Desconhecido)
            {
                return tipoEventoCTe;
            }

            if(xml.Contains("<tpEvento>110110</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.CartaCorrecao;
            }
            else if(xml.Contains("<tpEvento>110111</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.Cancelamento;
            }
            else if(xml.Contains("<tpEvento>110180</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.ComprovanteEntrega;
            }
            else if(xml.Contains("<tpEvento>110181</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.CancelamentoComprovanteEntrega;
            }
            else if(xml.Contains("<tpEvento>610110</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.PrestDesacordo;
            }
            else if(xml.Contains("<tpEvento>310620</tpEvento>"))
            {
                tipoEventoCTe = TipoEventoCTe.RegistroPassagem;
            }
            return tipoEventoCTe;
        }

        /// <summary>
        /// Detectar qual o tipo de evento do MDF-e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do MDF-e</returns>
        public static TipoEventoMDFe DetectEventoMDFeType(XmlDocument xml) => DetectEventoMDFeType(xml.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de evento do MDF-e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do MDF-e</returns>
        public static TipoEventoMDFe DetectEventoMDFeType(string xml)
        {
            var tipoEventoMDFe = TipoEventoMDFe.Desconhecido;

            if(DetectEventByDFeType(xml) == TipoDFe.Desconhecido)
            {
                return tipoEventoMDFe;
            }

            if(xml.Contains("<tpEvento>110111</tpEvento>"))
            {
                tipoEventoMDFe = TipoEventoMDFe.Cancelamento;
            }
            else if(xml.Contains("<tpEvento>110112</tpEvento>"))
            {
                tipoEventoMDFe = TipoEventoMDFe.Encerramento;
            }
            else if(xml.Contains("<tpEvento>110114</tpEvento>"))
            {
                tipoEventoMDFe = TipoEventoMDFe.InclusaoCondutor;
            }
            else if(xml.Contains("<tpEvento>110115</tpEvento>"))
            {
                tipoEventoMDFe = TipoEventoMDFe.InclusaoDFe;
            }

            return tipoEventoMDFe;
        }

        /// <summary>
        /// Detectar qual o tipo de evento do documento fiscal eletrônico do XML
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do documento eletrônico</returns>
        public static TipoEventoNFe DetectEventoNFeType(XmlDocument xml) => DetectEventoNFeType(xml.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de evento do documento fiscal eletrônico do XML
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do documento eletrônico</returns>
        public static TipoEventoNFe DetectEventoNFeType(string xml)
        {
            var tipoEventoNFe = TipoEventoNFe.Desconhecido;

            if(DetectEventByDFeType(xml) == TipoDFe.Desconhecido)
            {
                return tipoEventoNFe;
            }

            if(xml.Contains("<tpEvento>110110</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.CartaCorrecao;
            }
            else if(xml.Contains("<tpEvento>110111</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.Cancelamento;
            }
            else if(xml.Contains("<tpEvento>110112</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.CancelamentoPorSubstituicao;
            }
            else if(xml.Contains("<tpEvento>110140</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.EPEC;
            }
            else if(xml.Contains("<tpEvento>111500</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.PedidoProrrogacaoPrazo1;
            }
            else if(xml.Contains("<tpEvento>111501</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.PedidoProrrogacaoPrazo2;
            }
            else if(xml.Contains("<tpEvento>111502</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo1;
            }
            else if(xml.Contains("<tpEvento>111503</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo2;
            }
            else if(xml.Contains("<tpEvento>210200</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.ManifestacaoConfirmacaoOperacao;
            }
            else if(xml.Contains("<tpEvento>210210</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.ManifestacaoCienciaOperacao;
            }
            else if(xml.Contains("<tpEvento>210220</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.ManifestacaoDesconhecimentoOperacao;
            }
            else if(xml.Contains("<tpEvento>210240</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.ManifestacaoOperacaoNaoRealizada;
            }
            else if(xml.Contains("<tpEvento>411500</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.RespostaPedidoProrrogacaoPrazo1;
            }
            else if(xml.Contains("<tpEvento>411501</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.RespostaPedidoProrrogacaoPrazo2;
            }
            else if(xml.Contains("<tpEvento>411502</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.RespostaCancelamentoPedidoProrrogacaoPrazo1;
            }
            else if(xml.Contains("<tpEvento>411503</tpEvento>"))
            {
                tipoEventoNFe = TipoEventoNFe.RespostaCancelamentoPedidoProrrogacaoPrazo2;
            }

            return tipoEventoNFe;
        }

        /// <summary>
        /// Detectar qual o tipo de evento do NF3e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do NF3e</returns>
        public static TipoEventoNF3e DetectEventoNF3eType(XmlDocument xml) => DetectEventoNF3eType(xml.OuterXml);

        /// <summary>
        /// Detectar qual o tipo de evento do NF3e.
        /// </summary>
        /// <param name="xml">XML a ser analisado</param>
        /// <returns>Retorna o tipo do evento do NF3e</returns>
        public static TipoEventoNF3e DetectEventoNF3eType(string xml)
        {
            var tipoEventoNF3e = TipoEventoNF3e.Desconhecido;

            if (DetectEventByDFeType(xml) == TipoDFe.Desconhecido)
            {
                return tipoEventoNF3e;
            }

            if (xml.Contains("<tpEvento>110111</tpEvento>"))
            {
                tipoEventoNF3e = TipoEventoNF3e.Cancelamento;
            }

            return tipoEventoNF3e;
        }

        /// <summary>
        /// De acordo com os dados do XML será detectado de qual tipo ele é: XML de NFe, CTe, Consulta Status, Consulta Situação, Evento, etc...
        /// </summary>
        public static TipoXML DetectXMLType(XmlDocument xmlDoc)
        {
            var primeiraTagFilha = xmlDoc.DocumentElement.ChildNodes[0].Name;
            var tipoXML = TipoXML.NaoIdentificado;

            switch(xmlDoc.DocumentElement.Name)
            {
                #region XML NFe

                case "consStatServ":
                    tipoXML = TipoXML.NFeStatusServico;
                    break;

                case "consSitNFe":
                    tipoXML = TipoXML.NFeConsultaSituacao;
                    break;

                case "consReciNFe":
                    tipoXML = TipoXML.NFeConsultaRecibo;
                    break;

                case "ConsCad":
                    tipoXML = TipoXML.NFeConsultaCadastro;
                    break;

                case "distDFeInt":
                    if(xmlDoc.GetElementsByTagName("distDFeInt")[0].NamespaceURI.ToLower().Contains("/nfe"))
                    {
                        tipoXML = TipoXML.NFeDistribuicaoDFe;
                    }
                    else if(xmlDoc.GetElementsByTagName("distDFeInt")[0].NamespaceURI.ToLower().Contains("/cte"))
                    {
                        tipoXML = TipoXML.CTeDistribuicaoDFe;
                    }
                    break;

                case "envEvento":
                    tipoXML = TipoXML.NFeEnvioEvento;
                    break;

                case "inutNFe":
                    tipoXML = TipoXML.NFeInutilizacao;
                    break;

                case "NFe":
                    tipoXML = TipoXML.NFe;
                    break;

                case "enviNFe":
                    tipoXML = TipoXML.NFeEnvioEmLote;
                    break;

                case "nfeProc":
                    tipoXML = TipoXML.NFeDistribuicao;
                    break;

                #endregion XML NFe

                #region XML CTe

                case "consStatServCte":
                    tipoXML = TipoXML.CTeStatusServico;
                    break;

                case "consSitCTe":
                    tipoXML = TipoXML.CTeConsultaSituacao;
                    break;

                case "consReciCTe":
                    tipoXML = TipoXML.CTeConsultaRecibo;
                    break;

                case "eventoCTe":
                    tipoXML = TipoXML.CTeEnvioEvento;
                    break;

                case "CTe":
                    tipoXML = TipoXML.CTe;
                    break;

                case "enviCTe":
                    tipoXML = TipoXML.CTeEnvioEmLote;
                    break;

                case "CTeOS":
                    tipoXML = TipoXML.CTeOS;
                    break;

                case "cteProc":
                    tipoXML = TipoXML.CTeDistribuicao;
                    break;

                case "CTeSimp":
                    tipoXML = TipoXML.CTeSimp;
                    break;

                #endregion XML CTe

                #region XML do MDFe

                case "consStatServMDFe":
                    tipoXML = TipoXML.MDFeStatusServico;
                    break;

                case "consSitMDFe":
                    tipoXML = TipoXML.MDFeConsultaSituacao;
                    break;

                case "consReciMDFe":
                    tipoXML = TipoXML.MDFeConsultaRecibo;
                    break;

                case "eventoMDFe":
                    tipoXML = TipoXML.MDFeEnvioEvento;
                    break;

                case "MDFe":
                    tipoXML = TipoXML.MDFe;
                    break;

                case "enviMDFe":
                    tipoXML = TipoXML.MDFeEnvioEmLote;
                    break;

                case "consMDFeNaoEnc":
                    tipoXML = TipoXML.MDFeConsultaNaoEncerrado;
                    break;

                case "mdfeProc":
                    tipoXML = TipoXML.MDFeDistribuicao;
                    break;

                #endregion XML do MDFe

                #region NF3e

                case "consStatServNF3e":
                    tipoXML = TipoXML.NF3eStatusServico;
                    break;

                case "consSitNF3e":
                    tipoXML = TipoXML.NF3eConsultaSituacao;
                    break;

                case "consReciNF3e":
                    tipoXML = TipoXML.NF3eConsultaRecibo;
                    break;

                case "eventoNF3e":
                    tipoXML = TipoXML.NF3eEnvioEvento;
                    break;

                case "NF3e":
                    tipoXML = TipoXML.NF3e;
                    break;

                #endregion NF3e

                #region XML da NFCom

                case "consStatServNFCom":
                    tipoXML = TipoXML.NFComStatusServico;
                    break;

                case "consSitNFCom":
                    tipoXML = TipoXML.NFComConsultaSituacao;
                    break;

                case "eventoNFCom":
                    tipoXML = TipoXML.NFComEnvioEvento;
                    break;

                case "NFCom":
                    tipoXML = TipoXML.NFCom;
                    break;

                #endregion XML da NFCom

                case "eSocial":
                    tipoXML = ObterTipoXmlESocial(xmlDoc, primeiraTagFilha);
                    break;

                case "Reinf":
                    tipoXML = ObterTipoXmlEFDReinf(primeiraTagFilha);
                    break;

                default:
                    break;
            }

            return tipoXML;
        }

        /// <summary>
        /// Obtém o tipo de XML de eSocial baseado no conteúdo do XML
        /// </summary>
        /// <param name="xmlDoc">XML com o conteúdo do XML a ser analisado</param>
        /// <param name="primeiraTagFilha">Primeira tag filha do XML</param>
        /// <returns>Tipo do XML</returns>
        private static TipoXML ObterTipoXmlESocial(XmlDocument xmlDoc, string primeiraTagFilha)
        {
            switch (primeiraTagFilha)
            {
                case "envioLoteEventos":
                    return TipoXML.ESocialEnvioLoteEventos;

                case "consultaLoteEventos":
                    return TipoXML.ESocialConsultaLoteAssincrono;

                default:

                    if (xmlDoc.GetElementsByTagName("consultaEvtsEmpregador").Count > 0)
                    {
                        return TipoXML.ESocialConsultaEvtsEmpregador;
                    }
                    else if (xmlDoc.GetElementsByTagName("consultaEvtsTrabalhador").Count > 0)
                    {
                        return TipoXML.ESocialConsultaEvtsTrabalhador;
                    }
                    else if (xmlDoc.GetElementsByTagName("consultaEvtsTabela").Count > 0)
                    {
                        return TipoXML.ESocialConsultaEvtsTabela;
                    }
                    else if (xmlDoc.GetElementsByTagName("solicDownloadEvtsPorId").Count > 0)
                    {
                        return TipoXML.ESocialDownloadPorID;
                    }
                    else if (xmlDoc.GetElementsByTagName("solicDownloadEventosPorNrRecibo").Count > 0)
                    {
                        return TipoXML.ESocialDownloadPorNrRec;
                    }

                    else
                    {
                        return TipoXML.NaoIdentificado;
                    }
            }
        }

        /// <summary>
        /// Obtém o tipo de XML de EFD Reinf baseado no conteúdo do XML
        /// </summary>
        /// <param name="primeiraTagFilha">Primeira tag filha do XML</param>
        /// <returns>Tipo do XML</returns>
        private static TipoXML ObterTipoXmlEFDReinf(string primeiraTagFilha)
        {
            var tipoXML = TipoXML.NaoIdentificado;

            switch (primeiraTagFilha)
            {
                case "envioLoteEventos":
                    return TipoXML.EFDReinfEnvioLoteEventos;

                case "ConsultaLoteAssincrono":
                    return TipoXML.EFDReinfConsultaLoteAssincrono;

                case "ConsultaReciboEvento":
                    return TipoXML.EFDReinfConsultaReciboEvento;

                case "ConsultaResultadoFechamento2099":
                    return TipoXML.EFDReinfConsultaFechamento2099;
            }

            if (primeiraTagFilha.Contains("evt"))
            {
                return TipoXML.EFDReinfEvento;
            }

            return tipoXML;
        }

        /// <summary>
        /// Extrair conteúdo da chave do documento fiscal eletrônico (NFe, NFCe, CTe, MDFe, etc...) com elementos separados.
        /// </summary>
        /// <param name="chave">Chave do DFe para extrair o conteúdo</param>
        /// <returns>Estrutura contendo o valor de cada elemento que compõe a chave do DFe</returns>
        /// <example>
        ///
        /// var conteudo = XMLUtility.ExtrairConteudoChaveDFe("41210212345678000112650110000000069123456787");
        ///
        /// Console.WriteLine(conteudo.UFEmissor); //Output: PR
        /// Console.WriteLine(conteudo.AnoEmissao); //Output: 21
        /// Console.WriteLine(conteudo.MesEmissao); //Output: 02
        /// Console.WriteLine(conteudo.CNPJCPFEmissor); //Output: 12345678000112
        /// Console.WriteLine(conteudo.Modelo); //Output: NFCe
        /// Console.WriteLine(conteudo.Serie); //Output: 11
        /// Console.WriteLine(conteudo.NumeroDoctoFiscal); //Output: 6
        /// Console.WriteLine(conteudo.TipoEmissao); //Output: ContingenciaOffLine
        /// Console.WriteLine(conteudo.CodigoNumerico); //Output: 12345678
        /// Console.WriteLine(conteudo.DigitoVerificador); //Output: 7
        ///
        /// </example>
        public static ConteudoChaveDFe ExtrairConteudoChaveDFe(string chave)
        {
            var conteudo = new ConteudoChaveDFe
            {
                UFEmissor = (UFBrasil)Convert.ToInt32(chave.Substring(0, 2)),
                AnoEmissao = chave.Substring(2, 2),
                MesEmissao = chave.Substring(4, 2),
                CNPJCPFEmissor = chave.Substring(6, 14),
                Modelo = (ModeloDFe)Convert.ToInt32(chave.Substring(20, 2)),
                Serie = Convert.ToInt32(chave.Substring(22, 3)),
                NumeroDoctoFiscal = Convert.ToInt32(chave.Substring(25, 9)),
                TipoEmissao = (TipoEmissao)Convert.ToInt32(chave.Substring(34, 1)),
                CodigoNumerico = chave.Substring(35, 8),
                DigitoVerificador = Convert.ToInt32(chave.Substring(43, 1))
            };

            return conteudo;
        }

        /// <summary>
        /// Monta a chave da NFe com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos da NFe necessários para montagem da chave</param>
        /// <returns>Chave da NFe</returns>
        public static string MontarChaveNFe(ref ConteudoChaveDFe conteudoChaveDFe) => MontarChaveDFe(ref conteudoChaveDFe);

        /// <summary>
        /// Monta a chave do MDFe com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos do MDFe necessários para montagem da chave</param>
        /// <returns>Chave do MDFe</returns>
        public static string MontarChaveMDFe(ref ConteudoChaveDFe conteudoChaveDFe) => MontarChaveDFe(ref conteudoChaveDFe);

        /// <summary>
        /// Monta a chave do CTe com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos do CTe necessários para montagem da chave</param>
        /// <returns>Chave do CTe</returns>
        public static string MontarChaveCTe(ref ConteudoChaveDFe conteudoChaveDFe) => MontarChaveDFe(ref conteudoChaveDFe);

        /// <summary>
        /// Monta a chave do CTe com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos do CTe necessários para montagem da chave</param>
        /// <returns>Chave do CTe</returns>
        public static string MontarChaveNFCom(ref ConteudoChaveDFe conteudoChaveDFe) => MontarChaveNF3e(ref conteudoChaveDFe);


        /// <summary>
        /// Monta a chave do NF3e com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos do NF3e necessários para montagem da chave</param>
        /// <returns>Chave do NF3e</returns>
        public static string MontarChaveNF3e(ref ConteudoChaveDFe conteudoChaveDFe)
        {
            var chave = ((int)conteudoChaveDFe.UFEmissor).ToString() +
                conteudoChaveDFe.AnoEmissao +
                conteudoChaveDFe.MesEmissao +
                conteudoChaveDFe.CNPJCPFEmissor +
                ((int)conteudoChaveDFe.Modelo).ToString().PadLeft(2, '0') +
                conteudoChaveDFe.Serie.ToString().PadLeft(3, '0') +
                conteudoChaveDFe.NumeroDoctoFiscal.ToString().PadLeft(9, '0') +
                ((int)conteudoChaveDFe.TipoEmissao).ToString() +
                conteudoChaveDFe.NSiteAutoriz.ToString() +
                conteudoChaveDFe.CodigoNumerico.PadLeft(7, '0');

            conteudoChaveDFe.DigitoVerificador = XMLUtility.CalcularDVChave(chave);

            chave += conteudoChaveDFe.DigitoVerificador.ToString();

            return chave;
        }

        /// <summary>
        /// Monta a chave do DFE com base nos valores informados
        /// </summary>
        /// <param name="conteudoChaveDFe">Conteúdos do DFe necessários para montagem da chave</param>
        /// <returns>Chave do DFe</returns>
        private static string MontarChaveDFe(ref ConteudoChaveDFe conteudoChaveDFe)
        {
            var chave = ((int)conteudoChaveDFe.UFEmissor).ToString() +
                conteudoChaveDFe.AnoEmissao +
                conteudoChaveDFe.MesEmissao +
                conteudoChaveDFe.CNPJCPFEmissor +
                ((int)conteudoChaveDFe.Modelo).ToString().PadLeft(2, '0') +
                conteudoChaveDFe.Serie.ToString().PadLeft(3, '0') +
                conteudoChaveDFe.NumeroDoctoFiscal.ToString().PadLeft(9, '0') +
                ((int)conteudoChaveDFe.TipoEmissao).ToString() +
                conteudoChaveDFe.CodigoNumerico.PadLeft(8, '0');

            conteudoChaveDFe.DigitoVerificador = XMLUtility.CalcularDVChave(chave);

            chave += conteudoChaveDFe.DigitoVerificador.ToString();

            return chave;
        }

        /// <summary>
        /// Gera um número randômico para ser utilizado no Código Numérico da NFe, NFCe, CTe, MDFe, etc...
        /// </summary>
        /// <param name="numeroNF">Número da NF, CT ou MDF</param>
        /// <returns>Código numérico</returns>
        public static int GerarCodigoNumerico(int numeroNF)
        {
            var retorno = 0;

            while(retorno == 0)
            {
                var rnd = new Random(numeroNF);

                retorno = Convert.ToInt32(rnd.Next(1, 99999999).ToString("00000000"));
            }

            return retorno;
        }

        /// <summary>
        /// Busca o número da chave do Documento Fiscal Eletrônico no XML do Documento Fiscal Eletrônico
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <returns>Chave do DFe (Documento Fiscal Eletrônico = NFe, NFCe, CTe, etc...)</returns>
        public static string GetChaveDFe(string xml) => GetChaveDFe(xml, DetectDFeType(xml));

        /// <summary>
        /// Busca o número da chave do Documento Fiscal Eletrônico no XML do Documento Fiscal Eletrônico
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <param name="typeDFe">Tipo do DFe</param>
        /// <returns>Chave do DFe (Documento Fiscal Eletrônico = NFe, NFCe, CTe, etc...)</returns>
        public static string GetChaveDFe(string xml, TipoDFe typeDFe)
        {
            var typeString = "";

            switch(typeDFe)
            {
                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    typeString = "NFe";
                    break;

                case TipoDFe.CTe:
                case TipoDFe.CTeOS:
                    typeString = "CTe";
                    break;

                case TipoDFe.MDFe:
                    typeString = "MDFe";
                    break;

                case TipoDFe.CFe:
                    typeString = "CFe";
                    break;

                case TipoDFe.NF3e:
                    typeString = "NF3e";
                    break;
            }

            var pedacinhos = xml.Split(new string[] { $"Id=\"{typeString}" }, StringSplitOptions.None);

            return pedacinhos.Length < 1 ? default : pedacinhos[1].Substring(0, 44);
        }

        /// <summary>
        /// Busca o número da chave do evento do CT-e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <returns>Chave do evento do CT-e</returns>
        public static string GetChaveEventoCTe(string xml) => GetChaveEventoCTe(xml, DetectEventoCTeType(xml));

        /// <summary>
        /// Busca o número da chave do evento do CT-e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <param name="typeEventoCTe">Tipo de evento do CTe</param>
        /// <returns>Chave do evento do CT-e</returns>
        public static string GetChaveEventoCTe(string xml, TipoEventoCTe typeEventoCTe)
        {
            var typeString = "";

            switch(typeEventoCTe)
            {
                case TipoEventoCTe.CartaCorrecao:
                    typeString = "110110";
                    break;

                case TipoEventoCTe.Cancelamento:
                    typeString = "110111";
                    break;

                case TipoEventoCTe.ComprovanteEntrega:
                    typeString = "110180";
                    break;

                case TipoEventoCTe.CancelamentoComprovanteEntrega:
                    typeString = "110181";
                    break;

                case TipoEventoCTe.PrestDesacordo:
                    typeString = "610110";
                    break;

                case TipoEventoCTe.RegistroPassagem:
                    typeString = "310620";
                    break;
            }

            var pedacinhos = xml.Split(new string[] { $"Id=\"ID{typeString}" }, StringSplitOptions.None);

            return pedacinhos.Length < 1 ? default : pedacinhos[1].Substring(0, 44);
        }

        /// <summary>
        /// Busca o número da chave do evento do MDF-e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <returns>Chave do evento do MDF-e</returns>
        public static string GetChaveEventoMDFe(string xml) => GetChaveEventoMDFe(xml, DetectEventoMDFeType(xml));

        /// <summary>
        /// Busca o número da chave do evento do MDF-e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <param name="typeEventoMDFe">Tipo do evento do MDFe</param>
        /// <returns>Chave do evento do MDF-e</returns>
        public static string GetChaveEventoMDFe(string xml, TipoEventoMDFe typeEventoMDFe)
        {
            var typeString = "";

            switch(typeEventoMDFe)
            {
                case TipoEventoMDFe.Cancelamento:
                    typeString = "110111";
                    break;

                case TipoEventoMDFe.Encerramento:
                    typeString = "110112";
                    break;

                case TipoEventoMDFe.InclusaoCondutor:
                    typeString = "110114";
                    break;

                case TipoEventoMDFe.InclusaoDFe:
                    typeString = "110115";
                    break;
            }

            var pedacinhos = xml.Split(new string[] { $"Id=\"ID{typeString}" }, StringSplitOptions.None);

            return pedacinhos.Length < 1 ? default : pedacinhos[1].Substring(0, 44);
        }

        /// <summary>
        /// Busca o número da chave do Documento Fiscal Eletrônico no XML do Documento Fiscal Eletrônico
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <returns>Chave do DFe (Documento Fiscal Eletrônico = NFe, NFCe, CTe, etc...)</returns>
        public static string GetChaveEventoNFe(string xml) => GetChaveEventoNFe(xml, DetectEventoNFeType(xml));

        /// <summary>
        /// Busca o número da chave do Documento Fiscal Eletrônico no XML do Documento Fiscal Eletrônico
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <param name="typeEventoDFe">Tipo do Evento DFe</param>
        /// <returns>Chave do evento do DFe (Documento Fiscal Eletrônico = NFe, NFCe, CTe, etc...)</returns>
        public static string GetChaveEventoNFe(string xml, TipoEventoNFe typeEventoDFe)
        {
            var typeString = "";

            switch(typeEventoDFe)
            {
                case TipoEventoNFe.CartaCorrecao:
                    typeString = "110110";
                    break;

                case TipoEventoNFe.Cancelamento:
                    typeString = "110111";
                    break;

                case TipoEventoNFe.CancelamentoPorSubstituicao:
                    typeString = "110112";
                    break;

                case TipoEventoNFe.EPEC:
                    typeString = "110140";
                    break;

                case TipoEventoNFe.PedidoProrrogacaoPrazo1:
                    typeString = "111500";
                    break;

                case TipoEventoNFe.PedidoProrrogacaoPrazo2:
                    typeString = "111501";
                    break;

                case TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo1:
                    typeString = "111502";
                    break;

                case TipoEventoNFe.CancelamentoPedidoProrrogacaoPrazo2:
                    typeString = "111503";
                    break;

                case TipoEventoNFe.ManifestacaoConfirmacaoOperacao:
                    typeString = "210200";
                    break;

                case TipoEventoNFe.ManifestacaoCienciaOperacao:
                    typeString = "210210";
                    break;

                case TipoEventoNFe.ManifestacaoDesconhecimentoOperacao:
                    typeString = "210220";
                    break;

                case TipoEventoNFe.ManifestacaoOperacaoNaoRealizada:
                    typeString = "210240";
                    break;

                case TipoEventoNFe.RespostaPedidoProrrogacaoPrazo1:
                    typeString = "411500";
                    break;

                case TipoEventoNFe.RespostaPedidoProrrogacaoPrazo2:
                    typeString = "411501";
                    break;

                case TipoEventoNFe.RespostaCancelamentoPedidoProrrogacaoPrazo1:
                    typeString = "411502";
                    break;

                case TipoEventoNFe.RespostaCancelamentoPedidoProrrogacaoPrazo2:
                    typeString = "411503";
                    break;
            }

            var pedacinhos = xml.Split(new string[] { $"Id=\"ID{typeString}" }, StringSplitOptions.None);

            return pedacinhos.Length < 1 ? default : pedacinhos[1].Substring(0, 44);
        }

        /// <summary>
        /// Busca o número da chave do evento do NF3e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <returns>Chave do evento do NF3e</returns>
        public static string GetChaveEventoNF3e(string xml) => GetChaveEventoNF3e(xml, DetectEventoNF3eType(xml));

        /// <summary>
        /// Busca o número da chave do evento do NF3e
        /// </summary>
        /// <param name="xml">Conteúdo do XML para busca da chave</param>
        /// <param name="typeEventoNF3e">Tipo do evento do NF3e</param>
        /// <returns>Chave do evento do NF3e</returns>
        public static string GetChaveEventoNF3e(string xml, TipoEventoNF3e typeEventoNF3e)
        {
            var typeString = "";

            switch (typeEventoNF3e)
            {
                case TipoEventoNF3e.Cancelamento:
                    typeString = "110111";
                    break;
            }

            var pedacinhos = xml.Split(new string[] { $"Id=\"ID{typeString}" }, StringSplitOptions.None);

            return pedacinhos.Length < 1 ? default : pedacinhos[1].Substring(0, 44);
        }

        /// <summary>
        /// Serializar o objeto (Converte o objeto para XML)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="objeto">Objeto a ser serializado</param>
        /// <param name="namespaces">Namespaces a serem adicionados no XML</param>
        /// <returns>XML</returns>
        public static XmlDocument Serializar<T>(T objeto, List<TNameSpace> namespaces = null)
            where T : new()
        {
            var xml = XmlHelper.Serialize(objeto, namespaces?.Select(s => (s.NS, s.Prefix)).ToList());

            return xml;
        }

        /// <summary>
        /// Serializar o objeto (Converte o objeto para XML)
        /// </summary>
        /// <param name="objeto">Objeto a ser serializado</param>
        /// <param name="namespaces">Namespaces a serem adicionados no XML</param>
        /// <returns>XML</returns>
        public static XmlDocument Serializar(object objeto, List<TNameSpace> namespaces = null)
        {
            var xml = XmlHelper.Serialize(objeto, namespaces?.Select(s => (s.NS, s.Prefix)).ToList());

            return xml;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="xmlRootAttribute"></param>
        /// <param name="namespaces"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"></exception>
        public static XmlDocument Serializar(object obj, XmlRootAttribute xmlRootAttribute, List<TNameSpace> namespaces = null) => Serialize(obj, xmlRootAttribute, namespaces?.Select(s => (s.NS, s.Prefix)).ToList());

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="obj"></param>
        /// <param name="xmlRootAttribute"></param>
        /// <param name="namespaces"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"></exception>
        private static XmlDocument Serialize<T>(T obj, XmlRootAttribute xmlRootAttribute, List<(string Namespace, string Prefix)> namespaces = null)
        {
            if(obj == null)
            {
                throw new ArgumentNullException("obj");
            }

            var ns = new XmlSerializerNamespaces();
            namespaces?.ForEach(delegate ((string Namespace, string Prefix) n)
            {
                ns.Add(n.Prefix, n.Namespace);
            });
            var xmlSerializer = new XmlSerializer(obj.GetType(), xmlRootAttribute);
            var xmlDocument = new XmlDocument();
            StringWriter stringWriter = new Utf8StringWriter();
            xmlSerializer.Serialize(stringWriter, obj, ns);
            xmlDocument.LoadXml(stringWriter.ToString());

            return xmlDocument;
        }

        /// <summary>
        /// Busca o nome de uma determinada TAG em um Elemento do XML para ver se existe, se existir retorna seu conteúdo da TAG.
        /// </summary>
        /// <param name="xmlElement">Elemento do XML onde será pesquisado o Nome da TAG</param>
        /// <param name="tagName">Nome da Tag que será pesquisado</param>
        /// <returns>Conteúdo da tag</returns>
        public static bool TagExist(XmlElement xmlElement, string tagName) => XmlHelper.TagExist(xmlElement, tagName);

        /// <summary>
        /// Busca o nome de uma determinada TAG em um Elemento do XML para ver se existe, se existir retorna seu conteúdo da TAG.
        /// </summary>
        /// <param name="xmlElement">Elemento do XML onde será pesquisado o Nome da TAG</param>
        /// <param name="tagName">Nome da Tag que será pesquisado</param>
        /// <returns>Conteúdo da tag</returns>
        public static string TagRead(XmlElement xmlElement, string tagName) => XmlHelper.ReadTagValue(xmlElement, tagName);

        /// <summary>
        /// Tratar caracteres especiais existentes na string substituindo por escape.
        /// Caracteres substituídos: <![CDATA[  & < > \ ' ]]>
        /// Escapes utilizados: <![CDATA[  &amp; &lt; &gt; &quot; &#39; ]]>
        /// </summary>
        /// <param name="content">String a ser tratada</param>
        /// <returns>string com os caracteres especiais substituídos pelos seus escapes</returns>
        /// <example>
        /// var texto = "Dias \ Dias";
        /// texto = XMLUtility.TratarCaracterEspecial(texto);
        /// MessageBox.Show(texto); // Retorno será: "Dias &quot; Dias"
        /// </example>
        /// <seealso cref="ClearExtraSpaces(string)"/>
        public static string UnescapeReservedCharacters(string content)
        {
            if(string.IsNullOrWhiteSpace(content))
            {
                return content;
            }

            /*

	           There is always a solution

			           ,;~;,
			              /\_
			             (  /
			             ((),     ;,;
			             |  \\  ,;;'(
		             __ _(  )'~;;'   \
		           /'  '\'()/~' \ /'\.)
	            ,;(      )||     |
	           ,;' \    /-(.;,   )
		            ) /       ) /
		           //         ||
	              (_\         (_\

	           go horse <3

               Aqui detectamos o seguinte:
               Se o amiguinho(a) desenvolvedor(a) já passar convertido com &amp;,
                o replace abaixo ajusta para &
               Este problema só acontece quando se usa o objeto direto, não acontece quando
                serializa ou deserializa

               Sim, a gente poderia usar Regex, validar e tudo mais, mas assim é mais rápido.

                            Table Flip ...

                            (╯°□°）╯︵ ┻━┻

            */

            content = content.Replace("&amp;", "&");
            content = content.Replace("&lt;", "<");
            content = content.Replace("&gt;", ">");
            content = content.Replace("&quot;", "\"");
            content = content.Replace("&quot;", "\\");
            content = content.Replace("&#39;", "'");

            /*
                ... Put the table back

                ┬─┬﻿ ノ(゜-゜ノ)
             */

            return content;
        }

        #endregion Public Methods

        /// <summary>
        /// Gerar a chave dos seguintes documentos fiscais eletrônicos: NFe, NFCe, CTe, MFDe e CTeOS.
        /// </summary>
        /// <param name="cUF">UF do emitente</param>
        /// <param name="dhEmi">Data de emissão do documento</param>
        /// <param name="cnpjcpf">CNPJ ou CPF do emitente</param>
        /// <param name="mod">Código do modelo do documento fiscal eletrônicos</param>
        /// <param name="serie">Série do documento fiscal eletrônico</param>
        /// <param name="nNF">Número da nota fiscal</param>
        /// <param name="tpEmis">Tipo de emissão (Tag tpEmis)</param>
        /// <param name="cNF">Código numérico randômico (Deixe em branco ou nulo para que a DLL gera este código para você)</param>
        /// <returns>Retorna a chave, completa, do documento fiscal eletrônico com o dígito verificar calculado e concatenado a chave</returns>
        public static string MontarChaveDFe(UFBrasil cUF, DateTime dhEmi, string cnpjcpf, ModeloDFe mod, int serie, int nNF, TipoEmissao tpEmis, string cNF = "")
        {
            if(string.IsNullOrWhiteSpace(cNF))
            {
                cNF = XMLUtility.GerarCodigoNumerico(nNF).ToString("00000000");
            }

            var chaveDFe = ((int)cUF).ToString() +
                dhEmi.ToString("yyMM") +
                cnpjcpf.PadLeft(14, '0') +
                ((int)mod).ToString().PadLeft(2, '0') +
                serie.ToString().PadLeft(3, '0') +
                nNF.ToString().PadLeft(9, '0') +
                ((int)tpEmis).ToString() +
                cNF.PadLeft(8, '0');

            var cDV = XMLUtility.CalcularDVChave(chaveDFe);

            chaveDFe += cDV.ToString();

            return chaveDFe;
        }
    }

#if INTEROP

    /// <summary>
    /// Utilitários diversos para trabalhar com XML - Específico INTEROP
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.XMLUtilityInterop")]
    [ComVisible(true)]
    public class XMLUtilityInterop
    {
        /// <summary>
        /// Gerar a chave dos seguintes documentos fiscais eletrônicos: NFe, NFCe, CTe, MFDe e CTeOS.
        /// </summary>
        /// <param name="cUF">UF do emitente</param>
        /// <param name="dhEmi">Data de emissão do documento</param>
        /// <param name="cnpjcpf">CNPJ ou CPF do emitente</param>
        /// <param name="mod">Código do modelo do documento fiscal eletrônicos</param>
        /// <param name="serie">Série do documento fiscal eletrônico</param>
        /// <param name="nNF">Número da nota fiscal</param>
        /// <param name="tpEmis">Tipo de emissão (Tag tpEmis)</param>
        /// <param name="cNF">Código numérico randômico (Deixe em branco ou nulo para que a DLL gera este código para você)</param>
        /// <returns>Retorna a chave, completa, do documento fiscal eletrônico com o dígito verificar calculado e concatenado a chave</returns>
        public string MontarChaveDFe(UFBrasil cUF, DateTime dhEmi, string cnpjcpf, ModeloDFe mod, int serie, int nNF, TipoEmissao tpEmis, string cNF = "") => XMLUtility.MontarChaveDFe(cUF, dhEmi, cnpjcpf, mod, serie, nNF, tpEmis, cNF);

        /// <summary>
        /// Gera um número randômico para ser utilizado no Código Numérico da NFe, NFCe, CTe, MDFe, etc...
        /// </summary>
        /// <param name="numeroNF">Número da NF, CT ou MDF</param>
        /// <returns>Código numérico</returns>
        public int GerarCodigoNumerico(int numeroNF) => XMLUtility.GerarCodigoNumerico(numeroNF);

        /// <summary>
        /// Gerar o dígito da chave da NFe, CTe, MDFe ou NFCe
        /// </summary>
        /// <param name="chave">Chave do DFe (sem o dígito) que deve ser calculado o dígito verificador.</param>
        /// <returns>Dígito verificador</returns>
        public int CalcularDVChave(string chave) => XMLUtility.CalcularDVChave(chave);

        /// <summary>
        /// Validar o XML com o Unimake Validator
        /// </summary>
        /// <param name="xml">XML a ser validado</param>
        /// <returns>true=validado com sucesso, ou uma exceção com o erro de validação.</returns>
        /// <exception cref="ArgumentNullException">Quando o XML está nulo</exception>
        /// <exception cref="Exception">Quando o validador encontra algum erro no XML</exception>
        public bool Validate(string xml) => ValidatorFactory.BuidValidator(xml)?.Validate() ?? true;

        /// <summary>
        /// Executa uma verificação simples para garantir que a chave do DFe (NFe, CTe, MDfe, NFCe, CTeOS) é valida, se tiver erros retorna exceção.
        /// </summary>
        /// <param name="chave">Chave do DFe a ser verificada</param>
        /// <example>
        /// try
        /// {
        ///     XMLUtility.ChecarChaveDFe("41201280568835000181570010000004841004185096");
        /// }
        /// catch(Exception ex)
        /// {
        ///     //Se chave tiver algum erro, vai retornar uma exceção.
        ///     MessageBox.Show(ex.Message);
        /// }
        /// </example>
        public void ChecarChaveDFe(string chave) => XMLUtility.ChecarChaveDFe(chave);
    }

#endif

}