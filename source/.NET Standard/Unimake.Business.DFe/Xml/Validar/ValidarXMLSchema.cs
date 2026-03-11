using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Isoladores;
using Unimake.Business.DFe.Vinculadores;



namespace Unimake.Business.DFe
{
    /// <summary>
    /// Nova classe de validação de XML que centraliza toda a lógica de validação.
    /// </summary>
    public class ValidarEstruturaXML
    {
        /// <summary>
        /// Atributo de condiguracao
        /// </summary>
        private Configuracao _configuracao;


        /// <summary>
        /// Construtor que recebe as configurações necessárias para a validação do XML.
        /// </summary>
        /// <param name="configuracao"></param>
        public ValidarEstruturaXML(Configuracao configuracao) 
        {
            _configuracao = configuracao;
        }


        /// <summary>
        /// Classe de resultado da validação, contendo um boolean para indicar se a validação foi bem-sucedida 
        /// e um objeto de informação para retornar detalhes sobre o serviço e os schemas utilizados na validação.
        /// </summary>
        public class ResultadoValidacao 
        {
            /// <summary>
            /// Boolean retornado para informar a situação da validação.
            /// </summary>
            public bool Validado { get; set; }

            /// <summary>
            /// Objeto de informação para retornar a descrição 
            /// </summary>
            public string Descricao { get; set; }
        }

        

        /// <summary>
        /// Guarda as configurações do XML para facilitar o acesso durante a validação, evitando múltiplas consultas ao XML de configuração.
        /// </summary>

        public struct InformacaoXML
        {
            /// <summary>
            /// Tag raiz do XML, utilizada para identificar o tipo de documento e 
            /// buscar a configuração correta no XML de serviços.
            /// </summary>
            public string TagRaiz { get; set; }

            /// <summary>
            /// Tag que guarda a descrição do serviço do XML de Configuracao.
            /// </summary>
            public string Descricao { get; set; }

            /// <summary>
            /// Tag ou atributo que contém a versão do layout do XML. 
            /// A versão é crucial para validar contra o schema correto.
            /// </summary>
            public string Versao { get; set; }

            /// <summary>
            /// Schema que deve ser utilizado para validar o XML geral.
            /// </summary>
            public string SchemaArquivo { get; set; }

            /// <summary>
            /// Caso o XML contenha partes específicas que exigem validação 
            /// contra schemas diferentes (ex: eventos, modais)
            /// </summary>
            public string SchemasEspecifico { get; set; }

            /// <summary>
            /// </summary>
            public string SchemaArquivoEspecifico { get; set; }

            /// <summary>
            /// Tag que caso não seja null no arquivo de configuração, indica que o XML 
            /// possui eventos e que deve ser feita a vinculação do XML geral 
            /// com os específicos de cada evento para validação.
            /// </summary>
            public string TagEvento { get; set; }

            /// <summary>
            /// Tag que contem a Target Namespace do XML, utilizada para 
            /// validar o XML contra o schema correto.
            /// </summary>
            public string TargetNS { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita a assinatura digital no XML.
            /// </summary>
            public string TagAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para 
            /// referenciar a assinatura digital no XML.
            /// </summary>
            public string TagAtributoID { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita a assinatura digital 
            /// do lote no XML, caso o serviço trabalhe com lotes.
            /// </summary>
            public string TagLoteAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para referenciar a 
            /// assinatura digital do lote no XML, caso o serviço trabalhe com lotes.
            /// </summary>
            public string TagLoteAtributoID { get; set; }

            /// <summary>
            /// Tag que indica o local onde deve ser feita uma assinatura digital extra no XML, 
            /// caso o serviço exija mais de uma assinatura.
            /// </summary>
            public string TagExtraAssinatura { get; set; }

            /// <summary>
            /// Tag que indica o atributo ID que deve ser utilizado para referenciar 
            /// a assinatura digital extra no XML,
            /// </summary>
            public string TagExtraAtributoID { get; set; }
        }

        /// <summary>
        /// Configurações diversas para consumir os serviços
        /// </summary>
        public Configuracao Configuracoes { get; set; }

        private static XmlDocument CarregarConfigValidacao()
        {
            var assembly = typeof(ValidarEstruturaXML).Assembly;
            var resourceName = "Unimake.Business.DFe.Servicos.Config.ValidacaoConfig.xml";

            using (var stream = assembly.GetManifestResourceStream(resourceName))
            {
                if (stream == null)
                    throw new Exception($"Recurso não encontrado: {resourceName}");

                var xmlConfig = new XmlDocument();
                xmlConfig.Load(stream);
                return xmlConfig;
            }
        }

        /// <summary>
        /// Valida o XML de acordo com as regras definidas no XML de configuração de serviços.
        /// </summary>
        /// <param name="xml">Documento XML para a validação</param>
        /// <param name="certificado">Certificado Digital A1 para assinatura</param>
        /// <param name="padraoNFSe">Padrão para documentos NFSe</param>
        /// <exception cref="Exception"></exception>
        public ResultadoValidacao ValidarServico(XmlDocument xml, X509Certificate2 certificado, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            try
            {
            
               var xmlConfig = CarregarConfigValidacao();
               
                TipoDFe tipoDFe = padraoNFSe != PadraoNFSe.None
                    ? TipoDFe.NFSe
                    : DetectarTipoDFe(xml);

                string tagRaiz = xml.DocumentElement.Name;
                string versao = ObterVersao(xml, xmlConfig, tipoDFe);
                XmlNode servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

                //Caso o node servico venha vazio retorna por padrão que a validação deu certo pois significa que o serviço especidico não
                // está implementado no arquivo xml de validação, podendo ser uma versão antiga que ainda é utilizada mas não esta no arquivo de 
                // de configuração dessa forma retorna como validado.
                if (servico is null)
                    return new ResultadoValidacao
                    {
                        Validado = true,
                        Descricao = "Arquivo não validado: Possível versão antiga que ainda é utilizado porém não validada."
                    };


                var inform = MontarInformacaoGeral(servico);
                AssinarSeNecessario(xml, inform, certificado);

                // Se não tem schemas específicos, valida só o geral mesmo
                if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null)
                {
                    ValidarSchemaGeral(xml, inform, tipoDFe, padraoNFSe);
                    return new ResultadoValidacao
                    {
                        Validado = true,
                        Descricao = inform.Descricao
                    };
                }

                // Se chegou aqui, tem schemas específicos para validar, então precisa vincular o XML especifico com os schema para depois validar cada um
                bool isEvento = servico.SelectSingleNode(".//*[local-name()='TagEvento']") != null;

                var vinculador = VinculadorFactory.Criar(tipoDFe, isEvento);
                var nodes = vinculador.Vincular(servico, xml);

                foreach (var (tipoCorreto, node) in nodes)
                {
                    MontarInformacaoEspecifica(servico, tipoCorreto, inform);
                    ValidarSchemaGeral(xml, inform, tipoDFe);
                    ValidarSchemaEspecifico(node, inform, tipoDFe);
                }

                return new ResultadoValidacao
                {
                    Validado = true,
                    Descricao = inform.Descricao
                };
            }
            catch (Exception ex)
            {
                throw new Exception($"Erro ao validar o arquivo: {ex.Message}");
            }
        }



        private static XmlNode ObterServico(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            return padraoNFSe == PadraoNFSe.None
                ? TratarDFe(xml, versao, tipoDFe, tagRaiz, xmlConfig)
                : TratarNFSe(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);
        }




        private static XmlNode TratarNFSe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {

            string pathServicosNFSe = string.Empty;
            XmlNode servicoNFSe = null;

            if (versao.IsNullOrEmpty())
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}']";
                XmlNodeList servicosNFSe = xmlConfig.SelectNodes(pathServicosNFSe);

                foreach (XmlNode servico in servicosNFSe)
                {
                    var identificador = servico.Attributes["tagIdentificadora"]?.Value;

                    if (!string.IsNullOrEmpty(identificador))
                    {
                        var nodeServicoNFSe = xml.DocumentElement.SelectSingleNode($"//*[local-name()='{identificador}']");

                        if (!(nodeServicoNFSe is null))
                        {
                            servicoNFSe = servico;
                            break;
                        }
                    }
                }
            }
            else
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}' and @versao='{versao}']";
                servicoNFSe = xmlConfig.SelectSingleNode(pathServicosNFSe);

            }

            return servicoNFSe;
        }




        private static XmlNode TratarDFe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {

            string pathServico = $"//{tipoDFe}/Servico[@tagRaiz='{tagRaiz}' and @versao='{versao}']";
            XmlNode servico = xmlConfig.SelectSingleNode(pathServico);

            return servico;

        }


        private static InformacaoXML MontarInformacaoGeral(XmlNode servico)
        {
            return new InformacaoXML
            {
                TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                Versao = servico.Attributes["versao"]?.Value,
                Descricao = servico.SelectSingleNode("*[local-name()='Descricao']")?.InnerText,
                SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText,
                TagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText,
                TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText,
                TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText,
                TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText,
                TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText
            };
        }


        private static void MontarInformacaoEspecifica(XmlNode servico, XmlNode tipo, InformacaoXML inform)
        {
            inform.SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
            inform.SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText;

        }


        private static void AssinarSeNecessario(XmlDocument xml, InformacaoXML info, X509Certificate2 cert)
        {
            if (!string.IsNullOrEmpty(info.TagAssinatura))
            {
                Assinar(xml, info.TagAssinatura, info.TagAtributoID, cert);
            }

            if (!string.IsNullOrEmpty(info.TagLoteAssinatura))
            {
                Assinar(xml, info.TagLoteAssinatura, info.TagLoteAtributoID, cert);
            }

            if (!string.IsNullOrEmpty(info.TagExtraAssinatura)) 
            {
                Assinar(xml, info.TagExtraAssinatura, info.TagExtraAtributoID, cert);
            }

        }


        private static void Assinar(XmlDocument xml, string tagAssinatura, string tagID, X509Certificate2 cert)
        {

            var usarCertificado = true; 

            if (string.IsNullOrWhiteSpace(tagAssinatura))
                return;

            if (usarCertificado)
            {

                if (!AssinaturaDigital.EstaAssinado(xml, tagAssinatura))
                {
                    AssinaturaDigital.Assinar(xml, tagAssinatura, tagID, cert, AlgorithmType.Sha1);

                    //| TipoDFe         | Algoritmo |
                    //| --------------- | --------- | 
                    //| NFe             | SHA1      | 
                    //| CTe  CTeOS      | SHA1      | 
                    //| MDFe            | SHA1      | 
                    //| NFSe            | SHA1      | 
                    //| Reinf           | SHA256    |
                    //| eSocial         | SHA256    | 
                    //| DARE            | SHA256    |

                }
            }
           
        }


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            if (string.IsNullOrEmpty(info.SchemaArquivo))
                return;

            string schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoDFe.ToString()}.{info.SchemaArquivo}"
                : $"{tipoDFe.ToString()}.{padraoNFSe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new Exception($"Erro ao validar schema geral {validar.ErrorMessage}. XML foi identificado como ");
            }
        }


        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info, TipoDFe tipoDFe)
        {
            if (string.IsNullOrEmpty(info.SchemaArquivo))
                return;

            //Isolando cada XML dependendo do tipoDFe
            var isolador = IsoladorFactory.CriarIsolador(tipoDFe);
            var xmlEspecifico = isolador.Isolar(eventoNode);

            var validarEspecifico = new ValidarSchema();
            string schemaEspecifico = $"{tipoDFe.ToString()}.{info.SchemaArquivoEspecifico}";
            validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNS);

            if (!validarEspecifico.Success)
            {
                throw new Exception($"Erro ao validar schema específico {validarEspecifico.ErrorMessage}");
            }
        }



        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig, TipoDFe tipoDFe)
        {
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                var tagVersao = servico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count > 0)
                {
                    var versaoAtributo = ((XmlElement)nodeVersao[0]).GetAttribute("versao");
                    var versaoValor = nodeVersao[0].InnerText; // Em alguns casos a versão pode estar no valor do nó ao invés de um atributo, então verificamos os dois

                    if (!string.IsNullOrEmpty(versaoAtributo))
                        return versaoAtributo;

                    if (!string.IsNullOrEmpty(versaoValor))
                        return versaoValor;

                }
            }

            return string.Empty;
        }



        private static TipoDFe DetectarTipoDFe(XmlDocument xml)
        {
            var tipoDFe = TipoDFe.Desconhecido;

            switch (xml.DocumentElement.Name)
            {
                #region NFe

                case "consStatServ":
                case "consSitNFe":
                case "consReciNFe":
                case "ConsCad":
                case "envEvento":
                case "inutNFe":
                case "NFe":
                case "enviNFe":
                case "nfeProc":
                    tipoDFe = TipoDFe.NFe;
                    break;

                case "distDFeInt":
                    var ns = xml.GetElementsByTagName("distDFeInt")[0].NamespaceURI.ToLower();

                    if (ns.Contains("/nfe"))
                    {
                        tipoDFe = TipoDFe.NFe;
                    }
                    else if (ns.Contains("/cte"))
                    {
                        tipoDFe = TipoDFe.CTe;
                    }
                    break;

                #endregion

                #region CTe

                case "consStatServCte":
                case "consSitCTe":
                case "consReciCTe":
                case "eventoCTe":
                case "CTe":
                case "enviCTe":
                case "cteProc":
                case "CTeSimp":
                case "CTeOS":
                    tipoDFe = TipoDFe.CTe;
                    break;

                #endregion

                #region MDFe

                case "consStatServMDFe":
                case "consSitMDFe":
                case "consReciMDFe":
                case "eventoMDFe":
                case "MDFe":
                case "enviMDFe":
                case "consMDFeNaoEnc":
                case "mdfeProc":
                    tipoDFe = TipoDFe.MDFe;
                    break;

                #endregion

                #region NF3e

                case "consStatServNF3e":
                case "consSitNF3e":
                case "consReciNF3e":
                case "eventoNF3e":
                case "NF3e":
                    tipoDFe = TipoDFe.NF3e;
                    break;

                #endregion

                #region NFCom

                case "consStatServNFCom":
                case "consSitNFCom":
                case "eventoNFCom":
                case "NFCom":
                    tipoDFe = TipoDFe.NFCom;
                    break;

                #endregion

                default:
                    throw new Exception("Tipo do DFe não identificado");
            }

            return tipoDFe;
        }

    }
}

