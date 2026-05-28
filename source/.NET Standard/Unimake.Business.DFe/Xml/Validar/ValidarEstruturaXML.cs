using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Isoladores;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Vinculadores;
using Unimake.Business.DFe.Xml.Validar.QRCode;
using Unimake.Exceptions;
using System.Linq;





namespace Unimake.Business.DFe
{
    /// <summary>
    /// Nova classe de validação de XML que centraliza toda a lógica de validação.
    /// </summary>
    public class ValidarEstruturaXML
    {


        /// <summary>
        /// Classe de resultado da validação, contendo um boolean para indicar se a validação foi bem-sucedida,
        /// Descriação da validação, mensagem de retorno sobre a validação, status da valiação e o xml assinado.
        /// </summary>
        public class ResultadoValidacao
        {
            /// <summary>
            /// Boolean retornado para informar a situação da validação.
            /// </summary>
            public bool Validado { get; set; }

            /// <summary>
            /// Descrição do serviço validado, com o tipo e o serviço.
            /// </summary>
            public string Descricao { get; set; }

            /// <summary>
            /// Mensagem de retorno da validação, caso a validação tenha retornado false, para informar o motivo do erro.
            /// </summary>
            public string MensagemRetorno { get; set; }

            /// <summary>
            /// Status da validação
            /// </summary>
            public string StatusValidacao { get; set; }

            /// <summary>
            /// Xml após a assinatura 
            /// </summary>
            public XmlDocument XmlAssinado { get; set; }


        }


        /// <summary>
        /// Guarda as configurações do XML de configuracção para o acesso durante a validação, evitando múltiplas consultas ao XML de configuração.
        /// </summary>

        public class InformacaoXML
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
            /// Tag que contem a Target Namespace Específico (se tiver) do XML, utilizada para 
            /// validar o XML contra o schema correto.
            /// </summary>
            public string TargetNSEspecifico { get; set; }

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

            /// <summary>
            /// Tag que indica se o serviço utiliza certificado digital para assinatura, padrão é true, ou seja, se a tag estiver 
            /// presente e for diferente de "false" o serviço será assinado, caso contrário, não será assinado.
            /// </summary>
            public bool UsaCertificadoDigital { get; set; }

            /// <summary>
            /// Tag que indica se o serviço deve ser assinado ou não dependendo do Tipo Ambiente
            /// </summary>
            public TipoAmbiente? NaoAssina { get; set; }

            /// <summary>
            /// Tag que indica se o serviço utiliza QRcode
            /// </summary>
            public bool GerarQRCode { get; set; }
        }


        /// <summary>
        /// Retorna o XML de configuração de serviços, que contém as regras de validação para cada tipo de documento e serviço.
        /// </summary>
        /// <returns></returns>
        /// <exception cref="Exception"></exception>
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
        /// <param name="configuracao">Configurações com as informações de vcalidação do DFe</param>
        /// <exception cref="Exception"></exception>
        public ResultadoValidacao ValidarServico(XmlDocument xml, Configuracao configuracao)
        {
            var certificado = configuracao.CertificadoDigital;
            var tipoAmbiente = configuracao.TipoAmbiente;
            var padraoNFSe = configuracao.PadraoNFSe;
            var codigoUF = (UFBrasil)configuracao.CodigoUF;

            var tipoDFe = padraoNFSe != PadraoNFSe.None
            ? TipoDFe.NFSe
            : DetectarTipoDFe(xml);

            try
            {
                configuracao.TipoDFe = tipoDFe;
                var xmlConfig = CarregarConfigValidacao();
                var tagRaiz = xml.DocumentElement.Name;
                var versao = ObterVersao(xml, xmlConfig, tipoDFe, padraoNFSe);
                var servico = ObterServico(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);

                if (servico is null)
                {
                    throw new Exception($"Não foi possível encontrar a configuração para o tipo de DFe com tag raiz: {tagRaiz} ou versão: {versao}. Verfique se a versão e/ou tag raiz estão corretas paraa validação");
                }

                AtribuirUrl(servico, codigoUF, configuracao);

                var inform = MontarInformacaoGeral(servico, codigoUF);

                try
                {
                    //AssinarSeNecessario(xml, inform, certificado, configuracao, tipoAmbiente, tipoDFe); antes era aqui

                    // Se não tem schemas específicos, valida só o geral mesmo
                    if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null)
                    {

                        AssinarSeNecessario(xml, inform, certificado, configuracao, tipoAmbiente, tipoDFe);

                        ValidarSchemaGeral(xml, inform, tipoDFe, padraoNFSe);

                        return new ResultadoValidacao
                        {
                            Validado = true,
                            Descricao = inform.Descricao,
                            MensagemRetorno = "XML assinado e validado com sucesso.",
                            StatusValidacao = "1",
                            XmlAssinado = xml
                        };
                    }

                    // Se chegou aqui, tem schemas específicos para validar, então precisa vincular o XML especifico com os schema para depois validar cada um
                    bool isEvento = servico.SelectSingleNode(".//*[local-name()='TagEvento']") != null;
                    var vinculador = VinculadorFactory.Criar(tipoDFe, isEvento);
                    var nodes = vinculador.Vincular(servico, xml);

                    var xmlGeralValidado = false;

                    foreach (var (tipoCorreto, node) in nodes)
                    {
                        MontarInformacaoEspecifica(servico, tipoCorreto, inform); // sempre troca as tags assinatura

                        AssinarSeNecessario(xml, inform, certificado, configuracao, tipoAmbiente, tipoDFe);

                        if (!xmlGeralValidado)
                        {
                            ValidarSchemaGeral(xml, inform, tipoDFe);
                            xmlGeralValidado = true;
                        }
                        ValidarSchemaEspecifico(node, inform, tipoDFe);
                    }

                    return new ResultadoValidacao
                    {
                        Validado = true,
                        Descricao = inform.Descricao,
                        MensagemRetorno = "XML assinado e validado com sucesso.",
                        StatusValidacao = "1",
                        XmlAssinado = xml
                    };
                }

                catch (Exception ex)
                {
                    var status = ObterStatus(ex);

                    return new ResultadoValidacao
                    {
                        Validado = false,
                        Descricao = inform.Descricao,
                        MensagemRetorno = ex.Message,
                        StatusValidacao = status,
                        XmlAssinado = xml,
                    };
                }
            }
            catch (Exception ex)
            {
                var status = ObterStatus(ex);
                return new ResultadoValidacao
                {
                    Validado = false,
                    MensagemRetorno = ex.Message,
                    StatusValidacao = status,
                    XmlAssinado = xml,

                };
            }
        }



        private static XmlNode ObterServico(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {
            if (tipoDFe == TipoDFe.NFSe)
            {
                return TratarNFSe(xml, versao, tipoDFe, tagRaiz, xmlConfig, padraoNFSe);
            }

            if (tipoDFe == TipoDFe.ESocial || tipoDFe == TipoDFe.EFDReinf)
            {
                return TratarESocialEFDReinf(xml, versao, tipoDFe, tagRaiz, xmlConfig);
            }

            return TratarDFe(xml, versao, tipoDFe, tagRaiz, xmlConfig);
        }


        private static XmlNode TratarESocialEFDReinf(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig)
        {

            XmlNodeList nodeListDFe = xmlConfig.SelectNodes($"//{tipoDFe}/Servico");
            XmlNode nodeServicoCorreto = null;

            foreach (XmlNode nodeServico in nodeListDFe)
            {
                var tagIdentificadora = nodeServico.Attributes["tagIdentificadora"]?.Value;

                if (!tagIdentificadora.IsNullOrEmpty())
                {
                    var nodeServicoESocial = xml.DocumentElement.SelectSingleNode($"//*[local-name()='{tagIdentificadora}']");

                    if (!(nodeServicoESocial is null))
                    {
                        nodeServicoCorreto = nodeServico;
                        break;
                    }
                }
            }

            return nodeServicoCorreto;

        }


        private static XmlNode TratarNFSe(XmlDocument xml, string versao, TipoDFe tipoDFe, string tagRaiz, XmlDocument xmlConfig, PadraoNFSe padraoNFSe)
        {

            string pathServicosNFSe = string.Empty;
            XmlNode servicoNFSe = null;

            if (versao.IsNullOrEmpty())
            {
                pathServicosNFSe = $"//NFSe/Padrao[@nome='{padraoNFSe.ToString()}']/Servico[@tagRaiz='{tagRaiz}']";
                var servicosNFSe = xmlConfig.SelectNodes(pathServicosNFSe);

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


        private static InformacaoXML MontarInformacaoGeral(XmlNode servico, UFBrasil codigoMunicipio)
        {

            #region verifica se municipio usa certificado digital

            bool usaCertificado = VerificarUitlizacaoCertificadoDigital(servico, codigoMunicipio);

            #endregion

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
                TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText,

                //Criando a possibilidade de ser null para evitar o problema de atribuir o ambiente como produção caso a tag não exista.
                NaoAssina = servico.SelectSingleNode("*[local-name()='NaoAssina']")?.InnerText.ToLower() == "homologação" ? TipoAmbiente.Homologacao :
                (servico.SelectSingleNode("*[local-name()='NaoAssina']")?.InnerText.ToLower() == "produção" ? TipoAmbiente.Producao : (TipoAmbiente?)null),

                UsaCertificadoDigital = usaCertificado,

                GerarQRCode = servico.SelectSingleNode("*[local-name()='GerarQrCode']")?.InnerText?.Trim() == "true"

            };
        }

        private static bool VerificarUitlizacaoCertificadoDigital(XmlNode servico, UFBrasil codigoMunicipio)
        {
            var nodeCert = servico.SelectSingleNode("*[local-name()='UsaCertificadoDigital']");

            string valorCert = null;

            if (nodeCert != null)
            {
                var nodeExcecao = nodeCert.SelectSingleNode(
                    $"*[local-name()='Excecao' and @codMunicipio='{codigoMunicipio}']"
                );

                if (nodeExcecao != null)
                {
                    valorCert = nodeExcecao.InnerText;
                }
            }

            return valorCert?.Trim() != "false";
        }



        private static void MontarInformacaoEspecifica(XmlNode servico, XmlNode tipo, InformacaoXML inform)
        {
            inform.SchemaArquivo = tipo.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText;
            inform.SchemaArquivoEspecifico = tipo.SelectSingleNode("*[local-name()='SchemaArquivoEspecifico']")?.InnerText;
            inform.TargetNSEspecifico = tipo.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText ?? inform.TargetNS; // Caso o nó específico tenha uma TargetNS diferente da geral, utiliza a específica, caso contrário, mantém a geral
            inform.TagAtributoID = tipo.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText ?? inform.TagAtributoID; // Caso o nó específico tenha um atributo ID diferente da geral, utiliza o específico, caso contrário, mantém o geral
        }


        private void AssinarSeNecessario(XmlDocument xml, InformacaoXML inform, X509Certificate2 cert, Configuracao configuracao, TipoAmbiente tipoAmbiente, TipoDFe tipoDFe)
        {
            if (!string.IsNullOrEmpty(inform.TagAssinatura))
            {
                Assinar(xml, inform.TagAssinatura, inform.TagAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }

            if (!string.IsNullOrEmpty(inform.TagLoteAssinatura))
            {
                Assinar(xml, inform.TagLoteAssinatura, inform.TagLoteAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }

            if (!string.IsNullOrEmpty(inform.TagExtraAssinatura))
            {
                Assinar(xml, inform.TagExtraAssinatura, inform.TagExtraAtributoID, inform.NaoAssina, inform.UsaCertificadoDigital, inform.GerarQRCode, cert, tipoAmbiente, tipoDFe, configuracao);
            }

        }


        private void Assinar(XmlDocument xml,
            string tagAssinatura,
            string tagID,
            TipoAmbiente? tagNaoAssina,
            bool usaCertificado,
            bool gerarQrCode,
            X509Certificate2 cert,
            TipoAmbiente tipoAmbiente,
            TipoDFe tipoDFe,
            Configuracao configuracao)
        {

            if (string.IsNullOrWhiteSpace(tagAssinatura))
                return;
            var algoritmoAssinatura = AlgoritmoAssinatura(tipoDFe);

            if (usaCertificado)
            {
                if (tagNaoAssina is null || tagNaoAssina != tipoAmbiente)
                {
                    try
                    {
                        if (!AssinaturaDigital.EstaAssinado(xml, tagAssinatura))
                        {
                            AssinaturaDigital.Assinar(xml, tagAssinatura, tagID, cert, algoritmoAssinatura);


                        }

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
                    catch (Exception ex)
                    {

                        throw new AssinaturaException(
                            $"Ocorreu um erro ao assinar o XML: {ex.Message}");
                    }

                    MontarQRCode(xml, gerarQrCode, tipoDFe, configuracao);
                }
            }
        }


        private static AlgorithmType AlgoritmoAssinatura(TipoDFe tipoDFe)
        {
            switch (tipoDFe)
            {
                case TipoDFe.ESocial:
                case TipoDFe.EFDReinf:
                    return AlgorithmType.Sha256;
                default:
                    return AlgorithmType.Sha1;

            }
        }


        private static void MontarQRCode(XmlDocument xml, bool gerarQrCode, TipoDFe tipoDFe, Configuracao configuracao)
        {
            var geradorQrCode = QrCodeFactory.Criar(configuracao, gerarQrCode, tipoDFe);
            geradorQrCode?.GerarQrCode(xml, configuracao);
        }



        private static void AtribuirUrl(XmlNode servico, UFBrasil codigoUF, Configuracao configuracao)
        {
            foreach (XmlNode grupoUF in servico?.SelectNodes("GrupoUrl/Grupo"))
            {
                foreach (XmlNode uf in grupoUF.SelectNodes("UF"))
                {
                    if (uf.InnerText.Equals(codigoUF.ToString(), StringComparison.OrdinalIgnoreCase))
                    {
                        var urls = grupoUF.SelectSingleNode("Urls");

                        if (urls != null)
                        {
                            configuracao.UrlChaveHomologacao = urls["UrlChaveHomologacao"]?.InnerText;
                            configuracao.UrlChaveProducao = urls["UrlChaveProducao"]?.InnerText;
                            configuracao.UrlQrCodeHomologacao = urls["UrlQrCodeHomologacao"]?.InnerText;
                            configuracao.UrlQrCodeProducao = urls["UrlQrCodeProducao"]?.InnerText;
                        }

                        return;
                    }
                }
            }
        }


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe, PadraoNFSe padraoNFSe = PadraoNFSe.None)
        {
            // Caso não possua Schema para a validação retornar sem validar e deixar a validação por conta da prefeitura ao enviar
            if (string.IsNullOrEmpty(info.SchemaArquivo))
            {
                return;
            }

            var tipoBusca = tipoDFe == TipoDFe.NFCe ? TipoDFe.NFe : tipoDFe;

            string schema = padraoNFSe == PadraoNFSe.None
                ? $"{tipoBusca.ToString()}.{info.SchemaArquivo}"
                : $"{tipoBusca.ToString()}.{padraoNFSe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS, padraoNFSe);

            if (!validar.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema geral: {validar.ErrorMessage}.");
            }
        }


        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info, TipoDFe tipoDFe)
        {
            // Caso não possua Schema para a validação retornar sem validar e deixar a validação por conta da prefeitura ao enviar
            if (string.IsNullOrEmpty(info.SchemaArquivoEspecifico))
            {
                return;
            }

            //Isolando cada XML dependendo do tipoDFe
            var isolador = IsoladorFactory.CriarIsolador(tipoDFe);
            var xmlEspecifico = isolador.Isolar(eventoNode);

            var validarEspecifico = new ValidarSchema();
            string schemaEspecifico = $"{tipoDFe.ToString()}.{info.SchemaArquivoEspecifico}";
            validarEspecifico.Validar(xmlEspecifico, schemaEspecifico, info.TargetNSEspecifico);

            if (!validarEspecifico.Success)
            {
                throw new ValidarXMLException($"Erro ao validar schema específico: {validarEspecifico.ErrorMessage}.");
            }
        }



        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig, TipoDFe tipoDFe, PadraoNFSe padraoNFSe)
        {
            var servicoValidacao = xmlConfig.SelectSingleNode("ServicosValidacao");

            var nodeDFe = padraoNFSe == PadraoNFSe.None
            ? servicoValidacao.SelectSingleNode(tipoDFe.ToString())
            : servicoValidacao.SelectSingleNode($"{tipoDFe}/Padrao[@nome='{padraoNFSe}']");

            if (nodeDFe is null && padraoNFSe != PadraoNFSe.None)
            {
                throw new PadraoNaoImplementadoException($"Não foi possível encontrar a configuração para o padrão: {padraoNFSe}");
            }

            if (nodeDFe is null)
            {
                throw new Exception($"Não foi possível encontrar a configuração para o tipo de DFe: {tipoDFe}");
            }

            foreach (XmlNode nodeServico in nodeDFe.SelectNodes("Servico"))
            {
                var tagVersao = nodeServico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count > 0)
                {

                    var elemento = (XmlElement)nodeVersao[0];

                    var versaoAtributo = elemento.GetAttribute("versao");

                    versaoAtributo = string.IsNullOrWhiteSpace(versaoAtributo) ? elemento.GetAttribute("Versao") : versaoAtributo; // caso tenha atributo com primeira letra em uppercase

                    if (!string.IsNullOrEmpty(versaoAtributo))
                        return versaoAtributo;

                    var versaoValor = elemento.ChildNodes.Cast<XmlNode>().FirstOrDefault(x => x.NodeType == XmlNodeType.Text)?.Value; // pegando somente o valor direto do node para evitar erros

                    if (!string.IsNullOrEmpty(versaoValor))
                        return versaoValor;

                }
            }
            return string.Empty;
        }


        private string ObterStatus(Exception ex)
        {
            if (ex is AssinaturaException)
                return "4";

            if (ex is ValidarXMLException)
                return "2";

            if (ex is PadraoNaoImplementadoException)
                return "5";

            return "3";
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
                    var modeloDoc = xml.GetElementsByTagName("mod")[0]?.InnerText;

                    if (modeloDoc == ((int)ModeloDFe.NFCe).ToString())
                    {
                        tipoDFe = TipoDFe.NFCe;
                        break;
                    }

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

                case "eSocial":
                    tipoDFe = TipoDFe.ESocial;
                    break;
                case "Reinf":
                    tipoDFe = TipoDFe.EFDReinf;
                    break;

                default:
                    throw new Exception($"Não foi possível identificar o tipo do DFe pela tag raiz '{xml.DocumentElement.Name}'. " +
                        "Verifique se o XML está correto ou se o padrão da NFSe foi devidamente configurado.");
            }

            return tipoDFe;
        }


    }
}

