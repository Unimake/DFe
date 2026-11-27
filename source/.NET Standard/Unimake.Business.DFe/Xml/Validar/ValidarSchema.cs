#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Schema;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Validador de schemas de XML (XML x XSD)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.ValidarSchema")]
    [ComVisible(true)]
#endif
    public class ValidarSchema
    {
        private static readonly HashSet<string> SchemasNFeComLeiaute = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "nfe_v4.00.xsd",
            "enviNFe_v4.00.xsd",
            "procNFe_v4.00.xsd",
            "consReciNFe_v4.00.xsd",
            "retConsReciNFe_v4.00.xsd",
            "retEnviNFe_v4.00.xsd"
        };

        private static readonly Lazy<HashSet<string>> Recursos =
            new Lazy<HashSet<string>>(
                () => new HashSet<string>(
                    typeof(ValidarSchema).Assembly.GetManifestResourceNames(),
                    StringComparer.Ordinal),
                true);

        /// <summary>
        /// Erros ocorridos na validação
        /// </summary>
        private string ErroValidacao { get; set; }

        private bool SanitizarErrosAACBS { get; set; }
        private string RaizAACBS { get; set; }

        private static string SelecionarSchemaMonofasia(XmlDocument conteudoXML, string arqSchema)
        {
            var nomeSchema = SchemasNFeComLeiaute.FirstOrDefault(x =>
                string.Equals(arqSchema, x, StringComparison.OrdinalIgnoreCase) ||
                arqSchema.EndsWith("." + x, StringComparison.OrdinalIgnoreCase));
            if (nomeSchema == null)
            {
                return arqSchema;
            }

            var temGrupoLegado = false;
            var temGrupoAtual = false;
            var grupos = conteudoXML.GetElementsByTagName("gIBSCBSMono", "http://www.portalfiscal.inf.br/nfe");
            foreach (XmlNode grupo in grupos)
            {
                foreach (XmlNode filho in grupo.ChildNodes)
                {
                    switch (filho.LocalName)
                    {
                        case "gMonoPadrao":
                        case "gMonoReten":
                        case "gMonoRet":
                        case "gMonoDif":
                            temGrupoLegado = true;
                            break;

                        case "gIBSMonoAdRem":
                        case "gIBSMonoAdValorem":
                        case "gCBSMonoAdRem":
                        case "gCBSMonoAdValorem":
                            temGrupoAtual = true;
                            break;
                    }
                }
            }

            if (temGrupoLegado && temGrupoAtual)
            {
                throw new InvalidOperationException("O grupo gIBSCBSMono não pode misturar grupos do leiaute legado com grupos do leiaute atual.");
            }

            if (!temGrupoLegado)
            {
                return arqSchema;
            }

            var nomeLegado = Path.GetFileNameWithoutExtension(nomeSchema) + "_monofasia_legado.xsd";
            return arqSchema.Substring(0, arqSchema.Length - nomeSchema.Length) + nomeLegado;
        }

        private static string SelecionarSchemaCTeSimplificado(XmlDocument conteudoXML, string arqSchema)
        {
            if (conteudoXML == null)
            {
                return arqSchema;
            }

            string caminhoCTe;
            switch (arqSchema)
            {
                case "CTe.cteSimp_v4.00.xsd":
                    caminhoCTe = "/cte:CTeSimp";
                    break;
                case "CTe.procCTeSimp_v4.00.xsd":
                    caminhoCTe = "/cte:cteSimpProc/cte:CTeSimp";
                    break;
                default:
                    return arqSchema;
            }

            var namespaces = new XmlNamespaceManager(conteudoXML.NameTable);
            namespaces.AddNamespace("cte", "http://www.portalfiscal.inf.br/cte");
            var grupoLegado = conteudoXML.SelectSingleNode(caminhoCTe + "/cte:infCte/cte:imp/cte:IBSCBS", namespaces);

            // O schema legado aceita o grupo no cabeçalho e rejeita os grupos exclusivos do leiaute por prestação.
            return grupoLegado == null
                ? arqSchema
                : Path.GetFileNameWithoutExtension(arqSchema) + "_legado.xsd";
        }

        /// <summary>
        /// Extrai recursos (XSD) da DLL para efetuar a validação do XML, resolvendo recursivamente todos os includes/imports.
        /// </summary>
        /// <param name="arqSchema">Arquivo XSD a ser extraído</param>
        /// <param name="padraoNFSe">Padrão da NFSe (Necessário para determinar a subpasta de onde vai pegar o pacote de schemas)</param>
        /// <returns>Retorna os schemas a serem utilizados na validação</returns>
        private IEnumerable<XmlSchema> ExtractSchemasResource(string arqSchema, Servicos.PadraoNFSe padraoNFSe = Servicos.PadraoNFSe.None)
        {
            if (string.IsNullOrWhiteSpace(arqSchema))
            {
                throw new Exception("Arquivo de schema (XSD) não foi informado.");
            }

            var assembly = typeof(ValidarSchema).Assembly;
            var resources = Recursos.Value;

            // Ex.: Configuration.NamespaceSchema + "NFSe.NACIONAL.DPS_v1.01.xsd"
            var arquivoResource = Configuration.NamespaceSchema + arqSchema;

            // Descobre o "nome do arquivo" (parte final) para calcular o base path do resource
            var pesquisar = ".";
            if (padraoNFSe != Servicos.PadraoNFSe.None)
            {
                pesquisar += padraoNFSe.ToString() + ".";
            }

            var idx = arqSchema.IndexOf(pesquisar, StringComparison.Ordinal);
            var schemaPrincipal = (idx >= 0)
                ? arqSchema.Substring(idx + pesquisar.Length)
                : arqSchema; // fallback

            // Base do resource (prefixo até antes do nome do arquivo)
            var resourceBase = arquivoResource.EndsWith(schemaPrincipal, StringComparison.Ordinal)
                ? arquivoResource.Substring(0, arquivoResource.Length - schemaPrincipal.Length)
                : (Configuration.NamespaceSchema + arqSchema.Substring(0, Math.Max(0, arqSchema.Length - schemaPrincipal.Length)));

            // Controle do que já foi carregado (por resourceName) e do que já foi referenciado (por schemaLocation)
            var loadedResources = new HashSet<string>(StringComparer.Ordinal);
            var referencedLocations = new HashSet<string>(StringComparer.Ordinal);

            // Fila de processamento: (resourceName, logicalNameQueFechaORoot, inheritedTargetNamespace)
            // inheritedTargetNamespace é usado para xs:include/redefine "chameleon" (schema sem targetNamespace).
            var queue = new Queue<(string ResourceName, string LogicalName, string InheritedTargetNamespace)>();
            queue.Enqueue((arquivoResource, schemaPrincipal, null));
            referencedLocations.Add(schemaPrincipal);

            var files = new List<string>();

            string ResolveResourceName(string schemaLocation, string currentResourceName, string currentLogicalName)
            {
                // Normalizações básicas
                var loc = (schemaLocation ?? "").Trim();
                if (string.IsNullOrWhiteSpace(loc))
                {
                    return null;
                }

                var locNormalized = loc.Replace("\\", "/").TrimStart('.', '/');
                var fileOnly = Path.GetFileName(locNormalized);

                // Base do schema atual (prefixo até antes do nome do arquivo atual)
                var currentBase = (!string.IsNullOrWhiteSpace(currentLogicalName) &&
                                   currentResourceName.EndsWith(currentLogicalName, StringComparison.Ordinal))
                    ? currentResourceName.Substring(0, currentResourceName.Length - currentLogicalName.Length)
                    : resourceBase;

                // Candidatos (tenta mais de uma forma para suportar schemaLocation com pastas/paths)
                var candidates = new List<string>
                {
                    // relativo ao schema atual
                    currentBase + loc,
                    currentBase + loc.Replace("/", "."),
                    currentBase + fileOnly,
                    
                    // relativo ao schema principal
                    resourceBase + loc,
                    resourceBase + loc.Replace("/", "."),
                    resourceBase + fileOnly
                };

                foreach (var c in candidates.Distinct(StringComparer.Ordinal))
                {
                    if (resources.Contains(c))
                    {
                        return c;
                    }
                }

                // Fallback: procura por "qualquer resource que termine com .<arquivo>"
                // (ajuda se schemaLocation vier com subpasta mas o resource estiver "achatado")
                var suffix = "." + fileOnly;
                var bySuffix = resources
                    .Where(r => r.EndsWith(suffix, StringComparison.Ordinal))
                    .OrderBy(r => r, StringComparer.Ordinal)
                    .ToList();

                if (bySuffix.Count > 1)
                {
                    var proximidadeMaxima = bySuffix.Max(x => ObterTamanhoPrefixoComum(currentResourceName, x));
                    var maisProximos = bySuffix
                        .Where(x => ObterTamanhoPrefixoComum(currentResourceName, x) == proximidadeMaxima)
                        .ToList();

                    if (maisProximos.Count == 1)
                    {
                        return maisProximos[0];
                    }
                }

                if (bySuffix.Count == 1)
                {
                    return bySuffix[0];
                }

                if (bySuffix.Count > 1)
                {
                    throw new Exception(
                        "Referência de schema ambígua nos recursos da DLL.\r\n" +
                        "Schema principal: " + arqSchema + "\r\n" +
                        "schemaLocation: " + schemaLocation + "\r\n" +
                        "Recursos encontrados: " + string.Join(", ", bySuffix));
                }

                return null;
            }

            int ObterTamanhoPrefixoComum(string primeiro, string segundo)
            {
                var limite = Math.Min(primeiro?.Length ?? 0, segundo?.Length ?? 0);
                var indice = 0;

                while (indice < limite && primeiro[indice] == segundo[indice])
                {
                    indice++;
                }

                return indice;
            }

            while (queue.Count > 0)
            {
                var (resourceName, logicalName, inheritedTargetNamespace) = queue.Dequeue();

                if (loadedResources.Contains(resourceName))
                {
                    continue;
                }

                loadedResources.Add(resourceName);

                // Carregar o XSD do resource
                string xsdText;
                using (var stm = assembly.GetManifestResourceStream(resourceName))
                {
                    if (stm == null)
                    {
                        throw new Exception(
                            "Arquivo de schema referenciado não foi localizado nos recursos da DLL.\r\n" +
                            "Schema principal: " + arqSchema + "\r\n" +
                            "Resource buscado: " + resourceName + "\r\n" +
                            "Referência (schemaLocation): " + logicalName);
                    }

                    using (var reader = new StreamReader(stm))
                    {
                        xsdText = reader.ReadToEnd();
                    }
                }

                // Parse do XSD para achar includes/imports/redefine
                var doc = new XmlDocument
                {
                    XmlResolver = null
                };
                doc.LoadXml(xsdText);

                var schemaElement = doc.DocumentElement;
                var effectiveTargetNamespace = schemaElement?.GetAttribute("targetNamespace");

                // Trata xs:include/redefine "chameleon": se o XSD incluído não tem targetNamespace,
                // ele herda o targetNamespace do schema que incluiu.
                if (!string.IsNullOrWhiteSpace(inheritedTargetNamespace) &&
                    schemaElement != null &&
                    string.IsNullOrWhiteSpace(effectiveTargetNamespace))
                {
                    schemaElement.SetAttribute("targetNamespace", inheritedTargetNamespace);

                    if (string.IsNullOrWhiteSpace(schemaElement.GetAttribute("xmlns")))
                    {
                        schemaElement.SetAttribute("xmlns", inheritedTargetNamespace);
                    }

                    effectiveTargetNamespace = inheritedTargetNamespace;
                    xsdText = doc.OuterXml;
                }

                files.Add(xsdText);

                // Pega schemaLocation em include/import/redefine (filhos diretos do schema)
                var nodes = doc.SelectNodes(
                    "/*[local-name()='schema']/*[(local-name()='include' or local-name()='import' or local-name()='redefine') and @schemaLocation]");

                if (nodes == null || nodes.Count == 0)
                {
                    continue;
                }

                foreach (XmlNode node in nodes)
                {
                    var schemaLocation = node.Attributes?["schemaLocation"]?.Value?.Trim();
                    if (string.IsNullOrWhiteSpace(schemaLocation))
                    {
                        continue;
                    }

                    // Evita loop por schemaLocation (mesmo texto)
                    if (!referencedLocations.Add(schemaLocation))
                    {
                        continue;
                    }

                    var resolved = ResolveResourceName(schemaLocation, resourceName, logicalName);
                    if (string.IsNullOrWhiteSpace(resolved))
                    {
                        throw new Exception(
                            "Arquivo de schema referenciado via schemaLocation não foi localizado nos recursos da DLL.\r\n" +
                            "Schema principal: " + arqSchema + "\r\n" +
                            "Schema atual: " + logicalName + "\r\n" +
                            "schemaLocation: " + schemaLocation + "\r\n" +
                            "Dica: verifique se este XSD existe como Embedded Resource no mesmo pacote/pasta do schema principal.");
                    }

                    // logicalName aqui a gente passa o "nome do arquivo" que o resource termina
                    // (ajuda a recalcular currentBase corretamente)
                    var nextLogical = Path.GetFileName(schemaLocation.Replace("\\", "/"));

                    string nextInheritedTargetNamespace = null;
                    var relationType = node.LocalName;
                    if ((relationType == "include" || relationType == "redefine") && !string.IsNullOrWhiteSpace(effectiveTargetNamespace))
                    {
                        nextInheritedTargetNamespace = effectiveTargetNamespace;
                    }

                    queue.Enqueue((resolved, nextLogical, nextInheritedTargetNamespace));
                }
            }

            foreach (var file in files)
            {
                XmlSchema result;
                using (var reader = new StringReader(file))
                {
                    result = XmlSchema.Read(reader, null);
                }

                yield return result;
            }
        }

        /// <summary>
        /// Evento Executado em tempo de validação para retorno de erros
        /// </summary>
        /// <param name="sender">Object sender</param>
        /// <param name="e">Argumentos</param>
        private void Reader_ValidationEventHandler(object sender, ValidationEventArgs e) =>
            ErroValidacao += "Linha: " + e.Exception.LineNumber + " Coluna: " + e.Exception.LinePosition + " Erro: " +
            (SanitizarErrosAACBS ? "O XML AACBS de <" + RaizAACBS + "> não atende ao schema da operação." : e.Exception.Message) + "\r\n";

        /// <summary>
        /// Validar XML
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML as ser validado</param>
        /// <param name="settings">Parâmetros para validação</param>
        private void ValidateXMLAgainstSchema(string conteudoXML, XmlReaderSettings settings)
        {
            using (var xmlReader = XmlReader.Create(new StringReader(conteudoXML), settings))
            {
                ErroValidacao = "";

                try
                {
                    while (xmlReader.Read()) { }
                }
                catch (Exception ex)
                {
                    var mensagem = SanitizarErrosAACBS ? "O XML AACBS não pôde ser validado contra o schema da operação." : ex.Message;
                    if (!string.IsNullOrWhiteSpace(ErroValidacao))
                        ErroValidacao += mensagem;
                    else
                        ErroValidacao = mensagem;
                }

                xmlReader.Close();
            }
        }

        /// <summary>
        /// Código do erro em caso de falhas na validação
        /// </summary>
        public int ErrorCode { get; private set; }

        /// <summary>
        /// Mensagem de erro em caso de falhas na validação
        /// </summary>
        public string ErrorMessage { get; private set; }

        /// <summary>
        /// Se a validação foi bem sucedida (true/false)
        /// </summary>
        public bool Success { get; private set; }

        /// <summary>
        /// Método responsável por validar a estrutura do XML de acordo com o schema passado por parâmetro
        /// </summary>
        /// <param name="conteudoXML">Nome do arquivo XML a ser validado</param>
        /// <param name="arqSchema">Arquivo de schema para validação do XML (XSD) contido nos recursos da DLL.</param>
        /// <param name="targetNS">Target Name Space, se existir, para validação</param>
        /// <param name="padraoNFSe">Padrão da NFSe (Necessário para determinar a subpasta de onde vai pegar o pacote de schemas)</param>
        /// <example>
        /// //Validar arquivos de NFe
        /// Validar(xmlDocument, "NFe.consStatServCTe_v3.00.xsd")
        ///
        /// //Validar arquivos de CTe
        /// Validar(xmlDocument, "CTe.consStatServCTe_v3.00.xsd")
        ///
        /// //Validar arquivos de MDFe
        /// Validar(xmlDocument, "MDFe.consStatServ_v4.00.xsd")
        /// </example>
#if INTEROP
        [ComVisible(false)]
#endif
        public void Validar(XmlDocument conteudoXML, string arqSchema, string targetNS = "", Servicos.PadraoNFSe padraoNFSe = Servicos.PadraoNFSe.None)
        {
            ErroValidacao = "";
            SanitizarErrosAACBS = arqSchema != null && arqSchema.StartsWith("AACBS.", StringComparison.Ordinal);
            RaizAACBS = Unimake.Business.DFe.Xml.AACBS.AACBSFormat.IsRoot(conteudoXML?.DocumentElement?.LocalName ?? "")
                ? conteudoXML.DocumentElement.LocalName : "documento";

            if (string.IsNullOrEmpty(arqSchema))
            {
                throw new Exception("Arquivo de schema (XSD), necessário para validação do XML, não foi informado.");
            }

            Success = true;
            ErrorCode = 0;
            ErrorMessage = "";

            if (SanitizarErrosAACBS)
            {
                var schemaRaiz = arqSchema.Substring("AACBS.".Length);
                var versaoIndex = schemaRaiz.IndexOf("_v", StringComparison.Ordinal);
                var raizEsperada = versaoIndex > 0 ? schemaRaiz.Substring(0, versaoIndex) : string.Empty;
                if (!Unimake.Business.DFe.Xml.AACBS.AACBSFormat.IsDocument(conteudoXML?.DocumentElement) ||
                    !string.Equals(conteudoXML.DocumentElement.LocalName, raizEsperada, StringComparison.Ordinal))
                {
                    Success = false;
                    ErrorCode = 1;
                    ErrorMessage = "AACBS: raiz ou namespace incompatível com o schema da operação.";
                    return;
                }
            }

            arqSchema = SelecionarSchemaMonofasia(conteudoXML, arqSchema);
            arqSchema = SelecionarSchemaCTeSimplificado(conteudoXML, arqSchema);

            //if (padraoNFSe == Servicos.PadraoNFSe.None)
            //{
            var settings = new XmlReaderSettings
            {
                ValidationType = ValidationType.Schema,
                // Não habilitar ProcessSchemaLocation/ProcessInlineSchema para evitar buscar/usar schema do XML.
                // Não habilitar ReportValidationWarnings, ou gera erro na validação de alguns XMLs de eventos da NFe/NFCe.
                ValidationFlags = XmlSchemaValidationFlags.ProcessIdentityConstraints,
                DtdProcessing = DtdProcessing.Prohibit,
                XmlResolver = null
            };

            try
            {
                var schemas = new XmlSchemaSet
                {
                    XmlResolver = null // reforço: schemaSet também não resolve nada externo
                };

                settings.Schemas = schemas;

                //var resolver = new XmlUrlResolver
                //{
                //    Credentials = System.Net.CredentialCache.DefaultCredentials
                //};
                //settings.XmlResolver = resolver;

                //if (!string.IsNullOrWhiteSpace(targetNS))
                //{
                foreach (var schema in ExtractSchemasResource(arqSchema, padraoNFSe))
                {
                    settings.Schemas.Add(schema);
                }
                //}

                schemas.Compile(); //Pegar erros dos schemas aqui para não perder falhas na validação do XML

                settings.ValidationEventHandler += Reader_ValidationEventHandler;

                ValidateXMLAgainstSchema(SanitizarErrosAACBS
                    ? Utility.XMLUtility.ObterTextoXmlAACBS(conteudoXML)
                    : conteudoXML.OuterXml, settings);
            }
            catch (Exception ex)
            {
                ErroValidacao = (SanitizarErrosAACBS ? "Falha ao preparar a validação do schema AACBS." : ex.Message) + "\r\n";
            }

            if (ErroValidacao != "")
            {
                Success = false;
                ErrorCode = 1;
                ErrorMessage = "Início da validação...\r\n\r\n";
                ErrorMessage += ErroValidacao;
                ErrorMessage += "\r\n...Final da validação";
            }
            //}
        }

#if INTEROP

        /// <summary>
        /// Método responsável por validar a estrutura do XML de acordo com o schema passado por parâmetro
        /// </summary>
        /// <param name="path">Arquivo XML a ser validado</param>
        /// <param name="arqSchema">Arquivo de schema para validação do XML (XSD) contido nos recursos da DLL.</param>
        /// <param name="targetNS">Target Name Space, se existir, para validação</param>
        /// <example>
        /// //Validar arquivos de NFe
        /// Validar(@"C:\arquivo-procnfe.xml", "NFe.consStatServCTe_v3.00.xsd")
        ///
        /// //Validar arquivos de MDFe
        /// Validar(@"C:\arquivo-mdfe.xml", "MDFe.consStatServ_v4.00.xsd")
        /// </example>
        public void Validar(string path, string arqSchema, string targetNS = "")
        {
            var doc = CarregarDocumentoParaValidacao(documento => documento.Load(path), arqSchema);
            Validar(doc, arqSchema, targetNS);
        }

        /// <summary>
        /// Método responsável por validar a estrutura do XML de acordo com o schema passado por parâmetro
        /// </summary>
        /// <param name="xml">String do XML a ser validado</param>
        /// <param name="arqSchema">Arquivo de schema para validação do XML (XSD) contido nos recursos da DLL.</param>
        /// <param name="targetNS">Target Name Space, se existir, para validação</param>
        /// <example>
        /// //Validar arquivos de NFe
        /// Validar("string do xml a ser validado", "NFe.consStatServCTe_v3.00.xsd")
        ///
        /// //Validar arquivos de MDFe
        /// Validar("string do xml a ser validado", "MDFe.consStatServ_v4.00.xsd")
        /// </example>
        public void ValidarString(string xml, string arqSchema, string targetNS = "")
        {
            var doc = CarregarDocumentoParaValidacao(documento => documento.LoadXml(xml), arqSchema);

            Validar(doc, arqSchema, targetNS);
        }

        private static XmlDocument CarregarDocumentoParaValidacao(Action<XmlDocument> carregar, string arqSchema)
        {
            var aacbs = arqSchema != null && arqSchema.StartsWith("AACBS.", StringComparison.Ordinal);
            var documento = new XmlDocument { PreserveWhitespace = aacbs };
            try
            {
                carregar(documento);
                return documento;
            }
            catch (XmlException) when (aacbs)
            {
                // A exceção do parser pode conter trechos de texto opaco ou de segredo operacional.
                throw new XmlException("AACBS: não foi possível ler o XML para validação; verifique sua formação.");
            }
        }
#endif

    }
}
