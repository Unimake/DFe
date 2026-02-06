using Newtonsoft.Json;
using Org.BouncyCastle.Asn1.X509;
using Org.BouncyCastle.X509;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq.Expressions;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.Security;
using Unimake.DFe.Test.Utility.TesteValidacao.Vinculadores;
using Unimake.DFe.Test.Utility.TesteValidacao.Extractors;
using Xunit;
using Xunit.Abstractions;
using Unimake.DFe.Test.Utility.TesteValidacao.Isoladores;


namespace Unimake.DFe.Test.Utility.TesteValidacao
{

    public class InformacaoXML
    {
        public string TagRaiz { get; set; }
        public string Versao { get; set; }
        public string SchemaArquivo { get; set; }
        public string SchemasEspecifico { get; set; }
        public string SchemaArquivoEspecifico { get; set; }
        public string TagEvento { get; set; }
        public string TargetNS { get; set; }
        public string TagAssinatura { get; set; }
        public string TagAtributoID { get; set; }
        public string TagLoteAssinatura { get; set; }
        public string TagLoteAtributoID { get; set; }
        public string TagExtraAssinatura { get; set; }
        public string TagExtraAtributoID { get; set; }

        public override string ToString()
        {
            return
                $"TagRaiz: {TagRaiz}\n" +
                $"Versão: {Versao}\n" +
                $"SchemaArquivo: {SchemaArquivo}\n" +
                $"SchemasEspecifico: {SchemaArquivoEspecifico}\n" +
                $"TargetNS: {TargetNS}\n" +
                $"TagAssinatura: {TagAssinatura}\n" +
                $"TagAtributoID: {TagAtributoID}\n" +
                $"TagLoteAssinatura: {TagLoteAssinatura}\n" +
                $"TagLoteAtributoID: {TagLoteAtributoID}\n" +
                $"TagExtraAssinatura: {TagExtraAssinatura}\n" +
                $"TagExtraAtributoID: {TagExtraAtributoID}";
        }

    }

    public class ValidarXML
    {


        // Esse metodo irá ser chamado pelo UniNFe, onde ele passará o documento XML a ser validado e 
        // o certificado digital para assinar o XML caso necessário. Será utilizando o mesmo formato que o UniNFe faz para pegar o certificado
        // (CertificadoDigital = Empresas.Configuracoes[emp].X509Certificado)


        [Theory]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\eventoCTe_610111.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTe_ModalRodoviarioValido.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\consSitCTe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\4_00_CTe_ModalRodoviario.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTeOS-nfe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\CTe\CTeSimp.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\MDFe\EventoMDFePagamentoOperacaoMDFe.xml")]
        //[InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\MDFe\enviMDFe_ModalFerroviario.xml")]

        [InlineData(@"..\..\..\Utility\TesteValidacao\ConfigValidacao.xml", @"..\..\..\Utility\TesteValidacao\XMLteste\NFe\envEvento_211110.xml")]


        public static void CaregarServicoValidar(string caminhoServicoValidacao, string caminhoArquivo)
        {
            #region VaiSerApagada
            var CertificadoCaminho = @"C:\Projetos\Unimake_PV.pfx";
            var CertificadoSenha = "12345678";
            var certificado = new CertificadoDigital().CarregarCertificadoDigitalA1(CertificadoCaminho, CertificadoSenha);


            // Carregar config
            XmlDocument xmlConfig = new();
            xmlConfig.Load(caminhoServicoValidacao);

            if (!File.Exists(caminhoArquivo))
            {
                throw new Exception("Arquivo XML não encontrado");
            }

            XmlDocument xml = new(); // passar direto o XmlDocument  quando esse metodo for chamado no UniNFe
            xml.Load(caminhoArquivo);

            #endregion

            var tipoDFe = DetectarTipoDFe(xml);

            if (tipoDFe == TipoDFe.CTeOS) 
            {
                ValidarCTeOS(xml, certificado);
                return;
            }

            var tagRaiz = xml.DocumentElement.Name;
            var versao = ObterVersao(xml, xmlConfig);

            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {

                string tagRaizServico = servico.Attributes["tagRaiz"]?.Value;
                string versaoServico = servico.Attributes["versao"]?.Value;

                if (tagRaiz.Equals("NFe"))
                {
                    xml = MontarNFeEnvio(xml, versao);
                    tagRaiz = "enviNFe";

                }

                if (tagRaizServico != tagRaiz || versaoServico != versao)
                    continue;

             
                var inform = MontarInformacaoGeral(servico);
                AssinarSeNecessario(xml, inform, certificado);

                try { 
                if (servico.SelectSingleNode(".//*[local-name()='SchemasEspecificos']") is null) // Serviço sem schema específico então não precisa de validação Específica
                {
                    ValidarSchemaGeral(xml, inform, tipoDFe);
                    Console.WriteLine(inform);
                    return;
                }

                else if (!(servico.SelectSingleNode(".//*[local-name()='TagEvento']") is null))
                {
                    var vinculador = VinculadorFactory.CriarVinculadorEvento();
                    var eventos = VincularEventoSchema(servico, xml); // utilizar o metodo correto

                    foreach (var (tipoCorreto, eventoNode) in eventos)
                    {
                        MontarInformacaoEspecifica(servico, tipoCorreto, inform);
                        ValidarSchemaGeral(xml, inform, tipoDFe);
                        ValidarSchemaEspecifico(eventoNode, inform, tipoDFe);
                        Console.WriteLine(inform);
                    }
                    break;
                }


                else 
                {
                        var vinculador = VinculadorFactory.CriarVinculadorModal(tipoDFe);
                        var nodeVinculado = VincularEventoSchema(servico, xml);
                        foreach (var (tipoCorreto, cteNode) in nodeVinculado)
                        {
                            MontarInformacaoEspecifica(servico, tipoCorreto, inform);
                            ValidarSchemaGeral(xml, inform, tipoDFe);
                            ValidarSchemaEspecifico(cteNode, inform, tipoDFe);
                            Console.WriteLine(inform);
                        }
                        break;
                }
            }

        catch (Exception ex)
            {
                Console.WriteLine($"Erro ao validar schema específico: {ex.Message}");
                throw;
            }
        }
        return;
    }



        private static InformacaoXML MontarInformacaoGeral(XmlNode servico)
        {
            return new InformacaoXML
            {
                TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                Versao = servico.Attributes["versao"]?.Value,
                SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText,
                TagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText, // salvar tag evento no inform para poder fazer verificações futuras
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
            if (String.IsNullOrWhiteSpace(info.TagAssinatura))
                return;

            //| TipoDFe         | Algoritmo |
            //| --------------- | --------- | 
            //| NFe             | SHA1      | 
            //| CTe  CTeOS      | SHA1      | 
            //| MDFe            | SHA1      | 
            //| NFSe            | SHA1      | 
            //| Reinf           | SHA256    |
            //| eSocial         | SHA256    | 
            //| DARE            | SHA256    |

            try
            {
                AssinaturaDigital.Assinar(xml, info.TagAssinatura, info.TagAtributoID, cert, AlgorithmType.Sha1);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Erro ao assinar XML: {ex.Message}");
            }
        }


        private static void ValidarSchemaGeral(XmlDocument xml, InformacaoXML info, TipoDFe tipoDFe)
        {
            string schema = $"{tipoDFe.ToString()}.{info.SchemaArquivo}";

            var validar = new ValidarSchema();
            validar.Validar(xml, schema, info.TargetNS);

            if (!validar.Success)
            {
                throw new Exception($"Erro ao validar schema geral {validar.ErrorMessage}");
            }
        }


        private static void ValidarSchemaEspecifico(XmlNode eventoNode, InformacaoXML info, TipoDFe tipoDFe)
        {

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



        private static List<(XmlNode NodeIdCorreto, XmlNode eventoNode)> VincularEventoSchema(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode NodeIdCorreto, XmlNode eventoNode)>();

            var tagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText;
            if (string.IsNullOrWhiteSpace(tagEvento))
                throw new Exception("Tag de evento não configurada no serviço");

            XmlNodeList eventoXML = xml.GetElementsByTagName(tagEvento);
            if (eventoXML.Count == 0)
                throw new Exception($"Tag de evento '{tagEvento}' não encontrada no XML");

            XmlNode schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");

            foreach (XmlNode eventoNode in eventoXML)
            {
                var tpEventoNode = eventoNode.SelectSingleNode("*[local-name()='infEvento']/*[local-name()='tpEvento']");
                string tpEvento = tpEventoNode?.InnerText;

                if (string.IsNullOrWhiteSpace(tpEvento))
                    throw new Exception("Tag <tpEvento> não encontrada");

                XmlNode NodetipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, tpEvento);

                if (NodetipoCorreto is null)
                    throw new Exception($"Não existe Schema Específico configurado para o evento {tpEvento}");

                lista.Add((NodetipoCorreto, eventoNode));
            }

            return lista;
        }



        private static string ObterVersao(XmlDocument xml, XmlDocument xmlConfig)
        {
            // Versão como atributo da raiz
            var versao = xml.DocumentElement.GetAttribute("versao");
            if (!string.IsNullOrEmpty(versao))
                return versao;

            // Buscar em cada Servico configurado pela TagVersao
            XmlNodeList servicos = xmlConfig.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {
                var tagVersao = servico.SelectSingleNode("*[local-name()='TagVersao']")?.InnerText;

                if (string.IsNullOrWhiteSpace(tagVersao))
                    continue;

                var nodeVersao = xml.GetElementsByTagName(tagVersao);

                if (nodeVersao.Count > 0)
                {
                    versao = ((XmlElement)nodeVersao[0]).GetAttribute("versao");
                    if (!string.IsNullOrEmpty(versao))
                        return versao;
                }
            }

            return string.Empty;
        }


        private static XmlDocument MontarNFeEnvio(XmlDocument xml, string versao)
        {
            var nfeNode = xml.GetElementsByTagName("NFe")[0];

            var xmlNFe = $"<enviNFe versao=\"{versao}\" xmlns=\"http://www.portalfiscal.inf.br/nfe\">" + "<idLote>000000000000001</idLote>"
                         + "<indSinc>0</indSinc>" + nfeNode.OuterXml +
                         "</enviNFe>";

            var doc = new XmlDocument();
            doc.LoadXml(xmlNFe);

            return doc;

        }



        private static List<(XmlNode NodeIdCorreto, XmlNode NodeInfCTe)> VincularCTeSchema(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode NodeIdCorreto, XmlNode NodeInfCTe)>();
            var schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");

            if (schemasEspecificos is null)
                throw new Exception("Configuração de SchemasEspecificos não encontrada para CTe");

            var nodeCTe = xml.GetElementsByTagName("CTe");

            if (nodeCTe is null)
                throw new Exception("tag CTe não encontrada");

            foreach (XmlElement node in nodeCTe)
            {
                foreach (XmlElement nodeInfCte in node.GetElementsByTagName("infCte"))
                {
                    foreach (XmlElement ideNode in nodeInfCte.GetElementsByTagName("ide"))
                    {
                        var modal = ideNode.GetElementsByTagName("modal")[0]?.InnerText;

                        if (string.IsNullOrWhiteSpace(modal))
                            throw new Exception("Tag <modal> não encontrada ou está vazia");

                        // Extrai apenas o segundo caractere do modal
                        string modalCorreto = modal.Substring(1, 1);

                        XmlNode nodeTipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, modalCorreto);

                        if (nodeTipoCorreto is null)
                            throw new Exception($"Não existe Schema Específico configurado para o modal {modalCorreto}");

                        lista.Add((nodeTipoCorreto, nodeInfCte));
                    }
                }
            }

            return lista;
        }



        private static XmlNode EncontrarTipoSchemaPorId(XmlNode schemasEspecificos, string id)
        {
            if (schemasEspecificos is null)
                throw new Exception("Configuração de SchemasEspecificos não encontrada");

            if (string.IsNullOrWhiteSpace(id))
                throw new ArgumentException("ID não pode ser nulo ou vazio", nameof(id));

            foreach (XmlNode tipo in schemasEspecificos.SelectNodes("*[local-name()='Tipo']"))
            {
                string tipoId = tipo.SelectSingleNode("*[local-name()='ID']")?.InnerText;
                if (tipoId == id)
                    return tipo;
            }

            return null;
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
                    tipoDFe = TipoDFe.CTe;
                    break;

                case "CTeOS":
                    tipoDFe = TipoDFe.CTeOS;
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
                    tipoDFe = TipoDFe.Desconhecido;
                    break;
            }

            return tipoDFe;
        }



        private static void ValidarCTeOS(XmlDocument xml, X509Certificate2 certificado)
        {
            try
            {
                // 1️ Converter XmlDocument para string para ValidatorFactory
                string xmlString = xml.OuterXml;

                // 2️ Usar ValidatorFactory para encontrar e executar CTeOSValidator
                var validatorFactory = new ValidatorFactory();
                var validator = validatorFactory.BuidValidator(xmlString);

                if (validator is null)
                {
                    throw new Exception("Nenhum validador encontrado para CTeOS. Verifique se o XML tem a estrutura correta.");
                }

                // 3️ Executar validações de negócio do CTeOS
                if (!validator.Validate())
                {
                    throw new Exception("Validação de negócio do CTeOS falhou. Verifique as regras de CPF/CNPJ, códigos de município, etc.");
                }

                
            }
            catch (Exception ex)
            {
                throw new Exception($"Erro ao validar CTeOS: {ex.Message}", ex);
            }
        }

    }
}

