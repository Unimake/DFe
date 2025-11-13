using System;
using System.Collections.Generic;
using System.IO;
using System.Linq.Expressions;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;
using Xunit.Abstractions;


namespace Unimake.DFe.Test.Utility.TesteValidacao
{

    public class InformacaoXML
    {
        public string TagRaiz { get; set; }
        public string versao { get; set; }
        public string SchemaArquivo { get; set; }
        public string TargetNS { get; set; }
        public string TagAssinatura { get; set; }
        public string TagAtributoID { get; set; }
        public string TagLoteAssinatura { get; set; }
        public string TagLoteAtributoID { get; set; }
        public string TagExtraAssinatura { get; set; }
        public string TagExtraAtributoID { get; set; }

    }

    public class DicionarioServico
    {
        private readonly ITestOutputHelper _output;

        public DicionarioServico(ITestOutputHelper output)
        {
            _output = output;
        }
        private static readonly Dictionary<string, InformacaoXML> dicionario = new();


        // O parametro de caminhoArquivo foi utilizado de maneira para facilitar os testes locais, 
        // pórem ao utilizar pelo UniNFe o caminho não será passado e sim o XML em si. Ou outra forma que seja mais adequada.
        // O paametro caminhoServicoValidacao também será mudado para ser carregado internamente pelo UniNFe dependendo do local especifico.
        // LEMBRETE: implementar dicionario para envios de lote também.

        [Theory]
        [InlineData("C:\\Projetos\\GitHub\\DFe\\source\\Unimake.DFe.Test\\Utility\\TesteValidacao\\ServicoValidacao.xml", "consStatServ", "C:\\Users\\UnimakeLenovo\\Desktop\\testeDicionarioValidacao\\20100222T222310-ped-sta.xml")]
        [InlineData("C:\\Projetos\\GitHub\\DFe\\source\\Unimake.DFe.Test\\Utility\\TesteValidacao\\ServicoValidacao.xml", "consSitNFe", "C:\\Users\\UnimakeLenovo\\Desktop\\testeDicionarioValidacao\\99999999999999999999999999999999999999999993-ped-sit.xml")]
        public static void CaregarServico(string caminhoServicoValidacao, string tagRaiz, string caminhoArquivo) 
        {
            if (!File.Exists(caminhoServicoValidacao))
            {
                throw new Exception("Arquivo 'ServicoValidacao' não encontrado");
            }

            XmlDocument doCaminhoServicoValidacao = new();
            doCaminhoServicoValidacao.Load(caminhoServicoValidacao);


            if (!File.Exists(caminhoArquivo))
            {
                throw new Exception("Arquivo 'caminhoArquivo' não encontrado");
            }
          

            XmlDocument docCaminhoArquivo = new();
            docCaminhoArquivo.Load(caminhoArquivo);

            var versao = docCaminhoArquivo.DocumentElement.GetAttribute("versao");



            XmlNodeList servicos = doCaminhoServicoValidacao.GetElementsByTagName("Servico");

            foreach (XmlNode servico in servicos)
            {

                var informacaoXML = new InformacaoXML {

                    TagRaiz = servico.Attributes["tagRaiz"]?.Value,
                    versao = servico.Attributes["versao"]?.Value,
                    SchemaArquivo = servico.SelectSingleNode("*[local-name()='SchemaArquivo']")?.InnerText,
                    TargetNS = servico.SelectSingleNode("*[local-name()='TargetNS']")?.InnerText,
                    TagAssinatura = servico.SelectSingleNode("*[local-name()='TagAssinatura']")?.InnerText,
                    TagAtributoID = servico.SelectSingleNode("*[local-name()='TagAtributoID']")?.InnerText,
                    TagLoteAssinatura = servico.SelectSingleNode("*[local-name()='TagLoteAssinatura']")?.InnerText,
                    TagLoteAtributoID = servico.SelectSingleNode("*[local-name()='TagLoteAtributoID']")?.InnerText,
                    TagExtraAssinatura = servico.SelectSingleNode("*[local-name()='TagExtraAssinatura']")?.InnerText,
                    TagExtraAtributoID = servico.SelectSingleNode("*[local-name()='TagExtraAtributoID']")?.InnerText,

                };

                try
                {

                    dicionario[informacaoXML.TagRaiz] = informacaoXML;

                } catch (Exception e)
                {

                    Console.WriteLine($"Erro ao adicionar o serviço {informacaoXML.TagRaiz} no dicionário: {e.Message}");

                }


                if (informacaoXML.TagRaiz == tagRaiz && informacaoXML.versao == versao)
                {
                    // Serviço encontrado, pode prosseguir com a validação
                    Console.WriteLine($"Serviço encontrado: {informacaoXML.TagRaiz} com versão {informacaoXML.versao} e schema: {informacaoXML.SchemaArquivo}");
                }

            }


        }


  
     
        }





    } 