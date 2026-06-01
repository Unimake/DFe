#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using System;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Gerar identificador da operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class GerarIdOperacaoTransporte : ServicoBase<Xml.CIOT.GerarIdOperacaoTransporte, RetGerarIdOperacaoTransporte>, IInteropService<Xml.CIOT.GerarIdOperacaoTransporte>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGerarIdOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte(Xml.CIOT.GerarIdOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(NormalizarEnvio(xml), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            InicializarServico(NormalizarEnvio(new Xml.CIOT.GerarIdOperacaoTransporte().LerXML<Xml.CIOT.GerarIdOperacaoTransporte>(doc)), configuracao);
        }

        /// <summary>
        /// Processar retorno bruto recebido da API ANTT
        /// </summary>
        /// <param name="retorno">Conteúdo JSON ou texto puro recebido da API.</param>
        /// <returns>Retorno XML tipado.</returns>
        public RetGerarIdOperacaoTransporte ProcessarRetornoANTT(string retorno)
        {
            if (string.IsNullOrWhiteSpace(retorno))
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            var doc = new XmlDocument();
            var conteudo = retorno.Trim();

            if (Regex.IsMatch(conteudo, @"^[0-9]{12}$"))
            {
                doc.LoadXml("<temp><IdOperacaoTransporte>" + conteudo + "</IdOperacaoTransporte></temp>");
            }
            else
            {
                try
                {
                    doc = JsonConvert.DeserializeXmlNode(conteudo, "temp");
                }
                catch (JsonException ex)
                {
                    throw new InvalidOperationException("A API ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.", ex);
                }
            }

            if (doc?.DocumentElement == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.");
            }

            RetornoWSXML = doc;
            return Result;
        }

        /// <inheritdoc />
        protected override XmlDocument CriarXMLRetornoTipado()
        {
            var retorno = RetornoWSXML;
            var root = retorno?.DocumentElement;

            if (root == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            if (root.LocalName == nameof(RetGerarIdOperacaoTransporte))
            {
                return retorno;
            }

            if (root.LocalName == "temp" && root["error"] != null)
            {
                var retornoErro = base.CriarXMLRetornoTipado();
                return new RetGerarIdOperacaoTransporte().LerXML<RetGerarIdOperacaoTransporte>(retornoErro).GerarXML();
            }

            var idOperacaoTransporte = ObterValor(root, "IdOperacaoTransporte") ?? ObterValor(root, "idOperacaoTransporte");
            if (Regex.IsMatch(idOperacaoTransporte ?? "", @"^[0-9]{12}$"))
            {
                return new RetGerarIdOperacaoTransporte
                {
                    Versao = "1.00",
                    IdOperacaoTransporte = idOperacaoTransporte,
                    Codigo = "110",
                    Mensagem = "IdOperacaoTransporte gerado com sucesso."
                }.GerarXML();
            }

            var mensagem = ObterValor(root, "Message") ?? ObterValor(root, "message");
            if (!string.IsNullOrWhiteSpace(mensagem))
            {
                return new RetGerarIdOperacaoTransporte
                {
                    Versao = "1.00",
                    Codigo = "999",
                    Mensagem = mensagem
                }.GerarXML();
            }

            if (Regex.IsMatch(root.InnerText?.Trim() ?? "", @"^[0-9]{12}$"))
            {
                return new RetGerarIdOperacaoTransporte
                {
                    Versao = "1.00",
                    IdOperacaoTransporte = root.InnerText.Trim(),
                    Codigo = "110",
                    Mensagem = "IdOperacaoTransporte gerado com sucesso."
                }.GerarXML();
            }

            throw new InvalidOperationException("A API ANTT retornou conteúdo inesperado ao gerar o IdOperacaoTransporte: " + retorno.OuterXml);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.GerarIdOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(NormalizarEnvio(xml), configuracao);
            Executar();
        }
#endif

        private static Xml.CIOT.GerarIdOperacaoTransporte NormalizarEnvio(Xml.CIOT.GerarIdOperacaoTransporte xml)
        {
            if (xml == null)
            {
                throw new ArgumentNullException(nameof(xml));
            }

            if (!Regex.IsMatch(xml.CpfCnpj ?? "", @"^[0-9.\-/\s]+$"))
            {
                throw new ArgumentException("CPF/CNPJ deve conter somente dígitos ou caracteres de máscara.", nameof(xml));
            }

            xml.CpfCnpj = Regex.Replace(xml.CpfCnpj, @"[^0-9]", "");
            if (!Regex.IsMatch(xml.CpfCnpj, @"^[0-9]{11}$|^[0-9]{14}$"))
            {
                throw new ArgumentException("CPF/CNPJ deve conter 11 ou 14 dígitos.", nameof(xml));
            }

            xml.Versao = "1.00";
            return xml;
        }

        private static string ObterValor(XmlElement root, string nome)
        {
            if (root.LocalName == nome)
            {
                return root.InnerText;
            }

            var nodes = root.GetElementsByTagName(nome);
            return nodes.Count > 0 ? nodes[0].InnerText : null;
        }
    }
}
