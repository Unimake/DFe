#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar parâmetros de convênio municipal do padrão NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarConvenioMunicipal")]
    [ComVisible(true)]
#endif
    public class ConsultarConvenioMunicipal : ServicoBase
    {
        /// <summary>
        /// Código do município (obrigatório)
        /// </summary>
        public int CodigoMunicipio { get; set; }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarConvenioMunicipal() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="codigoMunicipio">Código do município</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarConvenioMunicipal(int codigoMunicipio, Configuracao configuracao) : base()
        {
            CodigoMunicipio = codigoMunicipio;
            
            var xmlConvenio = CriarXMLConvenio();
            Inicializar(xmlConvenio, configuracao);
        }

        /// <summary>
        /// Construtor (compatibilidade com XML)
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarConvenioMunicipal(XmlDocument conteudoXML, Configuracao configuracao) : base()
        {
            // Extrair código do município do XML se não foi definido
            if (CodigoMunicipio == 0)
            {
                var codigoMunicipioNode = conteudoXML.GetElementsByTagName("codigoMunicipio")[0];
                if (codigoMunicipioNode != null && int.TryParse(codigoMunicipioNode.InnerText, out int codigo))
                {
                    CodigoMunicipio = codigo;
                }
            }
            
            Inicializar(conteudoXML, configuracao);
        }

        /// <summary>
        /// Cria o XML para consulta de convênio
        /// </summary>
        private XmlDocument CriarXMLConvenio()
        {
            var xml = new XmlDocument();
            var xmlContent = $@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros>
    <codigoMunicipio>{CodigoMunicipio}</codigoMunicipio>
    <tipoParametro>convenio</tipoParametro>
</ConsultaParametros>";
            xml.LoadXml(xmlContent);
            return xml;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFSeConsultarConvenioMunicipal;
                Configuracoes.CodigoMunicipio =  1001058;
                Configuracoes.SchemaVersao = "1.00";

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            base.Executar();
        }
    }
}