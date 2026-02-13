#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar benefício municipal do padrão NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarBeneficioMunicipal")]
    [ComVisible(true)]
#endif
    public class ConsultarBeneficioMunicipal : ServicoBase
    {
        /// <summary>
        /// Código do município (obrigatório)
        /// </summary>
        public int CodigoMunicipio { get; set; }

        /// <summary>
        /// Número do benefício municipal (obrigatório)
        /// </summary>
        public string NumeroBeneficio { get; set; }

        /// <summary>
        /// Data de competência (obrigatória)
        /// </summary>
        public DateTime Competencia { get; set; }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarBeneficioMunicipal() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="codigoMunicipio">Código do município</param>
        /// <param name="numeroBeneficio">Número do benefício</param>
        /// <param name="competencia">Data de competência</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarBeneficioMunicipal(int codigoMunicipio, string numeroBeneficio, DateTime competencia, Configuracao configuracao) : base()
        {
            CodigoMunicipio = codigoMunicipio;
            NumeroBeneficio = numeroBeneficio;
            Competencia = competencia;

            var xmlBeneficio = CriarXMLBeneficio();
            Inicializar(xmlBeneficio, configuracao);
        }

        /// <summary>
        /// Construtor (compatibilidade com XML)
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarBeneficioMunicipal(XmlDocument conteudoXML, Configuracao configuracao) : base()
        {
            ExtrairDadosDoXML(conteudoXML);
            Inicializar(conteudoXML, configuracao);
        }

        /// <summary>
        /// Extrai os dados necessários do XML
        /// </summary>
        private void ExtrairDadosDoXML(XmlDocument xml)
        {
            var codigoMunicipioNode = xml.GetElementsByTagName("codigoMunicipio")[0];
            if (codigoMunicipioNode != null && int.TryParse(codigoMunicipioNode.InnerText, out int codigo))
            {
                CodigoMunicipio = codigo;
            }

            var numeroBeneficioNode = xml.GetElementsByTagName("numeroBeneficio")[0];
            if (numeroBeneficioNode != null)
            {
                NumeroBeneficio = numeroBeneficioNode.InnerText;
            }

            var competenciaNode = xml.GetElementsByTagName("competencia")[0];
            if (competenciaNode != null && DateTime.TryParse(competenciaNode.InnerText, out DateTime comp))
            {
                Competencia = comp;
            }
        }

        /// <summary>
        /// Cria o XML para consulta de benefício municipal
        /// </summary>
        private XmlDocument CriarXMLBeneficio()
        {
            var xml = new XmlDocument();
            var xmlContent = $@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros>
    <codigoMunicipio>{CodigoMunicipio}</codigoMunicipio>
    <numeroBeneficio>{NumeroBeneficio}</numeroBeneficio>
    <competencia>{Competencia:yyyy-MM-ddTHH:mm:ss}</competencia>
    <tipoParametro>beneficiomunicipal</tipoParametro>
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
                Configuracoes.Servico = Servico.NFSeConsultarBeneficioMunicipal;

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