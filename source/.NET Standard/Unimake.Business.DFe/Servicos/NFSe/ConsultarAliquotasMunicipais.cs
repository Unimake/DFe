#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar alíquotas municipais do padrão NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarAliquotasMunicipais")]
    [ComVisible(true)]
#endif
    public class ConsultarAliquotasMunicipais : ServicoBase
    {
        /// <summary>
        /// Código do município (obrigatório)
        /// </summary>
        public int CodigoMunicipio { get; set; }

        /// <summary>
        /// Código do serviço (obrigatório)
        /// </summary>
        public string CodigoServico { get; set; }

        /// <summary>
        /// Data de competência (obrigatória)
        /// </summary>
        public DateTime Competencia { get; set; }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarAliquotasMunicipais() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="codigoMunicipio">Código do município</param>
        /// <param name="codigoServico">Código do serviço</param>
        /// <param name="competencia">Data de competência</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarAliquotasMunicipais(int codigoMunicipio, string codigoServico, DateTime competencia, Configuracao configuracao) : base()
        {
            CodigoMunicipio = codigoMunicipio;
            CodigoServico = codigoServico;
            Competencia = competencia;

            var xmlAliquotas = CriarXMLAliquotas();
            Inicializar(xmlAliquotas, configuracao);
        }

        /// <summary>
        /// Construtor (compatibilidade com XML)
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarAliquotasMunicipais(XmlDocument conteudoXML, Configuracao configuracao) : base()
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

            var codigoServicoNode = xml.GetElementsByTagName("codigoServico")[0];
            if (codigoServicoNode != null)
            {
                CodigoServico = codigoServicoNode.InnerText;
            }

            var competenciaNode = xml.GetElementsByTagName("competencia")[0];
            if (competenciaNode != null && DateTime.TryParse(competenciaNode.InnerText, out DateTime comp))
            {
                Competencia = comp;
            }
        }

        /// <summary>
        /// Cria o XML para consulta de alíquotas
        /// </summary>
        private XmlDocument CriarXMLAliquotas()
        {
            var xml = new XmlDocument();
            var xmlContent = $@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros>
    <codigoMunicipio>{CodigoMunicipio}</codigoMunicipio>
    <codigoServico>{CodigoServico}</codigoServico>
    <competencia>{Competencia:yyyy-MM-ddTHH:mm:ss}</competencia>
    <tipoParametro>aliquotas</tipoParametro>
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
                Configuracoes.Servico = Servico.NFSeConsultarAliquotasMunicipais;
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