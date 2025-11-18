#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Consultar histórico de alíquotas municipais do padrão NACIONAL
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarHistoricoAliquotasMunicipais")]
    [ComVisible(true)]
#endif
    public class ConsultarHistoricoAliquotasMunicipais : ServicoBase
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
        /// Construtor
        /// </summary>
        public ConsultarHistoricoAliquotasMunicipais() : base()
        { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="codigoMunicipio">Código do município</param>
        /// <param name="codigoServico">Código do serviço</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarHistoricoAliquotasMunicipais(int codigoMunicipio, string codigoServico, Configuracao configuracao) : base()
        {
            CodigoMunicipio = codigoMunicipio;
            CodigoServico = codigoServico;

            var xmlHistorico = CriarXMLHistoricoAliquotas();
            Inicializar(xmlHistorico, configuracao);
        }

        /// <summary>
        /// Construtor (compatibilidade com XML)
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarHistoricoAliquotasMunicipais(XmlDocument conteudoXML, Configuracao configuracao) : base()
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
        }

        /// <summary>
        /// Cria o XML para consulta de histórico de alíquotas
        /// </summary>
        private XmlDocument CriarXMLHistoricoAliquotas()
        {
            var xml = new XmlDocument();
            var xmlContent = $@"<?xml version=""1.0"" encoding=""utf-8""?>
<ConsultaParametros>
    <codigoMunicipio>{CodigoMunicipio}</codigoMunicipio>
    <codigoServico>{CodigoServico}</codigoServico>
    <tipoParametro>historicoaliquotas</tipoParametro>
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
                Configuracoes.Servico = Servico.NFSeConsultarHistoricoAliquotasMunicipais;
                Configuracoes.CodigoMunicipio = 1001058;
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