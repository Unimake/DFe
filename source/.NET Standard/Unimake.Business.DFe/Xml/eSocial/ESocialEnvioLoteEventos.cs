#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// Envio lote de eventos do eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1", IsNullable = true)]
    public class ESocialEnvioLoteEventos : XMLBaseESocial
    {
        /// <summary>
        /// Versão do schema do XML de lote de eventos
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        /// <summary>
        /// Representa um lote de eventos do eSocial
        /// </summary>
        [XmlElement("envioLoteEventos")]
        public EnvioLoteEventosESocial EnvioLoteEventos { get; set; }

        /// <summary>
        /// Desserializar o XML de Lote de Eventos do eSocial no objeto ESocialEnvioLoteEventos
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de lote de eventos do eSocial</param>
        /// <returns>Objeto do ESocialEnvioLoteEventos</returns>
        public ESocialEnvioLoteEventos LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));

            return base.LerXML<ESocialEnvioLoteEventos>(doc);
        }

        /// <summary>
        /// Desserializar o XML de Lote de Eventos do eSocial no objeto ESocialEnvioLoteEventos
        /// </summary>
        /// <param name="xml">string do XML de lote de eventos do eSocial</param>
        /// <returns>Objeto do ESocialEnvioLoteEventos</returns>
        public ESocialEnvioLoteEventos LoadFromXML(string xml)
        {
            var doc = new XmlDocument();
            doc.LoadXml(xml);

            return base.LerXML<ESocialEnvioLoteEventos>(doc);
        }
    }

    /// <summary>
    /// Representa um lote de eventos do eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")]
    [ComVisible(true)]
#endif
    public class EnvioLoteEventosESocial
    {
        /// <summary>
        /// Informação do grupo de eventos
        /// </summary>
        [XmlAttribute(AttributeName = "grupo", DataType = "token")]
        public string Grupo { get; set; }

        /// <summary>
        /// Identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do transmissor (certificado digital)
        /// </summary>
        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        /// <summary>
        /// Contém a relação de eventos que compõe o lote
        /// </summary>
        [XmlElement("eventos")]
        public EventosESocial Eventos { get; set; }
    }

    /// <summary>
    /// Identificação do transmissor (certificado digital)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")]
    [ComVisible(true)]
#endif
    public class IdeTransmissor
    {
        /// <summary>
        /// Preencher com a opção conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    /// <summary>
    /// Contém a relação de eventos que compõe o lote
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventosESocial")]
    [ComVisible(true)]
#endif
    public class EventosESocial
    {
        /// <summary>
        /// Define os dados de um evento do eSocial
        /// </summary>
        [XmlElement("evento")]
        public List<EventoESocial> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(EventoESocial item)
        {
            if (Evento == null)
            {
                Evento = new List<EventoESocial>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public EventoESocial GetEvento(int index)
        {
            if ((Evento?.Count ?? 0) == 0)
            {
                return default;
            };

            return Evento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Evento
        /// </summary>
        public int GetEventoCount => (Evento != null ? Evento.Count : 0);

#endif
    }

    /// <summary>
    /// Define os dados de um evento do eSocial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventoESocial")]
    [ComVisible(true)]
#endif
    public class EventoESocial
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// S-1000 - Informações do Empregador/Contribuinte/Órgão Público
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_02_00")]
        public ESocial1000 ESocial1000 { get; set; }

        /// <summary>
        /// S-1005 - Tabela de Estabelecimentos, Obras ou Unidades de Órgãos Públicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabEstab/v_S_01_02_00")]
        public ESocial1005 ESocial1005 { get; set; }

        /// <summary>
        /// S-1010 - Tabela de Rubricas
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabRubrica/v_S_01_02_00")]
        public ESocial1010 ESocial1010 { get; set; }

        /// <summary>
        ///S-1020 - Tabela de Lotações Tributárias
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabLotacao/v_S_01_02_00")]
        public ESocial1020 ESocial1020 { get; set; }

        /// <summary>
        /// S-1070 - Tabela de Processos Administrativos/Judiciais
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabProcesso/v_S_01_02_00")]
        public ESocial1070 ESocial1070 { get; set; }

        /// <summary>
        /// S-1200 - Remuneração de Trabalhador vinculado ao Regime Geral de Previd. Social
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRemun/v_S_01_02_00")]
        public ESocial1200 ESocial1200 { get; set; }

        /// <summary>
        /// S-1202 - Remuneração de Servidor vinculado ao Regime Próprio de Previd. Social
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRmnRPPS/v_S_01_02_00")]
        public ESocial1202 ESocial1202 { get; set; }

        /// <summary>
        /// S-1207 - Benefícios - Entes Públicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBenPrRP/v_S_01_02_00")]
        public ESocial1207 ESocial1207 { get; set; }

        /// <summary>
        /// S-1210 - Pagamentos de Rendimentos do Trabalho
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtPgtos/v_S_01_02_00")]
        public ESocial1210 ESocial1210 { get; set; }

        /// <summary>
        /// S-1260 - Comercialização da Produção Rural Pessoa Física
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtComProd/v_S_01_02_00")]
        public ESocial1260 ESocial1260 { get; set; }

        /// <summary>
        /// S-1270 - Contratação de Trabalhadores Avulsos Não Portuários
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContratAvNP/v_S_01_02_00")]
        public ESocial1270 ESocial1270 { get; set; }

        /// <summary>
        /// S-1280 - Informações Complementares aos Eventos Periódicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoComplPer/v_S_01_02_00")]
        public ESocial1280 ESocial1280 { get; set; }

        /// <summary>
        /// S-1298 - Reabertura dos Eventos Periódicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReabreEvPer/v_S_01_02_00")]
        public ESocial1298 ESocial1298 { get; set; }

        /// <summary>
        /// S-1299 - Fechamento dos Eventos Periódicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFechaEvPer/v_S_01_02_00")]
        public ESocial1299 ESocial1299 { get; set; }

        /// <summary>
        /// S-2190 - Registro Preliminar de Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmPrelim/v_S_01_02_00")]
        public ESocial2190 ESocial2190 { get; set; }

        /// <summary>
        /// S-2200 - Cadastramento Inicial do Vínculo e Admissão/Ingresso de Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmissao/v_S_01_02_00")]
        public ESocial2200 ESocial2200 { get; set; }

        /// <summary>
        /// S-2205 - Alteração de Dados Cadastrais do Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltCadastral/v_S_01_02_00")]
        public ESocial2205 ESocial2205 { get; set; }

        /// <summary>
        /// S-2206 - Evento Alteração de Contrato de Trabalho/Relação Estatutária.
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltContratual/v_S_01_02_00")]
        public ESocial2206 ESocial2206 { get; set; }

        /// <summary>
        /// S-2210 - Comunicação de Acidente de Trabalho
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCAT/v_S_01_02_00")]
        public ESocial2210 ESocial2210 { get; set; }

        /// <summary>
        /// S-2220 - Monitoramento da Saúde do Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00")]
        public ESocial2220 ESocial2220 { get; set; }

        /// <summary>
        /// S-2221 - Exame Toxicológico do Motorista Profissional Empregado
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_02_00")]
        public ESocial2221 ESocial2221 { get; set; }

        /// <summary>
        /// S-2230 - Afastamento Temporário
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAfastTemp/v_S_01_02_00")]
        public ESocial2230 ESocial2230 { get; set; }

        /// <summary>
        /// S-2231 - Cessão/Exercício em Outro Órgão
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCessao/v_S_01_02_00")]
        public ESocial2231 ESocial2231 { get; set; }

        /// <summary>
        /// S-2240 - Condições Ambientais do Trabalho - Agentes Nocivos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00")]
        public ESocial2240 ESocial2240 { get; set; }

        /// <summary>
        /// S-2298 - Reintegração/Outros Provimentos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReintegr/v_S_01_02_00")]
        public ESocial2298 ESocial2298 { get; set; }

        /// <summary>
        /// S-2299 - Desligamento
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtDeslig/v_S_01_02_00")]
        public ESocial2299 ESocial2299 { get; set; }

        /// <summary>
        /// S-2300 - Trabalhador Sem Vínculo de Emprego/Estatutário - Início
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVInicio/v_S_01_02_00")]
        public ESocial2300 ESocial2300 { get; set; }

        /// <summary>
        /// S-2306 - Trabalhador Sem Vínculo de Emprego/Estatutário - Alteração Contratual
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVAltContr/v_S_01_02_00")]
        public ESocial2306 ESocial2306 { get; set; }

        /// <summary>
        /// S-2399 - Trabalhador Sem Vínculo de Emprego/Estatutário - Término
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVTermino/v_S_01_02_00")]
        public ESocial2399 ESocial2399 { get; set; }

        /// <summary>
        /// S-2400 - Cadastro de Beneficiário - Entes Públicos - Início
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefIn/v_S_01_02_00")]
        public ESocial2400 ESocial2400 { get; set; }

        /// <summary>
        /// S-2405 - Cadastro de Beneficiário - Entes Públicos - Alteração
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefAlt/v_S_01_02_00")]
        public ESocial2405 ESocial2405 { get; set; }

        /// <summary>
        /// S-2410 - Cadastro de Benefício - Entes Públicos - Início
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenIn/v_S_01_02_00")]
        public ESocial2410 ESocial2410 { get; set; }

        /// <summary>
        /// S-2416 - Cadastro de Benefício - Entes Públicos - Alteração
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenAlt/v_S_01_02_00")]
        public ESocial2416 ESocial2416 { get; set; }

        /// <summary>
        /// S-2418 - Reativação de Benefício - Entes Públicos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReativBen/v_S_01_02_00")]
        public ESocial2418 ESocial2418 { get; set; }

        /// <summary>
        /// S-2420 - Cadastro de Benefício - Entes Públicos - Término
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenTerm/v_S_01_02_00")]
        public ESocial2420 ESocial2420 { get; set; }

        /// <summary>
        /// S-2500 - Processo Trabalhista
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtProcTrab/v_S_01_02_00")]
        public ESocial2500 ESocial2500 { get; set; }

        /// <summary>
        /// S-2501 - Informações de Tributos Decorrentes de Processo Trabalhista
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContProc/v_S_01_02_00")]
        public ESocial2501 ESocial2501 { get; set; }

        /// <summary>
        /// S-3000 - Exclusão de Eventos
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExclusao/v_S_01_02_00")]
        public ESocial3000 ESocial3000 { get; set; }

        /// <summary>
        /// S-3500 - Evento Informações do Empregador. eSocial - 3500
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExcProcTrab/v_S_01_02_00")]
        public ESocial3500 ESocial3500 { get; set; }

        /// <summary>
        /// S-5001 - Informações das Contribuições Sociais por Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_02_00")]
        public ESocial5001 ESocial5001 { get; set; }

        /// <summary>
        /// S-5002 - Imposto de Renda Retido na Fonte por Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_02_00")]
        public ESocial5002 ESocial5002 { get; set; }

        /// <summary>
        /// S-5003 - Informações do FGTS por Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesFGTS/v_S_01_02_00")]
        public ESocial5003 ESocial5003 { get; set; }

        /// <summary>
        /// S-5011 - Informações das Contribuições Sociais Consolidadas por Contribuinte
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_02_00")]
        public ESocial5011 ESocial5011 { get; set; }

        /// <summary>
        /// S-5012 - Imposto de Renda Retido na Fonte Consolidado por Contribuinte
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrf/v_S_01_02_00")]
        public ESocial5012 ESocial5012 { get; set; }

        /// <summary>
        /// S-5013 - Informações do FGTS Consolidadas por Contribuinte
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTS/v_S_01_02_00")]
        public ESocial5013 ESocial5013 { get; set; }

        /// <summary>
        /// S-5501 - Informações Consolidadas de Tributos Decorrentes de Processo Trabalhista
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTribProcTrab/v_S_01_02_00")]
        public ESocial5501 ESocial5501 { get; set; }

        /// <summary>
        /// S-5503 - Informações do FGTS por Trabalhador em Processo Trabalhista
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTSProcTrab/v_S_01_02_00")]
        public ESocial5503 ESocial5503 { get; set; }

        /// <summary>
        /// S-8200 - Anotação Judicial do Vínculo
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAnotJud/v_S_01_02_00")]
        public ESocial8200 ESocial8200 { get; set; }

        /// <summary>
        /// S-8299 - Baixa Judicial do Vínculo
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBaixa/v_S_01_02_00")]
        public ESocial8299 ESocial8299 { get; set; }

        /// <summary>
        /// Retorna códigos dos eventos que estão sendo enviados no lote
        /// </summary>
        [XmlIgnore]
        public int TipoEvento
        {
            get
            {
                var propriedades = GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

                var tipoEvento = 0;
                foreach (var propriedade in propriedades)
                {
                    if (propriedade.PropertyType.Name.StartsWith("ESocial"))
                    {
                        var valor = propriedade.GetValue(this);

                        if (valor != null)
                        {
                            tipoEvento = Convert.ToInt32(propriedade.PropertyType.Name.Substring(7, 4));
                            break;

                        }
                    }
                }

                return tipoEvento;
            }
        }
    }
}
