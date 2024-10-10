#pragma warning disable CS1591

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
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1", IsNullable = true)]
    public class ESocialEnvioLoteEventos : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

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
            return XMLUtility.Deserializar<ESocialEnvioLoteEventos>(doc);
        }

        /// <summary>
        /// Desserializar o XML de Lote de Eventos do eSocial no objeto ESocialEnvioLoteEventos
        /// </summary>
        /// <param name="xml">string do XML de lote de eventos do eSocial</param>
        /// <returns>Objeto do ESocialEnvioLoteEventos</returns>
        public ESocialEnvioLoteEventos LoadFromXML(string xml) => XMLUtility.Deserializar<ESocialEnvioLoteEventos>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")]
    [ComVisible(true)]
#endif
    public class EnvioLoteEventosESocial
    {
        [XmlAttribute(AttributeName = "grupo", DataType = "token")]
        public string Grupo { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        [XmlElement("eventos")]
        public EventosESocial Eventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")]
    [ComVisible(true)]
#endif
    public class IdeTransmissor
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventosESocial")]
    [ComVisible(true)]
#endif
    public class EventosESocial
    {
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventoESocial")]
    [ComVisible(true)]
#endif
    public class EventoESocial
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_02_00")]
        public ESocial1000 ESocial1000 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabEstab/v_S_01_02_00")]
        public ESocial1005 ESocial1005 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabRubrica/v_S_01_02_00")]
        public ESocial1010 ESocial1010 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabLotacao/v_S_01_02_00")]
        public ESocial1020 ESocial1020 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabProcesso/v_S_01_02_00")]
        public ESocial1070 ESocial1070 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRemun/v_S_01_02_00")]
        public ESocial1200 ESocial1200 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRmnRPPS/v_S_01_02_00")]
        public ESocial1202 ESocial1202 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBenPrRP/v_S_01_02_00")]
        public ESocial1207 ESocial1207 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtPgtos/v_S_01_02_00")]
        public ESocial1210 ESocial1210 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtComProd/v_S_01_02_00")]
        public ESocial1260 ESocial1260 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContratAvNP/v_S_01_02_00")]
        public ESocial1270 ESocial1270 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoComplPer/v_S_01_02_00")]
        public ESocial1280 ESocial1280 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReabreEvPer/v_S_01_02_00")]
        public ESocial1298 ESocial1298 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFechaEvPer/v_S_01_02_00")]
        public ESocial1299 ESocial1299 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmPrelim/v_S_01_02_00")]
        public ESocial2190 ESocial2190 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAdmissao/v_S_01_02_00")]
        public ESocial2200 ESocial2200 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltCadastral/v_S_01_02_00")]
        public ESocial2205 ESocial2205 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltContratual/v_S_01_02_00")]
        public ESocial2206 ESocial2206 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCAT/v_S_01_02_00")]
        public ESocial2210 ESocial2210 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00")]
        public ESocial2220 ESocial2220 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_02_00")]
        public ESocial2221 ESocial2221 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAfastTemp/v_S_01_02_00")]
        public ESocial2230 ESocial2230 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCessao/v_S_01_02_00")]
        public ESocial2231 ESocial2231 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00")]
        public ESocial2240 ESocial2240 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReintegr/v_S_01_02_00")]
        public ESocial2298 ESocial2298 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtDeslig/v_S_01_02_00")]
        public ESocial2299 ESocial2299 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVInicio/v_S_01_02_00")]
        public ESocial2300 ESocial2300 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVAltContr/v_S_01_02_00")]
        public ESocial2306 ESocial2306 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVTermino/v_S_01_02_00")]
        public ESocial2399 ESocial2399 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefIn/v_S_01_02_00")]
        public ESocial2400 ESocial2400 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefAlt/v_S_01_02_00")]
        public ESocial2405 ESocial2405 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenIn/v_S_01_02_00")]
        public ESocial2410 ESocial2410 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenAlt/v_S_01_02_00")]
        public ESocial2416 ESocial2416 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReativBen/v_S_01_02_00")]
        public ESocial2418 ESocial2418 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenTerm/v_S_01_02_00")]
        public ESocial2420 ESocial2420 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtProcTrab/v_S_01_02_00")]
        public ESocial2500 ESocial2500 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContProc/v_S_01_02_00")]
        public ESocial2501 ESocial2501 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExclusao/v_S_01_02_00")]
        public ESocial3000 ESocial3000 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExcProcTrab/v_S_01_02_00")]
        public ESocial3500 ESocial3500 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_02_00")]
        public ESocial5001 ESocial5001 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_02_00")]
        public ESocial5002 ESocial5002 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesFGTS/v_S_01_02_00")]
        public ESocial5003 ESocial5003 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_02_00")]
        public ESocial5011 ESocial5011 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrf/v_S_01_02_00")]
        public ESocial5012 ESocial5012 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTS/v_S_01_02_00")]
        public ESocial5013 ESocial5013 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTribProcTrab/v_S_01_02_00")]
        public ESocial5501 ESocial5501 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTSProcTrab/v_S_01_02_00")]
        public ESocial5503 ESocial5503 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAnotJud/v_S_01_02_00")]
        public ESocial8200 ESocial8200 { get; set; }

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

                int tipoEvento = 0;
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
