#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial.Retorno
{
    /// <summary>
    /// Contém o retorno dos eventos processados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.ESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoProcessamento/v1_3_0", IsNullable = true)]
    public class RetornoEventoProcessado : XMLBase
    {
        /// <summary>
        /// Contém o resultado da operação de processamento de um lote de eventos
        /// </summary>
        [XmlElement("retornoProcessamentoLoteEventos")]
        public RetornoProcessamentoLoteEventos RetornoProcessamentoLoteEventos { get; set; }
    }

    /// <summary>
    /// Contém as ocorrências encontradas no lote quando o código de resposta
    /// contido na tag status indicar que foram encontrados erros ou advertências no lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Ocorrencias")]
    [ComVisible(true)]
#endif
    public class Ocorrencias
    {
        /// <summary>
        /// Contém cada uma das ocorrências encontradas no lote quando o código
        /// de resposta contido na tag status indicar que foram encontrados erros ou advertências no lote.
        /// </summary>
        [XmlElement("ocorrencia")]
        public List<Ocorrencia> Ocorrencia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOcorrencia(Ocorrencia item)
        {
            if (Ocorrencia == null)
            {
                Ocorrencia = new List<Ocorrencia>();
            }

            Ocorrencia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Ocorrencia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Ocorrencia</returns>
        public Ocorrencia GetOcorrencia(int index)
        {
            if ((Ocorrencia?.Count ?? 0) == 0)
            {
                return default;
            };

            return Ocorrencia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Ocorrencia
        /// </summary>
        public int GetOcorrenciaCount => (Ocorrencia != null ? Ocorrencia.Count : 0);
#endif
    }

    /// <summary>
    /// Contém cada uma das ocorrências encontradas no lote quando o código
    /// de resposta contido na tag status indicar que foram encontrados erros ou advertências no lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Ocorrencia")]
    [ComVisible(true)]
#endif
    public class Ocorrencia
    {
        /// <summary>
        /// Contém o código do tipo de ocorrência.
        /// </summary>
        [XmlElement("tipo")]
        public int Tipo { get; set; }

        /// <summary>
        /// Contém o código da ocorrência detectada em alguma das regras
        /// </summary>
        [XmlElement("codigo")]
        public int Codigo { get; set; }

        /// <summary>
        /// Contém a descrição da ocorrência detectada em alguma das regras
        /// </summary>
        [XmlElement("descricao")]
        public string Descricao { get; set; }

        /// <summary>
        /// Contém o caminho da tag  atributo em que ocorreu o erro.
        /// </summary>
        [XmlElement("localizacao")]
        public string Localizacao { get; set; }
    }

    /// <summary>
    /// Contém os dados relativos a recepção de um lote
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.DadosRecepcaoLote")]
    [ComVisible(true)]
#endif
    public class DadosRecepcaoLote
    {
        /// <summary>
        /// Contém a data/hora de recepção do lote de eventos
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTimeOffset DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhRecepcao.Millisecond.ToString().EndsWith("0") ? "" : "f"));
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Contém a versão do aplicativo de recepção
        /// </summary>
        [XmlElement("versaoAplicativoRecepcao")]
        public string VersaoAplicativoRecepcao { get; set; }

        /// <summary>
        /// Número sequencial único produzido no instante da recepção do lote de eventos
        /// </summary>
        [XmlElement("protocoloEnvio")]
        public string ProtocoloEnvio { get; set; }
    }

    /// <summary>
    /// Contém o resultado do processamento do lote. É nesta tag que haverá a informação se o lote já foi 
    /// processado e qual o resultado do processamento do lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Status")]
    [ComVisible(true)]
#endif
    public class Status
    {
        /// <summary>
        /// Contém o código de resposta do processamento do lote. Caso o código seja 
        /// de erro ou advertência, os detalhes do mesmo estarão na tag ocorrencias.
        /// </summary>
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        /// <summary>
        /// Contém a descrição correspondente ao código de resposta
        /// </summary>
        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        /// <summary>
        /// Contém as ocorrências encontradas no lote quando o código de resposta
        /// contido na tag status indicar que foram encontrados erros ou advertências no lote.
        /// </summary>
        [XmlElement("ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }
    }

    /// <summary>
    /// Contém os dados relativos ao processamento de um lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.DadosProcessamentoLote")]
    [ComVisible(true)]
#endif
    public class DadosProcessamentoLote
    {
        /// <summary>
        /// Contém a versão do aplicativo de processamento do lote
        /// </summary>
        [XmlElement("versaoAplicativoProcessamentoLote")]
        public string VersaoAplicativoProcessamentoLote { get; set; }
    }

    /// <summary>
    /// Contém o resultado da operação de processamento de um lote de eventos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoProcessamentoLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoProcessamentoLoteEventos
    {
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
        /// Contém o resultado do processamento do lote. É nesta tag que haverá a informação se o lote já foi 
        /// processado e qual o resultado do processamento do lote.
        /// </summary>
        [XmlElement("status")]
        public Status Status { get; set; }

        /// <summary>
        /// Contém os dados relativos a recepção de um lote
        /// </summary>
        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }

        /// <summary>
        /// /// <summary>
        /// Contém os dados relativos ao processamento de um lote.
        /// </summary>
        /// </summary>
        [XmlElement("dadosProcessamentoLote")]
        public DadosProcessamentoLote DadosProcessamentoLote { get; set; }

        [XmlElement("retornoEventos")]
        public RetornoEventos RetornoEventos { get; set; }
    }

    /// <summary>
    /// Contém o retorno com o resultado do processamento de cada um dos eventos contido no lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEventos
    {
        /// <summary>
        /// Contém o resultado do processamento de cada evento contido no lote e  resultado do respectivo totalizador, 
        /// caso o evento retorne algum dos totalizadores.
        /// </summary>
        [XmlElement("evento")]
        public List<Evento> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(Evento item)
        {
            if (Evento == null)
            {
                Evento = new List<Evento>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public Evento GetEvento(int index)
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
    /// Contém o resultado do processamento de cada evento contido no lote e  resultado do respectivo totalizador, 
    /// caso o evento retorne algum dos totalizadores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Evento")]
    [ComVisible(true)]
#endif
    public class Evento
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Contém o resultado do processamento de um evento contido no lote.
        /// </summary>
        [XmlElement("retornoEvento")]
        public RetornoEvento RetornoEvento { get; set; }

        /// <summary>
        /// Contém o resultado do processamento do totalizador de um evento contido no lote.
        /// </summary>
        [XmlElement("tot")]
        public List<Tot> Tot { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTot(Tot item)
        {
            if (Tot == null)
            {
                Tot = new List<Tot>();
            }

            Tot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Tot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Tot</returns>
        public Tot GetTot(int index)
        {
            if ((Tot?.Count ?? 0) == 0)
            {
                return default;
            };

            return Tot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Tot
        /// </summary>
        public int GetTotCount => (Tot != null ? Tot.Count : 0);
#endif
    }

    /// <summary>
    /// Contém o resultado do processamento do totalizador de um evento contido no lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Tot")]
    [ComVisible(true)]
#endif
    public class Tot
    {
        /// <summary>
        /// Deve ser preenchido com os valores válidos para este campo.
        /// </summary>
        [XmlAttribute(AttributeName = "tipo", DataType = "token")]
        public string Tipo { get; set; }

        /// <summary>
        /// S-5001 - Informações das Contribuições Sociais por Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_03_00")]
        public ESocial5001 ESocial5001 { get; set; }

        /// <summary>
        /// S-5002 - Imposto de Renda Retido na Fonte por Trabalhador
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_03_00")]
        public ESocial5002 ESocial5002 { get; set; }

        /// <summary>
        /// S-5011 - Informações das Contribuições Sociais Consolidadas por Contribuinte
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_03_00")]
        public ESocial5011 ESocial5011 { get; set; }

        /// <summary>
        /// S-5012 - Imposto de Renda Retido na Fonte Consolidado por Contribuinte
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrf/v_S_01_03_00")]
        public ESocial5012 ESocial5012 { get; set; }
    }

    /// <summary>
    /// Contém o resultado do processamento de um evento contido no lote.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEvento")]
    [ComVisible(true)]
#endif
    public class RetornoEvento
    {
        /// <summary>
        /// Contém a estrutura do evento de retorno
        /// </summary>
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/retornoEvento/v1_2_1")]
        public RetornoEventoESocial ESocial { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEventoESocial")]
    [ComVisible(true)]
#endif
    public class RetornoEventoESocial
    {
        /// <summary>
        /// Contém as informações do evento enviado (ID, dados de recepção)
        /// </summary>
        [XmlElement("retornoEvento")]
        public EventoRetorno RetornoEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Contém as informações do evento enviado (ID, dados de recepção)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.EventoRetorno")]
    [ComVisible(true)]
#endif
    public class EventoRetorno
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Contém os dados de recepção do evento.
        /// </summary>
        [XmlElement("recepcao")]
        public Recepcao Recepcao { get; set; }

        /// <summary>
        /// Contém os dados de processamento do evento.
        /// </summary>
        [XmlElement("processamento")]
        public Processamento Processamento { get; set; }

        /// <summary>
        /// Contém os dados do recibo de entrega do evento
        /// </summary>
        [XmlElement("recibo")]
        public Recibo Recibo { get; set; }
    }

    /// <summary>
    /// Contém os dados de recepção do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Recepcao")]
    [ComVisible(true)]
#endif
    public class Recepcao
    {
        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Data/hora da recepção do lote
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTime DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhRecepcao.Millisecond.ToString().EndsWith("0") ? "" : "f"));
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTime.Parse(value);
#endif
        }

        /// <summary>
        /// Versão do aplicativo da recepção do lote
        /// </summary>
        [XmlElement("versaoAppRecepcao")]
        public string VersaoAppRecepcao { get; set; }

        /// <summary>
        /// Protocolo do lote
        /// </summary>
        [XmlElement("protocoloEnvioLote")]
        public string ProtocoloEnvioLote { get; set; }
    }

    /// <summary>
    /// Contém os dados de processamento do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Processamento")]
    [ComVisible(true)]
#endif
    public class Processamento
    {
        /// <summary>
        /// Código de resposta do processamento do evento conforme tabela
        /// </summary>
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        /// <summary>
        /// Descrição da mensagem de retorno
        /// </summary>
        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        /// <summary>
        /// Versão do aplicativo de processamento de evento
        /// </summary>
        [XmlElement("versaoAppProcessamento")]
        public string VersaoAppProcessamento { get; set; }

        /// <summary>
        /// Data e horário do processamento do evento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhProcessamento { get; set; }
#else
        public DateTime DhProcessamento { get; set; }
#endif

        [XmlElement("dhProcessamento")]
        public string DhProcessamentoField
        {
            get => DhProcessamento.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhProcessamento.Millisecond.ToString().EndsWith("0") ? "" : "f"));
#if INTEROP
            set => DhProcessamento = DateTime.Parse(value);
#else
            set => DhProcessamento = DateTime.Parse(value);
#endif
        }

        /// <summary>
        /// Contém as ocorrências encontradas no lote quando o código de resposta
        /// contido na tag status indicar que foram encontrados erros ou advertências no lote.
        /// </summary>
        [XmlElement("ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }
    }

    /// <summary>
    /// Contém os dados do recibo de entrega do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Recibo")]
    [ComVisible(true)]
#endif
    public class Recibo
    {
        /// <summary>
        /// Número de recibo do evento
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Hash do evento. Será considerado o hash utilizado na assinatura do conteúdo evento.
        /// </summary>
        [XmlElement("hash")]
        public string Hash { get; set; }
    }
}
