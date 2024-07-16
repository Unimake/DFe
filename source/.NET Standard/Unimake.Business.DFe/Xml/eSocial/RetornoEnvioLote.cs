#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoEnvioLote")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoEnvio/v1_1_0", IsNullable = true)]
    public class RetornoEnvioLote : XMLBase
    {
        [XmlElement("retornoEnvioLoteEventos")]
        public RetornoEnvioLoteEventos RetornoEnvioLoteEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEnvioLoteEventos
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        [XmlElement("status")]
        public Status Status { get; set; }

        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Status")]
    [ComVisible(true)]
#endif
    public class Status
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        [XmlElement("ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Ocorrencias")]
    [ComVisible(true)]
#endif
    public class Ocorrencias
    {
        [XmlElement("Ocorrencia")]
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Ocorrencia")]
    [ComVisible(true)]
#endif
    public class Ocorrencia
    {
        [XmlElement("codigo")]
        public int Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }

        [XmlElement("tipo")]
        public int Tipo { get; set; }

        [XmlElement("localizacao")]
        public string Localizacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosRecepcaoLote")]
    [ComVisible(true)]
#endif
    public class DadosRecepcaoLote
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTimeOffset DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.fff");
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTimeOffset.Parse(value);
#endif

        }

        [XmlElement("versaoAplicativoRecepcao")]
        public string VersaoAplicativoRecepcao { get; set; }

        [XmlElement("protocoloEnvio")]
        public string ProtocoloEnvio { get; set; }
    }
}
