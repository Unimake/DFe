#pragma warning disable CS1591

#if INTEROP
using System.Collections.Generic;
using System.Runtime.InteropServices;
#else
using System.Collections.Generic;
#endif

using System;
using System.Xml.Serialization;
using Newtonsoft.Json;
using System.Globalization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetificacaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetificacaoOperacaoTransporte : XMLBase
    {
        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlIgnore]
        public double ValorFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ValorFrete para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("ValorFrete")]
        public string ValorFreteField
        {
            get => ValorFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorFrete = Converter.ToDouble(value);
        }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataFimViagem { get; set; }
#else
        public DateTimeOffset DataFimViagem { get; set; }
#endif

        [XmlElement("DataFimViagem")]
        [JsonProperty("DataFimViagem")]
        public string DataFimViagemField
        {
            get => DataFimViagem.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataFimViagem = DateTime.Parse(value);
#else
            set => DataFimViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlArray("OrigemDestino")]
        [XmlArrayItem("ParOrigemDestino")]
        public List<OrigemDestino> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCarga DadosCarga { get; set; }

        public bool ShouldSerializeValorFreteField() => ValorFrete > 0;
        public bool ShouldSerializeDataFimViagem() => false;
#if INTEROP
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTime.MinValue;
#else
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTimeOffset.MinValue;
#endif
        public bool ShouldSerializeOrigemDestino() => OrigemDestino?.Count > 0;
        public bool ShouldSerializeDadosCarga() => DadosCarga != null;

#if INTEROP
        public void AddOrigemDestino(OrigemDestino origemDestino)
        {
            if (OrigemDestino == null)
            {
                OrigemDestino = new List<OrigemDestino>();
            }

            OrigemDestino.Add(origemDestino);
        }

        public OrigemDestino GetOrigemDestino(int index)
        {
            if ((OrigemDestino?.Count ?? 0) == 0)
            {
                return default;
            }

            return OrigemDestino[index];
        }

        public int GetOrigemDestinoCount => (OrigemDestino != null ? OrigemDestino.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetRetificacaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetRetificacaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetRetificacaoOperacaoTransporte : XMLBase
    {
        [XmlElement("temp")]
        public Temp Temp { get; set; }

        [XmlElement("CodigoIdentificacaoOperacao")]
        public string CodigoIdentificacaoOperacao { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataRetificacao { get; set; }
#else
        public DateTimeOffset DataRetificacao { get; set; }
#endif

        [XmlElement("DataRetificacao")]
        [JsonProperty("DataRetificacao")]
        public string DataRetificacaoField
        {
            get => CIOTDateTimeFormat.DateTime(DataRetificacao);
            set => DataRetificacao = CIOTDateTimeFormat.ParseDateTimeOrMinValue(value);
        }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Mensagem")]
        public string Mensagem { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeDataRetificacao() => false;
#if INTEROP
        public bool ShouldSerializeDataRetificacaoField() => Temp == null && DataRetificacao > DateTime.MinValue;
#else
        public bool ShouldSerializeDataRetificacaoField() => Temp == null && DataRetificacao > DateTimeOffset.MinValue;
#endif
    }
}
