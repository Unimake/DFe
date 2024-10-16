#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2400")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefIn/v_S_01_02_00", IsNullable = false)]
    public class ESocial2400 : XMLBase
    {
        [XmlElement("evtCdBenefIn")]
        public EvtCdBenefIn EvtCdBenefIn { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenefIn")]
    [ComVisible(true)]
#endif
    public class EvtCdBenefIn
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2400 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("beneficiario")]
        public Beneficiario Beneficiario { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2400")]
    [ComVisible(true)]
#endif
    public class IdeEvento2400 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Beneficiario")]
    [ComVisible(true)]
#endif
    public class Beneficiario
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("nmBenefic")]
        public string NmBenefic { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

        [XmlElement("dtInicio")]
        public string DtInicioField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("sexo")]
#if INTEROP
        public TipoSexo Sexo { get; set; } = (TipoSexo)(-1);
#else
        public TipoSexo? Sexo { get; set; }
#endif

        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstCiv { get; set; } = (EstadoCivil)(-1);
#else
        public EstadoCivil? EstCiv { get; set; }
#endif

        [XmlElement("incFisMen")]
        public SimNaoLetra IncFisMen { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIncFisMen { get; set; }
#else
        public DateTimeOffset DtIncFisMen { get; set; }
#endif

        [XmlElement("dtIncFisMen")]
        public string DtIncFisMenField
        {
            get => DtIncFisMen.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIncFisMen = DateTime.Parse(value);
#else
            set => DtIncFisMen = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("endereco")]
        public Endereco Endereco { get; set; }

        [XmlElement("dependente")]
        public List<Dependente2400> Dependente { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente2400 item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente2400>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente2400 GetDependente(int index)
        {
            if ((Dependente?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dependente[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dependente
        /// </summary>
        public int GetDependenteCount => (Dependente != null ? Dependente.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeSexo() => Sexo != (TipoSexo)(-1);
#else
        public bool ShouldSerializeSexo() => Sexo != null;
#endif

#if INTEROP
        public bool ShouldSerializeEstCiv() => EstCiv != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeEstCiv() => EstCiv != null;
#endif

        public bool ShouldSerializeDtIncFisMenField() => DtIncFisMen > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Dependente2400")]
    [ComVisible(true)]
#endif
    public class Dependente2400
    {
        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        [XmlElement("nmDep")]
        public string NmDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("sexoDep")]
#if INTEROP
        public TipoSexo SexoDep { get; set; } = (TipoSexo)(-1);
#else
        public TipoSexo? SexoDep { get; set; }
#endif

        [XmlElement("depIRRF")]
        public SimNaoLetra DepIRRF { get; set; }

        [XmlElement("incFisMen")]
        public SimNaoLetra IncFisMen { get; set; }

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpDep() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDep() => TpDep != null;
#endif

        public bool ShouldSerializeCpfDep() => !string.IsNullOrEmpty(CpfDep);

#if INTEROP
        public bool ShouldSerializeSexoDep() => SexoDep != (TipoSexo)(-1);
#else
        public bool ShouldSerializeSexoDep() => SexoDep != null;
#endif

        #endregion

    }
}
