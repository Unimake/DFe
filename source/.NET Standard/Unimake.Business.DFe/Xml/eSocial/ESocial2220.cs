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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2220")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtMonit/v_S_01_02_00", IsNullable = false)]
    public class ESocial2220 : XMLBase
    {
        [XmlElement("evtMonit")]
        public EvtMonit EvtMonit { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtMonit")]
    [ComVisible(true)]
#endif
    public class EvtMonit
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculo ÌdeVinculo { get; set; }

        [XmlElement("exMedOcup")]
        public ExMedOcup ExMedOcup { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculoESocial2220")]
    [ComVisible(true)]
#endif
    public class IdeVinculoESocial2220
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("matricula")]
        public string Matricula { get; set; }

        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ExMedOcup")]
    [ComVisible(true)]
#endif
    public class ExMedOcup
    {
        [XmlElement("tpExameOcup")]
        public TpExameOcup TpExameOcup { get; set; }

        [XmlElement("aso")]
        public Aso Aso { get; set; }

        [XmlElement("respMonit")]
        public RespMonit RespMonit { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Aso")]
    [ComVisible(true)]
#endif
    public class Aso
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtAso { get; set; }
#else
        public DateTimeOffset DtAso { get; set; }
#endif

        [XmlElement("dtAso")]
        public string DtAsoField
        {
            get => DtAso.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAso = DateTime.Parse(value);
#else
            set => DtAso = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("resAso")]
#if INTEROP
        public ResAso ResAso { get; set; } = (ResAso)(-1);
#else
        public ResAso? ResAso { get; set; }
#endif


        [XmlElement("exame")]
        public List<Exame> Exame { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddExame(Exame item)
        {
            if (Exame == null)
            {
                Exame = new List<Exame>();
            }

            Exame.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Exame (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Exame</returns>
        public Exame GetExame(int index)
        {
            if ((Exame?.Count ?? 0) == 0)
            {
                return default;
            };

            return Exame[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Exame
        /// </summary>
        public int GetExameCount => (Exame != null ? Exame.Count : 0);
#endif

        [XmlElement("medico")]
        public Medico Medico { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeResAso() => ResAso != (ResAso)(-1);
#else
        public bool ShouldSerializeResAso() => ResAso != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exame")]
    [ComVisible(true)]
#endif
    public class Exame
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtExm { get; set; }
#else
        public DateTimeOffset DtExm { get; set; }
#endif

        [XmlElement("dtExm")]
        public string DtExmField
        {
            get => DtExm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExm = DateTime.Parse(value);
#else
            set => DtExm = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("procRealizado")]
        public string ProcRealizado { get; set; }

        [XmlElement("obsProc")]
        public string ObsProc { get; set; }

        [XmlElement("ordExame")]
#if INTEROP
        public OrdExame OrdExame { get; set; } = (OrdExame)(-1);
#else
        public OrdExame? OrdExame { get; set; }
#endif

        [XmlElement("indResult")]
#if INTEROP
        public IndResult IndResult { get; set; } = (IndResult)(-1);
#else
        public IndResult? IndResult { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeObsProc() => !string.IsNullOrEmpty(ObsProc);

#if INTEROP
        public bool ShouldSerializeOrdExame() => OrdExame != (OrdExame)(-1);
#else
        public bool ShouldSerializeOrdExame() => OrdExame != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndResult() => IndResult != (IndResult)(-1);
#else
        public bool ShouldSerializeIndResult() => IndResult != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Medico")]
    [ComVisible(true)]
#endif
    public class Medico
    {
        [XmlElement("nmMed")]
        public string NmMed { get; set; }

        [XmlElement("nrCRM")]
        public string NrCRM { get; set; }

        [XmlElement("ufCRM")]
#if INTEROP
        public UFBrasil UfCRM { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfCRM { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNrCRM() => !string.IsNullOrEmpty(NrCRM);

#if INTEROP
        public bool ShouldSerializeUfCRM() => UfCRM != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfCRM() => UfCRM != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RespMonit")]
    [ComVisible(true)]
#endif
    public class RespMonit
    {
        [XmlElement("cpfResp")]
        public string CpfResp { get; set; }

        [XmlElement("nmResp")]
        public string NmResp { get; set; }

        [XmlElement("nrCRM")]
        public string NrCRM { get; set; }

        [XmlElement("ufCRM")]
        public UFBrasil UfCRM { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCpfResp() => !string.IsNullOrEmpty(CpfResp);

        #endregion
    }
}
