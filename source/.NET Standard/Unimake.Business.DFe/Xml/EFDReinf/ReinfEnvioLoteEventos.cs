#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/envioLoteEventosAssincrono/v1_00_00", IsNullable = false)]
    public class ReinfEnvioLoteEventos : XMLBase
    {
        [XmlElement("envioLoteEventos")]
        public EnvioLoteEventos EnvioLoteEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EnvioLoteEventos")]
    [ComVisible(true)]
#endif
    public class EnvioLoteEventos
    {
        [XmlElement("ideContribuinte")]
        public IdeContribuinte IdeContribuinte { get; set; }

        [XmlElement("eventos")]
        public Eventos Eventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContribuinte")]
    [ComVisible(true)]
#endif
    public class IdeContribuinte
    {
        [XmlElement("tpInsc")]
#if INTEROP
        public TiposInscricao TpInsc { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Eventos")]
    [ComVisible(true)]
#endif
    public class Eventos
    {
        [XmlElement("evento")]
        public List<Evento> Evento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Evento")]
    [ComVisible(true)]
#endif
    public class Evento : ReinfEventoBase
    {
        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02")]
        public Reinf1000 Reinf1000 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt1050TabLig/v2_01_02")]
        public List<Reinf1050> Reinf1050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02")]
        public List<Reinf1070> Reinf1070 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTomadorServicos/v2_01_02")]
        public List<Reinf2010> Reinf2010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtPrestadorServicos/v2_01_02")]
        public List<Reinf2020> Reinf2020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRecebidoAssociacao/v2_01_02")]
        public List<Reinf2030> Reinf2030 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRepassadoAssociacao/v2_01_02")]
        public List<Reinf2040> Reinf2040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoProdRural/v2_01_02")]
        public List<Reinf2050> Reinf2050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt2055AquisicaoProdRural/v2_01_02")]
        public List<Reinf2055> Reinf2055 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoCPRB/v2_01_02")]
        public List<Reinf2060> Reinf2060 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtReabreEvPer/v2_01_02")]
        public List<Reinf2098> Reinf2098 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtFechamento/v2_01_02")]
        public List<Reinf2099> Reinf2099 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtEspDesportivo/v2_01_02")]
        public List<Reinf3010> Reinf3010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4010PagtoBeneficiarioPF/v2_01_02")]
        public List<Reinf4010> Reinf4010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4020PagtoBeneficiarioPJ/v2_01_02")]
        public List<Reinf4020> Reinf4020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4040PagtoBenefNaoIdentificado/v2_01_02")]
        public List<Reinf4040> Reinf4040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4080RetencaoRecebimento/v2_01_02")]
        public List<Reinf4080> Reinf4080 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4099FechamentoDirf/v2_01_02")]
        public List<Reinf4099> Reinf4099 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtExclusao/v2_01_02")]
        public List<Reinf9000> Reinf9000 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02")]
        public List<Reinf9001> Reinf9001 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRet/v2_01_02")]
        public List<Reinf9005> Reinf9005 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02")]
        public List<Reinf9011> Reinf9011 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRetCons/v2_01_02")]
        public List<Reinf9015> Reinf9015 { get; set; }

#if INTEROP
      
        #region Reinf1050
        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf1050(Reinf1050 item)
        {
            if (Reinf1050 == null)
            {
                Reinf1050 = new List<Reinf1050>();
            }

            Reinf1050.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf1050 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf1050</returns>
        public Reinf1050 GetReinf1050(int index)
        {
            if ((Reinf1050?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf1050[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf1070
        /// </summary>
        public int GetReinf1050Count => (Reinf1050 != null ? Reinf1050.Count : 0);
        #endregion
      
        #region Reinf1070
        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf1070(Reinf1070 item)
        {
            if (Reinf1070 == null)
            {
                Reinf1070 = new List<Reinf1070>();
            }

            Reinf1070.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf1070 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf1070</returns>
        public Reinf1070 GetReinf1070(int index)
        {
            if ((Reinf1070?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf1070[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf1070
        /// </summary>
        public int GetReinf1070Count => (Reinf1070 != null ? Reinf1070.Count : 0);
        #endregion

        #region Reinf2010

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2010(Reinf2010 item)
        {
            if (Reinf2010 == null)
            {
                Reinf2010 = new List<Reinf2010>();
            }

            Reinf2010.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2010 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2010</returns>
        public Reinf2010 GetReinf2010(int index)
        {
            if ((Reinf2010?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2010[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2010
        /// </summary>
        public int GetReinf2010Count => (Reinf2010 != null ? Reinf2010.Count : 0);

        #endregion

        #region Reinf2020

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2020(Reinf2020 item)
        {
            if (Reinf2020 == null)
            {
                Reinf2020 = new List<Reinf2020>();
            }

            Reinf2020.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2020 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2020</returns>
        public Reinf2020 GetReinf2020(int index)
        {
            if ((Reinf2020?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2020[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2020
        /// </summary>
        public int GetReinf2020Count => (Reinf2020 != null ? Reinf2020.Count : 0);

        #endregion

        #region Reinf2030

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2030(Reinf2030 item)
        {
            if (Reinf2030 == null)
            {
                Reinf2030 = new List<Reinf2030>();
            }

            Reinf2030.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2030 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2030</returns>
        public Reinf2030 GetReinf2030(int index)
        {
            if ((Reinf2030?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2030[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2030
        /// </summary>
        public int GetReinf2030Count => (Reinf2030 != null ? Reinf2030.Count : 0);

        #endregion

        #region Reinf2040

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2040(Reinf2040 item)
        {
            if (Reinf2040 == null)
            {
                Reinf2040 = new List<Reinf2040>();
            }

            Reinf2040.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2040 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2040</returns>
        public Reinf2040 GetReinf2040(int index)
        {
            if ((Reinf2040?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2040[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2040
        /// </summary>
        public int GetReinf2040Count => (Reinf2040 != null ? Reinf2040.Count : 0);

        #endregion

        #region Reinf2050

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2050(Reinf2050 item)
        {
            if (Reinf2050 == null)
            {
                Reinf2050 = new List<Reinf2050>();
            }

            Reinf2050.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2050 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2050</returns>
        public Reinf2050 GetReinf2050(int index)
        {
            if ((Reinf2050?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2050[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2050
        /// </summary>
        public int GetReinf2050Count => (Reinf2050 != null ? Reinf2050.Count : 0);

        #endregion

        #region Reinf2055

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2055(Reinf2055 item)
        {
            if (Reinf2055 == null)
            {
                Reinf2055 = new List<Reinf2055>();
            }

            Reinf2055.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2055 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2055</returns>
        public Reinf2055 GetReinf2055(int index)
        {
            if ((Reinf2055?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2055[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2055
        /// </summary>
        public int GetReinf2055Count => (Reinf2055 != null ? Reinf2055.Count : 0);

        #endregion

        #region Reinf2060

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2060(Reinf2060 item)
        {
            if (Reinf2060 == null)
            {
                Reinf2060 = new List<Reinf2060>();
            }

            Reinf2060.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2060 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2060</returns>
        public Reinf2060 GetReinf2060(int index)
        {
            if ((Reinf2060?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2060[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2060
        /// </summary>
        public int GetReinf2060Count => (Reinf2060 != null ? Reinf2060.Count : 0);

        #endregion

        #region Reinf2098

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2098(Reinf2098 item)
        {
            if (Reinf2098 == null)
            {
                Reinf2098 = new List<Reinf2098>();
            }

            Reinf2098.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2098 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2098</returns>
        public Reinf2098 GetReinf2098(int index)
        {
            if ((Reinf2098?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2098[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2098
        /// </summary>
        public int GetReinf2098Count => (Reinf2098 != null ? Reinf2098.Count : 0);

        #endregion

        #region Reinf2099

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf2099(Reinf2099 item)
        {
            if (Reinf2099 == null)
            {
                Reinf2099 = new List<Reinf2099>();
            }

            Reinf2099.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf2099 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf2099</returns>
        public Reinf2099 GetReinf2099(int index)
        {
            if ((Reinf2099?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf2099[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf2099
        /// </summary>
        public int GetReinf2099Count => (Reinf2099 != null ? Reinf2099.Count : 0);

        #endregion

        #region Reinf3010

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf3010(Reinf3010 item)
        {
            if (Reinf3010 == null)
            {
                Reinf3010 = new List<Reinf3010>();
            }

            Reinf3010.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf3010 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf3010</returns>
        public Reinf3010 GetReinf3010(int index)
        {
            if ((Reinf3010?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf3010[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf3010
        /// </summary>
        public int GetReinf3010Count => (Reinf3010 != null ? Reinf3010.Count : 0);

        #endregion

        #region Reinf4010

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf4010(Reinf4010 item)
        {
            if (Reinf4010 == null)
            {
                Reinf4010 = new List<Reinf4010>();
            }

            Reinf4010.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf4010 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf4010</returns>
        public Reinf4010 GetReinf4010(int index)
        {
            if ((Reinf4010?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf4010[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf4010
        /// </summary>
        public int GetReinf4010Count => (Reinf4010 != null ? Reinf4010.Count : 0);

        #endregion

        #region Reinf4020

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf4020(Reinf4020 item)
        {
            if (Reinf4020 == null)
            {
                Reinf4020 = new List<Reinf4020>();
            }

            Reinf4020.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf4020 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf4020</returns>
        public Reinf4020 GetReinf4020(int index)
        {
            if ((Reinf4020?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf4020[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf4020
        /// </summary>
        public int GetReinf4020Count => (Reinf4020 != null ? Reinf4020.Count : 0);

        #endregion

        #region Reinf4040

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf4040(Reinf4040 item)
        {
            if (Reinf4040 == null)
            {
                Reinf4040 = new List<Reinf4040>();
            }

            Reinf4040.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf4040 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf4040</returns>
        public Reinf4040 GetReinf4040(int index)
        {
            if ((Reinf4040?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf4040[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf4040
        /// </summary>
        public int GetReinf4040Count => (Reinf4040 != null ? Reinf4040.Count : 0);

        #endregion

        #region Reinf4080

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf4080(Reinf4080 item)
        {
            if (Reinf4080 == null)
            {
                Reinf4080 = new List<Reinf4080>();
            }

            Reinf4080.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf4080 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf4080</returns>
        public Reinf4080 GetReinf4080(int index)
        {
            if ((Reinf4080?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf4080[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf4080
        /// </summary>
        public int GetReinf4080Count => (Reinf4080 != null ? Reinf4080.Count : 0);

        #endregion

        #region Reinf4099

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf4099(Reinf4099 item)
        {
            if (Reinf4099 == null)
            {
                Reinf4099 = new List<Reinf4099>();
            }

            Reinf4099.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf4099 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf4099</returns>
        public Reinf4099 GetReinf4099(int index)
        {
            if ((Reinf4099?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf4099[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf4099
        /// </summary>
        public int GetReinf4099Count => (Reinf4099 != null ? Reinf4099.Count : 0);

        #endregion

        #region Reinf9000

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9000(Reinf9000 item)
        {
            if (Reinf9000 == null)
            {
                Reinf9000 = new List<Reinf9000>();
            }

            Reinf9000.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9000 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9000</returns>
        public Reinf9000 GetReinf9000(int index)
        {
            if ((Reinf9000?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9000[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9000
        /// </summary>
        public int GetReinf9000Count => (Reinf9000 != null ? Reinf9000.Count : 0);

        #endregion

        #region Reinf9001

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9001(Reinf9001 item)
        {
            if (Reinf9001 == null)
            {
                Reinf9001 = new List<Reinf9001>();
            }

            Reinf9001.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9001</returns>
        public Reinf9001 GetReinf9001(int index)
        {
            if ((Reinf9001?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9001[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9001
        /// </summary>
        public int GetReinf9001Count => (Reinf9001 != null ? Reinf9001.Count : 0);

        #endregion

        #region Reinf9005

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9005(Reinf9005 item)
        {
            if (Reinf9005 == null)
            {
                Reinf9005 = new List<Reinf9005>();
            }

            Reinf9005.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9005 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9005</returns>
        public Reinf9005 GetReinf9005(int index)
        {
            if ((Reinf9005?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9005[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9005
        /// </summary>
        public int GetReinf9005Count => (Reinf9005 != null ? Reinf9005.Count : 0);

        #endregion

        #region Reinf9011

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9011(Reinf9011 item)
        {
            if (Reinf9011 == null)
            {
                Reinf9011 = new List<Reinf9011>();
            }

            Reinf9011.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9011 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9011</returns>
        public Reinf9011 GetReinf9011(int index)
        {
            if ((Reinf9011?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9011[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9011
        /// </summary>
        public int GetReinf9011Count => (Reinf9011 != null ? Reinf9011.Count : 0);

        #endregion

        #region Reinf9015

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9015(Reinf9015 item)
        {
            if (Reinf9015 == null)
            {
                Reinf9015 = new List<Reinf9015>();
            }

            Reinf9015.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9015 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9015</returns>
        public Reinf9015 GetReinf9015(int index)
        {
            if ((Reinf9015?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9015[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9015
        /// </summary>
        public int GetReinf9015Count => (Reinf9015 != null ? Reinf9015.Count : 0);

        #endregion
#endif

    }

}