using System.IO;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial.Serializacao
{
    public class AtualizacaoSchemaS010300Test
    {
        [Fact]
        [Trait("DFe", "ESocial")]
        public void AfastamentoAceitaMotivoIndeterminadoEPreservaSimNao()
        {
            var afastamento = new IniAfastamento { InfoMesmoMtvIndicador = MesmoMotivoAfastamento.Indeterminado };
            var xml = Serializar(afastamento);

            Assert.Contains("<infoMesmoMtv>I</infoMesmoMtv>", xml);
            Assert.Equal(MesmoMotivoAfastamento.Indeterminado, Desserializar<IniAfastamento>(xml).InfoMesmoMtvIndicador);

            afastamento.InfoMesmoMtv = SimNaoLetra.Sim;
            var xmlLegado = Serializar(afastamento);
            Assert.Contains("<infoMesmoMtv>S</infoMesmoMtv>", xmlLegado);
            Assert.Equal(SimNaoLetra.Sim, Desserializar<IniAfastamento>(xmlLegado).InfoMesmoMtv);
        }

        [Fact]
        [Trait("DFe", "ESocial")]
        public void IndicativoCPFDesvinculadoFicaNoGrupoMudancaCPF()
        {
            var desligamento = Serializar(new MudancaCPF2299 { NovoCPF = "12345678909", IndCPFDesvinc = IndicativoCPFDesvinculado.SemVinculoNaRFB });
            var terminoTSV = Serializar(new MudancaCPF2399 { NovoCPF = "12345678909", IndCPFDesvinc = IndicativoCPFDesvinculado.SemVinculoNaRFB });
            var terminoBeneficio = Serializar(new InfoBenTermino2420 { MtvTermino = MtvTermino.Obito, IndCPFDesvinc = IndicativoCPFDesvinculado.SemVinculoNaRFB });

            Assert.Contains("<indCPFDesvinc>9</indCPFDesvinc>", desligamento);
            Assert.Contains("<indCPFDesvinc>9</indCPFDesvinc>", terminoTSV);
            Assert.Contains("<indCPFDesvinc>9</indCPFDesvinc>", terminoBeneficio);
            Assert.DoesNotContain("indCPFDesvinc", Serializar(new MudancaCPF2299 { NovoCPF = "12345678909" }));
            Assert.Equal(IndicativoCPFDesvinculado.SemVinculoNaRFB, Desserializar<MudancaCPF2299>(desligamento).IndCPFDesvinc);
        }

        [Fact]
        [Trait("DFe", "ESocial")]
        public void ProcessoTrabalhistaSerializaCamposNaSequenciaDoSchema()
        {
            var complemento = Serializar(new InfoCompl
            {
                NmCargo = "Cargo teste",
                CodCBO = "123456",
                NmFuncao = "Funcao teste",
                CBOFuncao = "654321"
            });

            Assert.True(complemento.IndexOf("<nmCargo>") < complemento.IndexOf("<codCBO>"));
            Assert.True(complemento.IndexOf("<codCBO>") < complemento.IndexOf("<nmFuncao>"));
            Assert.True(complemento.IndexOf("<nmFuncao>") < complemento.IndexOf("<CBOFuncao>"));
            Assert.Equal("Cargo teste", Desserializar<InfoCompl>(complemento).NmCargo);

            var trabalhador = Serializar(new IdeTrab { CpfTrab = "12345678909", NmSoc = "Nome social teste" });
            Assert.Contains("<nmSoc>Nome social teste</nmSoc>", trabalhador);

            var vinculo = Serializar(new InfoVinc
            {
                TpRegTrab = TipoRegimeTrabalhista.ContratoServidorPublicoNulo,
                TpRegPrev = TpRegPrev.RegimeGeral,
                LocalTrabalho = new LocalTrabalho2200
                {
                    LocalTrabGeral = new LocalTrabGeral2200 { TpInsc = TipoInscricaoEstabelecimento.CNPJ, NrInsc = "12345678000195" }
                }
            });
            Assert.Contains("<tpRegTrab>3</tpRegTrab>", vinculo);
            Assert.Contains("<localTrabalho>", vinculo);
            Assert.Contains("<localTrabGeral>", vinculo);
        }

        [Fact]
        [Trait("DFe", "ESocial")]
        public void AnotacaoJudicialSerializaNomeDoCargo()
        {
            var xml = Serializar(new Cargo { CBOCargo = "123456", NmCargo = "Cargo teste" });
            Assert.Contains("<nmCargo>Cargo teste</nmCargo>", xml);
            Assert.Equal("Cargo teste", Desserializar<Cargo>(xml).NmCargo);
        }

        [Fact]
        [Trait("DFe", "ESocial")]
        public void ReembolsoAceitaNovoCodigoTres()
        {
            var xml = Serializar(new InfoReembMed1210
            {
                IndOrgReemb = IndicativoOrigemReembolso.RessarcimentoDespesasPlanoSaude
            });

            Assert.Contains("<indOrgReemb>3</indOrgReemb>", xml);
            Assert.Equal(IndicativoOrigemReembolso.RessarcimentoDespesasPlanoSaude,
                Desserializar<InfoReembMed1210>(xml).IndOrgReemb);

            var retorno = Serializar(new InfoReembMed5002
            {
                IndOrgReemb = IndicativoOrigemReembolso.RessarcimentoDespesasPlanoSaude
            });
            Assert.Contains("<indOrgReemb>3</indOrgReemb>", retorno);
        }

        private static string Serializar<T>(T value)
        {
            var serializer = new XmlSerializer(typeof(T));
            using (var writer = new StringWriter())
            {
                serializer.Serialize(writer, value);
                return writer.ToString();
            }
        }

        private static T Desserializar<T>(string xml)
        {
            var serializer = new XmlSerializer(typeof(T));
            using (var reader = new StringReader(xml))
            {
                return (T)serializer.Deserialize(reader);
            }
        }
    }
}
