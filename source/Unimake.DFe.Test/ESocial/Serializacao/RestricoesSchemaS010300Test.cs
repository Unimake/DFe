using System;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial.Serializacao
{
    public class RestricoesSchemaS010300Test
    {
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(false, -1)]
        [InlineData(false, 0)]
        [InlineData(false, 1)]
        [InlineData(true, -1)]
        [InlineData(true, 0)]
        [InlineData(true, 1)]
        public void RegistroEletronicoOpcionalPreservaAusenciaEValoresNaInclusaoEAlteracao(bool alteracao, int codigo)
        {
            var modelo = Ler<ESocial1000>("1000_evtInfoEmpregador-esocial-evt.xml");
            var inclusao = modelo.EvtInfoEmpregador.InfoEmpregador.Inclusao;
            var cadastro = new InfoCadastro
            {
                ClassTrib = inclusao.InfoCadastro.ClassTrib,
                IndDesFolha = inclusao.InfoCadastro.IndDesFolha
            };
            if (codigo >= 0)
            {
                cadastro.IndOptRegEletron = (IndOptRegEletron)codigo;
            }
            inclusao.InfoCadastro = cadastro;
            inclusao.IdePeriodo.IniValid = new DateTimeOffset(2026, 1, 1, 0, 0, 0, TimeSpan.Zero);
            inclusao.IdePeriodo.FimValid = DateTimeOffset.MinValue;
            if (alteracao)
            {
                modelo.EvtInfoEmpregador.InfoEmpregador.Alteracao = new Alteracao1000
                {
                    IdePeriodo = inclusao.IdePeriodo,
                    InfoCadastro = cadastro
                };
                modelo.EvtInfoEmpregador.InfoEmpregador.Inclusao = null;
            }
            modelo.Signature = AssinaturaEstrutural();
            var xml = modelo.GerarXML();
            var ns = xml.DocumentElement.NamespaceURI;
            Assert.Equal(codigo < 0 ? 0 : 1, xml.GetElementsByTagName("indOptRegEletron", ns).Count);
            if (codigo >= 0)
            {
                var tag = xml.GetElementsByTagName("indOptRegEletron", ns)[0];
                Assert.Equal(codigo.ToString(), tag.InnerText);
                Assert.Equal("infoCadastro", tag.ParentNode.LocalName);
                Assert.Equal(alteracao ? "alteracao" : "inclusao", tag.ParentNode.ParentNode.LocalName);
                Assert.Equal("indDesFolha", tag.PreviousSibling.LocalName);
            }
            Validar(xml, "evtInfoEmpregador");
            var retorno = modelo.LerXML<ESocial1000>(xml);
            var cadastroRetorno = alteracao ? retorno.EvtInfoEmpregador.InfoEmpregador.Alteracao.InfoCadastro :
                retorno.EvtInfoEmpregador.InfoEmpregador.Inclusao.InfoCadastro;
            Assert.Equal(cadastro.IndOptRegEletron, cadastroRetorno.IndOptRegEletron);
            Assert.Equal(xml.InnerText, retorno.GerarXML().InnerText);
        }

        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(null)]
        [InlineData(SimNaoLetra.Sim)]
        [InlineData(SimNaoLetra.Nao)]
        public void AfastamentoSerializaDominioVigenteNoCaminhoCorreto(SimNaoLetra? valor)
        {
            var modelo = Ler<ESocial2230>("2230_evtAfastTemp-esocial-evt.xml");
            modelo.EvtAfastTemp.InfoAfastamento = new InfoAfastamento
            {
                IniAfastamento = new IniAfastamento
                {
                    DtIniAfast = new DateTimeOffset(2026, 10, 27, 0, 0, 0, TimeSpan.Zero),
                    CodMotAfast = "01",
                    InfoMesmoMtv = valor
                }
            };
            modelo.Signature = AssinaturaEstrutural();
            var xml = modelo.GerarXML();
            var tags = xml.GetElementsByTagName("infoMesmoMtv", xml.DocumentElement.NamespaceURI);
            Assert.Equal(valor.HasValue ? 1 : 0, tags.Count);
            if (valor.HasValue)
            {
                Assert.Equal("iniAfastamento", tags[0].ParentNode.LocalName);
                Assert.Equal("infoAfastamento", tags[0].ParentNode.ParentNode.LocalName);
                Assert.Equal("codMotAfast", tags[0].PreviousSibling.LocalName);
            }
            Validar(xml, "evtAfastTemp");
            Assert.Equal(valor, modelo.LerXML<ESocial2230>(xml).EvtAfastTemp.InfoAfastamento.IniAfastamento.InfoMesmoMtv);
            if (valor.HasValue)
            {
                tags[0].InnerText = "I";
                var validador = new ValidarSchema();
                validador.Validar(xml, "ESocial.S_01_03_00.evtAfastTemp.xsd", xml.DocumentElement.NamespaceURI);
                Assert.False(validador.Success);
                Assert.Contains("infoMesmoMtv", validador.ErrorMessage);
            }
        }

        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(typeof(InfoRegPrelim), "DtAdmField", "dtAdm", "evtAdmPrelim", "infoRegPrelim")]
        [InlineData(typeof(InfoCeletista2200), "DtAdmField", "dtAdm", "evtAdmissao", "vinculo/infoRegimeTrab/infoCeletista")]
        [InlineData(typeof(InfoEstatutario2200), "DtExercicioField", "dtExercicio", "evtAdmissao", "vinculo/infoRegimeTrab/infoEstatutario")]
        [InlineData(typeof(InfoDeslig), "DtDesligField", "dtDeslig", "evtDeslig", "infoDeslig")]
        [InlineData(typeof(InfoTSVInicio), "DtInicioField", "dtInicio", "evtTSVInicio", "infoTSVInicio")]
        [InlineData(typeof(InfoTSVTermino), "DtTermField", "dtTerm", "evtTSVTermino", "infoTSVTermino")]
        public void DatasPreservamFormatoEAceitamSomenteLimiteDoTipo1890(Type tipo, string propriedade, string tag, string evento, string grupo)
        {
            var arquivos = new System.Collections.Generic.Dictionary<string, string>
            {
                { "evtAdmPrelim", "2190_evtAdmPrelim-esocial-evt.xml" },
                { "evtAdmissao", "2200_evtAdmissao-esocial-evt.xml" },
                { "evtDeslig", "2299_evtDeslig-esocial-evt.xml" },
                { "evtTSVInicio", "2300_evtTSVInicio-esocial-evt.xml" },
                { "evtTSVTermino", "2399_evtTSVTermino-esocial-evt.xml" }
            };
            var documento = new XmlDocument();
            documento.Load(Path.Combine(@"..\..\..\ESocial\Resources\S_01_03_00", arquivos[evento]));
            var ns = documento.DocumentElement.NamespaceURI;
            var namespaces = new XmlNamespaceManager(documento.NameTable);
            namespaces.AddNamespace("e", ns);
            var no = documento.SelectSingleNode("/e:eSocial/e:" + evento + "/e:" + grupo.Replace("/", "/e:"), namespaces);
            var serializer = new XmlSerializer(tipo, null, Type.EmptyTypes, new XmlRootAttribute(grupo.Substring(grupo.LastIndexOf('/') + 1)), ns);
            object modelo;
            if (tipo == typeof(InfoEstatutario2200))
            {
                // A massa de admissão existente cobre a alternativa celetista do choice.
                modelo = new InfoEstatutario2200 { TpProv = TipoProvimento.NomeacaoCargoEfetivo };
            }
            else
            {
                Assert.NotNull(no);
                using (var reader = new StringReader(no.OuterXml))
                {
                    modelo = serializer.Deserialize(reader);
                }
            }
            var field = tipo.GetProperty(propriedade);
            field.SetValue(modelo, "1890-01-01");
            string xml;
            using (var writer = new StringWriter())
            {
                serializer.Serialize(writer, modelo);
                xml = writer.ToString();
            }
            Assert.Contains("<" + tag + ">1890-01-01</" + tag + ">", xml);
            using (var reader = new StringReader(xml))
            {
                Assert.Equal("1890-01-01", field.GetValue(serializer.Deserialize(reader)));
            }

            // Validação parcial do elemento exato, no fechamento compilado do evento raiz.
            var schemas = new XmlSchemaSet { XmlResolver = null };
            var schema = CarregarSchema(evento + ".xsd");
            schemas.Add(schema);
            schemas.Compile();
            Assert.Equal(ns, schema.TargetNamespace);
            var elemento = (XmlSchemaElement)schemas.GlobalElements[new XmlQualifiedName("eSocial", ns)];
            foreach (var segmento in (evento + "/" + grupo + "/" + tag).Split('/'))
            {
                elemento = EncontrarElemento(((XmlSchemaComplexType)elemento.ElementSchemaType).ContentTypeParticle, segmento);
                Assert.NotNull(elemento);
            }
            Assert.Equal("TS_dt1890", elemento.ElementSchemaType.QualifiedName.Name);
            ValidarData(schemas, elemento, ns, "1890-01-01");
            Assert.Throws<XmlSchemaValidationException>(() => ValidarData(schemas, elemento, ns, "1889-12-31"));
        }

        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(0)]
        [InlineData(1)]
        public void VersaoAnteriorPreservaRegistroEletronicoInformado(int codigo)
        {
            var modelo = Ler<ESocial1000>("1000_evtInfoEmpregador-esocial-evt.xml");
            modelo.VersaoSchema = "v_S_01_02_00";
            modelo.EvtInfoEmpregador.InfoEmpregador.Inclusao.InfoCadastro.IndOptRegEletron = (IndOptRegEletron)codigo;
            modelo.Signature = AssinaturaEstrutural();
            var xml = modelo.GerarXML();
            var validador = new ValidarSchema();
            validador.Validar(xml, "ESocial.S_01_02_00.evtInfoEmpregador.xsd", xml.DocumentElement.NamespaceURI);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        private static T Ler<T>(string arquivo) where T : XMLBaseESocial, new()
        {
            var xml = new XmlDocument();
            xml.Load(Path.Combine(@"..\..\..\ESocial\Resources\S_01_03_00", arquivo));
            return new T().LerXML<T>(xml);
        }

        private static void Validar(XmlDocument xml, string evento)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "ESocial.S_01_03_00." + evento + ".xsd", xml.DocumentElement.NamespaceURI);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        private static XmlSchema CarregarSchema(string arquivo)
        {
            const string prefixo = "Unimake.Business.DFe.Xml.Schemas.ESocial.S_01_03_00.";
            using (var stream = typeof(ESocial1000).Assembly.GetManifestResourceStream(prefixo + arquivo))
            {
                Assert.NotNull(stream);
                var schema = XmlSchema.Read(stream, null);
                foreach (XmlSchemaExternal externo in schema.Includes)
                {
                    externo.Schema = CarregarSchema(externo.SchemaLocation);
                }
                return schema;
            }
        }

        private static XmlSchemaElement EncontrarElemento(XmlSchemaParticle particle, string nome)
        {
            if (particle is XmlSchemaElement elemento)
            {
                return elemento.Name == nome ? elemento : null;
            }
            if (particle is XmlSchemaGroupBase grupo)
            {
                foreach (XmlSchemaParticle filho in grupo.Items)
                {
                    var encontrado = EncontrarElemento(filho, nome);
                    if (encontrado != null)
                    {
                        return encontrado;
                    }
                }
            }
            return null;
        }

        private static void ValidarData(XmlSchemaSet schemas, XmlSchemaElement elemento, string ns, string valor)
        {
            var nomes = new NameTable();
            var validator = new XmlSchemaValidator(nomes, schemas, new XmlNamespaceManager(nomes), XmlSchemaValidationFlags.None);
            validator.Initialize(elemento);
            validator.ValidateElement(elemento.Name, ns, null);
            validator.ValidateEndOfAttributes(null);
            validator.ValidateText(valor);
            validator.ValidateEndElement(null);
            validator.EndValidation();
        }

        // Conteúdo sintético para validar apenas a estrutura XMLDSig exigida pelo XSD, sem certificado ou assinatura real.
        private static Signature AssinaturaEstrutural() => new Signature
        {
            SignedInfo = new SignedInfo
            {
                CanonicalizationMethod = new CanonicalizationMethod { Algorithm = "http://www.w3.org/TR/2001/REC-xml-c14n-20010315" },
                SignatureMethod = new SignatureMethod { Algorithm = "http://www.w3.org/2000/09/xmldsig#rsa-sha1" },
                Reference = new Reference
                {
                    DigestMethod = new DigestMethod { Algorithm = "http://www.w3.org/2000/09/xmldsig#sha1" },
                    DigestValue = "AA=="
                }
            },
            SignatureValue = "AA=="
        };
    }
}
