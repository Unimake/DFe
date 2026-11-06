using System;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    /// <summary>
    /// Preserva o offset da contingência sem alterar o contrato público do INTEROP.
    /// </summary>
    [Trait("DFe", "NFe")]
    public class DhContOffsetTest
    {
        /// <summary>
        /// Mantém o texto de entrada e o instante na serialização e no round-trip.
        /// </summary>
        [Theory]
        [InlineData(55, "2026-11-06T23:30:00-02:00")]
        [InlineData(55, "2026-11-06T23:30:00-03:00")]
        [InlineData(55, "2026-11-06T23:30:00-04:00")]
        [InlineData(55, "2026-11-06T23:30:00-05:00")]
        [InlineData(65, "2026-11-06T23:30:00-02:00")]
        [InlineData(65, "2026-11-06T23:30:00-03:00")]
        [InlineData(65, "2026-11-06T23:30:00-04:00")]
        [InlineData(65, "2026-11-06T23:30:00-05:00")]
        [InlineData(55, "2028-02-29T23:59:59-04:00")]
        [InlineData(65, "2028-02-29T23:59:59-05:00")]
        public void SerializacaoPreservaHorarioEOffset(int modelo, string entrada)
        {
            var ide = CriarIde(modelo);
            ide.DhContField = entrada;
            Assert.Equal(entrada, ide.DhContField);
            Assert.True(ide.ShouldSerializeDhContField());

            var xml = Serializar(ide);
            var tag = XDocument.Parse(xml).Descendants().Single(x => x.Name.LocalName == "dhCont");
            Assert.Equal(entrada, tag.Value);
            Assert.Equal(DateTimeOffset.Parse(entrada).UtcDateTime, DateTimeOffset.Parse(tag.Value).UtcDateTime);

            var serializer = new XmlSerializer(typeof(Ide));
            var copia = (Ide)serializer.Deserialize(new StringReader(xml));
            Assert.Equal(entrada, copia.DhContField);
            Assert.Equal(entrada, XDocument.Parse(Serializar(copia)).Descendants().Single(x => x.Name.LocalName == "dhCont").Value);
#if INTEROP
            // O consumidor DateTime continua recebendo exatamente a conversão anterior.
            Assert.Equal(DateTime.Parse(entrada), ide.DhCont);
            Assert.Equal(DateTime.Parse(entrada).Kind, ide.DhCont.Kind);
#else
            Assert.Equal(DateTimeOffset.Parse(entrada), ide.DhCont);
            Assert.Equal(DateTimeOffset.Parse(entrada).Offset, ide.DhCont.Offset);
#endif
        }

        /// <summary>
        /// Uma atribuição tipada posterior substitui o offset guardado pelo setter textual.
        /// </summary>
        [Fact]
        public void AtribuicaoTipadaDescartaOffsetAnterior()
        {
            var ide = CriarIde(55);
            ide.DhContField = "2026-11-06T23:30:00-05:00";
#if INTEROP
            var novaData = new DateTime(2026, 11, 8, 12, 34, 56, DateTimeKind.Unspecified);
#else
            var novaData = new DateTimeOffset(2026, 11, 8, 12, 34, 56, TimeSpan.FromHours(-2));
#endif
            ide.DhCont = novaData;
            Assert.Equal(novaData.ToString("yyyy-MM-ddTHH:mm:sszzz"), ide.DhContField);
        }

        /// <summary>
        /// Limpar a data também elimina o estado de offset e omite a tag.
        /// </summary>
        [Fact]
        public void DataMinimaOmiteContingenciaDepoisDePreenchida()
        {
            var ide = CriarIde(65);
            ide.DhContField = "2026-11-06T23:30:00-05:00";
#if INTEROP
            ide.DhCont = DateTime.MinValue;
#else
            ide.DhCont = DateTimeOffset.MinValue;
#endif
            Assert.False(ide.ShouldSerializeDhContField());
            Assert.DoesNotContain(XDocument.Parse(Serializar(ide)).Descendants(), x => x.Name.LocalName == "dhCont");
        }

        /// <summary>
        /// Reutilizar o objeto não reaproveita um offset antigo.
        /// </summary>
        [Fact]
        public void AtribuicoesTextuaisConsecutivasSubstituemOffset()
        {
            var ide = CriarIde(55);
            foreach (var entrada in new[] { "2026-11-06T23:30:00-04:00", "2026-11-06T23:30:00-05:00", "2026-11-06T23:30:00-03:00" })
            {
                ide.DhContField = entrada;
                Assert.Equal(entrada, ide.DhContField);
            }
        }

        /// <summary>
        /// Um valor inválido não deixa estado parcialmente alterado.
        /// </summary>
        [Fact]
        public void TextoInvalidoPreservaEstadoAnterior()
        {
            var ide = CriarIde(55);
            const string anterior = "2026-11-06T23:30:00-04:00";
            ide.DhContField = anterior;
            Assert.Throws<FormatException>(() => ide.DhContField = "DATA INVALIDA");
            Assert.Equal(anterior, ide.DhContField);
        }

        /// <summary>
        /// O tipo público não muda para os consumidores existentes.
        /// </summary>
        [Fact]
        public void PreservaTipoDaPropriedadePublica()
        {
#if INTEROP
            Assert.Equal(typeof(DateTime), typeof(Ide).GetProperty(nameof(Ide.DhCont)).PropertyType);
#else
            Assert.Equal(typeof(DateTimeOffset), typeof(Ide).GetProperty(nameof(Ide.DhCont)).PropertyType);
#endif
        }

        private static Ide CriarIde(int modelo)
        {
            var ide = new Ide();
            // Inicializa enums válidos somente para permitir serializar o contêiner isolado.
            foreach (var property in typeof(Ide).GetProperties().Where(x => x.CanWrite && x.PropertyType.IsEnum))
            {
                property.SetValue(ide, Enum.GetValues(property.PropertyType).GetValue(0));
            }
            ide.Mod = (ModeloDFe)modelo;
            ide.DhEmiField = "2026-11-06T12:00:00-03:00";
            ide.CNF = "12345678";
            return ide;
        }

        private static string Serializar(Ide ide)
        {
            var serializer = new XmlSerializer(typeof(Ide));
            using (var writer = new StringWriter())
            {
                serializer.Serialize(writer, ide);
                return writer.ToString();
            }
        }
    }
}
