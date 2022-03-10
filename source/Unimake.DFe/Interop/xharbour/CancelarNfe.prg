* ---------------------------------------------------------------------------------
* Carregando certificado A1
* ---------------------------------------------------------------------------------
Function CancelarNfe()
 Local oCertificado, oCertificadoSelecionado

* Criar objeto com Certificado A1 informado
 oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
 oCertificadoSelecionado = oCertificado:CarregarCertificadoDigitalA1("C:\Projetos\certificados\UnimakePV.pfx","12345678")

 ? "ID do Certificado....: ", oCertificado:GetThumbPrint(oCertificadoSelecionado)
 ? 
 ? ">>> EM CONSTRUCAO <<<"

 /*
var xml = new EnvEvento
{
    Versao = "1.00",
    IdLote = "000000000000001",
    Evento = new List<Evento> {
        new Evento
        {
            Versao = "1.00",
            InfEvento = new Unimake.Business.DFe.Xml.NFe.InfEvento(new Unimake.Business.DFe.Xml.NFe.DetEventoCanc
            {
                NProt = "141190000660363",
                Versao = "1.00",
                XJust = "Justificativa para cancelamento da NFe de teste"
            })
            {
                COrgao = UFBrasil.PR,
                ChNFe = "41190806117473000150550010000579131943463890",
                CNPJ = "06117473000150",
                DhEvento = DateTime.Now,
                TpEvento = TipoEventoNFe.Cancelamento,
                NSeqEvento = 1,
                VerEvento = "1.00",
                TpAmb = TipoAmbiente.Homologacao
            }
        }
    }
};

var configuracao = new Configuracao
{
    CertificadoDigital = CertificadoSelecionado
};

var recepcaoEvento = new RecepcaoEvento(xml, configuracao);
recepcaoEvento.Executar();

MessageBox.Show(recepcaoEvento.RetornoWSString);
MessageBox.Show(recepcaoEvento.Result.XMotivo);

//Gravar o XML de distribuição se a inutilização foi homologada
if (recepcaoEvento.Result.CStat == 128) //128 = Lote de evento processado com sucesso
{
    switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
    {
        case 135: //Evento homologado com vinculação da respectiva NFe
        case 136: //Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
        case 155: //Evento de Cancelamento homologado fora do prazo permitido para cancelamento
            recepcaoEvento.GravarXmlDistribuicao(@"c:\testenfe\");
            break;

        default: //Evento rejeitado
            recepcaoEvento.GravarXmlDistribuicao(@"c:\testenfe\");
            break;
    }
}
*/


 Wait
RETURN