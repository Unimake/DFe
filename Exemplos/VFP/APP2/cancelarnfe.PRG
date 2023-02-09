IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
EnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
Evento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")
InfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
DetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCanc")

DetEventoCanc.NProt = "141190000660363"
DetEventoCanc.Versao = "1.00"
DetEventoCanc.XJust = "Justificativa para cancelamento da NFe de teste"
InfEvento.DetEvento = DetEventoCanc
InfEvento.COrgao = 33
InfEvento.ChNFe = "41190806117473000150550010000579131943463890"
InfEvento.CNPJ = "06117473000150"
InfEvento.DhEvento = DATETIME()
InfEvento.TpEvento = 110111
InfEvento.NSeqEvento = 1
InfEvento.VerEvento = "1.00"
InfEvento.TpAmb = 2
Evento.Versao = "1.00"
Evento.InfEvento = InfEvento
EnvEvento.AddEvento(Evento)
EnvEvento.Versao = "1.00"
EnvEvento.IdLote = "000000000000001"

RecepcaoEvento.Executar(EnvEvento,Aplicativo.Configuracao.Inicializar)
xmlDistrib = RecepcaoEvento.GetProcEventoNFeResultXMLByIndex(0)

MESSAGEBOX(xmlDistrib)

*Gravar o XML de distribuição se a inutilização foi homologada
IF RecepcaoEvento.result.CStat = 128 && 128 = Lote de evento processado com sucesso
    CStat = RecepcaoEvento.result.GetRetEvento(0).InfEvento.CStat
    
    * 135: Evento homologado com vinculação da respectiva NFe
    * 136: Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
    * 155: Evento de Cancelamento homologado fora do prazo permitido para cancelamento            
    DO CASE  
       CASE CStat = 135 .OR. CStat = 136 .OR. CStat = 155
       
       		MESSAGEBOX(recepcaoEvento.Result.GetRetEvento(0).InfEvento.DhRegEvento)
			MESSAGEBOX(recepcaoEvento.Result.GetRetEvento(0).InfEvento.NProt)
			
            RecepcaoEvento.GravarXmlDistribuicao(FULLPATH(CURDIR())+'Retorno\')
       OTHERWISE 
            MESSAGEBOX("Evento rejeitado")
    ENDCASE 
ENDIF 

MESSAGEBOX(RecepcaoEvento.RetornoWSString)

RELEASE RecepcaoEvento
RELEASE EnvEvento
RELEASE Evento
RELEASE InfEvento
RELEASE DetEventoCanc
