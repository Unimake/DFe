IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
EnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")

EnvEvento.AddEvento(CriarEvento("CFOP errada, segue CFOP correta.", 1))
EnvEvento.AddEvento(CriarEvento("Nome do transportador está errado, segue nome correto.", 2))

EnvEvento.Versao = "1.00"
EnvEvento.IdLote = "000000000000001"

RecepcaoEvento.Executar(EnvEvento,Aplicativo.Configuracao.Inicializar)

* Gravar o XML de distribuição se a inutilização foi homologada
IF (RecepcaoEvento.result.CStat = 128)  && 128 = Lote de evento processado com sucesso

	IF RecepcaoEvento.result.GetRetEventoCount > 0 
	    
	    lcretEvento = "RecepcaoEvento.result.GetRetEvento"
	    FOR i = 0 TO RecepcaoEvento.result.GetRetEventoCount -1
			retEvento =  EVALUATE(lcretEvento + "(" + ALLTRIM(STR(i)) + ")")
            
			IF NOT ISNULL(retEvento)
		    	MESSAGEBOX(ALLTRIM(retEvento.InfEvento.ChNFe) + " " + ALLTRIM(STR(retEvento.InfEvento.CStat)) + " " + retEvento.InfEvento.XMotivo)
			    
			    CStat = retEvento.InfEvento.CStat
			    
			    DO CASE 
			       CASE CStat = 135 .OR. CStat = 136 .OR. CStat = 155
			        
			           MESSAGEBOX(retEvento.InfEvento.DhRegEvento)
					   MESSAGEBOX(retEvento.InfEvento.NProt)
						
			           RecepcaoEvento.GravarXmlDistribuicao(FULLPATH(CURDIR())+'Retorno\')
			       OTHERWISE && Evento rejeitado
			           MESSAGEBOX("Evento rejeitado")
				ENDCASE 			   
			ENDIF 
		ENDFOR 
				    
	ELSE  
	    MESSAGEBOX("Algo ocorreu na consulta do evento que não retornou.")
	ENDIF 
ENDIF 

MESSAGEBOX(RecepcaoEvento.RetornoWSString)

RELEASE RecepcaoEvento
RELEASE EnvEvento 