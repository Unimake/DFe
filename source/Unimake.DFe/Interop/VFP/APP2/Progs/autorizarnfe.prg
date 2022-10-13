IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
EnviNFe.Versao = "4.00"
EnviNFe.IdLote = "000000000000001"
EnviNFe.IndSinc = 0
EnviNFe.AddNFe(GetNFe())

TRY   
	Autorizacao.Executar(EnviNFe,Aplicativo.Configuracao.Inicializar)
CATCH TO cErro
	=MESSAGEBOX("Não foi possível acessar o Servidor " + cErro.Message,0,'')
ENDTRY 	

MESSAGEBOX(Autorizacao.RetornoWSString)
MESSAGEBOX(ALLTRIM(STR(Autorizacao.Result.CStat)) + ": " + Autorizacao.Result.XMotivo)

IF TYPE("Autorizacao.Result.InfRec.NRec") = 'U' && 656 Consumo indevido
	RETURN 0 
ENDIF 

MESSAGEBOX("Recibo Numero: " + Autorizacao.Result.InfRec.NRec)

IF NOT ISNULL(Autorizacao.Result)
    IF Autorizacao.Result.CStat = 103 && 103 = Lote Recebido com Sucesso
        
        * Finalizar através da consulta do recibo.
		ConsReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
		retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
		ConsReciNFe.Versao = "4.00"
		ConsReciNFe.TpAmb = 2
		ConsReciNFe.NRec = Autorizacao.Result.InfRec.NRec
	
		TRY 
			retAutorizacao.Executar(ConsReciNFe,Aplicativo.Configuracao.Inicializar) 
		CATCH TO cErro
			=MESSAGEBOX("Não foi possível acessar o Servidor " + cErro.Message,0,'')
		ENDTRY 	

		IF retAutorizacao.Result.GetProtNFeCount > 0 
		    protNFe = retAutorizacao.Result.GetProtNFe(0)
		    
		    IF NOT ISNULL(protNFe)
			    IF VARTYPE(retAutorizacao.Result) = 'O'
			    	MESSAGEBOX(ALLTRIM(protNFe.InfProt.ChNFe) + " " + ALLTRIM(STR(protNFe.InfProt.CStat)) + " " + protNFe.InfProt.XMotivo)
			    		    	
				    Autorizacao.RetConsReciNFe = retAutorizacao.Result
				    
				    CStat = protNFe.InfProt.CStat
			        IF CStat = 100 .OR. CStat = 110 .OR. CStat = 150 .OR. CStat = 205 .OR. CStat = 301 .OR. CStat = 302 .OR. CStat = 303 
						MESSAGEBOX("Nota autorizada")
						Autorizacao.GravarXmlDistribuicao(FULLPATH(CURDIR())+ "Retorno\")
					ELSE 
						MESSAGEBOX("Nota não autorizada")
			 		ENDIF
			 	 ENDIF 
			ENDIF 
		ELSE  
		    MESSAGEBOX("Algo ocorreu na consulta recibo que não retornou o protocolo da nota, talvez tenha consultado um recibo que não existe.")
		ENDIF 
	ELSE 
		MESSAGEBOX(ALLTRIM(STR(retAutorizacao.Result.CStat)) + " " + retAutorizacao.Result.XMotivo)
	ENDIF 
ENDIF 

RELEASE EnviNFe 
RELEASE Autorizacao
RELEASE ConsReciNFe 
RELEASE retAutorizacao
