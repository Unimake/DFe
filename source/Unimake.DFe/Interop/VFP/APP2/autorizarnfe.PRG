IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
PUBLIC VPROD
XXPROD=0
  
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)


EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
EnviNFe.Versao = "4.00"
EnviNFe.IdLote = "000000000000001"
EnviNFe.IndSinc = 0
EnviNFe.AddNFe(GetNFe())

TRY   
    PUBLIC notaAssinada
    Autorizacao.SetXMLConfiguracao(EnviNFe,Aplicativo.Configuracao.Inicializar)
    notaAssinada = Autorizacao.GetConteudoNFeAssinada(0)
    WAIT CLEAR
*    DO FORM frm_histoa
    DT=DTOC(DATE())
    ARqtmp ="C:\NFEVFP\NFE\NFE"+PADL(ZZNFE,10,"0")+SUBSTR(DT,1,2)+SUBSTR(DT,4,2)+SUBSTR(DT,7,4)+".XML"
 	IF FILE(arqtmp)
	   DELETE FILE &arqtmp
	ENDIF
   LinhaL = ALLTRIM(notaAssinada)+CHR(13)+CHR(10)
   STRTOFILE(linhaL,arqtmp,1)
   MESSAGEBOX("Arquivo Temporario Criado em: "+CHR(13)+ARQTMP)
    autorizacao.Executar(EnviNFe,Aplicativo.Configuracao.Inicializar)
    =MESSAGEBOX("ARQUIVO GERADO: "+cErro.Message,0,'')
CATCH TO cErro
*	=MESSAGEBOX("ERRO INTERNO: Não foi possível acessar o Servidor "+cErro.Message,0,'')
ENDTRY 	
SELECT 200
MESSAGEBOX("CSTAT: "+ALLTRIM(STR(Autorizacao.Result.CStat)) + ": " + Autorizacao.Result.XMotivo)
IF TYPE("Autorizacao.Result.InfRec.NRec") = 'U' && 656 Consumo indevido
	RETURN 0 
ENDIF 

*MESSAGEBOX("Recibo Numero: " + Autorizacao.Result.InfRec.NRec)
replace email WITH cliente.email
REPLACE protocolo WITH  Autorizacao.Result.InfRec.NRec

IF NOT ISNULL(Autorizacao.Result)
    IF Autorizacao.Result.CStat = 103 && 103 = Lote Recebido com Sucesso
        
 *       MESSAGEBOX("Finalizar através da consulta do recibo.")
		ConsReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
		retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
		ConsReciNFe.Versao = "4.00"
		ConsReciNFe.TpAmb = VAMB
		ConsReciNFe.NRec = Autorizacao.Result.InfRec.NRec
	
		TRY 
			retAutorizacao.Executar(ConsReciNFe,Aplicativo.Configuracao.Inicializar) 
		CATCH TO cErro
			=MESSAGEBOX("Não foi possível acessar o Servidor " + cErro.Message,0,'')
		ENDTRY 	

		IF retAutorizacao.Result.GetProtNFeCount > 0 
		    protNFe = retAutorizacao.Result.GetProtNFe(0)
		  *  MESSAGEBOX(""+PROTNFE)
		    IF NOT ISNULL(protNFe)
			    IF VARTYPE(retAutorizacao.Result) = 'O'
			    *	MESSAGEBOX("PROT: "+ALLTRIM(protNFe.InfProt.ChNFe) + " " + ALLTRIM(STR(protNFe.InfProt.CStat)) + " " + protNFe.InfProt.XMotivo)
			    	*MESSAGEBOX("CHAVE: "+ALLTRIM(protNFe.InfProt.ChNFe)+CHR(13)+" " +"CODIGO : "+ ALLTRIM(STR(protNFe.InfProt.CStat)) +CHR(13)+ "DESCRICAO: " + protNFe.InfProt.XMotivo)
			    		    	
				    Autorizacao.RetConsReciNFe = retAutorizacao.Result
			      *		        MESSAGEBOX(Autorizacao.RetConsReciNFe)	    
				    CStat = protNFe.InfProt.CStat
			       * MESSAGEBOX("SAT"+STR(CSTAT))	    
		
			        IF CStat = 100 .OR. CStat = 110 .OR. CStat = 150 .OR. CStat = 205 .OR. CStat = 301 .OR. CStat = 302 .OR. CStat = 303 
					*	MESSAGEBOX("Nota autorizada")
				   	 *   MESSAGEBOX("CHAVE: "+ALLTRIM(protNFe.InfProt.ChNFe)+CHR(13)+" " +"CODIGO : "+ ALLTRIM(STR(protNFe.InfProt.CStat)) +CHR(13)+ "DESCRICAO: " + protNFe.InfProt.XMotivo)
			             waitCenter("Aguarde: Not autorizada Finalizando para impressao")

			            SELECT 200
			            replace chave WITH ALLTRIM(protNFe.InfProt.ChNFe)
			            REPLACE STATUS WITH "Autorizada"
			            REPLACE MOTIVO WITH  ALLTRIM(STR(protNFe.InfProt.CStat))+"-"+protNFe.InfProt.XMotivo
			           * replace valor WITH xxprod
			            SET KEY TO 
			            GOTO bottom
			            XXSAIR="S"
			            SELECT 1
			            USE nfe
			            GOTO TOP
			*            REPLACE NNF WITH nnf+1
			            USE
			            mesano=SUBSTR(DTOC(DATE()),4,2)+ALLTRIM(STR(YEAR(DATE()),4))
						Autorizacao.GravarXmlDistribuicao(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\")
					    MESSAGEBOX("CHAVE: "+ALLTRIM(protNFe.InfProt.ChNFe)+CHR(13)+" " +"CODIGO : "+ ALLTRIM(STR(protNFe.InfProt.CStat)) +CHR(13)+ "DESCRICAO: " + protNFe.InfProt.XMotivo+CHR(13)+;
					    "XML GRAVADO NA PASTA: "+FULLPATH(CURDIR())+ "Retorno\")
				        SELECT 200
				        vxMl=ALLTRIM(CHAVE)+"-procnfe.XML"
                        PFILE=(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\"+VXML)
                      *  loShell = CREATEOBJECT("Shell.Application")
                      *  loShell.ShellExecute(pFile)
                      *  RELEASE loShell
                   	  *   MESSAGEBOX(PFILE)
                     
                   	   IF FILE(pfile)
                         waitCenter("Aguarde: Finalizando NOTA PARA IMPRESSAO")

                         PFILE=(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\"+VXML)
                         PFILEPDF=(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\"+VXML+".PDF")
                       *  MESSAGEBOX(PFILE)
                         Config = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
                         Config.Arquivo = PFILE &&"C:\Projetos\testes\xHb\Unidanfe.Dll\modelo VBS\41101201761135000132550010000000015187640532-procNFe.xml"
                         Config.NomePDF = PFILEPDF &&"C:\Projetos\testes\xHb\Unidanfe.Dll\modelo VBS\qq.pdf"
                         Config.Imprimir = .F.
                         Config.Visualizar = .T.
                         Dfe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
                         Dfe.Execute(Config)
                         SELECT 214
	                     USE docref
	                     GOTO TOP
                         REPLACE XCHAVE WITH " "
                         USE
                         
                    
                     ELSE
                        MESSAGEBOX("ERRO: ARQUIVO PARA IMPRESSAO NAO ENCONTRADO NA PASTA")    
                     endif
      
      			
					ELSE 
					*	MESSAGEBOX("Nota não autorizada")
				        XXSAIR="S"
				        mesano=SUBSTR(DTOC(DATE()),4,2)+ALLTRIM(STR(YEAR(DATE()),4))
*						Autorizacao.GravarXmlDistribuicao(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\")
					    MESSAGEBOX("CHAVE: "+ALLTRIM(protNFe.InfProt.ChNFe)+CHR(13)+" " +"CODIGO : "+ ALLTRIM(STR(protNFe.InfProt.CStat)) +CHR(13)+ "DESCRICAO: " + protNFe.InfProt.XMotivo)
			            SELECT 200
			            replace chave WITH ALLTRIM(protNFe.InfProt.ChNFe)
			            REPLACE STATUS WITH "Rejeitada"
			            REPLACE MOTIVO WITH ALLTRIM(STR(protNFe.InfProt.CStat))+"-" + protNFe.InfProt.XMotivo
			         *   replace valor WITH xxprod
			          
			            SET KEY TO 
			            GOTO bottom
			            SELECT 1
			            USE nfe
			            
			            GOTO TOP
			            REPLACE NNF WITH nnf+1
			            USE
			       
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
