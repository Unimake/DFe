IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

DistDFeInt = CreateObject("Unimake.Business.DFe.xml.NFe.DistDFeInt")
DistNSU = CreateObject("Unimake.Business.DFe.xml.NFe.DistNSU")
DistribuicaoDFe = CreateObject("Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe")

nsu = "000000000000000"

DO WHILE .T.
    WAIT WINDOW "Aguarde, consultando NSU número " + nsu NOWAIT
    DistNSU.UltNSU = nsu
    DistDFeInt.Versao = "1.01"
    DistDFeInt.TpAmb = 2
    DistDFeInt.CNPJ = "06117473000150"
    DistDFeInt.CUFAutor = 41
    DistDFeInt.DistNSU = DistNSU

    DistribuicaoDFe.Executar(DistDFeInt,Aplicativo.Configuracao.Inicializar)
    
    IF DistribuicaoDFe.result.CStat = 593 
    	MESSAGEBOX(ALLTRIM(STR(DistribuicaoDFe.result.CStat)) + ": " + DistribuicaoDFe.result.XMotivo)
    	EXIT 
    ENDIF 
    
    If DistribuicaoDFe.result.CStat = 138  && Documentos localizados

        pastaDistribuicao = FULLPATH(CURDIR())+'Distribuicao'    
		If NOT DIRECTORY(pastaDistribuicao ) 
			MKDIR(pastaDistribuicao)
		ENDIF
        
        DistribuicaoDFe.GravarXMLDocZIP(FULLPATH(CURDIR())+'Distribuicao\', .t. ) && Parametro bool true
    ENDIF 
    
    nsu = distribuicaoDFe.Result.UltNSU
    
    MESSAGEBOX(nsu) 
        
	IF INT(VAL(DistribuicaoDFe.result.UltNSU)) >= INT(VAL(DistribuicaoDFe.result.MaxNSU))
		EXIT
	ENDIF 	
ENDDO  

RELEASE DistDFeInt
RELEASE DistNSU 
RELEASE DistribuicaoDFe