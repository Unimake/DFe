* ----------------------------------------------------------------------------------- 
* Consulta de documentos fiscais destinados / Consulta NFe´s emitidas contra meu CNPJ 
* ----------------------------------------------------------------------------------- 
FUNCTION ConsultaDFe() 
   LOCAL oConfig 
   LOCAL oDistDFeInt, oDistNSU 
   LOCAL oErro, oExceptionInterop 
   LOCAL oDistribuicaoDFe 
   LOCAL nsu, folder 
    
 * Criar configuração básica para consumir o serviço 
   oConfig := CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig:CertificadoSenha   := "12345678"
    
   nsu := strzero( 0, 15 ) 
    
   ? "vai processar nsu ", nsu      
    
 * Criar objeto para pegar exceção do lado do CSHARP 
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper") 
    
   TRY 
      DO WHILE .T. 
       * Criar XML da consulta DFe 
         oDistDFeInt = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.DistDFeInt") 
         oDistDFeInt:Versao = "1.35" && ou 1.01 
         oDistDFeInt:TpAmb = 2 && 2=Homologação 
         oDistDFeInt:CNPJ = "06117473000150"     
         oDistDFeInt:CUFAutor = 41 && UFBrasil.PR 
          
         oDistNSU        := CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.DistNSU") 
         oDistNSU:UltNSU := nsu 
          
         oDistDFeInt:DistNSU = oDistNSU
 
       * Consumir o serviço (enviar o XML de consulta e tratar o retorno do webservice) 
         oDistribuicaoDFe = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe") 
         oDistribuicaoDFe:Executar(oDistDFeInt, oConfig) 
          
         ? oDistribuicaoDFe:RetornoWSString
          
       * Gravar o XML retornado da receita federal no HD 
         ferase( "d:\testenfe\RetornoDfe-" + nsu + ".xml" )
         memowrit( "d:\testenfe\RetornoDfe-" + nsu + ".xml",  oDistribuicaoDFe:RetornoWSString )
          
         IF oDistribuicaoDFe:Result:CStat = 138 && 138=Documentos localizados 
          * Pasta onde vamos gravar os XMLs retornados pela SEFAZ 
            folder = "d:\testenfe\doczip"
 
          * Salvar o XML retornados na consulta             
          * <param name="folder">Nome da pasta onde é para salvar os XML</param> 
          * <param name="saveXMLSummary">Salvar os arquivos de resumo da NFe e Eventos?</param> 
          * <param name="fileNameWithNSU">true=Salva os arquivos da NFe e seus eventos com o NSU no nome do arquivo / false=Salva os arquivos da NFe e seus eventos com o CHAVE da NFe no nome do arquivo</param> 
            oDistribuicaoDFe:GravarXMLDocZIP(folder, .T., .T.) 
         ELSE  
            IF oDistribuicaoDFe:Result:CStat = 656 && 656 = Consumo indevido 
               * Abortar a operação e só voltar a consultar novamente após 1 hora (nossa experiencia nos levou a usar 1h10m) 
               EXIT 
            ENDIF  
         ENDIF       
 
       * Salvar o conteudo da variável "nsu" na base de dados para que na proxima consulta continue de  
       * onde parou para não gerar consumo indevido 
         nsu := oDistribuicaoDFe:Result:UltNSU 
          
       * Se atingiu o maxNSU tem que parar as consultas e iniciar novamente após 1 hora (para não gerar consumo indevido e nossa experiencia nos levou a usar 1h10m), o mesmo processo, só que não mais 
       * do nsu ZERO e sim do que ficou na variável "nsu" acima, ou seja, vai dar sequencia.   
         IF Val(oDistribuicaoDFe:Result:UltNSU) >= Val(oDistribuicaoDFe:Result:MaxNSU)
            EXIT 
         ENDIF 
      ENDDO 
	  
	  Wait
    
    CATCH oErro 
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()      
	  ?  

	  Wait
	  cls   
   END
RETURN