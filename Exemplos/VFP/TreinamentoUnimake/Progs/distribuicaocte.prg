* -----------------------------------------------------------------------------------
* Distribuição CTe - Download dos XMLs de CTe pelos envolvidos/intereressados
* -----------------------------------------------------------------------------------
FUNCTION DistribuicaoCTe()
   LOCAL oConfig
   LOCAL oDistDFeInt, oDistNSU
   LOCAL oErro, oExceptionInterop
   LOCAL oDistribuicaoDFe
   LOCAL nsu, folder
   
 * Criar configuração básica para consumir o serviço
   oConfig = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.TipoDFe = 2 && CTe
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"  
   
   nsu = "000000000000000"
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")
   
   TRY
      DO WHILE .T.
       * Criar XML da consulta DFe
         oDistDFeInt = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.DistDFeInt")
         oDistDFeInt.Versao = "1.00"
         oDistDFeInt.TpAmb= 1 && 1=Produção
         oDistDFeInt.CNPJ = "06117473000150"
         oDistDFeInt.CUFAutor = 41 && UFBrasil.PR
         
         oDistNSU = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.DistNSU")
         oDistNSU.UltNSU = nsu
         
         oDistDFeInt.DistNSU = oDistNSU

       * Consumir o serviço (enviar o XML de consulta e tratar o retorno do webservice)
         oDistribuicaoDFe = CREATEOBJECT("Unimake.Business.DFe.Servicos.CTe.DistribuicaoDFe")
         oDistribuicaoDFe.Executar(oDistDFeInt, oConfig)
         
         MESSAGEBOX(oDistribuicaoDFe.RetornoWSString)
         
       * Gravar o XML retornado da receita federal no HD
         DELETE FILE 'd:\testenfe\' + 'RetornoDfe-' + nsu + '.xml'
	     StrToFile(oDistribuicaoDFe.RetornoWSString, 'd:\testenfe\' + 'RetornoDfe-' + nsu + '.xml', 0)
         
         IF oDistribuicaoDFe.Result.CStat = 138 && 138=Documentos localizados
          * Pasta onde vamos gravar os XMLs retornados pela SEFAZ
            folder = "d:\testenfe\doczip"
            oDistribuicaoDFe.GravarXMLDocZIP(folder)
         ELSE 
            IF oDistribuicaoDFe.Result.CStat = 656 && 656 = Consumo indevido
               * Abortar a operação e só voltar a consultar novamente após 1 hora (nossa experiencia nos levou a usar 1h10m)
               EXIT
            ENDIF 
         ENDIF      

       * Salvar o conteudo da variável "nsu" na base de dados para que na proxima consulta continue de 
       * onde parou para não gerar consumo indevido
         nsu = oDistribuicaoDFe.Result.UltNSU
         
       * Se atingiu o maxNSU tem que parar as consultas e iniciar novamente após 1 hora (para não gerar consumo indevido e nossa experiencia nos levou a usar 1h10m), o mesmo processo, só que não mais
       * do nsu ZERO e sim do que ficou na variável "nsu" acima, ou seja, vai dar sequencia.  
         IF oDistribuicaoDFe.Result.UltNSU >= oDistribuicaoDFe.Result.MaxNSU
            EXIT
         ENDIF
      ENDDO
	  
    CATCH TO oErro
    * Exceção do FOXPRO
	* Mais sobre exceção em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX(oErro.ErrorNo)
	  MESSAGEBOX("Exceção foxpro: " + oErro.Message)
	  
    * Exceção do CSHARP
      MESSAGEBOX(oExceptionInterop.GetMessage())
      MESSAGEBOX(oExceptionInterop.GetErrorCode())
   ENDTRY   
RETURN