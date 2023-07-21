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
   oConfig = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"  
   
   nsu = "000000000000000"
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")
   
   TRY
      DO WHILE .T.
       * Criar XML da consulta DFe
         oDistDFeInt = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.DistDFeInt")
         oDistDFeInt.Versao = "1.35" && ou 1.01
         oDistDFeInt.TpAmb= 1 && 1=Homologação
         oDistDFeInt.CNPJ = "06117473000150"
         oDistDFeInt.CUFAutor = 41 && UFBrasil.PR
         
         oDistNSU = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.DistNSU")
         oDistNSU.UltNSU = nsu
         
         oDistDFeInt.DistNSU = oDistNSU

       * Consumir o serviço (enviar o XML de consulta e tratar o retorno do webservice)
         oDistribuicaoDFe = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe")
         oDistribuicaoDFe.Executar(oDistDFeInt, oConfig)
         
         MESSAGEBOX(oDistribuicaoDFe.RetornoWSString)
         
       * Gravar o XML retornado da receita federal no HD
         DELETE FILE 'd:\testenfe\' + 'RetornoDfe-' + nsu + '.xml'
	     StrToFile(oDistribuicaoDFe.RetornoWSString, 'd:\testenfe\' + 'RetornoDfe-' + nsu + '.xml', 0)
         
         IF oDistribuicaoDFe.Result.CStat = 138 && 138=Documentos localizados
          * Pasta onde vamos gravar os XMLs retornados pela SEFAZ
            folder = "d:\testenfe\doczip"

          * Salvar o XML retornados na consulta            
          * <param name="folder">Nome da pasta onde é para salvar os XML</param>
          * <param name="saveXMLSummary">Salvar os arquivos de resumo da NFe e Eventos?</param>
          * <param name="fileNameWithNSU">true=Salva os arquivos da NFe e seus eventos com o NSU no nome do arquivo / false=Salva os arquivos da NFe e seus eventos com o CHAVE da NFe no nome do arquivo</param>
            oDistribuicaoDFe.GravarXMLDocZIP(folder, .T., .T.) 
            
          * Como pegar o conteúdo retornado na consulta no formato string
            FOR I = 1 TO oDistribuicaoDFe.Result.LoteDistDFeInt.GetDocZipCount()
                oDocZip = oDistribuicaoDFe.Result.LoteDistDFeInt.GetDocZip(I-1)
                
              * Conteudo do XML retornado no formato string
                MESSAGEBOX(oDocZip.ConteudoXML)
                
              * Tipo do XML:
              * 1 = XML de resumo de eventos
              * 2 = XML de resumo da NFe
              * 3 = XML de distribuição de eventos da NFe (XML completo do evento)
              * 4 = XML de distribuição da NFe (XML completo da NFe)
              * 5 = XML de distribuição de eventos da CTe (XML completo do evento)
              * 6 = XML de distribuição do CTe (XML completo do CTe)
              * 0 = XML desconhecido
                MESSAGEBOX(oDocZip.TipoXML)
            NEXT I
            
          * Como pegar os retornos dos resumos de eventos em objeto
            FOR I = 1 TO oDistribuicaoDFe.GetResEventosCount()
                oResEvento = oDistribuicaoDFe.GetResEvento(I-1)
                
                MESSAGEBOX(oResEvento.ChNFe)
                MESSAGEBOX(oResEvento.CNPJ)
            NEXT I   
            
          * Como pegar os retornos dos resumos de NFe em objeto
            FOR I = 1 TO oDistribuicaoDFe.GetResNFeCount()
                oResNFe = oDistribuicaoDFe.GetResNFe(I-1)
                
                MESSAGEBOX(oResNFe.ChNFe)
                MESSAGEBOX(oResNFe.CNPJ)
            NEXT I
            
          * Como pegar os retornos dos XML de Distribuição dos Eventos (XML completos dos eventos)
            FOR I = 1 TO oDistribuicaoDFe.GetProcEventoNFesCount()
                oProcEventoNFe = oDistribuicaoDFe.GetProcEventoNFes(I-1)
                
                MESSAGEBOX(oProcEventoNFe.Evento.InfEvento.CNPJ)
                MESSAGEBOX(oProcEventoNFe.Evento.InfEvento.ChNFe)
            NEXT I   

          * Como pegar os retornos dos XML de Distribuição das NFes (XML completos das NFes)
            FOR I = 1 TO oDistribuicaoDFe.GetProcNFesCount()
                oNfeProc = oDistribuicaoDFe.GetProcNFes(I-1)
                
                oInfNFe = oNfeProc.NFe.GetInfNFe(0)

                MESSAGEBOX(oInfNFe.Id)
                MESSAGEBOX(oInfNFe.IDE.CUF)
                MESSAGEBOX(oInfNFe.IDE.CNF)                
                MESSAGEBOX(oNFeProc.ProtNFe.InfProt.ChNFe)
                MESSAGEBOX(oNFeProc.ProtNFe.InfProt.NProt)
             NEXT I        
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
         IF STR(oDistribuicaoDFe.Result.UltNSU,15) >= STR(oDistribuicaoDFe.Result.MaxNSU)  
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