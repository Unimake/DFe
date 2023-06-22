* ---------------------------------------------------------------------------------
* Autorizar o XML de NFe gerado em EPEC
* ---------------------------------------------------------------------------------
FUNCTION EPECEnviarXMLNFe()         
   Local oConfig
   Local oEnviNFe, oNfe, oInfNFe, oIde, oEmit, oEnderEmit, oDest, oEnderDest
   Local oDet, oProd
   Local oImposto, oICMS, oICMSSN101, oPIS, oPISOutr, oCOFINS, oCOFINSOutr
   Local oTotal, oICMSTot, oImpostoDevol
   Local oTransp, oVol
   Local oCobr, oFat, oDup
   Local oPag, oDetPag
   Local oInfAdic, oInfRespTec
   Local oAutorizacao, oRetAutorizacao, oXmlRec, oConfigRec
   Local I, oErro, notaAssinada
   Local oXmlConsSitNFe, oConteudoNFe, oConteudoInfNFe, chaveNFe, oConfigConsSitNFe, oConsultaProtocolo

 * Criar configuracao basica para consumir o servico
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.TipoDfe = 0 && 0=nfe
   oConfig.TipoEmissao = 1 && 1=TipoEmissao.NORMAL
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"
   
 * Criar a tag <enviNFe>
   oEnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oEnviNFe.Versao = "4.00"
   oEnviNFe.IdLote = "000000000000001"
   oEnviNFe.IndSinc = 1 && 1=Sim 0=Nao
   
 * Criar a tag <NFe>  
   oNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
   oEnviNFe.AddNFe(oNFe.LoadFromFile("D:\testenfe\epec\41230606117473000150550030000000064860795147-nfe.xml"))

   TRY 
   * Criar o objeto para consumir o serviço, mas vamos somente pegar o XML e não vamos enviar
     oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
     oAutorizacao.SetXMLConfiguracao(oEnviNFe, oConfig)
     
   * Guardar o XML da NFe já assinada para transmitirmos sem mudar nada depois que o serviço voltar ao normal.
   * XML tem que ser guardado e este que deve ser transmitido para evitar diferenças entre a NFe ou CTe e o evento de EPEC
     notaAssinada = oAutorizacao.GetConteudoNFeAssinada(0)
     
     MessageBox(notaAssinada) && Demonstrar o XML da nota assinada na tela

     oAutorizacao.Executar(oEnviNFe, oConfig)
	  
     MESSAGEBOX(oAutorizacao.RetornoWSString)

     IF oAutorizacao.Result.CStat == 104 && 104 = Lote Processado
        IF oAutorizacao.Result.ProtNFe.InfProt.CStat == 100 && 100 = Autorizado o uso da NF-e
         * Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
           oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
			
		 * Pegar a string do XML de distribuição
           docProcNFe = oAutorizacao.GetNFeProcResults(chaveNFe)
		   MESSAGEBOX(docProcNFe)

         * Como pegar o numero do protocolo de autorizacao para gravar na base
		   MESSAGEBOX(oAutorizacao.Result.ProtNFe.InfProt.NProt)
		ELSE
          * Rejeitada ou Denegada - Fazer devidos tratamentos		 
        ENDIF
	  ENDIF
	  
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      