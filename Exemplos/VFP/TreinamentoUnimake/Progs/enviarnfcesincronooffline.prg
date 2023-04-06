* ---------------------------------------------------------------------------------
* Enviar NFCe no modo síncro em contingência off-line
* situação (consulta via chave da nfe)
* ---------------------------------------------------------------------------------

* - Status de envio do proprio software
* -- 0 - Processo de envio iniciado
* -- 1 - XML gerado, assinado e arquivado
* -- 2 - XML gerado e emitido danfe em contingência offline
* -- 3 - XML enviado
* -- 4 - Retorno recebido e dados importantes arquivados (Protocolo, etc.)
* -- 5 - XML de distribuição arquivado
* -- 6 - Processo de envio concluído com sucesso
* - E se a nota foi duplicada? Com isso não vai acontecer, mas se for faça o seguinte

FUNCTION EnviarNfceSincronoOffLine()
   LOCAL Config
   LOCAL EnviNFe, oNfe, oInfNFe, oIde, oEmit, oEnderEmit, oDest, oEnderDest
   LOCAL Det, oProd
   LOCAL Imposto, oICMS, oICMSSN101, oPIS, oPISOutr, oCOFINS, oCOFINSOutr
   LOCAL Total, oICMSTot, oImpostoDevol
   LOCAL Transp, oVol
   LOCAL Cobr, oFat, oDup
   LOCAL Pag, oDetPag
   LOCAL InfAdic, oInfRespTec
   LOCAL Autorizacao, oRetAutorizacao, oXmlRec, oConfigRec
   LOCAL oErro, notaAssinada
   LOCAL XmlConsSitNFe, oConteudoNFe, oConteudoInfNFe, chaveNFe, oConfigConsSitNFe, oConsultaProtocolo
   LOCAL nStatusEnvio, lEnvioOffline, lSair, lConexaoNet

   nStatusEnvio = 0 && Processo de envio iniciado   
   lEnvioOffline = .F.
   lConexaoNet = .T. && Temos conexão com a internet (Executar alguma rotina que teste a internet)
   xJust = "Emitida em contingencia em decorrencia de falhas da infraestrutura de rede/internet"
   lSair = .T.
   
   IF .Not. lConexaoNet
      lEnvioOffLine = .T.
      xJust = "Emitido em contingencia por falta de internet"
   ENDIF      

   DO WHILE .T.
	 * Criar configuracao basica para consumir o servico
	   oConfig = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
	   oConfig.TipoDfe = 1 && 1=nfce
	   oConfig.TipoEmissao = IIF(lEnvioOffLine,9,1) && 1=Normal 9=Contingência offline
	   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
	   oConfig.CertificadoSenha = "12345678"
	   oConfig.CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG"
	   oConfig.CSCIDToken = 2   
	   
	 * Criar a tag <enviNFe>
	   oEnviNFe = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.EnviNFe")
	   oEnviNFe.Versao = "4.00"
	   oEnviNFe.IdLote = "000000000000001"
	   oEnviNFe.IndSinc = 1 && 1=Sim 0=Nao
	   
	 * Criar a tag <NFe>  
	   oNfe = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.NFe")
	   
       IF nStatusEnvio >= 1 .And. .Not. lEnvioOffline && XML gerado, assinado e arquivado
	    * Vamos desserializar XML assinado que já temos no HD ou no banco de dados
	      oEnviNFe.AddNFe(oNFe.LoadFromFile("D:\testenfe\cont\41230406117473000150650010000590059431907003-nfe.xml"))
	   ELSE
		 * Criar tag InfNfe
		   oInfNFe = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.InfNFe")
		   oInfNFe.Versao = "4.00"

		 * cria tag Ide
		   oIde = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Ide")
		   oIde.CUF = 41 && Brasil.PR
		   oIde.NatOp = "VENDA PRODUC.DO ESTABELEC"
		   oIde.Mod = 65 && NFCe
		   oIde.Serie    = 1
		   oIde.NNF      = 59005
		   oIde.DhEmi    = DateTime()
		   oIde.DhSaiEnt = DateTime()
		   oIde.TpNF     = 1 && Saida
		   oIde.IdDest   = 1 && DestinoOperacao.OperacaoInterna
		   oIde.CMunFG   = 4118402
		   oIde.TpImp    = 4 && FormatoImpressaoDANFE.NFCe 
		   oIde.TpEmis   = IIF(lEnvioOffline, 9, 1) && TipoEmissao.ContingenciaOFFLine ###
		   oIde.TpAmb    = 2 && TipoAmbiente.Homologacao
		   oIde.FinNFe   = 1 && FinalidadeNFe.Normal
		   oIde.IndFinal = 1 && SimNao.Sim
		   oIde.IndPres  = 1 && IndicadorPresenca.OperacaoPresencial
		   oIde.ProcEmi  = 0 && ProcessoEmissao.AplicativoContribuinte
		   oIde.VerProc  = "TESTE 1.00"
		   
		   IF lEnvioOffLine
	   	      oIde.DhCont   = DateTime()  && ###
	          oIde.xJust    = xJust
	       ENDIF
		   
		 * adicionar a tag Ide dentro da tag InfDfe
		   oInfNFe.Ide = oIde

		 * criar tag Emit
		   oEmit = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Emit")
		   oEmit.CNPJ  = "06117473000150"
		   oEmit.XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
		   oEmit.XFant = "UNIMAKE - PARANAVAI"
		   oEmit.IE    = "9032000301"
		   oEmit.IM    = "14018"
		   oEmit.CNAE  = "6202300"
		   oEmit.CRT   = 1 && CRT.SimplesNacional

		   oEnderEmit = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.EnderEmit")
		   oEnderEmit.XLgr    = "RUA PAULO ANTONIO COSTA"
		   oEnderEmit.Nro     = "575"
		   oEnderEmit.XBairro = "CENTRO"
		   oEnderEmit.CMun    = 4118402
		   oEnderEmit.XMun    = "PARANAVAI"
		   oEnderEmit.UF      = 41 && UFBrasil.PR
		   oEnderEmit.CEP     = "87707210"
		   oEnderEmit.Fone    = "04431421010"
		   
		 * adicionar a tag EnderEmit dentro da tag Emit 
		   oEmit.EnderEmit = oEnderEmit
		   
		 * adicionar a tag Emit dentro da tag InfNfe
		   oInfNfe.Emit = oEmit
		   
	     * criar tag Dest	   
	       oDest = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Dest") 
	       oDest.CNPJ      = "04218457000128"
	       oDest.XNome     = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
	       oDest.IndIEDest = 9 && IndicadorIEDestinatario.NaoContribuinte,
	     * oDest.Email     = "janelaorp@janelaorp.com.br"
	   
	     * oEnderDest = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.EnderDest")
	     * oEnderDest.XLgr    = "AVENIDA DA SAUDADE"
	     * oEnderDest.Nro     = "1555"
	     * oEnderDest.XBairro = "CAMPOS ELISEOS"
	     * oEnderDest.CMun    = 3543402
	     * oEnderDest.XMun    = "RIBEIRAO PRETO"
	     * oEnderDest.UF      = 35 // UFBrasil.SP
	     * oEnderDest.CEP     = "14080000"
	     * oEnderDest.Fone    = "01639611500"

	     * adicionar a tag EnderDest dentro da tag Dest 
	     * oDest:EnderDest = oEnderDest

	     * adicionar a tag Emit dentro da tag InfNfe
	       oInfNfe.Dest = oDest
		   
		   For I = 1 To 3 && 3 produtos para teste    
		     * criar tag Det
		       oDet = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Det")
			   oDet.NItem = I
			   
		       oProd          = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Prod")
		       oProd.CProd    = AllTrim(Str(I,5))
		       oProd.CEAN     = "SEM GTIN"
		       oProd.XProd    = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
		       oProd.NCM      = "84714900"
		       oProd.CFOP     = "6101"
		       oProd.UCom     = "LU"
		       oProd.QCom     = 1.00
		       oProd.VUnCom   = 84.90
		       oProd.VProd    = 84.90
		       oProd.CEANTrib = "SEM GTIN"
		       oProd.UTrib = "LU"
		       oProd.QTrib = 1.00
		       oProd.VUnTrib = 84.90
		       oProd.IndTot = 1 && SimNao.Sim
		   
		     * adicionar a tag Prod dentro da tag Det
		       oDet.Prod = oProd
			   
		     * criar tag Imposto
		       oImposto          = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Imposto")
		       oImposto.VTotTrib = 12.63
			   
		     * criar tag Icms
		       oICMS             = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.ICMS")
			   
		     * criar tag ICMSSN101
		       oICMSSN101            = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
		       oICMSSN101.Orig       = 0 && OrigemMercadoria.Nacional
		       oICMSSN101.PCredSN     = 2.8255
		       oICMSSN101.VCredICMSSN = 2.40
			   
		     * adicionar a tag ICMSSN101 dentro da tag ICMS
		       oICMS.ICMSSN101 = oICMSSN101		   
			   
		     * adicionar a tag ICMS dentro da tag Imposto
		       oImposto.Icms = oICMS
			   
		     * criar tag PIS
		       oPIS           = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.PIS")

		     * criar tag PISOutr
		       oPISOutr      = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.PISOutr")
		       oPISOutr.CST  = "99"
		       oPISOutr.VBC  = 0.00
		       oPISOutr.PPIS = 0.00
		       oPISOutr.VPIS = 0.00

		     * adicionar a PisOutr dentro da tag Pis
		       oPIS.PISOutr = oPISOutr   

		     * adicionar a tag Pis dentro da tag Imposto
		       oImposto.PIS = oPIS

		     * criar tag COFINS
		       oCOFINS      = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.COFINS")

		     * criar tag COFINSOutr
		       oCOFINSOutr         = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
		       oCOFINSOutr.CST     = "99"
		       oCOFINSOutr.VBC     = 0.00
		       oCOFINSOutr.PCOFINS = 0.00
		       oCOFINSOutr.VCOFINS = 0.00

		     * adicionar a COFINSOutr dentro da tag COFINS
		       oCOFINS.COFINSOutr = oCOFINSOutr

		     * adicionar a tag COFINS dentro da tag Imposto
		       oImposto.COFINS = oCOFINS
			   
		     * adicionar a tag Imposto dentro da tag Det
		       oDet.Imposto = oImposto
			  
		     * adicionar a tag Det dentro da tag InfNfe 
		       oInfNfe.AddDet(oDet)	  
		   Next I
		   
		 * Criar tag Total
		   oTotal = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Total")

		 * Criar tag ICMSTot
		   oICMSTot = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.ICMSTot")
		   oICMSTot.VBC = 0
		   oICMSTot.VICMS = 0
		   oICMSTot.VICMSDeson = 0
		   oICMSTot.VFCP = 0
		   oICMSTot.VBCST = 0
		   oICMSTot.VST = 0
		   oICMSTot.VFCPST = 0
		   oICMSTot.VFCPSTRet = 0
		   oICMSTot.VProd = 254.70
		   oICMSTot.VFrete = 0
		   oICMSTot.VSeg = 0
		   oICMSTot.VDesc = 0
		   oICMSTot.VII = 0
		   oICMSTot.VIPI = 0
		   oICMSTot.VIPIDevol = 0
		   oICMSTot.VPIS = 0
		   oICMSTot.VCOFINS = 0
		   oICMSTot.VOutro = 0
		   oICMSTot.VNF = 254.70
		   oICMSTot.VTotTrib = 37.89  

		 * adicionar a tag ICMSTot dentro da tag Total
		   oTotal.ICMSTot = oICMSTot
		   
		 * adicionar a tag Total dentro da tag InfNfe
		   oInfNfe.Total = oTotal
		   
		 * Criar a tag Transp  
		   oTransp = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Transp")
		   oTransp.ModFrete = 9 && ModalidadeFrete.SemOcorrenciaTransporte 

		 * adicionar a tag Transp dentro da tag InfNfe
		   oInfNfe.Transp = oTransp

		 * criar tag Pag
		   oPag = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.Pag")

		 * criar tag DetPag 
		   oDetPag = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.DetPag")
		   oDetPag.IndPag = 0 && IndicadorPagamento.PagamentoVista
		   oDetPag.TPag   = 1 && MeioPagamento.Dinheiro
		   oDetPag.VPag   = 254.70
		 
		 * adicionar a tag DetPag dentro da tag Tag
		   oPag.AddDetPag(oDetPag)

		 * adicionar a tag Pag dentro da InfNfe
		   oInfNFe.Pag = oPag

		 * criar tag InfAdic
		   oInfAdic = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.InfAdic")
		   oInfAdic.InfCpl = "Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008"
		 
		 * adicionar a tag InfAdic dentro da tag InfNfe
		   oInfNFe.InfAdic = oInfAdic

		 * criar tag InfRespTec
		   oInfRespTec = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.InfRespTec")
		   oInfRespTec.CNPJ     = "06117473000150"
		   oInfRespTec.XContato = "Ze das Couves"
		   oInfRespTec.Email    = "zedascouves@gmail.com"
		   oInfRespTec.Fone     = "04430000000"

		 * adicionar a tag InfRespTec dentro da tag InfNfe
		   oInfNfe.InfRespTec = oInfRespTec

		 * adicionar a tag InfNfe dentro da tag Nfe
		   oNfe.AddInfNFe(oInfNFe)

		 * adiconar a tag nfe dentro da tag EnviNfe 
		   oEnviNFe.AddNfe(oNfe)
	   ENDIF   
	   
	 * Recuperar a chave da NFe:
	   oConteudoNFe = oEnviNFe.GetNFe(0)
	   oConteudoInfNFe = oConteudoNFe.GetInfNFe(0)
	   chaveNFe = oConteudoInfNFe.Chave
			 
	   MESSAGEBOX("Chave da NFe:" + chaveNFe)

	 * Consumir o serviÃ§o (Enviar NFE para SEFAZ)
	   oAutorizacao = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")
	   
	 * Criar objeto para pegar excecao do lado do CSHARP
	   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")   
   
      TRY
         oAutorizacao.SetXMLConfiguracao(oEnviNFe, oConfig)        
 	     notaAssinada = oAutorizacao.GetConteudoNFeAssinada(0)
         MESSAGEBOX(notaAssinada) && Demonstrar o XML da nota assinada na tela

         IF ! lEnvioOffLine
            IF nStatusEnvio <> 2
*                THROW CREATEOBJECT("myException")
            ENDIF  

            IF nStatusEnvio < 1 OR nStatusEnvio == 2 && XML gerado, assinado e arquivado 
             * Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
             * Gravar o XML assinado no HD, antes de enviar.
               DELETE FILE 'd:\testenfe\' + chaveNFe + '-nfe.xml'
	           StrToFile(notaAssinada, 'd:\testenfe\' + chaveNFe + '-nfe.xml', 0)  
	  
               nStatusEnvio = 1 && XML gerado, assinado e arquivado   
      
             * Enviar a nota para SEFAZ
               oAutorizacao.Executar(oEnviNFe, oConfig) 
               nStatusEnvio = 3 && XML enviado
	  
             * XML Retornado pela SEFAZ
               MESSAGEBOX(oAutorizacao.RetornoWSString)

             * Codigo de Status e Motivo
               MESSAGEBOX(AllTrim(Str(oAutorizacao.Result.CStat,5)) + " " +oAutorizacao.Result.XMotivo)
               nStatusEnvio = 4 && Retorno recebido e dados importantes arquivados
            
               IF oAutorizacao.Result.CStat == 104 && 104 = Lote Processado
                  IF oAutorizacao.Result.ProtNFe.InfProt.CStat == 100 && 100 = Autorizado o uso da NF-e
                   * Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
                     oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
	
      		       * Pegar a string do XML de distribuição
                     docProcNFe = oAutorizacao.GetNFeProcResults(chaveNFe)
   			         MESSAGEBOX(docProcNFe)

                     nStatusEnvio = 5 && XML de distribuição arquivado
			   
                   * Como pegar o numero do protocolo de autorizacao para gravar na base
		             MESSAGEBOX(oAutorizacao.Result.ProtNFe.InfProt.NProt)
		       
                     nStatusEnvio = 6 && Processo de envio concluido com sucesso		       
	   	          ELSE		    
                   * Rejeitada - Fazer devidos tratamentos
                     nStatusEnvio = 0 && Voltar a ZERO para permitir o envio da nova novamente
                  ENDIF
               ELSE
                  IF oAutorizacao.Result.CStat == 108 && Servidor de Processamento está Paralisado temorariamente ###
                     lEnvioOffline = .T.
                     xJust = "Servidor de processamento da SEFAZ está paralisado temporariamente - cStat = 108"
                     lSair = .F.
                  ELSE
                     IF oAutorizacao.Result.CStat == 109 && Servidor de Processamento está Paralisado sem Previsão de retorno ###
                        lEnvioOffline = .T.
                        xJust = "Servidor de Processamento está Paralisado sem Previsão de retorno - cStat = 109"
                        lSair = .F.
                     ENDIF   
                  ENDIF
               
                * Rejeitada - Fazer devidos tratamentos
                  nStatusEnvio = 0 && Voltar a ZERO para permitir o envio da nova novamente   
               ENDIF
            ELSE
             * Criar configuração mínima para consumir o serviço de consulta protocolo
               oConfigConsSitNFe = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
               oConfigConsSitNFe.TipoDfe = 0 && 0=nfe
               oConfigConsSitNFe.TipoEmissao = 1 && 1=Normal
               oConfigConsSitNFe.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
               oConfigConsSitNFe.CertificadoSenha = "12345678"
         
             * Consultar a nota pela chave e se autorizada finalizar gerando o XML de distribuição   
               oConsSitNFe = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
               oConsSitNFe.Versao = "4.00"
               oConsSitNFe.TpAmb = 2 && Homologação
               oConsSitNFe.ChNFe = chaveNFe
         
               oConsultaProtocolo = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo")
               oConsultaProtocolo.Executar(oConsSitNFe, oConfigConsSitNFe)
         
               MESSAGEBOX(oConsultaProtocolo.RetornoWSString)
         
               IF oConsultaProtocolo.Result.CStat == 100
                  oAutorizacao.AddRetConsSitNFes(oConsultaProtocolo.Result)

                * Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
                  oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
	
		        * Pegar a string do XML de distribuição
                  docProcNFe = oAutorizacao.GetNFeProcResults(chaveNFe)
			      MESSAGEBOX(docProcNFe)
			
                  nStatusEnvio = 6 && Processo de envio concluido com sucesso
               ELSE
                  MESSAGEBOX(ALLTRIM(STR(oConsultaProtocolo.Result.CStat,5)) + " - " + oConsultaProtocolo.Result.XMotivo)
                  nStatusEnvio = 0 && Volta para o inicio para permitir alterar e enviar a nota novamente
               ENDIF                  
            ENDIF
         ELSE
          * Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
          * Gravar o XML assinado no HD, antes de enviar.
            DELETE FILE 'd:\testenfe\cont\' + chaveNFe + '-nfe.xml'
            StrToFile(notaAssinada, 'd:\testenfe\cont\' + chaveNFe + '-nfe.xml', 0)

          *
          * Criar as configurações para impressao do DANFe.
          *
          * Lista de parametros/propriedades que podem ser utilizadas:
          * https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
          *
            oConfigDANFe = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
            oConfigDANFe.Arquivo = notaAssinada && "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
            oConfigDANFe.Visualizar = .T.
            oConfigDANFe.Imprimir = .F.
            oConfigDANFe.EnviaEmail = .F.

          * Disparar a impressao do DANFe
            oDANFe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
            oDANFe.Execute(oConfigDANFe)
            
            lSair = .T.
            lStatusEnvio = 2 && XML gerado e emitido danfe em contingência offline
         ENDIF   
	  
      CATCH TO oErro
       * Excecao do FOXPRO
	   * Mais sobre excecao em FOXPRO
	   * http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
  	
   	     MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
   	  
       * Excecao do CSHARP
         MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
         
         lEnvioOffline = .T.
         xJust = "Emitida em contingencia em decorrencia de falhas da infraestrutura de rede/internet"
         nStatusEnvio = 0
         lSair = .F.
      ENDTRY
      
      IF lSair
         EXIT
      ENDIF   
   ENDDO   
RETURN
