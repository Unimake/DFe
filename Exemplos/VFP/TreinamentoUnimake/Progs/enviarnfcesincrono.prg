* ---------------------------------------------------------------------------------
* Gerar XML da NFe e enviar no modo síncrono
* ---------------------------------------------------------------------------------
Function EnviarNfCeSincrono()
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
   oConfig.TipoDfe = 1 && 1=nfce     ###
   oConfig.TipoEmissao = 1 && 1=Normal
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"  
   oConfig.CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG"  && ###  
   oConfig.CSCIDToken = 2      && ###
   
 * Criar a tag <enviNFe>
   oEnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oEnviNFe.Versao = "4.00"
   oEnviNFe.IdLote = "000000000000001"
   oEnviNFe.IndSinc = 1 && 1=Sim 0=Nao ###
  
  
 * Criar a tag NFe e deserializar o XML já gravado no HD para já preencher o objeto para envio
*  onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
*  oEnviNFe.AddNFe(oNFe.LoadFromFile("D:\testenfe\41230206117473000150550010000590081999182930-nfe.xml")) 

 * Como deserializar partindo da string do XML
   && oEnviNFe.AddNFe(oNFe.LoadFromXML("asldkjaslkdjasldjaslkdjasldkjasldksjadas"))    
   
 * Criar a tag <NFe>  
   oNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
 * Criar tag InfNfe
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
   oInfNFe.Versao = "4.00"

 * cria tag Ide
   oIde = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
   oIde.CUF = 41 && Brasil.PR
   oIde.NatOp = "VENDA "
   oIde.Mod = 65 && NFCe ###
   oIde.Serie    = 1
   oIde.NNF      = 59006
   oIde.DhEmi    = DateTime()
   oIde.DhSaiEnt = DateTime()
   oIde.TpNF     = 1 && Saida
   oIde.IdDest   = 1 && OperacaoEstadual
   oIde.CMunFG   = 4118402
   oIde.TpImp    = 4 && FormatoImpressaoDANFE.NFCe ###
   oIde.TpEmis   = 1 && TipoEmissao.Normal
   oIde.TpAmb    = 2 && TipoAmbiente.Homologacao
   oIde.FinNFe   = 1 && FinalidadeNFe.Normal ###
   oIde.IndFinal = 1 && SimNao.Sim ### 
   oIde.IndPres  = 1 && IndicadorPresenca.OperacaoPresencial
   oIde.ProcEmi  = 0 && ProcessoEmissao.AplicativoContribuinte
   oIde.VerProc  = "TESTE 1.00"
   
 * adicionar a tag Ide dentro da tag InfDfe
   oInfNFe.Ide = oIde

 * criar tag Emit
   oEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
   oEmit.CNPJ  = "06117473000150"
   oEmit.XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
   oEmit.XFant = "UNIMAKE - PARANAVAI"
   oEmit.IE    = "9032000301"
   oEmit.IM    = "14018"
   oEmit.CNAE  = "6202300"
   oEmit.CRT   = 1 && CRT.SimplesNacional

   oEnderEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
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
   oDest = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest") && ###
   oDest.CNPJ      = "04218457000128"
   oDest.XNome     = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oDest.IndIEDest = 9 && IndicadorIEDestinatario.NaoContribuinte,
   
*   oDest.Email     = "janelaorp@janelaorp.com.br"
*   oEnderDest = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
*   oEnderDest.XLgr    = "AVENIDA DA SAUDADE"
*   oEnderDest.Nro     = "1555"
*   oEnderDest.XBairro = "CAMPOS ELISEOS"
*   oEnderDest.CMun    = 3543402
*   oEnderDest.XMun    = "RIBEIRAO PRETO"
*   oEnderDest.UF      = 35 && UFBrasil.SP
*   oEnderDest.CEP     = "14080000"
*   oEnderDest.Fone    = "01639611500"

 * adicionar a tag EnderDest dentro da tag Dest 
*   oDest.EnderDest = oEnderDest

 * adicionar a tag Emit dentro da tag InfNfe
   oInfNfe.Dest = oDest
   
   For I = 1 To 3 && 3 produtos para teste    
     * criar tag Det
       oDet = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
	   oDet.NItem = I
	   
       oProd          = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
       oProd.CProd    = AllTrim(Str(I,5))
       oProd.CEAN     = "SEM GTIN"
       oProd.XProd    = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
       oProd.NCM      = "84714900"
       oProd.CFOP     = "5102"
       oProd.UCom     = "LU"
       oProd.QCom     = 1.00
       oProd.VUnCom   = 84.90
       oProd.VProd    = 84.90
       oProd.CEANTrib = "SEM GTIN"
       oProd.UTrib = "LU"
       oProd.QTrib = 1.00
       oProd.VUnTrib = 84.90
       oProd.IndTot = 1 && SimNao.Sim
       oProd.XPed = "300474"
       oProd.NItemPed = 1
	   oProd.VDesc = 0.00
   
     * adicionar a tag Prod dentro da tag Det
       oDet.Prod = oProd
	   
     * criar tag Imposto
       oImposto          = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
       oImposto.VTotTrib = 12.63
	   
     * criar tag Icms
       oICMS             = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
	   
     * criar tag ICMSSN101
       oICMSSN102            = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN102")
       oICMSSN102.Orig       = 0 && OrigemMercadoria.Nacional
       oICMSSN102.CSOSN      = "102"
	   
     * adicionar a tag ICMSSN101 dentro da tag ICMS
       oICMS.ICMSSN102 = oICMSSN102
	   
*	 * criar tag ICMS00   
*      oICMS00 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS00")
*      oICMS00.CST = "00"
*      oICMS00.Orig = 0 && OrigemMercadoria.Nacional
*      oICMS00.ModBC = 3 && ModalidadeBaseCalculoICMS.ValorOperacao
*      oICMS00.VBC = 0.00
*      oICMS00.PICMS = 0.00
*      oICMS00.VICMS = 0.00
*      oICMS00.PFCP = 0.00
*      oICMS00.VFCP = 0.00
*
*    * adicionar a tag ICMS00 dentro da tag ICMS
* 	   oICMS.ICMS00 = oICMS00
*
*	 * criar tag ICMS40
*      oICMS40 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS40")
*      oICMS40.CST = "40"
*      oICMS40.MotDesICMS = 7 && MotivoDesoneracaoICMS.SUFRAMA
*      oICMS40.Orig = 0 && OrigemMercadoria.Nacional
*      oICMS40.VICMSDeson = 0.00
*
*    * adicionar a tag ICMS40 dentro da tag ICMS
* 	   oICMS.ICMS40 = oICMS40
*
*	 * criar tag ICMS20
*      oICMS20 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS20")
*      oICMS20.CST = "20"
*      oICMS20.ModBC = 3 && ModalidadeBaseCalculoICMS.ValorOperacao
*      oICMS20.MotDesICMS = 7 && MotivoDesoneracaoICMS.SUFRAMA
*      oICMS20.Orig = 0 && OrigemMercadoria.Nacional
*      oICMS20.PFCP = 0.00
*      oICMS20.PICMS = 0.00
*      oICMS20.PRedBC = 0.00
*      oICMS20.VBC = 0.00
*      oICMS20.VBCFCP = 0.00
*      oICMS20.VFCP = 0.00
*      oICMS20.VICMS = 0.00
*      oICMS20.VICMSDeson = 0.00                                                           
*
*    * adicionar a tag ICMS20 dentro da tag ICMS
* 	   oICMS.ICMS20 = oICMS20
*	   
*	 * criar tag ICMS51
*      oICMS51 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS51")
*      oICMS51.CST = "51"
*      oICMS51.ModBC = 3 && ModalidadeBaseCalculoICMS.ValorOperacao
*      oICMS51.Orig = 0 && OrigemMercadoria.Nacional
*      oICMS51.PDif = 100.00
*      oICMS51.PFCP = 0.00
*      oICMS51.PFCPDif = 0.00
*      oICMS51.PICMS = 0.00
*      oICMS51.PRedBC = 0.00
*      oICMS51.VBC = 0.00
*      oICMS51.VBCFCP = 0.00
*      oICMS51.VFCP = 0.00
*      oICMS51.VFCPDif = 0.00
*      oICMS51.VFCPEfet = 0.00
*      oICMS51.VICMS = 0.00
*      oICMS51.VICMSDif = 0.00
*      oICMS51.VICMSOp = 0.00
*
*    * adicionar a tag ICMS51 dentro da tag ICMS
* 	   oICMS.ICMS51 = oICMS51
	   
     * adicionar a tag ICMS dentro da tag Imposto
       oImposto.Icms = oICMS
	   
     * criar tag PIS
       oPIS           = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")

     * criar tag PISOutr
       oPISOutr      = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
       oPISOutr.CST  = "99"
       oPISOutr.VBC  = 0.00
       oPISOutr.PPIS = 0.00
       oPISOutr.VPIS = 0.00

     * adicionar a PisOutr dentro da tag Pis
       oPIS.PISOutr = oPISOutr   

     * adicionar a tag Pis dentro da tag Imposto
       oImposto.PIS = oPIS

     * criar tag COFINS
       oCOFINS      = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")

     * criar tag COFINSOutr
       oCOFINSOutr         = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
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
   oTotal = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")

 * Criar tag ICMSTot
   oICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
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
   oICMSTot.VDesc = 0.00
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
   oTransp = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
   oTransp.ModFrete = 9 && ModalidadeFrete.SemOcorrenciaTransporte 

 * adicionar a tag Transp dentro da tag InfNfe
   oInfNfe.Transp = oTransp

 * criar tag Pag
   oPag = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")

 * criar tag DetPag (pode ter mais que uma, sÃ³ foi criada uma como exemplo)
   oDetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
   oDetPag.IndPag = 0 && IndicadorPagamento.PagamentoVista
   oDetPag.TPag   = 1 && MeioPagamento.Dinheiro
   oDetPag.VPag   = 254.70
   
 * Criar grupo de tag <card>  
   oCard = CreateObject("Unimake.Business.DFe.Xml.NFe.Card")
   
   oCard.TpIntegra = 1 && TipoIntegracaoPagamento.PagamentoNaoIntegrado,
   oCard.CAut = ""
   oCard.CNPJ = ""
   oCard.TBand = 1 && BandeiraOperadoraCartao.Visa
  
   oDetPag.Card = oCard
 
 * adicionar a tag DetPag dentro da tag Tag
   oPag.AddDetPag(oDetPag)
   
 * Adicionar a tag de Troco
   oPag.vTroco = 0.00 

 * adicionar a tag Pag dentro da InfNfe
   oInfNFe.Pag = oPag

 * criar tag InfAdic
   oInfAdic = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
   oInfAdic.InfCpl = "Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008"
 
 * adicionar a tag InfAdic dentro da tag InfNfe
   oInfNFe.InfAdic = oInfAdic

 * criar tag InfRespTec
   oInfRespTec = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
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
   
 * Recuperar a chave da NFe:
   oConteudoNFe = oEnviNFe.GetNFe(0)
   oConteudoInfNFe = oConteudoNFe.GetInfNFe(0)
   chaveNFe = oConteudoInfNFe.Chave
		 
   MessageBox("Chave da NFe:" + chaveNFe)

 * Consumir o serviÃ§o (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Autorizacao") && ###
   
 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
      oAutorizacao.SetXMLConfiguracao(oEnviNFe, oConfig)      
	  
    * Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
	  notaAssinada = oAutorizacao.GetConteudoNFeAssinada(0) && ###
      MessageBox(notaAssinada) && Demonstrar o XML da nota assinada na tela

    * Gravar o XML assinado no HD, antes de enviar.
      DELETE FILE 'd:\testenfe\' + chaveNFe + '-nfe.xml'
	  StrToFile(notaAssinada, 'd:\testenfe\' + chaveNFe + '-nfe.xml', 0)  
  
    * Enviar a nota para SEFAZ
	  oAutorizacao.Executar(oEnviNFe, oConfig) 
	  
    * XML Retornado pela SEFAZ
      MessageBox(oAutorizacao.RetornoWSString)

    * Codigo de Status e Motivo
      MessageBox(AllTrim(Str(oAutorizacao.Result.CStat,5)) + " " +oAutorizacao.Result.XMotivo)
	  
	  if oAutorizacao.Result.CStat == 104 && 104 = Lote Processado
         if oAutorizacao.Result.ProtNFe.InfProt.CStat == 100 && 100 = Autorizado o uso da NF-e
          * Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
            oAutorizacao.GravarXmlDistribuicao("d:\testenfe")
			
		  * Pegar a string do XML de distribuição
            docProcNFe = oAutorizacao.GetNFeProcResults(chaveNFe)
			MessageBox(docProcNFe)

          * Como pegar o numero do protocolo de autorizacao para gravar na base
		    MessageBox(oAutorizacao.Result.ProtNFe.InfProt.NProt)
		 else
          * Rejeitada ou Denegada - Fazer devidos tratamentos		 
         Endif		 
	  endif
	  
   Catch To oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   EndTry
Return

