* ---------------------------------------------------------------------------------
* Gerar XML da NFe e enviar no modo Assíncrono
* ---------------------------------------------------------------------------------
Function EnviarNfeAssincrono()
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
   oConfig.TipoEmissao = 1 && 1=Normal
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"
   
 * Criar a tag <enviNFe>
   oEnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oEnviNFe.Versao = "4.00"
   oEnviNFe.IdLote = "000000000000001"
   oEnviNFe.IndSinc = 0 && 1=Sim 0=Nao   **DIFERENTE AQUI 1 PARA 0
   
 * Criar a tag <NFe>  
   oNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
 * Criar tag InfNfe
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
   oInfNFe.Versao = "4.00"

 * cria tag Ide
   oIde = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
   oIde.CUF = 41 && Brasil.PR
   oIde.NatOp = "VENDA PRODUC.DO ESTABELEC"
   oIde.Mod = 55 && NFe
   oIde.Serie    = 1
   oIde.NNF      = 59007
   oIde.DhEmi    = DateTime()
   oIde.DhSaiEnt = DateTime()
   oIde.TpNF     = 1 && Saida
   oIde.IdDest   = 2 && OperacaoInterestadual
   oIde.CMunFG   = 4118402
   oIde.TpImp    = 1 && FormatoImpressaoDANFE.NormalRetrato
   oIde.TpEmis   = 1 && TipoEmissao.Normal
   oIde.TpAmb    = 2 && TipoAmbiente.Homologacao
   oIde.FinNFe   = 1 && FinalidadeNFe.Normal
   oIde.IndFinal = 1 && SimNao.Sim
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
   oDest = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
   oDest.CNPJ      = "04218457000128"
   oDest.XNome     = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oDest.IndIEDest = 1 && IndicadorIEDestinatario.ContribuinteICMS,
   oDest.IE        = "582614838110"
   oDest.Email     = "janelaorp@janelaorp.com.br"
   
   oEnderDest = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
   oEnderDest.XLgr    = "AVENIDA DA SAUDADE"
   oEnderDest.Nro     = "1555"
   oEnderDest.XBairro = "CAMPOS ELISEOS"
   oEnderDest.CMun    = 3543402
   oEnderDest.XMun    = "RIBEIRAO PRETO"
   oEnderDest.UF      = 35 && UFBrasil.SP
   oEnderDest.CEP     = "14080000"
   oEnderDest.Fone    = "01639611500"

 * adicionar a tag EnderDest dentro da tag Dest 
   oDest.EnderDest = oEnderDest

 * adicionar a tag Emit dentro da tag InfNfe
   oInfNfe.Dest = oDest
   
   For I = 1 To 3 && 3 produtos para teste    
     * criar tag Det
       oDet = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
	   oDet.NItem = I
	   
       oProd          = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
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
       oProd.XPed = "300474"
       oProd.NItemPed = 1

*	 * criar a tag <DI> - tem como ter mais de uma tag DI, vou criar duas para ficar de exemplo, esta Ã© a primeira
*	   oDI = CreateObject("Unimake.Business.DFe.Xml.NFe.DI")
*	   oDI.CExportador = ""
*      oDI.CNPJ = ""
*      oDI.DDesemb = DateTime()
*      oDI.DDI = DateTime()
*	   oDI.TpViaTransp = 7 && ViaTransporteInternacional.Rodoviaria
*	   oDI.TpIntermedio = 2 && FormaImportacaoIntermediacao.ImportacaoPorContaOrdem
*	   oDI.NDI = "1222"
*	   
*	 * Criar tag <adi>
*      oAdi = CreateObject("Unimake.Business.DFe.Xml.NFe.Adi")
*      oAdi.CFabricante = ""
*      oAdi.NDraw = "12344"
*
*    * Adicionar a tag <adi> dentro do grupo de tag <DI>
*	   oDI.AddAdi(oAdi)
*
*	 * Criar tag <adi> -> Posso ter mais de uma tag <adi> entÃ£o vou criar uma segunda vez para ficar o exemplo
*      oAdi = CreateObject("Unimake.Business.DFe.Xml.NFe.Adi")
*      oAdi.CFabricante = ""
*      oAdi.NDraw = "12344"
*
*    * Adicionar a tag <adi> dentro do grupo de tag <DI>
*	   oDI.AddAdi(oAdi)
*	   
*	 * Adicionar a tag <DI> dentro do grupo de tag <prod>   
*	   oProd.AddDI(oDI)
*
*	 * criar a tag <DI> - Segunda tag <DI>
*	   oDI = CreateObject("Unimake.Business.DFe.Xml.NFe.DI")
*	   oDI.CExportador = ""
*      oDI.CNPJ = ""
*      oDI.DDesemb = DateTime()
*      oDI.DDI = DateTime()
*	   oDI.TpViaTransp = 7 && ViaTransporteInternacional.Rodoviaria
*	   oDI.TpIntermedio = 2 && FormaImportacaoIntermediacao.ImportacaoPorContaOrdem
*	   oDI.NDI = "1222"
*	   
*	 * Criar tag <adi>
*      oAdi = CreateObject("Unimake.Business.DFe.Xml.NFe.Adi")
*      oAdi.CFabricante = ""
*      oAdi.NDraw = "12344"
*
*    * Adicionar a tag <adi> dentro do grupo de tag <DI>
*	   oDI.AddAdi(oAdi)
*
*	 * Criar tag <adi> -> Posso ter mais de uma tag <adi> entÃ£o vou criar uma segunda vez para ficar o exemplo
*      oAdi = CreateObject("Unimake.Business.DFe.Xml.NFe.Adi")
*      oAdi.CFabricante = ""
*      oAdi.NDraw = "12344"
*
*    * Adicionar a tag <adi> dentro do grupo de tag <DI>
* 	   oDI.AddAdi(oAdi)
*	   
*	 * Adicionar a tag <DI> dentro do grupo de tag <prod>   
* 	   oProd.AddDI(oDI)	
   
     * adicionar a tag Prod dentro da tag Det
       oDet.Prod = oProd
	   
     * criar tag Imposto
       oImposto          = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
       oImposto.VTotTrib = 12.63
	   
     * criar tag Icms
       oICMS             = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
	   
     * criar tag ICMSSN101
       oICMSSN101            = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
       oICMSSN101.Orig       = 0 && OrigemMercadoria.Nacional
       oICMSSN101.PCredSN     = 2.8255
       oICMSSN101.VCredICMSSN = 2.40
	   
     * adicionar a tag ICMSSN101 dentro da tag ICMS
       oICMS.ICMSSN101 = oICMSSN101
	   
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
       
*     * crirar tag PISNT
*       oPISNT = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.PISNT")  
*       oPISNT.CST = "07" && Pode ser CST 04 ou 07
       
*     * adicionar a tag PISNT dentro da tag PIS
*       oPIS.PISNT = oPISNT         

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
	   
*	 * Criar tag <impostoDevol>
*	   oImpostoDevol = CreateObject("Unimake.Business.DFe.Xml.NFe.ImpostoDevol")
*	   oImpostoDevol.PDevol = 0.00
*	   
*	 * Criar tag <IPI>
*      oIPIDevol = CreateObject("Unimake.Business.DFe.Xml.NFe.IPIDevol")
*	   oIPIDevol.VIPIDevol = 0.00
	   
*	 * Adicionar o grupo de tag <IPI> dentro do grupo <impostoDevol>
*	   oImpostoDevol.IPI = oIPIDevol 
	   
*	 * Adicionar o grupo de tag <impostoDevol> dentro do grupo <det>
*	   oDet.ImpostoDevol = oImpostoDevol	   
	  
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
   oTransp = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
   oTransp.ModFrete = 0 && ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF

 * Criar a tag Vol
   oVol       = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")
   oVol.QVol  = 1
   oVol.Esp   = "LU"
   oVol.Marca = "UNIMAKE"
   oVol.PesoL = 0.000
   oVol.PesoB = 0.000

 * adicionar a tag Vol na tag Transp
   oTransp.AddVol(oVol)

 * adicionar a tag Transp dentro da tag InfNfe
   oInfNfe.Transp = oTransp

 * Criar tag Cobr 
   oCobr = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")

 * Criar tag Fat 
   oFat  = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
   oFat.NFat = "057910"
   oFat.VOrig = 254.70
   oFat.VDesc = 0
   oFat.VLiq = 254.70

 * Criar tag Dup (parcela 1)
   oDup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
   oDup.NDup  = "001"
   oDup.DVenc = Date()
   oDup.VDup  = 127.35

 * adicionar a tag Dup dentro da tag Cobr
   OCobr.AddDup(oDup)

 * Criar tag Dup (parcela 2)
   oDup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
   oDup.NDup  = "002"
   oDup.DVenc = Date()
   oDup.VDup  = 127.35

 * adicionar a tag Dup dentro da tag Cobr
   oCobr.AddDup(oDup)

 * adicionar a tag Fat dentro da tag Cobr
   oCobr.Fat = oFat
   
 * adicionar a tag Cobr dentro da tag InfNfe
   oInfNfe.Cobr = oCobr

 * criar tag Pag
   oPag = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")

 * criar tag DetPag (pode ter mais que uma, sÃ³ foi criada uma como exemplo)
   oDetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
   oDetPag.IndPag = 0 && IndicadorPagamento.PagamentoVista
   oDetPag.TPag   = 1 && MeioPagamento.Dinheiro
   oDetPag.VPag   = 254.70
 
 * adicionar a tag DetPag dentro da tag Tag
   oPag.AddDetPag(oDetPag)

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
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
   
 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
      oAutorizacao.SetXMLConfiguracao(oEnviNFe, oConfig)      
	  
    * Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
	  notaAssinada = oAutorizacao.GetConteudoNFeAssinada(0)
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
	  
	  if oAutorizacao.Result.CStat == 103 && 103 = Lote Recebido com Sucesso   **DIFERENTE AQUI 104 para 103
	   * Criar o objeto de configuração mínimo para consumir o serviço de consulta recibo
         oConfigRec                    = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
         oConfigRec.TipoDFe            = 0 && TipoDFe.NFe
         oConfigRec.CertificadoSenha   = "12345678"
         oConfigRec.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

       * Fazer a consulta do recibo do lote enviado	para ver se a(s) nota(s) foi(ram) autorizada(s)
         oConsReciNFe   = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
         oConsReciNFe.Versao = "4.00"
         oConsReciNFe.TpAmb  = 2 && TipoAmbiente.Homologacao
         oConsReciNFe.NRec   = oAutorizacao.Result.InfRec.NRec
		 
         oRetAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
         oRetAutorizacao.Executar(oConsReciNFe, oConfigRec)

       * Vamos ver a string do XML retornado pela SEFAZ
         MESSAGEBOX(oRetAutorizacao.RetornoWSString)
	    
       * Vamos verificar se o status do lote enviado está como processado com sucesso
	     IF oRetAutorizacao.Result.CStat = 104 && Lote processado com sucesso
          * Informar para o objeto de autorização qual é o resultado das notas. É necessário esta informação para que 
          * a classe tenha informações para juntar NFe com seu protocolo na hora de gerar o XML de distribuição.
	        oAutorizacao.RetConsReciNFe = oRetAutorizacao.Result
	        
          * Agora vamos pegar o protocolo de todas as notas enviadas no lote, nota a nota.
            For I = 1 TO oRetAutorizacao.Result.GetProtNfeCount()
			    oProtNfe = oRetAutorizacao.Result.GetProtNfe(I-1)

				MESSAGEBOX(ALLTRIM(Str(oProtNfe.InfProt.CStat,5)) + " - " + oProtNfe.InfProt.XMotivo)
				
              * Salvar XML de distribuicao das notas enviadas na pasta informada  
			    Try
                   if oProtNFe.InfProt.CStat == 100 &&100=NFe Autorizada				
                    * Gravar o XML de distribuição em uma pasta
				      oAutorizacao.GravarXmlDistribuicao("d:\testenfe")				      
				  
                    * Demonstrar a chave da NFe pegando do protocolo retornado
                      MESSAGEBOX(oProtNFe.InfProt.NProt)
				      MESSAGEBOX(oProtNFe.InfProt.chNFe)
				  
   		            * Pegar a string do XML de distribuição para gravar na base de dados
		              docProcNFe = oAutorizacao.GetNFeProcResults(oProtNFe.InfProt.chNFe)
		              
		              MESSAGEBOX(docProcNFe)
		              
		            * Gravar o XML de distribuição com um nome diferente do sugerido pela DLL
		            *  
			        * Nome do XML de distribuição gerado pela DLL segue o o seguinte padrão:
			        *   41220606117473000150550010000580151230845956-procnfe.xml
			        *
			        * Vamos mudar e deixar ele assim, por exemplo:
			        *   NFe41220606117473000150550010000580151230845956-ProcNFe.xml			          			          
					  DELETE FILE 'D:\testenfe\NFe' + oProtNFe.InfProt.chNFe + '-ProcNFe.xml'
                	  StrToFile(docProcNFe, 'D:\testenfe\NFe' + oProtNFe.InfProt.chNFe + '-ProcNFe.xml', 0)               	  
				   else
                    * NFe rejeitada, fazer devidos tratamentos				   
				   endif
				Catch TO oErroProt
                 * Excecao do FOXPRO
                 * Mais sobre excecao em FOXPRO
                 * http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
                   MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErroProt.ErrorNo,10))+ " - Message: " + oErroProt.Message)
	  
                 * Excecao do CSHARP
                   MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
				EndTry
			Next I	        	        
	     ELSE
	        * Pode ser que o lote já tenha sido rejeitado na sua totalidade, tem que fazer os devidos tratamentos
	        * Ou pode ser que o lote esteja em processamento, então tem que efetuar a consulta recibo novamente mais tarde
	     ENDIF
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

