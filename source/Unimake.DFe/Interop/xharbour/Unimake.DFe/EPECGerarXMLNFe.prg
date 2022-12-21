* ---------------------------------------------------------------------------------
* EPEC - Gerar o XML da NFe com EPEC
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EPECGerarXMLNFe()
   Local oInicializarConfiguracao
   Local oXml, oNfe, oInfNFe, oIde, oEmit, oEnderEmit, oDest, oEnderDest
   Local oDet, oProd
   Local oImposto, oICMS, oICMSSN101, oPIS, oPISOutr, oCOFINS, oCOFINSOutr
   Local oTotal, oICMSTot
   Local oTransp, oVol
   Local oCobr, oFat, oDup
   Local oPag, oDetPag
   Local oInfAdic, oInfRespTec
   Local oAutorizacao, oRetAutorizacao, oXmlRec, oConfigRec
   Local I, oErro, notaAssinada
   Local oXmlConsSitNFe, oConteudoNFe, oConteudoInfNFe, chaveNFe, oConfigConsSitNFe, oConsultaProtocolo
   Local nHandle

 * Criar configuraçao básica para consumir o serviço
   oInicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   
   oInicializarConfiguracao:TipoDfe = 0 // 0=nfe
   oInicializarConfiguracao:Servico = 6 // 6=Autorização Nfe
   oInicializarConfiguracao:TipoEmissao = 4 // 4=TipoEmissao.ContingenciaEPEC
   oInicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oInicializarConfiguracao:CertificadoSenha = "12345678"
   
 * Criar XML   
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 1 // 1=Sim 0=Nao
   
   onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   
   // criar tag InfNfe
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
   oInfNFe:Versao = "4.00"

   // cria tag Ide
   oIde = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
   oIde:CUF = 41 //Brasil.PR
   oIde:NatOp = "VENDA PRODUC.DO ESTABELEC"
   oIde:Mod = 55 //NFe
   oIde:Serie    = 1
   oIde:NNF      = 58015
   oIde:DhEmi    = DateTime()
   oIde:DhSaiEnt = DateTime()
   oIde:TpNF     = 1 // Saida
   oIde:IdDest   = 2 // OperacaoInterestadual
   oIde:CMunFG   = 4118402
   oIde:TpImp    = 1 // FormatoImpressaoDANFE.NormalRetrato
   oIde:TpEmis   = 4 // 4=TipoEmissao.ContingenciaEPEC
   oIde:TpAmb    = 2 // TipoAmbiente.Homologacao
   oIde:FinNFe   = 1 // FinalidadeNFe.Normal
   oIde:IndFinal = 1 // SimNao.Sim
   oIde:IndPres  = 1 // IndicadorPresenca.OperacaoPresencial
   oIde:ProcEmi  = 0 // ProcessoEmissao.AplicativoContribuinte
   oIde:VerProc  = "TESTE 1.00"
   oIde:DhCont   = DateTime()
   oIde:xJust    = "Teste de contingencia EPEC"
   
   //adicionar a tag Ide dentro da tag InfDfe
   oInfNFe:Ide = oIde

   // criar tag Emit
   oEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
   oEmit:CNPJ  = "06117473000150"
   oEmit:XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
   oEmit:XFant = "UNIMAKE - PARANAVAI"
   oEmit:IE    = "9032000301"
   oEmit:IM    = "14018"
   oEmit:CNAE  = "6202300"
   oEmit:CRT   = 1 // CRT.SimplesNacional

   oEnderEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
   oEnderEmit:XLgr    = "RUA PAULO ANTONIO COSTA"
   oEnderEmit:Nro     = "575"
   oEnderEmit:XBairro = "CENTRO"
   oEnderEmit:CMun    = 4118402
   oEnderEmit:XMun    = "PARANAVAI"
   oEnderEmit:UF      = 41 // UFBrasil.PR
   oEnderEmit:CEP     = "87707210"
   oEnderEmit:Fone    = "04431421010"
   
   // adicionar a tag EnderEmit dentro da tag Emit 
   oEmit:EnderEmit = oEnderEmit
   
   // adicionar a tag Emit dentro da tag InfNfe
   oInfNfe:Emit = oEmit
   
   // criar tag Dest
   oDest = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
   oDest:CNPJ      = "04218457000128"
   oDest:XNome     = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oDest:IndIEDest = 1 // IndicadorIEDestinatario.ContribuinteICMS,
   oDest:IE        = "582614838110"
   oDest:Email     = "janelaorp@janelaorp.com.br"
   
   oEnderDest = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
   oEnderDest:XLgr    = "AVENIDA DA SAUDADE"
   oEnderDest:Nro     = "1555"
   oEnderDest:XBairro = "CAMPOS ELISEOS"
   oEnderDest:CMun    = 3543402
   oEnderDest:XMun    = "RIBEIRAO PRETO"
   oEnderDest:UF      = 35 // UFBrasil.SP
   oEnderDest:CEP     = "14080000"
   oEnderDest:Fone    = "01639611500"

   // adicionar a tag EnderDest dentro da tag Dest 
   oDest:EnderDest = oEnderDest

   // adicionar a tag Emit dentro da tag InfNfe
   oInfNfe:Dest = oDest
   
   For I = 1 To 3 // 3 produtos para teste    
       // criar tag Det
       oDet = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
	   oDet:NItem = I
	   
       oProd          = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
       oProd:CProd    = StrZero(I,5)
       oProd:CEAN     = "SEM GTIN"
       oProd:XProd    = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
       oProd:NCM      = "84714900"
       oProd:CFOP     = "6101"
       oProd:UCom     = "LU"
       oProd:QCom     = 1.00
       oProd:VUnCom   = 84.90
       oProd:VProd    = 84.90
       oProd:CEANTrib = "SEM GTIN"
       oProd:UTrib = "LU"
       oProd:QTrib = 1.00
       oProd:VUnTrib = 84.90
       oProd:IndTot = 1 // SimNao.Sim
       oProd:XPed = "300474"
       oProd:NItemPed = 1
	   
    // adicionar a tag Prod dentro da tag Det
       oDet:Prod = oProd
	   
    // criar tag Imposto
       oImposto          = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
       oImposto:VTotTrib = 12.63
	   
    // criar tag Icms
       oICMS             = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
	   
    // criar tag ICMSSN101
       oICMSSN101            = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
       oICMSSN101:Orig       = 0 // OrigemMercadoria.Nacional
       oICMSSN101:PCredSN     = 2.8255
       oICMSSN101:VCredICMSSN = 2.40
	   
    // adicionar a tag ICMSSN101 dentro da tag ICMS
       oICMS:ICMSSN101 = oICMSSN101
	   
    // adicionar a tag ICMS dentro da tag Imposto
       oImposto:Icms = oICMS
	   
    // criar tag PIS
       oPIS           = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")

    // criar tag PISOutr
       oPISOutr      = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
       oPISOutr:CST  = "99"
       oPISOutr:VBC  = 0.00
       oPISOutr:PPIS = 0.00
       oPISOutr:VPIS = 0.00

    // adicionar a PisOutr dentro da tag Pis
       oPIS:PISOutr = oPISOutr   

    // adicionar a tag Pis dentro da tag Imposto
       oImposto:PIS = oPIS

    // criar tag COFINS
       oCOFINS      = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")

    // criar tag COFINSOutr
       oCOFINSOutr         = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
       oCOFINSOutr:CST     = "99"
       oCOFINSOutr:VBC     = 0.00
       oCOFINSOutr:PCOFINS = 0.00
       oCOFINSOutr:VCOFINS = 0.00

    // adicionar a COFINSOutr dentro da tag COFINS
       oCOFINS:COFINSOutr = oCOFINSOutr

    // adicionar a tag COFINS dentro da tag Imposto
       oImposto:COFINS = oCOFINS
	   
    // adicionar a tag Imposto dentro da tag Det
       oDet:Imposto = oImposto
	  
    // adicionar a tag Det dentro da tag InfNfe 
       oInfNfe:AddDet(oDet)	  
   Next I
   
   // Criar tag Total
   oTotal = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")

   // Criar tag ICMSTot
   oICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
   oICMSTot:VBC = 0
   oICMSTot:VICMS = 0
   oICMSTot:VICMSDeson = 0
   oICMSTot:VFCP = 0
   oICMSTot:VBCST = 0
   oICMSTot:VST = 0
   oICMSTot:VFCPST = 0
   oICMSTot:VFCPSTRet = 0
   oICMSTot:VProd = 254.70
   oICMSTot:VFrete = 0
   oICMSTot:VSeg = 0
   oICMSTot:VDesc = 0
   oICMSTot:VII = 0
   oICMSTot:VIPI = 0
   oICMSTot:VIPIDevol = 0
   oICMSTot:VPIS = 0
   oICMSTot:VCOFINS = 0
   oICMSTot:VOutro = 0
   oICMSTot:VNF = 254.70
   oICMSTot:VTotTrib = 37.89  

// adicionar a tag ICMSTot dentro da tag Total
   oTotal:ICMSTot = oICMSTot
   
// adicionar a tag Total dentro da tag InfNfe
   oInfNfe:Total = oTotal
   
// Criar a tag Transp  
   oTransp = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
   oTransp:ModFrete = 0 // ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF

// Criar a tag Vol
   oVol       = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")
   oVol:QVol  = 1
   oVol:Esp   = "LU"
   oVol:Marca = "UNIMAKE"
   oVol:PesoL = 0.000
   oVol:PesoB = 0.000

   // adicionar a tag Vol na tag Transp
   oTransp:AddVol(oVol)

   // adicionar a tag Transp dentro da tag InfNfe
   oInfNfe:Transp = oTransp

   // Criar tag Cobr 
   oCobr = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")

   // Criar tag Fat 
   oFat  = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
   oFat:NFat = "057910"
   oFat:VOrig = 254.70
   oFat:VDesc = 0
   oFat:VLiq = 254.70

   // Criar tag Dup (parcela 1)
   oDup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
   oDup:NDup  = "001"
   oDup:DVenc = StoD(TtoS(Date() + 30))
   oDup:VDup  = 127.35

   // adicionar a tag Dup dentro da tag Cobr
   OCobr:AddDup(oDup)

   // Criar tag Dup (parcela 2)
   oDup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
   oDup:NDup  = "002"
   oDup:DVenc = StoD(TtoS(Date() + 60))
   oDup:VDup  = 127.35

   // adicionar a tag Dup dentro da tag Cobr
   oCobr:AddDup(oDup)

   // adicionar a tag Fat dentro da tag Cobr
   oCobr:Fat = oFat
   
   // adicionar a tag Cobr dentro da tag InfNfe
   oInfNfe:Cobr = oCobr

   // criar tag Pag
   oPag = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")

   // criar tag DetPag (pode ter mais que uma, só foi criada uma como exemplo)
   oDetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
   oDetPag:IndPag = 0 // IndicadorPagamento.PagamentoVista
   oDetPag:TPag   = 1 // MeioPagamento.Dinheiro
   oDetPag:VPag   = 254.70
 
   // adicionar a tag DetPag dentro da tag Tag
   oPag:AddDetPag(oDetPag)

   // adicionar a tag Pag dentro da InfNfe
   oInfNFe:Pag = oPag

   // criar tag InfAdic
   oInfAdic = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
   oInfAdic:InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;"

   // adicionar a tag InfAdic dentro da tag InfNfe
   oInfNFe:InfAdic = oInfAdic

   // criar tag InfRespTec
   oInfRespTec = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
   oInfRespTec:CNPJ     = "06117473000150"
   oInfRespTec:XContato = "Ze das Couves"
   oInfRespTec:Email    = "zedascouves@gmail.com"
   oInfRespTec:Fone     = "04430000000"

   // adicionar a tag InfRespTec dentro da tag InfNfe
   oInfNfe:InfRespTec = oInfRespTec

   // adicionar a tag InfNfe dentro da tag Nfe
   oNfe:AddInfNFe(oInfNFe)

   // adiconar a tag nfe dentro da tag EnviNfe 
   oXml:AddNfe(oNfe)   
   
 * Recuperar a chave da NFe:
   oConteudoNFe = oXml:GetNFe(0)
   oConteudoInfNFe = oConteudoNFe:GetInfNFe(0)
   chaveNFe = oConteudoInfNFe:Chave
		 
   ? "Chave da NFe:", chaveNFe
   Wait
   Cls

 * Consumir o serviço (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
      oAutorizacao:SetXMLConfiguracao(oXml, oInicializarConfiguracao)      
	  
      // Pode-se gravar o conteudo do XML assinado na base de dados antes do envio, caso queira recuperar para futuro tratamento, isso da garantias
	  notaAssinada = oAutorizacao:GetConteudoNFeAssinada(0)
      ? notaAssinada //Demonstrar o XML da nota assinada na tela
	  
      Wait
	  cls
	  
	  ? "d:\testenfe\" + chaveNFe + "-nfe.xml"
	  
	  //Salvar o XML para posterior envio
      nHandle := fCreate("d:\testenfe\" + chaveNFe + "-nfe.xml")
 	  FWrite(nHandle, notaAssinada)
	  FClose(nHandle)

      Wait
	  cls
	  
	  
	  
   Catch oErro
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
   End
Return

