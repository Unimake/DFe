* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------

#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarNfeAssincrono()
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

 * Criar configuraçao básica para consumir o serviço
   oInicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oInicializarConfiguracao:TipoDfe = 0 // 0=nfe
   oInicializarConfiguracao:Servico = 6 // 6=Autorização Nfe
   oInicializarConfiguracao:TipoEmissao = 1 // 1=Normal
   oInicializarConfiguracao:CertificadoSenha = "12345678"
   oInicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar XML
   // criar tag EnviNfe
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 0 // 1=Sim 0=Nao
   
   // criar tag Nfe
   onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")

   // criar tag InfNfe
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
   oInfNFe:Versao = "4.00"

   // cria tag Ide
   oIde = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
   oIde:CUF      = 41 // Brasil.PR
   oIde:NatOp    = "VENDA PRODUC.DO ESTABELEC"
   oIde:Mod      = 55 // NFe
   oIde:Serie    = 1
   oIde:NNF      = 57980
   oIde:DhEmi    = DateTime() 
   oIde:DhSaiEnt = DateTime()
   oIde:TpNF     = 1 // Saida
   oIde:IdDest   = 2 // OperacaoInterestadual
   oIde:CMunFG   = 4118402
   oIde:TpImp    = 1 // FormatoImpressaoDANFE.NormalRetrato
   oIde:TpEmis   = 1 // TipoEmissao.Normal
   oIde:TpAmb    = 2 // TipoAmbiente.Homologacao
   oIde:FinNFe   = 1 // FinalidadeNFe.Normal
   oIde:IndFinal = 1 // SimNao.Sim
   oIde:IndPres  = 1 // IndicadorPresenca.OperacaoPresencial
   oIde:ProcEmi  = 0 // ProcessoEmissao.AplicativoContribuinte
   oIde:VerProc  = "TESTE 1.00"

   // adicionar a tag Ide dentro da tag InfDfe
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

   // criar tag Emit
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
       oDet           = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
       oDet:NItem     = I

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
       oImposto:AddIcms(oICMS)

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
   OCobr:AddDup(oDup)

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

/*
            var configSit = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };
*/            
 * Consumir o serviço (Enviar NFE para SEFAZ)
   oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
   oAutorizacao:SetXMLConfiguracao(oXml,oInicializarConfiguracao)
   notaAssinada = oAutorizacao:GetConteudoNFeAssinada(0)
   ? notaAssinada
   Wait
   oAutorizacao:Executar(oXml,oInicializarConfiguracao)
   
   ? oAutorizacao:GetConteudoXMLAssinado() // por enquanto só consegue pegar depois de executar, ainda vai ser resolvido isso

   ? "XML Retornado pela SEFAZ"
   ? "========================"
   ? oAutorizacao:RetornoWSString
   ?
   ? "Codigo de Status e Motivo"
   ? "========================="
   ? AllTrim(Str(oAutorizacao:Result:CStat,5)),oAutorizacao:Result:XMotivo


   if oAutorizacao:Result <> NIL
     
    * Consultar o Recibo retornado no envio da nota para saber se a mesma foi autorizada  
      if oAutorizacao:Result:CStat == 103 //103 = Lote Recebido com Sucesso
         oXmlRec        = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
         oXmlRec:Versao = "4.00"
         oXmlRec:TpAmb  = 2 // TipoAmbiente.Homologacao,
         oXmlRec:NRec   = oAutorizacao:Result:InfRec:NRec

         oConfigRec                    = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
         oConfigRec:TipoDFe            = 0 // TipoDFe.NFe
         oConfigRec:CertificadoSenha   = "12345678"
         oConfigRec:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
         
         oRetAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
         oRetAutorizacao:Executar(oXmlRec, oConfigRec)
  
         If oRetAutorizacao:Result <> NIL
            oAutorizacao:RetConsReciNFe = oRetAutorizacao:Result

          * Modelo para buscar o resultado de cada nota do lote enviado  
            ? "Codigo de Status e Motivo"
            ? "========================="
            For I = 1 TO oRetAutorizacao:Result:GetProtNfeCount()
                oProtNfe = oRetAutorizacao:Result:GetProtNfe(I-1)
                ? AllTrim(Str(oProtNfe:InfProt:CStat,5)),oProtNfe:InfProt:XMotivo
            Next I

          * salvar XML de distribuicao das notas enviadas na pasta informada  
            Try
               oAutorizacao:GravarXmlDistribuicao(".\tmp")
            Catch oErro
               ? "ERRO"
               ? "===="
               ? "Nao foi possivel gravar o XML de distribuicao, pois nao foi localizado o protocolo de autorizacao (NOTA NAO FOI AUTORIZADA)"
               ? oErro:Description
               ? oErro:Operation
            End
         EndIf
      EndIf   
	  
	  
	  Wait
    * Modelo de código de como finalizar a nota pela consulta situação da NFe via chave
	  Cls
	  
	  ? "Consultando a situação da chave da NFe..."

    * Abrir um for no XML da NFe para ver nota a nota que tem no lote
      For I = 1 TO oAutorizacao:EnviNFe:GetNFeCount	
	     oConteudoNFe = oAutorizacao:EnviNFe:GetNFe(i-1)
         oConteudoInfNFe = oConteudoNFe:GetInfNFe(0)
		 chaveNFe = oConteudoInfNFe:Chave
		 
		 ? "Chave consultada:", chaveNFe
		 
	     oXmlConsSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")	  
	     oXmlConsSitNFe:Versao = "4.00"
	     oXmlConsSitNFe:TpAmb = 2 // TipoAmbiente.Homologacao
	     oXmlConsSitNFe:ChNFe = chaveNFe
		 
         oConfigConsSitNFe = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
         oConfigConsSitNFe:TipoDFe            = 0 // TipoDFe.NFe
         oConfigConsSitNFe:CertificadoSenha   = "12345678"
         oConfigConsSitNFe:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

         oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
         oConsultaProtocolo:Executar(oXmlConsSitNFe, oConfigConsSitNFe)
		 
		 ? "Xml Retornado na consulta: "
		 ? oConsultaProtocolo:RetornoWSString		 		 
		
		 if oConsultaProtocolo:Result:CStat == 100 //Nota autorizada
		    oAutorizacao:SetNullRetConsReciNFe()
			oAutorizacao:AddRetConsSitNFes(oConsultaProtocolo:Result)
		
            oAutorizacao:GravarXmlDistribuicao(".\tmp")
		 else
		    //Nota rejeitada, fazer devidos tratamentos
		 Endif
      Next I
   EndIf         
   Wait
*/
 * ---------------------------------
 * Recuperando informações do objeto
 * ---------------------------------
   ? "resgatando conteudos da tag xml"
   ? oxml:versao
   ? oxml:IdLote
   ?

   ? "resgatando conteudos da tag Nfe"
   oTagNfe = oxml:GetNfe(0)
   ? oTagNfe:GetInfNfeCount
   ?

   ? "resgatando conteudos da tag InfNfe e suas filhas"
   oTagInfNfe = oTagNfe:GetInfNfe(0)
   ? oTagInfNfe:Ide:VerProc
   ? oTagInfNfe:Ide:cUf
   ? oTagInfNfe:Ide:DhEmi
   ? oTagInfNfe:Emit:XNome
   ? oTagInfNfe:Emit:EnderEmit:XLgr
   ? oTagInfNfe:Dest:XNome
   ? oTagInfNfe:Dest:EnderDest:XLgr
   ?

   ? "resgatando conteudo da tag Det e suas filhas"
   For I = 1 To oTagInfNfe:GetDetCount() // loop com todos os produtos
       oTagDet = oTagInfNfe:GetDet(I-1) 
       // dados do item
       ? oTagDet:NItem, oTagDet:Prod:CProd, oTagDet:Prod:xProd
       // dados do imposto do item
       ? Space(2),oTagDet:Imposto:VTotTrib

       // pega o primeiro ICMS definido (na prática nunca tem mais que um)
       oTagIcms = oTagDet:Imposto:GetIcms(0) 

       // mostra dados do ICMSSN101 se existir
       If oTagIcms:ICMSSN101 <> NIL
          ?? "",oTagIcms:ICMSSN101:Orig
          ?? "",oTagIcms:ICMSSN101:PCredSN
          ?? "",oTagIcms:ICMSSN101:VCredICMSSN
       EndIf

       // mostra dados do ICMSSN102 se existir
       If oTagIcms:ICMSSN102 <> NIL
          ?? "",oTagIcms:ICMSSN102:Orig
       EndIf

       // dados do PIS
       ?? "",oTagDet:Imposto:Pis:PISOutr:CST
       ?? "",oTagDet:Imposto:Pis:PISOutr:VBC
       ?? "",oTagDet:Imposto:Pis:PISOutr:PPIS
       ?? "",oTagDet:Imposto:Pis:PISOutr:VPIS

       // dados do COFINS
       ?? "",oTagDet:Imposto:COFINS:COFINSOutr:CST
       ?? "",oTagDet:Imposto:COFINS:COFINSOutr:VBC
       ?? "",oTagDet:Imposto:COFINS:COFINSOutr:PCOFINS
       ?? "",oTagDet:Imposto:COFINS:COFINSOutr:VCOFINS
    Next I    

 // dados do total
    ? "Total" 
    ? "ICMSTot.VNF      = ",oTagInfNfe:Total:ICMSTot:VNF
    ? "ICMSTot.VTotTrib = ",oTagInfNfe:Total:ICMSTot:VTotTrib

 // dados da Transp  
    ? "Transp.ModFrete  = ",oTagInfNfe:Transp:ModFrete

 // dados do volume
    FOR I = 1 TO oTagInfNfe:Transp:GetVolCount()
       oTagVol = oTagInfNfe:Transp:GetVol(I-1)
       ? Space(7) + "Vol.QVol  = ",oTagVol:QVol
       ? Space(7) + "Vol.Esp   = ",oTagVol:Esp
       ? Space(7) + "Vol.Marca = ",oTagVol:Marca
       ? Space(7) + "Vol.PesoL = ",oTagVol:PesoL
       ? Space(7) + "Vol.PesoB = ",oTagVol:PesoB
    Next I
    ?

 // dados da cobrança
    ? "Fat.NFat  = ",oTagInfNfe:Cobr:Fat:NFat
    ? "Fat.VOrig = ",oTagInfNfe:Cobr:Fat:VOrig
    ? "Fat.VDesc = ",oTagInfNfe:Cobr:Fat:VDesc
    ? "Fat.VLiq  = ",oTagInfNfe:Cobr:Fat:VLiq
    ?
    // Dados das duplicatas
    ? "Duplicatas:"
    FOR I = 1 To oTagInfNfe:Cobr:GetDupCount()
        oTagDup = oTagInfNfe:Cobr:GetDup(I-1)
        ? "  Dup.NDup  = ",oTagDup:NDup
        ?? " Dup.DVenc = ",oTagDup:DVenc
        ?? " Dup.VDup  = ",oTagDup:VDup
    Next I   
    ?
    // Informações das Formas de Pagamento
    ? "Formas de pagamento:"
    FOR I = 1 To oTagInfNfe:Pag:GetDetPagCount()
        oTagDetPag = oTagInfNfe:Pag:GetDetPag(I-1)
        ? "  DetPag.IndPag  = ",oDetPag:IndPag
        ?? " DetPag.TPag    = ",oDetPag:TPag
        ?? " DetPag.VPag    = ",oDetPag:VPag
    Next I   
    ?
    // Informacoes Adicionais
    ? "Informacoes Adicionais"
    ? "    InfAdic = ",oTagInfNfe:InfAdic:InfCpl
    ? 
 
    // Informacoes do responsavel tecnico
    ? "Informacoes do responsavel tecnico"
    ? "    CNPJ     = ",oTagInfNfe:InfRespTec:CNPJ
    ? "    XContato = ",oTagInfNfe:InfRespTec:XContato
    ? "    Email    = ",oTagInfNfe:InfRespTec:Email
    ? "    Fone     = ",oTagInfNfe:InfRespTec:Fone
    ?
    Wait
Return

