* ---------------------------------------------------------------------------------
* Enviar BP-e TA modo sincrono
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarBPeTASincrono()
   Local oConfiguracao, oBPeTA, oInfViagem, oCompValor, oPag, oAutorizacao
   Local oErro, oExceptionInterop, xmlDistribuicao

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoEmissao = 1 //Normal
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oBPeTA = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.BPeTA" )
   oBPeTA:InfBPe = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.InfBPe" )
   oBPeTA:InfBPe:Versao = "1.00"

   oBPeTA:InfBPe:Ide = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.Ide" )
   oBPeTA:InfBPe:Ide:CUF = 35 //Sao Paulo
   oBPeTA:InfBPe:Ide:TpAmb = 2 //Homologacao
   oBPeTA:InfBPe:Ide:Mod = 63 //BP-e
   oBPeTA:InfBPe:Ide:Serie = 1
   oBPeTA:InfBPe:Ide:NBP = 1
   oBPeTA:InfBPe:Ide:CBP = "12345678"
   oBPeTA:InfBPe:Ide:Modal = 2
   oBPeTA:InfBPe:Ide:DhEmi = DateTime()
   oBPeTA:InfBPe:Ide:TpEmis = 1
   oBPeTA:InfBPe:Ide:VerProc = "1.0"
   oBPeTA:InfBPe:Ide:TpBPe = 0
   oBPeTA:InfBPe:Ide:TpCompra = 0
   oBPeTA:InfBPe:Ide:IndPres = 1
   oBPeTA:InfBPe:Ide:UFIni = 35 //Sao Paulo
   oBPeTA:InfBPe:Ide:CMunIni = "3550308"
   oBPeTA:InfBPe:Ide:UFFim = 33 //Rio de Janeiro
   oBPeTA:InfBPe:Ide:CMunFim = "3304557"

   oBPeTA:InfBPe:Emit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Emit" )
   oBPeTA:InfBPe:Emit:CNPJ = "12345678000195"
   oBPeTA:InfBPe:Emit:IE = "123456789012"
   oBPeTA:InfBPe:Emit:XNome = "EMPRESA BP-E TA"
   oBPeTA:InfBPe:Emit:CRT = 3 //Regime normal
   oBPeTA:InfBPe:Emit:EnderEmit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.EnderEmit" )
   oBPeTA:InfBPe:Emit:EnderEmit:XLgr = "RUA TESTE"
   oBPeTA:InfBPe:Emit:EnderEmit:Nro = "100"
   oBPeTA:InfBPe:Emit:EnderEmit:XBairro = "CENTRO"
   oBPeTA:InfBPe:Emit:EnderEmit:CMun = "3550308"
   oBPeTA:InfBPe:Emit:EnderEmit:XMun = "SAO PAULO"
   oBPeTA:InfBPe:Emit:EnderEmit:UF = 35 //Sao Paulo

   oBPeTA:InfBPe:InfPassagem = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.InfPassagem" )
   oBPeTA:InfBPe:InfPassagem:DhEmb = DateTime()
   oBPeTA:InfBPe:InfPassagem:DhValidade = DateTime()
   oBPeTA:InfBPe:InfPassagem:InfPassageiro = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.InfPassageiro" )
   oBPeTA:InfBPe:InfPassagem:InfPassageiro:XNome = "PASSAGEIRO TESTE"
   oBPeTA:InfBPe:InfPassagem:InfPassageiro:CPF = "12345678901"
   oBPeTA:InfBPe:InfPassagem:InfPassageiro:TpDoc = 1
   oBPeTA:InfBPe:InfPassagem:InfPassageiro:NDoc = "12345678"

   oInfViagem = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.InfViagem" )
   oInfViagem:NroVoo = "1234"
   oInfViagem:SiglaCiaOperVoo = "UMK"
   oInfViagem:TpViagem = 0
   oInfViagem:CAeroOrig = "GRU"
   oInfViagem:CAeroDest = "GIG"
   oInfViagem:TpServ = 12
   oInfViagem:TpAcomodacao = 6
   oInfViagem:TpTrecho = 1
   oInfViagem:DhViagem = DateTime()
   oBPeTA:InfBPe:AddInfViagem( oInfViagem )

   oBPeTA:InfBPe:InfValorBPe = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.InfValorBPe" )
   oBPeTA:InfBPe:InfValorBPe:VBP = 100.00
   oBPeTA:InfBPe:InfValorBPe:VDesconto = 0.00
   oBPeTA:InfBPe:InfValorBPe:VPgto = 100.00
   oBPeTA:InfBPe:InfValorBPe:VTroco = 0.00

   oCompValor = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.CompValor" )
   oCompValor:TpComp = 1
   oCompValor:VComp = 100.00
   oBPeTA:InfBPe:InfValorBPe:AddComp( oCompValor )

   oBPeTA:InfBPe:Imp = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.Imp" )
   oBPeTA:InfBPe:Imp:IBSCBS = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.IBSCBS" )
   oBPeTA:InfBPe:Imp:IBSCBS:CST = "000"
   oBPeTA:InfBPe:Imp:IBSCBS:CClassTrib = "000001"
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.GIBSCBS" )
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:VBC = 100.00
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSUF = CreateObject( "Unimake.Business.DFe.Xml.BPe.GIBSUF" )
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSUF:PIBSUF = 0.1000
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSUF:VIBSUF = 0.10
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSMun = CreateObject( "Unimake.Business.DFe.Xml.BPe.GIBSMun" )
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSMun:PIBSMun = 0.1000
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GIBSMun:VIBSMun = 0.10
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:VIBS = 0.20
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GCBS = CreateObject( "Unimake.Business.DFe.Xml.BPe.GCBS" )
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GCBS:PCBS = 0.1000
   oBPeTA:InfBPe:Imp:IBSCBS:GIBSCBS:GCBS:VCBS = 0.10
   oBPeTA:InfBPe:Imp:VTotDFe = 100.30

   oPag = CreateObject( "Unimake.Business.DFe.Xml.BPeTA.Pag" )
   oPag:TPag = 1
   oPag:VPag = 100.00
   oBPeTA:InfBPe:AddPag( oPag )

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
      ? oBPeTA:GerarXMLString()

    * Consumir o servico
      oAutorizacao = CreateObject( "Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA" )
      oAutorizacao:Executar( oBPeTA, oConfiguracao )

      ? oAutorizacao:RetornoWSString

      If oAutorizacao:Result:CStat = 100 .And. ValType( oAutorizacao:Result:ProtBPe ) = "O" .And. ;
         oAutorizacao:Result:ProtBPe:InfProt:CStat = 100

         ? oAutorizacao:Result:ProtBPe:InfProt:NProt
         oAutorizacao:GravarXmlDistribuicao( "d:\testenfe" )
         xmlDistribuicao = oAutorizacao:GetBPeTAProcResults( oBPeTA:InfBPe:Chave )
         ? xmlDistribuicao
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
