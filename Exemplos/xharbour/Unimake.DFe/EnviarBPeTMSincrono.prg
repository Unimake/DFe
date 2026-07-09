* ---------------------------------------------------------------------------------
* Enviar BP-e TM modo sincrono
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarBPeTMSincrono()
   Local oConfiguracao, oBPeTM, oDetBPeTM, oDet, oComp, oPgto, oAutorizacao
   Local oErro, oExceptionInterop, xmlDistribuicao

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoEmissao = 1 //Normal
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oBPeTM = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.BPeTM" )
   oBPeTM:InfBPe = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.InfBPe" )
   oBPeTM:InfBPe:Versao = "1.00"

   oBPeTM:InfBPe:Ide = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Ide" )
   oBPeTM:InfBPe:Ide:CUF = 35 //Sao Paulo
   oBPeTM:InfBPe:Ide:TpAmb = 2 //Homologacao
   oBPeTM:InfBPe:Ide:Mod = 63 //BP-e
   oBPeTM:InfBPe:Ide:Serie = 1
   oBPeTM:InfBPe:Ide:NBP = 1
   oBPeTM:InfBPe:Ide:CBP = "12345678"
   oBPeTM:InfBPe:Ide:Modal = 1
   oBPeTM:InfBPe:Ide:DhEmi = DateTime()
   oBPeTM:InfBPe:Ide:DCompet = SToD( "20260706" )
   oBPeTM:InfBPe:Ide:TpEmis = 1
   oBPeTM:InfBPe:Ide:VerProc = "1.0"
   oBPeTM:InfBPe:Ide:TpBPe = 4
   oBPeTM:InfBPe:Ide:CFOP = "5353"

   oBPeTM:InfBPe:Emit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Emit" )
   oBPeTM:InfBPe:Emit:CNPJ = "12345678000195"
   oBPeTM:InfBPe:Emit:IE = "123456789012"
   oBPeTM:InfBPe:Emit:XNome = "EMPRESA BP-E TM"
   oBPeTM:InfBPe:Emit:CRT = 3 //Regime normal
   oBPeTM:InfBPe:Emit:EnderEmit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.EnderEmit" )
   oBPeTM:InfBPe:Emit:EnderEmit:XLgr = "RUA TESTE"
   oBPeTM:InfBPe:Emit:EnderEmit:Nro = "100"
   oBPeTM:InfBPe:Emit:EnderEmit:XBairro = "CENTRO"
   oBPeTM:InfBPe:Emit:EnderEmit:CMun = "3550308"
   oBPeTM:InfBPe:Emit:EnderEmit:XMun = "SAO PAULO"
   oBPeTM:InfBPe:Emit:EnderEmit:UF = 35 //Sao Paulo

   oDetBPeTM = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.DetBPeTM" )
   oDetBPeTM:IdEqpCont = 1
   oDetBPeTM:UFIniViagem = 35 //Sao Paulo

   oDet = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Det" )
   oDet:NViagem = 1
   oDet:CMunIni = "3550308"
   oDet:QPass = "1"
   oDet:VBP = 10.00
   oDet:Imp = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Imp" )
   oDet:Imp:ICMS = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.ICMS" )
   oDet:Imp:ICMS:ICMS00 = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.ICMS00" )
   oDet:Imp:ICMS:ICMS00:CST = 0
   oDet:Imp:ICMS:ICMS00:VBC = 10.00
   oDet:Imp:ICMS:ICMS00:PICMS = 18.00
   oDet:Imp:ICMS:ICMS00:VICMS = 1.80

   oComp = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Comp" )
   oComp:XNome = "TARIFA"
   oComp:QComp = "00001"
   oDet:AddComp( oComp )
   oDetBPeTM:AddDet( oDet )
   oBPeTM:InfBPe:AddDetBPeTM( oDetBPeTM )

   oBPeTM:InfBPe:Total = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Total" )
   oBPeTM:InfBPe:Total:QPass = 1
   oBPeTM:InfBPe:Total:VBP = 10.00
   oBPeTM:InfBPe:Total:ICMSTot = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.ICMSTot" )
   oBPeTM:InfBPe:Total:ICMSTot:VBC = 10.00
   oBPeTM:InfBPe:Total:ICMSTot:VICMS = 1.80
   oBPeTM:InfBPe:Total:VTotDFe = 10.00

   oBPeTM:InfBPe:PgtoVinc = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.PgtoVinc" )
   oPgto = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Pgto" )
   oPgto:NPag = "1"
   oPgto:IdTransacao = "TRANSACAO123"
   oPgto:TpMeioPgto = 17
   oPgto:CNPJReceb = "12345678000195"
   oPgto:CNPJBasePSP = "12345678"
   oBPeTM:InfBPe:PgtoVinc:AddPgto( oPgto )

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
      ? oBPeTM:GerarXMLString()

    * Consumir o servico
      oAutorizacao = CreateObject( "Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM" )
      oAutorizacao:Executar( oBPeTM, oConfiguracao )

      ? oAutorizacao:RetornoWSString

      If oAutorizacao:Result:CStat = 100 .And. ValType( oAutorizacao:Result:ProtBPe ) = "O" .And. ;
         oAutorizacao:Result:ProtBPe:InfProt:CStat = 100

         ? oAutorizacao:Result:ProtBPe:InfProt:NProt
         oAutorizacao:GravarXmlDistribuicao( "d:\testenfe" )
         xmlDistribuicao = oAutorizacao:GetBPeTMProcResults( oBPeTM:InfBPe:Chave )
         ? xmlDistribuicao
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
