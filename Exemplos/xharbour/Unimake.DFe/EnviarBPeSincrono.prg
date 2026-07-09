* ---------------------------------------------------------------------------------
* Enviar BP-e modo sincrono
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarBPeSincrono()
   Local oConfiguracao, oBPe, oInfViagem, oCompValor, oPag, oAutorizacao
   Local oErro, oExceptionInterop, xmlDistribuicao

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
   oConfiguracao:TipoDFe = 22 //22=BPe
   oConfiguracao:TipoEmissao = 1 //Normal
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"

 * Criar XML
   oBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.BPe" )
   oBPe:InfBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfBPe" )
   oBPe:InfBPe:Versao = "1.00"

   oBPe:InfBPe:Ide = CreateObject( "Unimake.Business.DFe.Xml.BPe.Ide" )
   oBPe:InfBPe:Ide:CUF = 35 //Sao Paulo
   oBPe:InfBPe:Ide:TpAmb = 2 //Homologacao
   oBPe:InfBPe:Ide:Mod = 63 //BP-e
   oBPe:InfBPe:Ide:Serie = 1
   oBPe:InfBPe:Ide:NBP = 1
   oBPe:InfBPe:Ide:CBP = "12345678"
   oBPe:InfBPe:Ide:Modal = 1
   oBPe:InfBPe:Ide:DhEmi = DateTime()
   oBPe:InfBPe:Ide:TpEmis = 1
   oBPe:InfBPe:Ide:VerProc = "1.0"
   oBPe:InfBPe:Ide:TpBPe = 0
   oBPe:InfBPe:Ide:IndPres = 1
   oBPe:InfBPe:Ide:UFIni = 35 //Sao Paulo
   oBPe:InfBPe:Ide:CMunIni = "3550308"
   oBPe:InfBPe:Ide:UFFim = 33 //Rio de Janeiro
   oBPe:InfBPe:Ide:CMunFim = "3304557"

   oBPe:InfBPe:Emit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.Emit" )
   oBPe:InfBPe:Emit:CNPJ = "12345678000195"
   oBPe:InfBPe:Emit:IE = "123456789012"
   oBPe:InfBPe:Emit:XNome = "EMPRESA BP-E"
   oBPe:InfBPe:Emit:CRT = 3 //Regime normal
   oBPe:InfBPe:Emit:EnderEmit = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.EnderEmit" )
   oBPe:InfBPe:Emit:EnderEmit:XLgr = "RUA TESTE"
   oBPe:InfBPe:Emit:EnderEmit:Nro = "100"
   oBPe:InfBPe:Emit:EnderEmit:XBairro = "CENTRO"
   oBPe:InfBPe:Emit:EnderEmit:CMun = "3550308"
   oBPe:InfBPe:Emit:EnderEmit:XMun = "SAO PAULO"
   oBPe:InfBPe:Emit:EnderEmit:UF = 35 //Sao Paulo

   oBPe:InfBPe:InfPassagem = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfPassagem" )
   oBPe:InfBPe:InfPassagem:CLocOrig = "3550308"
   oBPe:InfBPe:InfPassagem:XLocOrig = "SAO PAULO"
   oBPe:InfBPe:InfPassagem:CLocDest = "3304557"
   oBPe:InfBPe:InfPassagem:XLocDest = "RIO DE JANEIRO"
   oBPe:InfBPe:InfPassagem:DhEmb = DateTime()
   oBPe:InfBPe:InfPassagem:DhValidade = DateTime()
   oBPe:InfBPe:InfPassagem:InfPassageiro = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfPassageiro" )
   oBPe:InfBPe:InfPassagem:InfPassageiro:XNome = "PASSAGEIRO TESTE"
   oBPe:InfBPe:InfPassagem:InfPassageiro:CPF = "12345678901"
   oBPe:InfBPe:InfPassagem:InfPassageiro:TpDoc = 1
   oBPe:InfBPe:InfPassagem:InfPassageiro:NDoc = "12345678"

   oInfViagem = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfViagem" )
   oInfViagem:CPercurso = "001"
   oInfViagem:XPercurso = "SAO PAULO - RIO DE JANEIRO"
   oInfViagem:TpViagem = 0
   oInfViagem:TpServ = 1
   oInfViagem:TpAcomodacao = 1
   oInfViagem:TpTrecho = 1
   oInfViagem:DhViagem = DateTime()
   oBPe:InfBPe:AddInfViagem( oInfViagem )

   oBPe:InfBPe:InfValorBPe = CreateObject( "Unimake.Business.DFe.Xml.BPe.InfValorBPe" )
   oBPe:InfBPe:InfValorBPe:VBP = 100.00
   oBPe:InfBPe:InfValorBPe:VDesconto = 0.00
   oBPe:InfBPe:InfValorBPe:VPgto = 100.00
   oBPe:InfBPe:InfValorBPe:VTroco = 0.00

   oCompValor = CreateObject( "Unimake.Business.DFe.Xml.BPe.CompValor" )
   oCompValor:TpComp = 1
   oCompValor:VComp = 100.00
   oBPe:InfBPe:InfValorBPe:AddComp( oCompValor )

   oBPe:InfBPe:Imp = CreateObject( "Unimake.Business.DFe.Xml.BPe.Imp" )
   oBPe:InfBPe:Imp:ICMS = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.ICMS" )
   oBPe:InfBPe:Imp:ICMS:ICMS00 = CreateObject( "Unimake.Business.DFe.Xml.BPeTM.ICMS00" )
   oBPe:InfBPe:Imp:ICMS:ICMS00:CST = 0
   oBPe:InfBPe:Imp:ICMS:ICMS00:VBC = 100.00
   oBPe:InfBPe:Imp:ICMS:ICMS00:PICMS = 18.00
   oBPe:InfBPe:Imp:ICMS:ICMS00:VICMS = 18.00

   oPag = CreateObject( "Unimake.Business.DFe.Xml.BPe.Pag" )
   oPag:TPag = 1
   oPag:VPag = 100.00
   oBPe:InfBPe:AddPag( oPag )

 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try
      ? oBPe:GerarXMLString()

    * Consumir o servico
      oAutorizacao = CreateObject( "Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe" )
      oAutorizacao:Executar( oBPe, oConfiguracao )

      ? oAutorizacao:RetornoWSString

      If oAutorizacao:Result:CStat = 100 .And. ValType( oAutorizacao:Result:ProtBPe ) = "O" .And. ;
         oAutorizacao:Result:ProtBPe:InfProt:CStat = 100

         ? oAutorizacao:Result:ProtBPe:InfProt:NProt
         oAutorizacao:GravarXmlDistribuicao( "d:\testenfe" )
         xmlDistribuicao = oAutorizacao:GetBPeProcResults( oBPe:InfBPe:Chave )
         ? xmlDistribuicao
      Endif

   Catch oErro
      ? "Erro Harbour: " + oErro:Description
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
   End

   Wait
Return
