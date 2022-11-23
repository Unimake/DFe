* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarMDFeSincrono()
   Local oConfiguracao, oErro
   Local oMDFe, oInfMDFe, oIDE, oInfMunCarrega, oEmit, oEnderEmit
   Local oInfModal, oRodo, oInfANTT, oInfContratante, oVeicTracao, oCondutor
   Local oInfDoc, oInfMunDescarga, oInfCTe, oInfNFe, oInfUnidTransp, oLacUnidTransp
   Local oInfUnidCarga, oLacUnidCarga, oSeg, oInfResp, oInfSeg, oProdPred
   Local oInfLotacao, oInfLocalCarrega, oInfLocalDescarrega, oTot, oLacres, oLacre
   Local oInfAdic, oInfRespTec, oExceptionInterop, oAutorizacao, oConfigRec, oConsReciMDFe
   Local oRetAutorizacao, oProtMDFe
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 4 //4=MDFe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"   
  
 * Criar o XML do MDFe
   oMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   
   //Criar a tag <infMDFe>
   oInfMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMDFe")
   oInfMDFe:Versao = "3.00"
   
   //Criar a tag <ide>
   oIDE = CreateObject("Unimake.Business.DFe.Xml.MDFe.IDE")
   
   oIDE:CUF = 41 //UFBrasil.PR
   oIDE:TpAmb = 2 //TipoAmbiente.Homologacao
   oIDE:TpEmit = 1 //TipoEmitenteMDFe.PrestadorServicoTransporte
   oIDE:Mod = 58 //ModeloDFe.MDFe
   oIDE:Serie = 1
   oIDE:NMDF = 861
   oIDE:CMDF = "01722067"
   oIDE:Modal = 1 //ModalidadeTransporteMDFe.Rodoviario
   oIDE:DhEmi = DateTime()
   oIDE:TpEmis = 1 //TipoEmissao.Normal
   oIDE:ProcEmi = 0 //ProcessoEmissao.AplicativoContribuinte
   oIDE:VerProc = "UNICO V8.0"
   oIDE:UFIni = 41 //UFBrasil.PR
   oIDE:UFFim = 35 //UFBrasil.SP
   oIDE:DhIniViagem = DateTime() 

   //Criar a tag <infMunCarrega>
   oInfMunCarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunCarrega")
   oInfMunCarrega:CMunCarrega = 4118402
   oInfMunCarrega:XMunCarrega = "PARANAVAI"   
   
   //Adicionar o grupo de tag <infMunCarrega> dentro da IDE
   oIDE:AddInfMunCarrega(oInfMunCarrega)
   
   //Adicionar o grupo de tag <ide> dentro da tag <infMDFe>
   oInfMDFe:IDE = oIDE
   
   //Criar a tag <emit>
   oEmit = CreateObject("Unimake.Business.DFe.Xml.MDFe.Emit")
   oEmit:CNPJ = "06117473000150"
   oEmit:IE = "9456656656"
   oEmit:XNome = "XXXXXX XXXXXX XXXXXX"
   oEmit:XFant = "XXXXXX XXXXXX" 

   //Criar a tag <enderEmit>
   oEnderEmit = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnderEmit")
   oEnderEmit:XLgr = "RUA XXXXXXX X. XX XXXXX"
   oEnderEmit:Nro = "01112"
   oEnderEmit:XBairro = "VILA XXXXXXXXX"
   oEnderEmit:CMun = 4118402
   oEnderEmit:XMun = "PARANAVAI"
   oEnderEmit:CEP = "87706000"
   oEnderEmit:UF = 41 //UFBrasil.PR
   oEnderEmit:Fone = "04433333333"
   
   //Adicionar o grupo de tag <enderEmit> dentro da tag <emit>   
   oEmit:EnderEmit = oEnderEmit
   
   //Adicionar o grupo de tag <emit> dentro da tag <infMDFe>
   oInfMDFe:Emit = oEmit
   
   //Criar o grupo de tag <infModal>
   oInfModal = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfModal")
   oInfModal:VersaoModal = "3.00"
   
   //Criar o grupo de tag <rodo>
   oRodo = CreateObject("Unimake.Business.DFe.Xml.MDFe.Rodo")

   //Criar o grupo de tag <infANTT>
   oInfANTT = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfANTT")
   oInfANTT:RNTRC = "44556666"
   
   //Criar o grupo de tag <infContratante>
   oInfContratante = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfContratante")
   oInfContratante:CNPJ = "06117473000150"
   
   //Adicionar o grupo de tag <infContratante> dentro da tag <infANTT>
   oInfANTT:AddInfContratante(oInfContratante)
   
   //Criar um novo grupo de tag <infContratante>
   oInfContratante = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfContratante")
   oInfContratante:CNPJ = "06117473000150"
   
   //Adicionar o grupo de tag <infContratante> dentro da tag <infANTT>
   oInfANTT:AddInfContratante(oInfContratante)
   
   //Adicionar o grupo de tag <infANTT> dentro do grupo <rodo>
   oRodo:InfANTT = oInfANTT
   
   //Criar o grupo de tag <veicTracao>
   oVeicTracao = CreateObject("Unimake.Business.DFe.Xml.MDFe.VeicTracao")
   oVeicTracao:CInt = "AXF0000"
   oVeicTracao:Placa = "AXF0000"
   oVeicTracao:Tara = 0
   oVeicTracao:CapKG = 5000
   oVeicTracao:TpRod = 2 //TipoRodado.Toco
   oVeicTracao:TpCar = 2 //TipoCarroceriaMDFe.FechadaBau
   oVeicTracao:UF = 41 //UFBrasil.PR
   
   //Criar o grupo de tag <prop>
   oProp = CreateObject("Unimake.Business.DFe.Xml.MDFe.Prop")
   oProp:CNPJ = "06117443000150"
   oProp:RNTRC = "44556666"
   oProp:XNome = "XXXXXX XXXXXX XXXXXX"
   oProp:IE = "5545546656"
   oProp:UF = 41 //UFBrasil.PR
   oProp:TpProp = 2 //TipoProprietarioMDFe.Outros
   
   //Adicionar o grupo de tag <prop> dentro do grupo <veicTracao>
   oVeicTracao:Prop = oProp
   
   //Criar o grupo de tag <condutor>
   oCondutor = CreateObject("Unimake.Business.DFe.Xml.MDFe.Condutor")
   oCondutor:XNome = "XXXXXXXXX XXXXX XX XXXXX"
   oCondutor:CPF = "02133333333"
   
   //Adicionar o grupo de tag <condutor> dentro do grupo <veicTracao>
   oVeicTracao:AddCondutor(oCondutor)
   
   //Adicionar o grupo de tag <veicTracao> dentro do grupo <rodo>
   oRodo:VeicTracao = oVeicTracao
   
   //Adicionar o grupo de tag <rodo> dentro do grupo <infModal>
   oInfModal:Rodo = oRodo
   
   //Adicionar o grupo de tag <infModal> dentro do grupo <infMDFe>
   oInfMDFe:InfModal = oInfModal

   //Criar o grupo de tag <infDoc>
   oInfDoc = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfDocInfMDFe")
   
   //Criar o grupo de tag <infMunDescarga>
   oInfMunDescarga = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescarga")
   oInfMunDescarga:CMunDescarga = 3505708
   oInfMunDescarga:XMunDescarga = "BARUERI"
   
   //Criar o grupo de tag <infCTe>
   oInfCTe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe")
   oInfCTe:ChCTe = "41000000000000000000000000000000000000000006"
   
   //Adicionar o grupo de tag <infCTe> dentro do grupo <infMunDescarga>
   oInfMunDescarga:AddInfCTe(oInfCTe)

   //Criar um novo grupo de tag <infCTe>
   oInfCTe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe")
   oInfCTe:ChCTe = "41000000000000000000000000000000000000000004"
   
   //Adicionar o grupo de tag <infCTe> dentro do grupo <infMunDescarga>
   oInfMunDescarga:AddInfCTe(oInfCTe)
   
   //Criar o grupo de tag <infNFe>
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfNFe")
   oInfNFe:ChNFe = "12345678901234567890123456789012345678901234"
   
   //Criar o grupo de tag <infUnidTransp>
   oInfUnidTransp = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfUnidTransp")
   oInfUnidTransp:IdUnidTransp = "122"
   oInfUnidTransp:TpUnidTransp = 2 //TipoUnidadeTransporte.RodoviarioReboque
   
   //Criar o grupo de tag <lacUnidTransp>
   oLacUnidTransp = CreateObject("Unimake.Business.DFe.Xml.MDFe.LacUnidTransp")
   oLacUnidTransp:NLacre = "12334"
   
   //Adicionar o grupo de tag <lacUnidTransp> dentro do grupo <infUnidTransp>
   oInfUnidTransp:AddLacUnidTransp(oLacUnidTransp)
   
   //Criar o grupo de tag <infUnidCarga>
   oInfUnidCarga = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfUnidCarga")
   oInfUnidCarga:TpUnidCarga = 1 //TipoUnidadeCarga.Container
   oInfUnidCarga:IdUnidCarga = "123"
   
   //Criar o grupo de tag <lacUnidCarga>
   oLacUnidCarga = CreateObject("Unimake.Business.DFe.Xml.MDFe.LacUnidCarga")
   oLacUnidCarga:NLacre = "3333333"
   
   //Adicionar o grupo de tag <lacUnidCarga> dentro do grupo <infUnidCarga>
   oInfUnidCarga:AddLacUnidCarga(oLacUnidCarga)
   
   //Adicionar o grupo de tag <infUnidCarga> dentro do grupo <infUnidTransp>
   oInfUnidTransp:AddInfUnidCarga(oInfUnidCarga)
   
   //Adicionar o grupo de tag <infUnidTrans> dentro do grupo <infNFe>
   oInfNFe:AddInfUnidTransp(oInfUnidTransp)
   
   //Adicionar o grupo <infNfe> dentro do grupo <infMunDescarga>
   oInfMunDescarga:AddInfNFe(oInfNFe)
   
   //Adicionar o grupo de tag <infMunDescarga> dentro do grupo <infDoc>
   oInfDoc:AddInfMunDescarga(oInfMunDescarga)
   
   //Criar um novo grupo de tag <infMunDescarga>
   oInfMunDescarga = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescarga")
   oInfMunDescarga:CMunDescarga = 3550308
   oInfMunDescarga:XMunDescarga = "SAO PAULO"
   
   //Criar o grupo de tag <infCTe>
   oInfCTe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe")
   oInfCTe:ChCTe = "41000000000000000000000000000000000000000000"
   
   //Adicionar o grupo de tag <infCTe> dentro do grupo <infMunDescarga>
   oInfMunDescarga:AddInfCTe(oInfCTe)
   
   //Adicionar o grupo de tag <infMunDescarga> dentro do grupo <infDoc>
   oInfDoc:AddInfMunDescarga(oInfMunDescarga)
   
   //Adicionar o grupo de tag <infDoc> dentro do grupo <infMDFe>
   oInfMDFe:InfDoc = oInfDoc
   
   //Criar o grupo de tag <seg>
   oSeg = CreateObject("Unimake.Business.DFe.Xml.MDFe.Seg")
   oSeg:NApol = "033666565656"
   
   //Criar o grupo de tag <infResp>
   oInfResp = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfResp")
   oInfResp:RespSeg = 1 //ResponsavelSeguroMDFe.EmitenteMDFe
   oInfResp:CNPJ = "06117473000150"
   
   //Adicionar o grupo de tag <infResp> dentro do grupo <seg>
   oSeg:InfResp = oInfResp
   
   //Criar o grupo de tag <infSeg>
   oInfSeg = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfSeg")
   oInfSeg:XSeg = "PORTO SEGURO"
   oInfSeg:CNPJ = "06117473000150"
   
   //Adicionar o grupo de tag <infSeg> dentro do grupo <seg>
   oSeg:InfSeg = oInfSeg
   
   //Adicionar tag <nAver> no grupo <seg> quantas vezes for necessário (Pode ter mais de uma)
   oSeg:AddNAver("0000000000000000000000000000000000000000") 
   oSeg:AddNAver("0000000000000000000000000000000000000000") 
   
   //Adicionar o grupo <seg> dentro do grupo <infMDFe>
   oInfMDFe:AddSeg(oSeg)
   
   //Criar o grupo de tag <prodPred>
   oProdPred = CreateObject("Unimake.Business.DFe.Xml.MDFe.ProdPred")
   oProdPred:TpCarga = 5 //TipoCargaMDFe.CargaGeral
   oProdPred:XProd = "TESTE DE PRODUTO PREDOMINANTE"
   
   //Criar o grupo de tag <infLotacao>
   oInfLotacao = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLotacao")
   
   //Criar o grupo de tag <infLocalCarrega>
   oInfLocalCarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLocalCarrega")
   oInfLocalCarrega:CEP = "87302080"
   
   //Adicionar o grupo de tag <infLocalCarrega> dentro do grupo <infLotacao>
   oInfLotacao:InfLocalCarrega = oInfLocalCarrega
   
   //Criar o grupo de tag <infLocalDescarrega>
   oInfLocalDescarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLocalDescarrega")
   oInfLocalDescarrega:CEP = "25650208"
   
   //Adicionar o grupo de tag <infLocalDescarrega> dentro do grupo de tag <oInfLotacao>
   oInfLotacao:InfLocalDescarrega = oInfLocalDescarrega
   
   //Adicionar a tag <infLotacao> dentro da tag <prodPred>
   oProdPred:InfLotacao = oInfLotacao
   
   //Adicionar a tag <prodPred> dentro do grupo <infMDFe>
   oInfMDFe:ProdPred = oProdPred
   
   //Criar o grupo de tag <tot>
   oTot = CreateObject("Unimake.Business.DFe.Xml.MDFe.Tot")
   oTot:QCTe = 3
   oTot:VCarga = 56599.09
   oTot:CUnid = 1 //CodigoUnidadeMedidaMDFe.KG
   oTot:QCarga = 2879.00
   
   //Adicionar o grupo de tag <tot> dentro do grupo <infMDFe>
   oInfMDFe:Tot = oTot
   
   //Criar o grupo de tag <lacres>
   oLacre = CreateObject("Unimake.Business.DFe.Xml.MDFe.Lacre")
   oLacre:NLacre = "1111111"
   
   //Adicionar o grupo de tag <lacre> dentro do grupo <lacres>
   oInfMDFe:AddLacres(oLacre)
   
   //Criar um novo grupo de tag <lacre>
   oLacre = CreateObject("Unimake.Business.DFe.Xml.MDFe.Lacre")
   oLacre:NLacre = "2222222"
   
   //Adicionar o grupo de tag <lacre> dentro do grupo <lacres>
   oInfMDFe:AddLacres(oLacre)
   
   //Criar o grupo de tag <infAdic>
   oInfAdic = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfAdic")
   oInfAdic:InfCpl = "DATA/HORA PREVISTA PARA O INICO DA VIAGEM: 10/08/2020 as 08:00"
   
   //Adicionar o grupo de tag <infAdic> dentro do grupo <infMDFe>
   oInfMDFe:InfAdic = oInfAdic
   
   //Criar o grupo de tag <infRespTec>
   oInfRespTec = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfRespTec")
   oInfRespTec:CNPJ = "99999999999999"
   oInfRespTec:XContato = "Teste de Responsavel Tecnico"
   oInfRespTec:Email = "testey@teste.com.br"
   oInfRespTec:Fone = "04431414900"
   
   //Adicionar o grupo de tag <infRespTec> dentro do grupo <infMDFe>
   oInfMDFe:InfRespTec = oInfRespTec
   
   //Adicionar o grupo de tag <InfMDFe> dentro do grupo <MDFe>
   oMDFe:InfMDFe = oInfMDFe
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   ? "CNPJ Emitente:", oMDFe:InfMDFe:Emit:CNPJ
   ? "Razao Emitente:", oMDFe:InfMDFe:Emit:XNome
   ? "Data Emissao:", oMDFe:InfMDFe:IDE:DhEmi
   ? "Chave do MDFe:", oMDFe:InfMDFe:Chave
   ?
   ? 
   Wait
   ?
   ?
   ?
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
    * Criar o objeto para consumir o serviço de autorização do MDFe
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")
	  oAutorizacao:SetXMLConfiguracao(oMDFe, oConfiguracao)

	  //O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
	  //caso seja necessário. Imagine que de um problema no envio do MDFe e vc precise resgatar para enviar novamente.
	  ? "Demonstrar o XML do MDFe assinadom: "
	  ?
	  ? oAutorizacao:GetConteudoMDFeAssinado()
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?

      oAutorizacao:Executar(oMDFe, oConfiguracao)   
   
	  ? "XML retornado pela SEFAZ no envio do XML de MDFe:"
	  ?
	  ? oAutorizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
   
	  If oAutorizacao:Result <> NIL
	     ? "Status envio:", oAutorizacao:Result:CStat, oAutorizacao:Result:XMotivo
		 ?
		 ?
		 Wait
		 ?
		 ?
		 ?

         If oAutorizacao:Result:CStat == 104 //Lote processado
            If oAutorizacao:Result:ProtMDFe:InfProt:CStat == 100 //MDFe autorizado
  		       ? "Status da de autorizacao/rejeicao do MDFe:", oAutorizacao:Result:ProtMDFe:InfProt:CStat, oAutorizacao:Result:ProtMDFe:InfProt:CStat
			   ? "Protocolo de autorizacao: ", oAutorizacao:Result:ProtMDFe:InfProt:NProt
			   ?
			   Wait
			   ?
			   ?
			   ?
  	           oAutorizacao:GravarXmlDistribuicao("d:\testenfe")			   			   
			   
			   stringXmlDistribuicao = oAutorizacao:GetMDFeProcResults(oMDFe:InfMDFe:Chave)
			Else   
               //Rejeitado, fazer devidos tratamentos
			Endif
		 Else
             //Rejeitado, fazer devidos tratamentos		 
		 Endif		 
	  Endif   
   
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