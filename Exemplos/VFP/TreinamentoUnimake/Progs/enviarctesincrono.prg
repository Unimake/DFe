* ---------------------------------------------------------------------------------
* Enviar CTe de forma síncrona
* ---------------------------------------------------------------------------------
FUNCTION EnviarCTeSincrono()
   LOCAL oConfiguracao, oErro, oExceptionInterop
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 2 && 2=CTe
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"   
   
 * Criar grupo de tag <CTe>
   oCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.CTe")   
   
 * Criar a tag <infCte>
   oInfCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCTe")
   oInfCTe.Versao = "3.00" 

 * Criar grupo de tag <ide>
   oIde = CreateObject("Unimake.Business.DFe.Xml.CTe.Ide")   
   oIde.CUF = 41 && UFBrasil.PR
   oIde.CCT = "01722067"
   oIde.CFOP  = "6352"
   oIde.NatOp = "PREST.SERV.TRANSP.INDUSTR"
   oIde.Mod = 57 && ModeloDFe.CTe
   oIde.Serie = 1
   oIde.NCT = 861
   oIde.DhEmi = DateTime()
   oIde.TpImp = 2 && FormatoImpressaoDACTE.NormalPaisagem
   oIde.TpEmis = 1 && TipoEmissao.Normal
   oIde.TpAmb = 2 && TipoAmbiente.Homologacao
   oIde.TpCTe = 0 && TipoCTe.Normal
   oIde.ProcEmi = 0 && ProcessoEmissao.AplicativoContribuinte
   oIde.VerProc = "UNICO V8.0"
   oIde.CMunEnv = "4118402"
   oIde.XMunEnv = "PARANAVAI"
   oIde.UFEnv = 41 && UFBrasil.PR
   oIde.Modal =  01 && ModalidadeTransporteCTe.Rodoviario
   oIde.TpServ = 0 && TipoServicoCTe.Normal
   oIde.CMunIni = "4118402"
   oIde.XMunIni = "PARANAVAI"
   oIde.UFIni = 41 && UFBrasil.PR
   oIde.CMunFim = "3305109"
   oIde.XMunFim = "SAO JOAO DE MERITI"
   oIde.UFFim =  33 && UFBrasil.RJ
   oIde.Retira = 0 && SimNao.Nao
   oIde.IndIEToma = 1 && IndicadorIEDestinatario.ContribuinteICMS
   
 * Criar grupo de tag <toma3>
   oToma3 = CreateObject("Unimake.Business.DFe.Xml.CTe.Toma3")
   oToma3.Toma = 0 && TomadorServicoCTe.Remetente
   
 * Adicionar o grupo de tag <toma3> dentro do grupo <ide>
   oIde.Toma3 = oToma3
   
 * Adicionar o grupo de tag <ide> dentro do grupo <infCte>
   oInfCTe.Ide = oIde
   
 * Criar grupo de tag <emit>
   oEmit = CreateObject("Unimake.Business.DFe.Xml.CTe.Emit")
   oEmit.CNPJ = "31905001000109"
   oEmit.IE = "9079649730"
   oEmit.XNome = "EXATUS MOVEIS EIRELI"
   oEmit.XFant = "EXATUS MOVEIS"
   
 * Criar o grupo de tag <enderEmit>
   oEnderEmit = CreateObject("Unimake.Business.DFe.Xml.CTe.EnderEmit")
   oEnderEmit.XLgr = "RUA JOAQUIM F. DE SOUZA"
   oEnderEmit.Nro = "01112"
   oEnderEmit.XBairro = "VILA TEREZINHA"
   oEnderEmit.CMun = 4118402
   oEnderEmit.XMun = "PARANAVAI"
   oEnderEmit.CEP = "87706675"
   oEnderEmit.UF = 41 && UFBrasil.PR
   oEnderEmit.Fone = "04434237530"
   
 * Adicionar o grupo de tag <enderEmit> dentro do grupo <emit>
   oEmit.EnderEmit = oEnderEmit
   
 * Adicionar o grupo de tag <emit> dentro do grupo <infCte>
   oInfCTe.Emit = oEmit  

 * Criar grupo de tag <rem>
   oRem = CreateObject("Unimake.Business.DFe.Xml.CTe.Rem")
   oRem.CNPJ = "10197843000183"
   oRem.IE = "9044791606"
   oRem.XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oRem.XFant = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oRem.Fone = "04434225480"
   
 * Criar o grupo de tag <enderReme>
   oEnderReme = CreateObject("Unimake.Business.DFe.Xml.CTe.EnderReme")
   oEnderReme.XLgr = "RUA JOAQUIM F. DE SOUZA"
   oEnderReme.Nro = "01112"
   oEnderReme.XBairro = "VILA TEREZINHA"
   oEnderReme.CMun = 4118402
   oEnderReme.XMun = "PARANAVAI"
   oEnderReme.CEP = "87706675"
   oEnderReme.UF = 41 && UFBrasil.PR
   oEnderReme.CPais = 1058
   oEnderReme.XPais = "BRASIL"
   
 * Adicionar o grupo de tag <enderReme> dentro do grupo <rem>
   oRem.EnderReme = oEnderReme
   
 * Adicionar o grupo de tag <rem> dentro do grupo <infCte>
   oInfCTe.Rem = oRem

 * Criar grupo de tag <dest>
   oDest = CreateObject("Unimake.Business.DFe.Xml.CTe.Dest")
   oDest.CNPJ = "00000000075108"
   oDest.IE = "ISENTO"
   oDest.XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   
 * Criar o grupo de tag <enderDest>
   oEnderDest = CreateObject("Unimake.Business.DFe.Xml.CTe.EnderDest")
   oEnderDest.XLgr = "R. GESSYR GONCALVES FONTES, 55"
   oEnderDest.Nro = "55"
   oEnderDest.XBairro = "CENTRO"
   oEnderDest.CMun = 3305109
   oEnderDest.XMun = "SAO JOAO DE MERITI"
   oEnderDest.CEP = "25520570"
   oEnderDest.UF = 33 && UFBrasil.RJ
   oEnderDest.CPais = 1058
   oEnderDest.XPais = "BRASIL"
   
 * Adicionar o grupo de tag <enderDest> dentro do grupo <dest>
   oDest.EnderDest = oEnderDest
   
 * Adicionar o grupo de tag <dest> dentro do grupo <infCte>
   oInfCTe.Dest = oDest
   
 * Criar grupo de tag <vPrest>
   oVPrest = CreateObject("Unimake.Business.DFe.Xml.CTe.VPrest")
   oVPrest.VTPrest = 50.00
   oVPrest.VRec = 50.00
   
 * Criar grupo de tag <Comp>
   oComp = CreateObject("Unimake.Business.DFe.Xml.CTe.Comp")
   oComp.XNome = "FRETE VALOR"
   oComp.VComp = 50.00
   
 * Adicionar o grupo de tag <Comp> dentro do grupo <vPrest>
   oVPrest.AddComp(oComp)
   
 * Criar grupo de tag <Comp>
   oComp = CreateObject("Unimake.Business.DFe.Xml.CTe.Comp")
   oComp.XNome = "FRETE XALOR"
   oComp.VComp = 51.00
   
 * Adicionar o grupo de tag <Comp> dentro do grupo <vPrest>
   oVPrest.AddComp(oComp)
    
 * Adicionar o grupo de tag <vPrest> dentro do grupo <infCte>
   oInfCTe.VPrest = oVPrest
   
 * Criar grupo de tag <imp>
   oImp = CreateObject("Unimake.Business.DFe.Xml.CTe.Imp")
   
 * Criar grupo de tag <ICMS>
   oICMS = CreateObject("Unimake.Business.DFe.Xml.CTe.ICMS")
   
 * Criar grupo de tag <ICMSSN>
   oICMSSN = CreateObject("Unimake.Business.DFe.Xml.CTe.ICMSSN")
   oICMSSN.CST = "90"
   oICMSSN.IndSN = 1 && SimNao.Sim

 * Adicionar o grupo de tag <ICMSSN> dentro do grupo <ICMS>
   oICMS.ICMSSN = oICMSSN
   
 * Adicionar o grupo de tag <ICMS> dentro do grupo <imp>
   oIMP.ICMS = oICMS

 * Adicionar o grupo de tag <imp> dentro do grupo <infCte>
   oInfCTe.Imp = oImp
   
 * Criar grupo de tag <infCTeNorm>
   oInfCTeNorm = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCTeNorm")
   
 * Criar grupo de tag <infCarga>
   oInfCarga = CreateObject("Unimake.Business.DFe.Xml.CTe.InfCarga")
   oInfCarga.VCarga = 6252.96
   oInfCarga.ProPred = "MOVEIS"
   
 * Criar grupo de tag <infQ>
   oInfQ = CreateObject("Unimake.Business.DFe.Xml.CTe.InfQ")
   oInfQ.CUnid = 1 && CodigoUnidadeMedidaCTe.KG,
   oInfQ.TpMed ="PESO BRUTO"
   oInfQ.QCarga = 320.0000
   
 * Adicionar o grupo de tag <infQ> dentro do grupo <infCarga>             
   oInfCarga.AddInfQ(oInfQ)
   
 * Criar grupo de tag <infQ>
   oInfQ = CreateObject("Unimake.Business.DFe.Xml.CTe.InfQ")
   oInfQ.CUnid = 3 && CodigoUnidadeMedidaCTe.UNIDADE,
   oInfQ.TpMed ="UNIDADE"
   oInfQ.QCarga = 1.0000
   
 * Adicionar o grupo de tag <infQ> dentro do grupo <infCarga>             
   oInfCarga.AddInfQ(oInfQ)
   
 * Adicionar o grupo de tag <infCarga> dentro do grupo <infCTeNorm>             
   oInfCTeNorm.infCarga = oInfCarga
   
 * Criar grupo de tag <infDoc>
   oInfDoc = CreateObject("Unimake.Business.DFe.Xml.CTe.InfDoc")
   
 * Criar grupo de tag <infNFe>
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.CTe.InfNFe")
   oInfNFe.Chave = "41200306117473000150550030000652511417023254"
   
 * Adicionar o grupo de tag <infNFe> dentro do grupo <infDoc>
   oInfDoc.AddInfNFe(oInfNFe)
   
 * Adicionar o grupo de tag <infDoc> dentro do grupo <infCTeNorm>
   oInfCTeNorm.InfDoc = oInfDoc
   
 * Criar grupo de tag <infModal>
   oInfModal = CreateObject("Unimake.Business.DFe.Xml.CTe.InfModal")
   oInfModal.VersaoModal="3.00"
   
 * Criar grupo de tag <rodo>
   oRodo = CreateObject("Unimake.Business.DFe.Xml.CTe.Rodo")
   oRodo.RNTRC = "44957333"
   
 * Criar grupo de tag <occ>
   oOcc = CreateObject("Unimake.Business.DFe.Xml.CTe.Occ")
   oOcc.NOcc = 810
   oOcc.DEmi = DateTime()
   
 * Criar grupo de tag <emiOcc>
   oEmiOcc = CreateObject("Unimake.Business.DFe.Xml.CTe.EmiOcc")
   oEmiOcc.CNPJ = "31905001000109"
   oEmiOcc.CInt = "0000001067"
   oEmiOcc.IE = "9079649730"
   oEmiOcc.UF = 41 && UFBrasil.PR
   oEmiOcc.Fone = "04434237530"
   
 * Adicionar o grupo de tag <emiOcc> dentro do grupo <occ>
   oOcc.EmiOcc = oEmiOcc
        
 * Adicionar o grupo de tag <occ> dentro do grupo <rodo>
   oRodo.AddOcc(oOcc)
   
 * Adicionar o grupo de tag <rodo> dentro do grupo <infModal>
   oInfModal.Rodo = oRodo
   
 * Adicionar o grupo de tag <infModal> dentro do grupo <infCTeNorm>
   oInfCTeNorm.InfModal = oInfModal
   
 * Adicionar o grupo de tag <infCTeNorm> dentro do grupo <infCte>
   oInfCte.InfCTeNorm = oInfCTeNorm
   
 * Criar grupo de tag <infRespTec>
   oInfRespTec = CreateObject("Unimake.Business.DFe.Xml.CTe.InfRespTec")
   oInfRespTec.CNPJ = "06117473000150"
   oInfRespTec.XContato = "teste teste teste"
   oInfRespTec.Email= "teste@teste.com.br"
   oInfRespTec.Fone = "04431414900"
   
 * Adicionar o grupo de tag <infRespTec> dentro do grupo <infCte>
   oInfCTe.InfRespTec = oInfRespTec
   
 * Adicionar o grupo de tag <infCte> dentro do grupo <CTe>
   oCTe.InfCTe = oInfCTe   

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")   
  
   MESSAGEBOX("CNPJ Emitente: " + oCTe.InfCTe.Emit.CNPJ)
   MESSAGEBOX("Razao Emitente: " + oCTe.InfCTe.Emit.XNome)
   MESSAGEBOX("Chave do CTe: " + oCTe.InfCTe.Chave)

   TRY
    * Criar o objeto para consumir o serviço de autorização do CTe
      oAutorizacaoSinc = CreateObject("Unimake.Business.DFe.Servicos.CTe.AutorizacaoSinc")
      oAutorizacaoSinc.SetXMLConfiguracao(oCTe, oConfiguracao)
   
    * O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
    * caso seja necessário. Imagine que de um problema no envio do CTe e vc precise resgatar para enviar novamente.
	  MESSAGEBOX(oAutorizacaoSinc.GetConteudoCTeAssinado(0))
	  
	* Gravar o XML assinado na pasta  
      DELETE FILE 'd:\testenfe\' + oCTe.InfCTe.Chave + '-cte.xml'
      StrToFile(oAutorizacaoSinc.GetConteudoCTeAssinado(0), 'd:\testenfe\' + oCTe.InfCTe.Chave + '-cte.xml', 0)

    * Enviar o XML
      oAutorizacaoSinc.Executar(oCTe, oConfiguracao)

    * Demonstrar o XML retornado pela SEFAZ
      MESSAGEBOX(oAutorizacaoSinc.RetornoWSString)
   
	  If .NOT. ISNULL(oAutorizacaoSinc.Result)
	     MESSAGEBOX("Status envio: " + ALLTRIM(STR(oAutorizacaoSinc.Result.CStat,4)) + " - " + oAutorizacaoSinc.Result.XMotivo)

         If oAutorizacaoSinc.Result.CStat == 104 && Lote processado
            MESSAGEBOX("Status do CTe enviado: " + ALLTRIM(STR(oAutorizacaoSinc.Result.ProtCTe.InfProt.CStat,4)) + " - " + oAutorizacaoSinc.Result.ProtCTe.InfProt.XMotivo)
            If oAutorizacaoSinc.Result.ProtCTe.InfProt.CStat == 100 && CTe autorizado
               MESSAGEBOX("Protocolo de autorizacao: " + oAutorizacaoSinc.Result.ProtCTe.InfProt.NProt)

             * Gravar o XML de distribuição do CTe no HD, se preferir assim.
  	           oAutorizacaoSinc.GravarXmlDistribuicao("d:\testenfe")
			 
			 * Pegar a string do XML de distribuição para gravar na base de dados, se preferir assim.  
			   stringXmlDistribuicao = oAutorizacaoSinc.GetCTeProcResults(oCTe.InfCTe.Chave)
			Else   
             * Rejeitado, fazer devidos tratamentos
			Endif
		 Else
             * Rejeitado, fazer devidos tratamentos
		 Endif		 
	  Endif   
	     
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY
RETURN

