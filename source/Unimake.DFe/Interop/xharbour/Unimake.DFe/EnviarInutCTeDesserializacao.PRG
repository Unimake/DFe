* ---------------------------------------------------------------------------------
* Enviar xml de inutilização CTe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarInutCTeDesserializacao()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oInutCTe, oInfInut, oInutilizacao
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 2 // 0=CTe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   
 * Criar o grupo de tag <inutCTe>
   oInutCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.InutCTe")
*   oInutCTe = oInutCTe:LoadFromFile("D:\testenfe\teste-ped-inu.xml")
   
   stringXml = [<?xml version="1.0" encoding="utf-8"?><inutCTe versao="3.00" xmlns="http://www.portalfiscal.inf.br/cte"><infInut Id="ID310611747300015057001000057919000057919"><tpAmb>2</tpAmb><xServ>INUTILIZAR</xServ><cUF>31</cUF><ano>19</ano><CNPJ>06117473000150</CNPJ><mod>57</mod><serie>1</serie><nCTIni>57919</nCTIni><nCTFin>57919</nCTFin><xJust>Justificativa da inutilizacao de testexx</xJust></infInut></inutCTe>]
   oInutCTe = oInutCte:LoadFromXML(stringXML)

 * Resgatando alguns dados do objeto do XML de inutilização
   ? oInutCTe:Versao
   ? oInutCTe:InfInut:Ano
   ? oInutCTe:InfInut:CNPJ
   ? oInutCTe:InfInut:CUF
   ? oInutCTe:InfInut:XJust
   ?
   Wait   
   Cls 

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try 
    * Enviar evento
      oInutilizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.Inutilizacao")
      oInutilizacao:SetXMLConfiguracao(oInutCTe, oConfiguracao)
	  
	  cls
	  ? oInutilizacao:GetConteudoXMLAssinado()
	  ?
	  ?
	  ?
	  Wait
	  
	  cls
	  
	  oInutilizacao:Executar(oInutCTe, oConfiguracao)
	  
	  ? "CStat Retornado:", oInutilizacao:Result:InfInut:CStat
	  ? "XMotivo Retornado: ", oInutilizacao:Result:InfInut:XMotivo
	  ?
	  ?
	  Wait	
	  
	  Cls
	  
	  ? oInutilizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls

      SWITCH oInutilizacao:Result:InfInut:CStat
         CASE 102 //Inutilização Homologada
              oInutilizacao:GravarXmlDistribuicao("tmp\testenfe") //Grava o XML de distribuição
              Exit
   				 
        #Ifdef __XHARBOUR__
         DEFAULT
        #Else
         OTHERWISE    
        #endif
              // Inutilização rejeitada
              // Realizar as ações necessárias
              Exit
       END
	  
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