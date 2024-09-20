* ---------------------------------------------------------------------------------
* Enviar Nfe de forma síncrona - Deserializando XML
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2221Desserializando()
   Local oConfiguracao
   Local xmlString
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe           := 12 // 12 = eSocial
   oConfiguracao:Servico           := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo:= "D:\projetos\unimake_pv.pfx"
   oConfiguracao:CertificadoSenha  := "1234"
   oConfiguracao:TipoAmbiente      := 2  // TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
	  xmlString:= [<?xml version="1.0" encoding="utf-8"?>] + ;
                      [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtToxic/v_S_01_02_00">] + ;
                        [<evtToxic Id="ID1475922250000002024091211312700001">] + ; 
                          [<ideEvento>] + ;
                            [<indRetif>1</indRetif>] + ; 
                            [<tpAmb>2</tpAmb>] + ;
                            [<procEmi>1</procEmi>] + ; 
                            [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                          [</ideEvento>] + ;
                          [<ideEmpregador>] + ;
                            [<tpInsc>1</tpInsc>] + ;
                            [<nrInsc>47592225</nrInsc>] + ;
                          [</ideEmpregador>] + ;
                          [<ideVinculo>] + ;
                            [<cpfTrab>32495144896</cpfTrab>] + ; 
                            [<matricula>52</matricula>] + ;
                          [</ideVinculo>] + ;
                          [<toxicologico>] + ;
                            [<dtExame>2024-09-02</dtExame>] + ; 
                            [<cnpjLab>51509164000180</cnpjLab>] + ;
                            [<codSeqExame>BB222222222</codSeqExame>] + ;
                            [<nmMed>Fulano de Tal</nmMed>] + ;
                            [<nrCRM>59327</nrCRM>] + ; 
                            [<ufCRM>SP</ufCRM>] + ; 
                          [</toxicologico>] + ;
                        [</evtToxic>] + ; 
                        [<evtToxic Id="ID1475922250000002024091211312700002">] + ;
                          [<ideEvento>] + ; 
                            [<indRetif>1</indRetif>] + ;
                            [<tpAmb>2</tpAmb>] + ;
                            [<procEmi>1</procEmi>] + ;
                            [<verProc>SGOWIN_Versao24091</verProc>] + ; 
                          [</ideEvento>] + ;
                          [<ideEmpregador>] + ;
                            [<tpInsc>1</tpInsc>] + ;
                            [<nrInsc>47592225</nrInsc>] + ; 
                          [</ideEmpregador>] + ;
                          [<ideVinculo>] + ; 
                            [<cpfTrab>22321798858</cpfTrab>] + ; 
                            [<matricula>66</matricula>] + ;
                          [</ideVinculo>] + ;
                          [<toxicologico>] + ;
                            [<dtExame>2024-09-01</dtExame>] + ;
                            [<cnpjLab>49881147000127</cnpjLab>] + ; 
                            [<codSeqExame>AA111111111</codSeqExame>] + ;
                            [<nmMed>Sicrano de Tal</nmMed>] + ;
                            [<nrCRM>0075866</nrCRM>] + ; 
                            [<ufCRM>SP</ufCRM>] + ;
                          [</toxicologico>] + ;
                        [</evtToxic>] + ;
                      [</eSocial>]

	  ? "String do XML:"
	  ?
	  ?
	  ? xmlString
	  ?
	  ?
	  Wait
	  Cls 
 /*
   
 * Desserializar o XML no objeto a partir do arquivo no HD  // não existe
*  oEventoS2221 = oEventoS2221:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\Xmls2221.xml") 

 * Desserializar o objeto a partir de uma string
   oEventoS2221 = oEventoS2221:LoadFromXML(xmlString) // não existe
 
*/
 
         oEnviarLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Xml.ESocial.EnviarLoteEventosESocial")
         oEnviarLoteEventosESocial:Executar(xmlString, oConfiguracao)
	  ? oEnviarLoteEventosESocial:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
   Catch oErro
      * Demonstrar exceções geradas no proprio Harbour, se existir.
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
	  
      * Demonstrar a exceção do CSHARP
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
      Wait
      Cls   
   End
Return

