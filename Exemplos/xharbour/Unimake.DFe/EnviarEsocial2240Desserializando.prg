* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2240
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2240Desserializando()
   Local oConfiguracao
   Local xmlString
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe           := 12 // 12 = eSocial
   oConfiguracao:Servico           := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo:= "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha  := "12345678"
   oConfiguracao:TipoAmbiente      := 2  // TipoAmbiente.Homologação

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      xmlString:= [<?xml version="1.0" encoding="utf-8"?>] + ;
                  [<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">] + ;
                    [<envioLoteEventos grupo="2">] + ;
                      [<ideEmpregador>] + ;
                        [<tpInsc>1</tpInsc>] + ;
                          [<nrInsc>99999999</nrInsc>] + ;
                            [</ideEmpregador>] + ;
                              [<ideTransmissor>] + ;
                                [<tpInsc>1</tpInsc>] + ;
                                [<nrInsc>99999999999999</nrInsc>] + ;
                              [</ideTransmissor>] + ;
                              [<eventos>] + ;
                                [<evento Id="ID1999999990000002024090421022000001">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00">] + ;
                                    [<evtExpRisco Id="ID1999999990000002024091310242800001">] + ;
                                      [<ideEvento>] + ;
                                       [<indRetif>1</indRetif>] + ;
                                       [<tpAmb>2</tpAmb>] + ;
                                       [<procEmi>1</procEmi>] + ;
                                       [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                      [</ideEvento>] + ;
                                      [<ideEmpregador>] + ;
                                       [<tpInsc>1</tpInsc>] + ;
                                       [<nrInsc>12345678</nrInsc>] + ;
                                      [</ideEmpregador>] + ;
                                      [<ideVinculo>] + ;
                                       [<cpfTrab>22222222222</cpfTrab>] + ;
                                       [<matricula>66</matricula>] + ;
                                      [</ideVinculo>] + ;
                                      [<infoExpRisco>] + ;
                                       [<dtIniCondicao>2022-08-08</dtIniCondicao>] + ;
                                       [<infoAmb>] + ;
                                         [<localAmb>1</localAmb>] + ;
                                         [<dscSetor>OPERACIONAL</dscSetor>] + ;
                                         [<tpInsc>1</tpInsc>] + ;
                                         [<nrInsc>12345678000148</nrInsc>] + ;
                                       [</infoAmb>] + ;
                                       [<infoAtiv>] + ;
                                         [<dscAtivDes>Prestar assistencia farmaceutica orientar os pacientes quanto aos exames realizar exames analisar amostras de materiais biologicos verificar resultados das analises</dscAtivDes>] + ;
                                       [</infoAtiv>] + ;
                                       [<agNoc>] + ;
                                         [<codAgNoc>03.01.001</codAgNoc>] + ;
                                         [<tpAval>2</tpAval>] + ;
                                         [<epcEpi>] + ;
                                           [<utilizEPC>0</utilizEPC>] + ;
                                           [<utilizEPI>2</utilizEPI>] + ;
                                           [<eficEpi>S</eficEpi>] + ;
                                           [<epi>] + ;
                                             [<docAval>27785</docAval>] + ;
                                           [</epi>] + ;
                                           [<epi>] + ;
                                             [<docAval>9149</docAval>] + ;
                                           [</epi>] + ;
                                           [<epiCompl>] + ;
                                             [<medProtecao>S</medProtecao>] + ;
                                             [<condFuncto>S</condFuncto>] + ;
                                             [<usoInint>S</usoInint>] + ;
                                             [<przValid>S</przValid>] + ;
                                             [<periodicTroca>S</periodicTroca>] + ;
                                             [<higienizacao>S</higienizacao>] + ;
                                           [</epiCompl>] + ;
                                         [</epcEpi>] + ;
                                       [</agNoc>] + ;
                                       [<respReg>] + ;
                                         [<cpfResp>11111111111</cpfResp>] + ;
                                         [<ideOC>9</ideOC>] + ;
                                         [<dscOC>MTE</dscOC>] + ;
                                         [<nrOC>0131369</nrOC>] + ;
                                         [<ufOC>SP</ufOC>] + ;
                                       [</respReg>] + ;
                                      [</infoExpRisco>] + ;
                                    [</evtExpRisco>] + ;
                                  [</eSocial>] + ;
                                [</evento>] + ;
                                [<evento Id="ID1999999990000002024090421022000002">] + ;
                                  [<eSocial xmlns="http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00">] + ;
                                    [<evtExpRisco Id="ID1999999990000002024091310242800002">] + ;
                                      [<ideEvento>] + ;
                                       [<indRetif>1</indRetif>] + ;
                                       [<tpAmb>2</tpAmb>] + ;
                                       [<procEmi>1</procEmi>] + ;
                                       [<verProc>SGOWIN_Versao24091</verProc>] + ;
                                      [</ideEvento>] + ;
                                      [<ideEmpregador>] + ;
                                       [<tpInsc>1</tpInsc>] + ;
                                       [<nrInsc>12345678</nrInsc>] + ;
                                      [</ideEmpregador>] + ;
                                      [<ideVinculo>] + ;
                                       [<cpfTrab>22222222111</cpfTrab>] + ;
                                       [<matricula>67</matricula>] + ;
                                      [</ideVinculo>] + ;
                                      [<infoExpRisco>] + ;
                                       [<dtIniCondicao>2022-08-08</dtIniCondicao>] + ;
                                       [<infoAmb>] + ;
                                         [<localAmb>1</localAmb>] + ;
                                         [<dscSetor>ADMINISTRATIVO</dscSetor>] + ;
                                         [<tpInsc>1</tpInsc>] + ;
                                         [<nrInsc>12345678000148</nrInsc>] + ;
                                       [</infoAmb>] + ;
                                       [<infoAtiv>] + ;
                                         [<dscAtivDes>Executar servicos de apoio na area administrativa preparar e preencher fichas cadastrar pacientes encaminhar pacientes para realizacao de exames fazer digitacao de laudos</dscAtivDes>] + ;
                                       [</infoAtiv>] + ;
                                       [<agNoc>] + ;
                                         [<codAgNoc>09.01.001</codAgNoc>] + ;
                                       [</agNoc>] + ;
                                       [<respReg>] + ;
                                         [<cpfResp>11111111111</cpfResp>] + ;
                                         [<ideOC>9</ideOC>] + ;
                                         [<dscOC>MTE</dscOC>] + ;
                                         [<nrOC>0131369</nrOC>] + ;
                                         [<ufOC>SP</ufOC>] + ;
                                       [</respReg>] + ;
                                      [</infoExpRisco>] + ;
                                    [</evtExpRisco>] + ;
                                  [</eSocial>] + ;
                                [</evento>] + ;
                              [</eventos>] + ;
                    [</envioLoteEventos>] + ;
                  [</eSocial>]

	  ? "String do XML:"
	  ?
	  ?
	  ? xmlString
	  ?
	  ?
	  Wait
	  Cls 
      ? "String do XML:"
      ?
      ?
      ? xmlString
      ?
      ?

      nomeArqLoteEvento:= "D:\enzza\unimake_esocial\risco_teste-esocial-loteevt.xml"
      hb_MemoWrit(nomeArqLoteEvento, xmlString)

      Wait
      Cls 
	
    * Criar objeto do lote de eventos
      oESocialEnvioLoteEventos:= CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")
	
    * Desserializar o XML de lote de eventos a partir do arquivo gravado no HD	 
*      oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromFile("D:\enzza\unimake_esocial\risco_teste-esocial-loteevt.xml")
	
    * Desserializar o XML de lote de eventos a partir da string do XML
      //oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromXML("D:\enzza\unimake_esocial\risco_teste-esocial-loteevt.xml")
      oESocialEnvioLoteEventos:= oESocialEnvioLoteEventos:LoadFromXML(xmlString)

    * Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
      oEnviarLoteEventosESocial:= CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")
	
    * Consumir o serviço	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)
      
      stringXMLLoteAssinado:= oEnviarLoteEventosESocial:GetConteudoXMLAssinado()
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\enzza\unimake_esocial\xmlloteeventosassinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\enzza\unimake_esocial\xmlloteeventos-risco-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)
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

