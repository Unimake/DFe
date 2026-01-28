* ---------------------------------------------------------------------------------
* EFD-Reinf - Consultar Lote Assincrono
* ---------------------------------------------------------------------------------
Function EFDConsultarLoteAssincrono()
   LOCAL oConfig
   LOCAL oErro, oExceptionInterop
   LOCAL oReinfConsultaLoteAssincrono
   LOCAL oConsultaLoteAssincrono
   
 * Criar configuração básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.TipoDFe = 11 && 11=EFDReinf
   oConfig.CertificadoArquivo = "C:\Projetos\Unimake_PV.pfx"
   oConfig.CertificadoSenha = "12345678"
   oConfig.TipoAmbiente = 1 && Homologação
   oConfig.Servico = 66 && Servico.EFDReinfConsultaLoteAssincrono

 * Criar XML   
   oReinfConsultaLoteAssincrono = CREATEOBJECT("Unimake.Business.DFe.Xml.EFDReinf.ReinfConsultaLoteAssincrono")
   oReinfConsultaLoteAssincrono.Versao = "1.05.01"
   oReinfConsultaLoteAssincrono.ConsultaLoteAssincrono = CREATEOBJECT("Unimake.Business.DFe.Xml.EFDReinf.ConsultaLoteAssincrono")
   oReinfConsultaLoteAssincrono.ConsultaLoteAssincrono.NumeroProtocolo = "1.202601.797994138"
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   TRY   
    * Consumir o serviço
      oConsultaLoteAssincrono = CREATEOBJECT("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono")
      oConsultaLoteAssincrono.Executar(oReinfConsultaLoteAssincrono, oConfig)

	* String do XML retornado pela receita
	  MESSAGEBOX(oConsultaLoteAssincrono.RetornoWSString)   

    * Salvar o conteúdo do XML retornado da receita
      DELETE FILE 'd:\testenfe\EFDReinfRetornoProtocolo2055_retorno.xml'
      StrToFile(oConsultaLoteAssincrono.RetornoWSString, 'd:\testenfe\EFDReinfRetornoProtocolo2055_retorno.xml', 0)   
      
      IF oConsultaLoteAssincrono.Result.RetornoLoteEventosAssincrono.Status.CdResposta = 2 &&Lote processado com sucesso
         oConsultaLoteAssincrono.GravarXmlDistribuicao("d:\testenfe")
      ENDIF
	  
      MESSAGEBOX("Terminou")   
	  
    Catch To oErro
    * Exceção do CSHARP
      MessageBox(oExceptionInterop.GetMessage())
      MessageBox(oExceptionInterop.GetErrorCode())
      
    * Exceção do FOXPRO
	* Mais sobre exceção em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox(oErro.ErrorNo)
	  MessageBox("Exceção foxpro: " + oErro.Message)
	  
      DELETE FILE 'd:\testenfe\erroReinf.err'
      StrToFile(oErro.Message, 'd:\testenfe\erroReinf.err', 0)	  
   EndTry   
Return