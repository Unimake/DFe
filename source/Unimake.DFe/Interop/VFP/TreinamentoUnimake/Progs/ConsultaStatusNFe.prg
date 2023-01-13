* ---------------------------------------------------------------------------------
* Consumindo o serviço de consulta status da NFe
* ---------------------------------------------------------------------------------
Function ConsultaStatusNfe()
   Local oConfig
   Local oConsStatServ, oErro, oExceptionInterop
   Local oStatusServico
   
 * Criar configuração básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.TipoDFe = 0 && 0=NFe
   oConfig.Servico = 0 && 0=NFe Status Serviço
   oConfig.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig.CertificadoSenha = "12345678"
   
 * Criar XML   
   oConsStatServ = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsStatServ")
   oConsStatServ.Versao = "4.00"
   oConsStatServ.TpAmb = 2 && 2=Homologação
   oConsStatServ.CUF = 41 && 41=Paraná
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try
    * Consumir o serviço
      oStatusServico = CreateObject("Unimake.Business.DFe.Servicos.NFe.StatusServico")
	  oStatusServico.Executar(oConsStatServ, oConfig)

    * XML retornado pela SEFAZ
	  MessageBox(oStatusServico.RetornoWSString)
    
    * Código de Status e Motivo 	
	  MessageBox(Alltrim(Str(oStatusServico.Result.CStat,5))+" - "+oStatusServico.Result.XMotivo)
	  
    Catch To oErro
    * Exceção do FOXPRO
	  MessageBox(oErro.ErrorNo)
	  MessageBox(oErro.Message)
	  
    * Exceção do CSHARP
      MessageBox(oExceptionInterop.GetMessage())
      MessageBox(oExceptionInterop.GetErrorCode())
   EndTry   
Return