* ---------------------------------------------------------------------------------------------------------------
* Imprimir DANFE NFe e NFCe, DANFE NFe Simplificado, DANFE NFe Etiqueta, DACTE, DAMDFe, Eventos, etc.
* Se voc� optar por informar onde fica a pasta com as tabelas de configura��es do UNIDANFE, deve informar para 
* todos os comandos.
*
* Lista de parametros/propriedades que podem ser utilizadas:
* https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
* ---------------------------------------------------------------------------------------------------------------

FUNCTION ImprimirDFePastaConfig
   PRIVATE oUnidanfeConfiguration, cPastaConfig, oUnidanfeServices 
     
   * Impress�o via DLL
    
   * Criar as configura��es 
     oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")   
     oUnidanfeConfiguration.WaitProcess = .F.
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml" 
     oUnidanfeConfiguration.Visualizar = .T.
     oUnidanfeConfiguration.Imprimir = .F.
     oUnidanfeConfiguration.EnviaEmail = .F.    
	 
	 * N�o informe a subpasta dados, pois isso a DLL j� sabe. Pasta que deve ser informada � exatamente a que tem a subpasta "dados".
	 *   Forma correta: "C:\Unimake\Unimake.UniDANFe\Unimake_EXE" 
	 *   Forma errada: "C:\Unimake\Unimake.UniDANFe\Unimake_EXE\dados" 
	 cPastaConfig = "C:\Unimake\Unimake.UniDANFe\Unimake_EXE" 
	 
	 oUnidanfeConfiguration.PastaConfiguracao = cPastaConfig
     
   * Disparar a impressao DANFe NFe
     oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
     oUnidanfeServices.Execute(oUnidanfeConfiguration)
	 MESSAGEBOX("Aguarde!") 
	 
   * Observe que todos os comandos a seguir passa por par�metro a pasta de configura��o, pois todos tem que seguir um padr�o para n�o pegar configura��es diferentes. Repare que o nome dos m�todos muda quando tem o par�metro.	 

   * Executar tela de configura��o passando onde fica a pasta de configura��es
     oUnidanfeServices.ShowConfigurationScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")
     
   * Executar tela de gerenciamento de emails
     oUnidanfeServices.ShowEmailScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")              
		 
   * Executar tela de gerenciamento de licen�as 
     oUnidanfeServices.ShowLicencaScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")     
RETURN