* ---------------------------------------------------------------------------------------------------------------
* Imprimir DANFE NFe e NFCe, DANFE NFe Simplificado, DANFE NFe Etiqueta, DACTE, DAMDFe, Eventos, etc.
* Se você optar por informar onde fica a pasta com as tabelas de configurações do UNIDANFE, deve informar para 
* todos os comandos.
*
* Lista de parametros/propriedades que podem ser utilizadas:
* https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
* ---------------------------------------------------------------------------------------------------------------

FUNCTION ImprimirDFePastaConfig
   PRIVATE oUnidanfeConfiguration, cPastaConfig, oUnidanfeServices 
     
   * Impressão via DLL
    
   * Criar as configurações 
     oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")   
     oUnidanfeConfiguration.WaitProcess = .F.
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml" 
     oUnidanfeConfiguration.Visualizar = .T.
     oUnidanfeConfiguration.Imprimir = .F.
     oUnidanfeConfiguration.EnviaEmail = .F.    
	 
	 * Não informe a subpasta dados, pois isso a DLL já sabe. Pasta que deve ser informada é exatamente a que tem a subpasta "dados".
	 *   Forma correta: "C:\Unimake\Unimake.UniDANFe\Unimake_EXE" 
	 *   Forma errada: "C:\Unimake\Unimake.UniDANFe\Unimake_EXE\dados" 
	 cPastaConfig = "C:\Unimake\Unimake.UniDANFe\Unimake_EXE" 
	 
	 oUnidanfeConfiguration.PastaConfiguracao = cPastaConfig
     
   * Disparar a impressao DANFe NFe
     oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
     oUnidanfeServices.Execute(oUnidanfeConfiguration)
	 MESSAGEBOX("Aguarde!") 
	 
   * Observe que todos os comandos a seguir passa por parâmetro a pasta de configuração, pois todos tem que seguir um padrão para não pegar configurações diferentes. Repare que o nome dos métodos muda quando tem o parâmetro.	 

   * Executar tela de configuração passando onde fica a pasta de configurações
     oUnidanfeServices.ShowConfigurationScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")
     
   * Executar tela de gerenciamento de emails
     oUnidanfeServices.ShowEmailScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")              
		 
   * Executar tela de gerenciamento de licenças 
     oUnidanfeServices.ShowLicencaScreenPastaConfig(cPastaConfig)
     MESSAGEBOX("Aguarde!")     
RETURN