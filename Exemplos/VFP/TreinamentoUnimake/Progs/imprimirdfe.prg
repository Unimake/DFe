* ---------------------------------------------------------------------------------------------------------------
* Imprimir DANFE NFe e NFCe, DANFE NFe Simplificado, DANFE NFe Etiqueta, DACTE, DAMDFe, Eventos, etc.
*
* Lista de parametros/propriedades que podem ser utilizadas:
* https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
* ---------------------------------------------------------------------------------------------------------------


FUNCTION ImprimirDFe
   DECLARE INTEGER ShellExecute IN shell32.dll ;
      INTEGER hndWin, ;
      STRING cAção, ;
      STRING cNomeArquivo, ;
      STRING cParams, ;  
      STRING cDir, ;
      INTEGER nShowWin
     
   * Impressão via DLL
    
   * Criar as configurações 
     oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")   
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml" 
     oUnidanfeConfiguration.Visualizar = .T.
     oUnidanfeConfiguration.Imprimir = .F.
     oUnidanfeConfiguration.EnviaEmail = .F.    
     
   * Disparar a impressao DANFe NFe
     oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
     oUnidanfeServices.Execute(oUnidanfeConfiguration)  
     MESSAGEBOX("Aguarde!") 
     
   * Disparar a impressao DANFe NFCe
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220706117473000150650010000580151230845952-procnfe.xml"
     
     * Comprovante TEF 1
     oUnidanfeConfiguration.AddComprovanteTEF("D:\testenfe\foxpro\unimake.dfe\testeComprovanteTEF1.txt")     

     * Comprovante TEF 2
     oUnidanfeConfiguration.AddComprovanteTEF("D:\testenfe\foxpro\unimake.dfe\testeComprovanteTEF2.txt")     
     
     * Imprimir o DANFE NFCe com 2 comprovantes de TEF, pode ter até 9
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")
     
   * Disparar a impressao DACTE
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\Backup\Autorizados\202012\41201200000000000000570010000001111001111111-procCTe.xml"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")
     
   * Disparar a impressao DACTEOS
     oUnidanfeConfiguration.Arquivo = "C:\projetos\uninfe\exemplos\CTe 3.00\CTeOS\41170899999999999999670010000000131000000041-cte.xml"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")     
          
   * Disparar a impressao DAMDFe
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41201280568835000181580010000010401406004659-procMDFe.xml"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")   
    
     
   * Disparar a impressao CFe/SAT
     oUnidanfeConfiguration.Arquivo = "C:\projetos\uninfe\exemplos\SAT\SATAutorizado-sat.xml"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")    
     
   * Disparar a impressao de um EVENTO
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\35221046105508000155550000000051471230408814_110111_01-proceventonfe.xml"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)     
     MESSAGEBOX("Aguarde!")

   * Disparar a impressao DANFe NFe Simplificado
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
     oUnidanfeConfiguration.Configuracao = "DANFE_SIMPL"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)
     MESSAGEBOX("Aguarde!")
     
   * Disparar a impressao DANFe NFe Etiqueta
     oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
     oUnidanfeConfiguration.Configuracao = "DANFE_ETIQ"
     oUnidanfeServices.Execute(oUnidanfeConfiguration)
     MESSAGEBOX("Aguarde!")     
     
   * Gerar somente o PDF do DANFe em uma pasta sem visualização ou impressão
     oUnidanfeConfiguration.Arquivo = "\\192.168.0.250\app\adm\NFe\Emp0001_Fil002_Envio\Enviadas\Autorizados\202202\41220206117473000150550010000717301715751703-procNFe.xml"
     oUnidanfeConfiguration.NomePDF = "D:\testenfe\pdf\41220206117473000150550010000717301715751703.pdf"
     oUnidanfeConfiguration.Visualizar = .F.
     oUnidanfeConfiguration.Imprimir = .F.
     
     oUnidanfeServices.Execute(oUnidanfeConfiguration) 
     MESSAGEBOX("Aguarde!")     

   * Executar tela de configuração
     oUnidanfeServices.ShowConfigurationScreen()     
     MESSAGEBOX("Aguarde!")
     
   * Executar tela de gerenciamento de emails
     oUnidanfeServices.ShowEmailScreen()
     MESSAGEBOX("Aguarde!")              
		 
   * Executar tela de gerenciamento de licenças 
     oUnidanfeServices.ShowLicencaScreen()
     MESSAGEBOX("Aguarde!")     

   * Somente executar o UniDANFe.EXE
     ShellExecute(0, "open", "D:\testenfe\unidanfe\unidanfe.exe", "", "D:\testenfe\unidanfe", 0)
     MESSAGEBOX("Aguarde!")  
     
   * Impressão via Unidanfe.EXE   
     ShellExecute(0, "open", "D:\testenfe\unidanfe\unidanfe.exe", "A=D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml", "D:\testenfe\unidanfe", 0)
     MESSAGEBOX("Aguarde!") 
RETURN