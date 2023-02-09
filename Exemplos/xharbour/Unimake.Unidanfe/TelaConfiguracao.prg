* ---------------------------------------------------------------------------------
* Carregar tela de configuração do UniDANFE
* ---------------------------------------------------------------------------------
Function TelaConfiguracao()
   Local TelaConfig
  
 * Criar objeto
   TelaConfig = CreateObject("Unimake.Unidanfe.UnidanfeServices")
   
 * Abrir tela de configuração  
   TelaConfig:ShowConfigurationScreen()
   
   Wait
RETURN nil