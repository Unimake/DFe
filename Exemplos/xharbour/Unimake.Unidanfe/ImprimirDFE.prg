* ---------------------------------------------------------------------------------
* Imprimir Documento Fiscal
* ---------------------------------------------------------------------------------
Function ImprimirDFE()
   Local Dfe
   Local Config
   
 * Configuracoes
   Config = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
   Config:Arquivo = ".\41101201761135000132550010000000015187640532-procNFe.xml"
   Config:Imprimir = .F.
   Config:Visualizar = .T.

 * Executar Impressao
   Dfe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
   Dfe:Execute(Config)

   Wait
RETURN NIL