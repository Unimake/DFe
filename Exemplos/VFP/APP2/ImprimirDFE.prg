* ---------------------------------------------------------------------------------
* Unimake Software
*
* Imprimir Documento Fiscal usando DLL do UniDANFE
* 
* Data: 17/02/2022 
* Autores: Edson Mundin Ferreira
*          Wandrey Mundin Ferreira
* ---------------------------------------------------------------------------------

* Configuracoes
  Config = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
  Config.Arquivo = "C:\Projetos\testes\xHb\Unidanfe.Dll\modelo VBS\41101201761135000132550010000000015187640532-procNFe.xml"
  Config.NomePDF = "C:\Projetos\testes\xHb\Unidanfe.Dll\modelo VBS\qq.pdf"
  Config.Imprimir = .F.
  Config.Visualizar = .T.

* Executar Impressao
  Dfe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
  Dfe.Execute(Config)
