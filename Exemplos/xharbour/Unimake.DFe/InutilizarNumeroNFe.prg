* ---------------------------------------------------------------------------------
* Inutilizar números da NF-e
* ---------------------------------------------------------------------------------
Function InutilizarNumeroNfe()
   Local InicializarConfiguracao
   Local InutNFe
   Local InutNFeInfInut
   Local inutilizacao

 * Criar configuraçao básica para consumir o serviço
   InicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   InicializarConfiguracao:TipoDfe = 0 // 0=nfe
   InicializarConfiguracao:CertificadoSenha = "12345678"
   InicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar XML
   InutNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFe")
   InutNFe:Versao = "4.00"
   
   InutNFeInfInut = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")
   InutNFeInfInut:TpAmb  = 2  // Homologação
   InutNFeInfInut:Ano = "19"
   InutNFeInfInut:CNPJ = "06117473000150"
   InutNFeInfInut:CUF = 41 // PR
   InutNFeInfInut:Mod = 55 // Modelo NFe
   InutNFeInfInut:NNFIni = 57919
   InutNFeInfInut:NNFFin = 57919
   InutNFeInfInut:Serie = 1
   InutNFeInfInut:XJust = "Justificativa da inutilizacao de teste"   
   InutNfe:InfInut = InutNFeInfInut

 * Consumir o serviço
   inutilizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Inutilizacao")
   inutilizacao:Executar(InutNFe,InicializarConfiguracao)

   ? "XML Retornado pela SEFAZ"
   ? "========================"
   ? inutilizacao:RetornoWSString
   ?
   ? "Codigo de Status e Motivo"
   ? "========================="
   ? AllTrim(Str(inutilizacao:Result:InfInut:CStat,5)),inutilizacao:Result:InfInut:XMotivo
   ?
   Wait
Return

