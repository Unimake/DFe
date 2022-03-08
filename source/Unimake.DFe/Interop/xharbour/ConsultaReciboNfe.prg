* ---------------------------------------------------------------------------------
* Consumindo o serviço de consulta recibo da NFe
* ---------------------------------------------------------------------------------
Function ConsultaReciboNfe()
   Local InicializarConfiguracao
   Local ConsReciNFe
   Local retAutorizacao

 * Criar configuraçao básica para consumir o serviço
   InicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   InicializarConfiguracao:TipoDfe = 0 // 0=nfe
   InicializarConfiguracao:Servico = 2 // 2=Consulta Recibo
   InicializarConfiguracao:CertificadoSenha = "12345678"
   InicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

  * Criar XML
   ConsReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
   ConsReciNFe:Versao = "4.00"
   ConsReciNFe:TpAmb  = 2  // Homologação
   ConsReciNFe:NRec = "411234567890123" // número do recibo
   
  * Consumir o serviço
    retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
    retAutorizacao:Executar(ConsReciNFe,InicializarConfiguracao)

   ? "XML Retornado pela SEFAZ"
   ? "========================"
   ? retAutorizacao:RetornoWSString
   ?
   ? "Codigo de Status e Motivo"
   ? "========================="
   ? AllTrim(Str(retAutorizacao:Result:CStat,5)),retAutorizacao:Result:XMotivo
   ?
   Wait
Return

