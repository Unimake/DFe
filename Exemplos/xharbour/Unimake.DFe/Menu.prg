/*

Unimake Software
Exemplo de uso da DLL Unimake.Dfe
Data: 15/02/2022 
Autores: 
- Edson Mundin Ferreira
- Wandrey Mundin Ferreira

*/
FUNCTION Main()
   Local aOpcoes, aAcoes, nOpcao

   aOpcoes := {}
   aAcoes := {}

   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar Status NFe", {|| ConsultaStatusNfe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar Situacao NFe", {|| ConsultaSituacaoNfe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFe - Modo sincrono", {|| EnviarNfeSincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFe - Modo assincrono", {|| EnviarNfeAssincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFe - Desserializando o XML", {|| EnviarNfeDeserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Testes diversos com certificado digital no xHarbour pago ou free (BBC)", {|| TesteDiversoCertificado()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de Cancelamento da NFe", {|| CancelarNFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de Cancelamento da NFe - Desserializando o XML", {|| CancelarNFeDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Gerar XML de distribuicao com um nome diferente do padrao da DLL", {|| GerarXmlDistribuicaoNomeDif()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar Status MDFe", {|| ConsultaStatusMDFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar MDFe - Modo assincrono", {|| EnviarMDFeAssincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar MDFe - Modo assincrono - Desserializando o XML", {|| EnviarMDFeAssincronoDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFCe - Modo sincrono", {|| EnviarNfceSincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFCe - Modo sincrono - Desserializando o XML", {|| EnviarNFCeSincronoDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar MDFe - Modo sincrono", {|| EnviarMDFeSincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Validar XML", {|| ValidarXML()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de Cancelamento da NFCe", {|| CancelarNFCe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar GTIN", {|| ConsultarGTIN()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Imprimir DANFe/DACTe/DAMDFe via DLL UniDANFE", {|| ImprimirDANFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Executar telas do UniDANFE", {|| ExecutarTelaUniDANFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de cancelamento do MDFe", {|| CancelarMDFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de encerramento do MDFe", {|| EncerramentoMDFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Finalizar a nota pela consulta situacao da NFe", {|| FinalizarNFePelaConsultaSituacao()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Gerando a NFCe em contingencia OffLine", {|| EnviarNFCeSincronoOffline()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar CTe - Modo Assincrono", {|| EnviarCTeAssincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - PM Sao Paulo - Enviar Lote RPS - Assincrono", {|| EnviarLoteRPSAssincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - PM Sao Paulo - Enviar Consulta Lote RPS", {|| EnviarConsultaLoteRPS()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - PM Sao Paulo - Enviar Cancelamento da NFSe", {|| EnviarCancelamentoNFSe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar NFe em contingencia SVC-AN e SVC-RS", {|| EnviarNFeContigenciaSVC()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar CTe em contingencia SVC-SP e SVC-RS", {|| EnviarCTeContigenciaSVC()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Como encriptar a tag <Assinatura> NFSe Sao Paulo", {|| EncriptarAssinaturaSP()})
   AddOpcaoMenu(aOpcoes, aAcoes, "EPEC NFe - Gerar XML NFe em contingencia EPEC", {|| EPECGerarXMLNFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "EPEC NFe - Enviar Evento de EPEC da NFe", {|| EPECEnviarEventoEPEC()})
   AddOpcaoMenu(aOpcoes, aAcoes, "EPEC NFe - Enviar o XML da NFe", {|| EPECEnviarXMLNFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar CTe - Modo Assincrono - Desserializando o XML", {|| EnviarCTeAssincronoDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar MDFe - Modo sincrono - Desserializando o XML", {|| EnviarMDFeSincronoDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Gerar XML da NFSe", {|| GerarXmlNFSe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de Cancelamento do CTe", {|| EnviarCancCTe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de Cancelamento do CTe - Desserializando XML", {|| EnviarCancCTeDesserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Desserializar XML NFe compra p/dar entrada no ERP (B2B)", {|| DesserializarXMLNFeCompra()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Testes diversos com certificado digital no Harbour 3.x", {|| TesteDiversoCertificadoHarbour3x()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar XML de Inutilizacao do CTe", {|| EnviarInutCTe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar XML de Inutilizacao do CTe - Com Desserializacao", {|| EnviarInutCTeDesserializacao()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar Evento de CCE da NFe", {|| EnviarEventoCCeNFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Enviar CTe - Modo Assincrono - Desserializando o XML", {|| EnviarCTeAssincronoDesserializando2()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consulta de documentos fiscais eletronicos destinados", {|| ConsultaDFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar Status CTe", {|| ConsultaStatusCTe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consultar URL NFSe", {|| ConsultarURLNfse()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Gerando XML de distribuicao Evento Canc NFe via consulta situacao", {|| GerarXmlDistribuicaoEvento()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Consulta MDFes nao encerrados", {|| ConsultaMDFeNaoEnc()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Obter a versao da DLL Unimake.DFe", {|| ExibirVersaoDLL()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Extrair eventos retornados na consulta situacao da NFe/NFCe", {|| ExtrairEventoPedSitNFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Verificar se a DLL esta instalada no PC", {|| VerificarDLLInstalada()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Consultar lote assincrono", {|| eSocialConsultaLoteAssincrono()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2210 - Enviar lote", {|| EnviarEsocial2210()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2210 - Enviar lote - XML desserializado", {|| EnviarEsocial2210Desserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2221 - Enviar lote", {|| EnviarEsocial2221()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1010 - Enviar lote", {|| EnviarEsocial1010()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2220 - Enviar lote", {|| EnviarEsocial2220()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1200 - Enviar lote", {|| EnviarEsocial1200()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2240 - Enviar lote", {|| EnviarEsocial2240()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1210 - Enviar lote", {|| EnviarEsocial1210()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2220 - Enviar lote - XML desserializado", {|| EnviarEsocial2220Desserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2200 - Enviar lote", {|| EnviarEsocial2200()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2230 - Enviar lote", {|| EnviarEsocial2230()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2299 - Enviar lote", {|| EnviarEsocial2299()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Download Eventos Por ID", {|| DownloadEventoESocialPorID()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Download Eventos Por Nr Recibo", {|| DownloadEventoESocialPorNRRec()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Consultar Eventos - Tabela", {|| ConsultarEvtsTabelaESocial()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Consultar Eventos - Trabalhador", {|| ConsultarEvtsTrabalhadorESocial()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Consultar Eventos - Empregador", {|| ConsultarEvtsEmpregadorESocial()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1200 - Enviar lote - XML desserializado", {|| EnviarEsocial1200Desserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1000 - Enviar lote", {|| EnviarEsocial1000()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 1000 - Enviar lote - XML desserializado", {|| EnviarEsocial1000Desserializando()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Assinar XML", {|| AssinarXML()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 2206 - Enviar lote", {|| EnviarEsocial2206()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - Nacional - Enviar NFSe", {|| NACIONALGerarNFSe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - Nacional - Consultar DPS", {|| NACIONALConsultarNFSeDPS()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - Nacional - Consultar NFSe", {|| NACIONALConsultarNFSe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "NFSe - Nacional - Consultar PDF", {|| NACIONALConsultarNFSePDF()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Inutilizar Numero NF-e", {|| InutilizarNumeroNfe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Inutilizar Numero NFC-e", {|| InutilizarNumeroNfce()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Imprimir NFS-e com UniDANFE", {|| ImprimirNFSeUniDANFe()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Executar tela de configuracao do UniDANFE", {|| UniDANFEConfiguracao()})
   AddOpcaoMenu(aOpcoes, aAcoes, "Executar tela de configuracao do UniDANFE", {|| UniDANFEConfiguracao()})
   AddOpcaoMenu(aOpcoes, aAcoes, "eSocial - Evento 3000 - Enviar lote", {|| EnviarEsocial3000()})

   Do While .T.
      Cls

      @ 1,2 Say "Unimake.Dfe DLL for " + Version()

      nOpcao := AChoice(3, 2, 30, 80, aOpcoes)

      Cls

      if LastKey() = 27
         Exit
      endif

      if nOpcao > 0 .and. nOpcao <= Len(aAcoes)
         Eval(aAcoes[nOpcao])
      endif
   EndDo
Return

Static Procedure AddOpcaoMenu(aOpcoes, aAcoes, cDescricao, bAcao)
   AAdd(aOpcoes, cDescricao)
   AAdd(aAcoes, bAcao)
Return

Static Procedure ExibirVersaoDLL()
   Local oInfoInterop

   oInfoInterop = CreateObject("Unimake.Business.DFe.Utility.InfoInterop")
   Cls

   ? oInfoInterop:VersaoDLL
   ?
   ?
   Wait

   Cls
Return
