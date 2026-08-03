* ---------------------------------------------------------------------------------
* Gerar o XML do evento de comprovante de entrega da NFe e enviar para SEFAZ
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function EnviarEventoComprovanteEntregaNFe()
   Local oErro, oExceptionInterop
   Local oConfiguracao
   Local oEnvEvento, oEvento, oDetEventoCompEntregaNFe, oInfEvento

 * Criar configuracao basica para consumir o servico
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 0 // 0=NFe
   oConfiguracao:Servico = 5 // 5=Envio de evento
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar tag EnvEvento
   oEnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
   oEnvEvento:Versao = "1.00"
   oEnvEvento:IdLote = "000000000000001"

 * -------------------------------------------------
 * Criar tags do evento sequencia 1
 * -------------------------------------------------
 * Criar tag Evento
   oEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")
   oEvento:Versao = "1.00"

 * Criar tag DetEventoCompEntregaNFe
   oDetEventoCompEntregaNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCompEntregaNFe")
   oDetEventoCompEntregaNFe:Versao = "1.00"
   oDetEventoCompEntregaNFe:COrgaoAutor = 41 // UFBrasil.PR
   oDetEventoCompEntregaNFe:TpAutor = 1 // TipoAutor.EmpresaEmitente
   oDetEventoCompEntregaNFe:VerAplic = "ERP 1.0"
   oDetEventoCompEntregaNFe:DhEntrega = DateTime()
   oDetEventoCompEntregaNFe:NDoc = "00000000000" // Documento de quem assinou o comprovante
   oDetEventoCompEntregaNFe:XNome = "NOME DE QUEM ASSINOU O COMPROVANTE"
   oDetEventoCompEntregaNFe:LatGPS = "37.774929"
   oDetEventoCompEntregaNFe:LongGPS = "122.419418"
   oDetEventoCompEntregaNFe:HashComprovante = "2eDWGfx2xZJVFTKXGuiGZgzE2W4="
   oDetEventoCompEntregaNFe:DhHashComprovante = DateTime()

 * Criar tag InfEvento
   oInfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")

 * Adicionar a tag DetEventoCompEntregaNFe dentro da Tag DetEvento
   oInfEvento:DetEvento = oDetEventoCompEntregaNFe

 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualizacao da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCompEntregaNFe para que funcione sem erro
   oInfEvento:COrgao = 91 // UFBrasil.AN
   oInfEvento:ChNFe = "41191006117473000150550010000579281779843610"
   oInfEvento:CNPJ = "06117473000150"
   oInfEvento:DhEvento = DateTime()
   oInfEvento:TpEvento = 110130 // TipoEventoNFe.ComprovanteEntregaNFe
   oInfEvento:NSeqEvento = 1
   oInfEvento:VerEvento = "1.00"
   oInfEvento:TpAmb = 2 // TipoAmbiente.Homologacao

 * Adicionar a tag InfEvento dentro da tag Evento
   oEvento:InfEvento = oInfEvento

 * Adicionar a tag Evento dentro da tag EnvEvento
   oEnvEvento:AddEvento(oEvento)

 * Resgatar alguns dados do objeto do XML do evento
   ? oEnvEvento:Versao, oEnvEvento:IdLote
   ? "Qde eventos:", oEnvEvento:GetEventoCount()

   For I = 1 To oEnvEvento:GetEventoCount()
       oTagEvento := oEnvEvento:GetEvento(I - 1)
       ? I, oTagEvento:InfEvento:NSeqEvento, oTagEvento:InfEvento:COrgao
   Next I

   ?
   ?
   Wait

   // Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")

   Try
    * Enviar evento
      oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
      oRecepcaoEvento:Executar(oEnvEvento, oConfiguracao)

      eventoAssinado = oRecepcaoEvento:GetConteudoXMLAssinado()
      ? eventoAssinado

    * Gravar o XML assinado no HD, antes de enviar
      hb_MemoWrit("D:\testenfe\ComprovanteDeEntregaNFe.xml", eventoAssinado)

      ? "CStat do Lote Retornado:", oRecepcaoEvento:Result:CStat, "- XMotivo:", oRecepcaoEvento:Result:XMotivo

      If oRecepcaoEvento:Result:CStat == 128 // 128=Lote de evento processado com sucesso
       * Como pode existir varios eventos no XML, e necessario fazer um loop
       * para verificar a autorizacao de cada um deles
         For I = 1 To oRecepcaoEvento:Result:GetRetEventoCount()
             oRetEvento = oRecepcaoEvento:Result:GetRetEvento(I - 1)

             SWITCH oRetEvento:InfEvento:CStat
               CASE 135 // Evento homologado com vinculacao da respectiva NFe
               CASE 136 // Evento homologado sem vinculacao com a respectiva NFe
               CASE 155 // Evento homologado fora do prazo permitido
                    oRecepcaoEvento:GravarXmlDistribuicao("tmp\testenfe") // Grava o XML de distribuicao
                    oProcEventoNFe = oRecepcaoEvento:GetProcEventoNFeResult(0)
                    ? "Arquivo de distribuicao:", oProcEventoNFe:NomeArquivoDistribuicao
                    Exit

              #Ifdef __XHARBOUR__
               DEFAULT
              #Else
               OTHERWISE
              #endif
                    // Evento rejeitado
                    // Realizar as acoes necessarias
                    Exit
             END

             ? "CStat do evento", AllTrim(Str(I, 10)), "retornado:", oRetEvento:InfEvento:CStat, "- xMotivo:", oRetEvento:InfEvento:XMotivo
         Next
      EndIf
      ?
      ?
      Wait

   Catch oErro
      // Demonstrar excecoes geradas no proprio Harbour, se existir
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar enviar o evento de comprovante de entrega da NFe."
      ? oErro:Description
      ? oErro:Operation

      // Demonstrar a excecao do CSHARP
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?

      Wait
      Cls
   End
Return
