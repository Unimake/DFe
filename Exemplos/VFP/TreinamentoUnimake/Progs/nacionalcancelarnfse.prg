* ------------------------------------------------------------------
* Cancelar NFSe - Padrão NACIONAL
* ------------------------------------------------------------------
FUNCTION NACIONALCancelarNFSe()
   LOCAL oConfiguracao
   LOCAL oPedRegEvento
   LOCAL oRecepcionarEvento
   LOCAL oExceptionInterop
   LOCAL cXML
   LOCAL oErro

* Criar objeto para pegar exceção do lado do C#
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")

   TRY

   * Criar objeto de configuração mínima
      oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe            = 5          && 5 = NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
      oConfiguracao.CertificadoSenha   = "12345678"
      oConfiguracao.CodigoMunicipio    = 1001058    && Padrão Nacional
      oConfiguracao.TipoAmbiente       = 2          && Homologação
      oConfiguracao.Servico            = 91         && NFSeRecepcionarEventosDiversos
      oConfiguracao.SchemaVersao       = "1.01"

   * Criar o XML do evento de cancelamento da NFSe
      oPedRegEvento = CreateObject("Unimake.Business.DFe.Xml.NFSe.NACIONAL.PedRegEvento")
      oPedRegEvento.Versao = "1.01"

      oPedRegEvento.InfPedReg = CreateObject("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfPedReg")
      oPedRegEvento.InfPedReg.TpAmb     = 2
      oPedRegEvento.InfPedReg.VerAplic  = "Teste1"
      oPedRegEvento.InfPedReg.DhEvento  = DATETIME()
      oPedRegEvento.InfPedReg.CNPJAutor = "01761135000132"
      oPedRegEvento.InfPedReg.ChNFSe    = "14001591201761135000132000000000000022096100197260"

      oPedRegEvento.InfPedReg.E101101 = CreateObject("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E101101")
      oPedRegEvento.InfPedReg.E101101.XDesc    = "Cancelamento de NFS-e"
      oPedRegEvento.InfPedReg.E101101.CMotivo  = 1   && CodigoJustificativaCancelamento.ErroNaEmissao
      oPedRegEvento.InfPedReg.E101101.XMotivo  = "Teste de cancelamento da NFSe Nacional"

   * Gerar XML
      cXML = oPedRegEvento.GerarXMLString()

      MESSAGEBOX(cXML)

   * Gravar XML gerado
      DELETE FILE "d:\testenfe\CancelamentoNFSeNacional.xml"
      STRTOFILE(cXML, "d:\testenfe\CancelamentoNFSeNacional.xml", 0)

   * Criar serviço de recepção do evento
      oRecepcionarEvento = CreateObject("Unimake.Business.DFe.Servicos.NFSe.RecepcionarEvento")

   * Executar serviço
      oRecepcionarEvento.Executar(cXML, oConfiguracao)

      MESSAGEBOX("XML retornado pelo Ambiente Nacional:" + CHR(13)+CHR(10) + ;
                 oRecepcionarEvento.RetornoWSString)

   * Gravar retorno
      DELETE FILE "d:\testenfe\CancelamentoNFSeNacional_retorno.xml"
      STRTOFILE(oRecepcionarEvento.RetornoWSString, ;
                "d:\testenfe\CancelamentoNFSeNacional_retorno.xml", 0)

   * Verificar se o evento foi homologado
      IF VARTYPE(oRecepcionarEvento.Result) = "O"

         oRecepcionarEvento.GravarXmlDistribuicao( ;
            "d:\testenfe", ;
            oRecepcionarEvento.Result.InfNFSe.Id + "-procEventoNFSe.xml", ;
            oRecepcionarEvento.RetornoWSString)

      ELSE

         IF VARTYPE(oRecepcionarEvento.ResultErro) = "O"
            IF VARTYPE(oRecepcionarEvento.ResultErro.Erro) = "O"
               MESSAGEBOX( ;
                  oRecepcionarEvento.ResultErro.Erro.Descricao + ;
                  " - " + ;
                  TRANSFORM(oRecepcionarEvento.ResultErro.Erro.Codigo))
            ENDIF
         ENDIF

      ENDIF

   CATCH TO oErro

   * Excecao do FOXPRO
      MESSAGEBOX("FOXPRO - ErrorCode: " + ;
                 ALLTRIM(STR(oErro.ErrorNo,10)) + ;
                 " - Message: " + oErro.Message)

   * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ;
                 ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + ;
                 " - Message: " + ;
                 oExceptionInterop.GetMessage())

   ENDTRY

RETURN
