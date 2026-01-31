* ---------------------------------------------------------------------------------
* EFD-Reinf - Consulta recibo evento
* ---------------------------------------------------------------------------------
Function EFDConsultaReciboEvento()
   LOCAL oConfig
   LOCAL oErro, oExceptionInterop
   LOCAL oReinfConsulta
   LOCAL oConsultaReciboEvento
   LOCAL I, oRegOcorrsRetorno
   
 * Criar configuração básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig.TipoDFe = 11 && 11=EFDReinf
   oConfig.CertificadoArquivo = "C:\Projetos\Unimake_PV.pfx"
   oConfig.CertificadoSenha = "12345678"
   oConfig.TipoAmbiente = 1 && Homologação
   oConfig.Servico = 65 && Servico.EFDReinfConsultaReciboEvento

 * Criar XML   
   oReinfConsulta = CREATEOBJECT("Unimake.Business.DFe.Xml.EFDReinf.ReinfConsulta")
   oReinfConsulta.Versao = "1.05.01"
   oReinfConsulta.ConsultaReciboEvento = CREATEOBJECT("Unimake.Business.DFe.Xml.EFDReinf.ConsultaReciboEvento")
   oReinfConsulta.ConsultaReciboEvento.TipoEvento = "1000"
   oReinfConsulta.ConsultaReciboEvento.TpInsc = 1 && CNPJ
   oReinfConsulta.ConsultaReciboEvento.NrInsc = "00000000"
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   TRY   
    * Consumir o serviço
      oConsultaReciboEvento= CREATEOBJECT("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento")
      oConsultaReciboEvento.Executar(oReinfConsulta, oConfig)

	* String do XML retornado pela receita
	  MESSAGEBOX(oConsultaReciboEvento.RetornoWSString)   

    * Salvar o conteúdo do XML retornado da receita
      DELETE FILE 'd:\testenfe\EFDReinfConsultaEvento_retorno.xml'
      StrToFile(oConsultaReciboEvento.RetornoWSString, 'd:\testenfe\EFDReinfConsultaEvento_retorno.xml', 0)      
      
      IF oConsultaReciboEvento.Result.IdeStatus.CdRetorno = "0" &&Nenhum evento encontrado
         MESSAGEBOX(oConsultaReciboEvento.Result.IdeStatus.CdRetorno + " - " + oConsultaReciboEvento.Result.IdeStatus.DescRetorno)
      ELSE 
         IF oConsultaReciboEvento.Result.IdeStatus.CdRetorno = "3" &&Erro
            MESSAGEBOX(oConsultaReciboEvento.Result.IdeStatus.CdRetorno + " - " + oConsultaReciboEvento.Result.IdeStatus.DescRetorno)
            
            && Se tiver ocorrencias vamos analsiar elas?
            IF oConsultaReciboEvento.Result.IdeStatus.GetRegOcorrsCount() > 0
               MESSAGEBOX("Tem ocorrencias")
               FOR I = 1 TO oConsultaReciboEvento.Result.IdeStatus.GetRegOcorrsCount()
                   oRegOcorrsRetorno = oConsultaReciboEvento.Result.IdeStatus.GetRegOcorrs(I-1)
                   
                   MESSAGEBOX("Ocorrencia número: " + ALLTRIM(STR(I,10)))
                   
                   MESSAGEBOX("Tipo da Ocorrencia: " + ALLTRIM(STR(oRegOcorrsRetorno.TpOcorr,10)))
                   
                   IF oRegOcorrsRetorno.LocalErroAviso != ""
                      MESSAGEBOX("Local Erro Aviso: " + oRegOcorrsRetorno.LocalErroAviso)
                   ENDIF
                   
                   IF oRegOcorrsRetorno.CodResp != ""
                      MESSAGEBOX("Código da resposta: " + oRegOcorrsRetorno.CodResp)
                   ENDIF
                   
                   MESSAGEBOX("Descrição da resposta: " + oRegOcorrsRetorno.DscResp)
               NEXT I            
            ENDIF
         ELSE
            IF oConsultaReciboEvento.Result.IdeStatus.CdRetorno = "1" &&Um ou mais eventos encontrados
               MESSAGEBOX(oConsultaReciboEvento.Result.IdeStatus.CdRetorno + " - " + oConsultaReciboEvento.Result.IdeStatus.DescRetorno)               
               
               && Tem recibo de eventos? Vamos analisar eles?
               IF oConsultaReciboEvento.Result.RetornoEventos.GetEventoCount() > 0
                  FOR I = 1 TO oConsultaReciboEvento.Result.RetornoEventos.GetEventoCount()
                      oEvento = oConsultaReciboEvento.Result.RetornoEventos.GetEvento(I-1)
                      
                      MESSAGEBOX("Recibo de evento: " + ALLTRIM(STR(I,10)))
                      
                      MESSAGEBOX("Data hora do recebimento: " + oEvento.DtHoraRecebimentoField)
                      
                      IF oEvento.NrProtocolo != ""
                         MESSAGEBOX("Número do protocolo: " + oEvento.NrProtocolo)
                      ENDIF
                      
                      IF oEvento.NrRecibo != ""
                         MESSAGEBOX("Número do recibo: " + oEvento.NrRecibo)
                      ENDIF
                      
                      IF oEvento.NrProc != ""
                         MESSAGEBOX("NrProc: " + oEvento.NrProc)
                      ENDIF
                      
                      MESSAGEBOX("Situação do evento: " + ALLTRIM(STR(oEvento.SituacaoEvento,10)))
                      MESSAGEBOX("Tipo Processamento: " + ALLTRIM(STR(oEvento.TpProc,10)))
                      MESSAGEBOX("Número Processamento: " + oEvento.NrProc)
                      MESSAGEBOX("Início da validade: " + oEvento.IniValidField)
                      MESSAGEBOX("Fim da validade: " + oEvento.FimValidField)                      
                  NEXT I
               ENDIF   
            ELSE
               MESSAGEBOX("Status retornado não reconhecido")
            ENDIF  
         ENDIF    
      ENDIF
	  
      MESSAGEBOX("Terminou")   
	  
    Catch To oErro
    * Exceção do CSHARP
      MessageBox(oExceptionInterop.GetMessage())
      MessageBox(oExceptionInterop.GetErrorCode())
      
    * Exceção do FOXPRO
	* Mais sobre exceção em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox(oErro.ErrorNo)
	  MessageBox("Exceção foxpro: " + oErro.Message)
	  
      DELETE FILE 'd:\testenfe\erroReinf.err'
      StrToFile(oErro.Message, 'd:\testenfe\erroReinf.err', 0)	  
   EndTry   
Return