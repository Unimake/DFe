* ---------------------------------------------------------------------------------
* Enviar lote de eventos do eSocial - Evento 2206
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarEsocial2206()
   Local oConfiguracao, oExceptionInterop, oErro
   Local nomeArqLoteEvento, stringXMLLoteAssinado
   Local oESocialEnvioLoteEventos
   Local oEnviarLoteEventosESocial

 * Criar o objeto de configuração mínima
   oConfiguracao:= CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe           := 12 // 12 = eSocial
   oConfiguracao:Servico           := 69 // Servico.ESocialEnviarLoteEventos
   oConfiguracao:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente      := 2  // TipoAmbiente.Homologação

   oExceptionInterop := CreateObject("Unimake.Exceptions.ThrowHelper")   // Criar objeto para capturar exceções do lado C#

   Try         
      oESocialEnvioLoteEventos := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")   // Montando o XML do lote de eventos      

      oEnvioLoteEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EnvioLoteEventosESocial")   // Objeto principal do envio do lote de eventos
      oEnvioLoteEventosESocial:Grupo := "1"   // Grupo do lote: 1 = Eventos iniciais e de tabela
      
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")   // Identificação do empregador no lote
      oIdeEmpregador:tpInsc := 1   // Tipo de inscrição: 1 = CNPJ
      oIdeEmpregador:nrInsc := "00000000000000"   // Número de inscrição do empregador (CNPJ)
      oEnvioLoteEventosESocial:IdeEmpregador := oIdeEmpregador   // Atribuir o empregador ao lote
      
      oIdeTransmissor := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeTransmissor")   // Identificação do transmissor
      oIdeTransmissor:tpInsc := 1   // Tipo de inscrição do transmissor: 1 = CNPJ
      oIdeTransmissor:nrInsc := "00000000000000"   // Número de inscrição do transmissor (CNPJ)
      oEnvioLoteEventosESocial:IdeTransmissor := oIdeTransmissor   // Atribuir o transmissor ao lote
      
      oEventosESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventosESocial")   // Coleção de eventos do lote
      
      oEventoESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.EventoESocial")   // Objeto de um evento individual
      oEventoESocial:Id := "ID1000000000000002017102608080800001"   // Identificador único do evento
      
      oESocial2206 := CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocial2206")   // Objeto específico do evento S-2206
	 
      oEvtAltContratual := CreateObject("Unimake.Business.DFe.Xml.ESocial.EvtAltContratual")   // Dados do evento de alteração contratual
      oEvtAltContratual:Id := "ID1000000000000002017102608080800001"   // Identificador único do evento de alteração
	  
      oIdeEvento := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEvento2206")   // Identificação do evento
      oIdeEvento:indRetif := 1   // Indicador de retificação: 1 = Original
      oIdeEvento:nrRecibo := "1.1.0000000000000000001"   // Número do recibo do evento original (se retificação)
      oIdeEvento:TpAmb := 1   // Tipo de ambiente: 1 = Produção
      oIdeEvento:ProcEmi := 1   // Processo de emissão: 1 = Aplicativo do empregador
      oIdeEvento:VerProc := "str1234"   // Versão do processo/aplicativo
      oEvtAltContratual:IdeEvento := oIdeEvento   // Atribuir identificação ao evento
	  
      oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")   // Identificação do empregador no evento
      oIdeEmpregador:tpInsc := 1   // Tipo de inscrição: 1 = CNPJ
      oIdeEmpregador:nrInsc := "00000000000000"   // Número de inscrição do empregador (CNPJ)
      oEvtAltContratual:IdeEmpregador := oIdeEmpregador   // Atribuir empregador ao evento

      oIdeVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2206")   // Identificação do vínculo
      oIdeVinculo:cpfTrab := "12345678901"   // CPF do trabalhador
      oIdeVinculo:matricula := "str1234"   // Matrícula do empregado
      oEvtAltContratual:IdeVinculo := oIdeVinculo   // Atribuir vínculo ao evento

      oAltContratual := CreateObject("Unimake.Business.DFe.Xml.ESocial.AltContratual")   // Dados da alteração contratual
      oAltContratual:dtAlteracao := "2012-12-13"   // Data da alteração contratual
      oAltContratual:dtEf := "2012-12-13"   // Data de efetivação da alteração
      oAltContratual:dscAlt := "str1234"   // Descrição da alteração

      oVinculo := CreateObject("Unimake.Business.DFe.Xml.ESocial.Vinculo")   // Dados do vínculo após alteração
      oVinculo:tpRegPrev := 1   // Tipo de regime previdenciário: 1 = RGPS

      oInfoRegimeTrab := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoRegimeTrab")   // Informações do regime de trabalho
      oInfoCeletista := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoCeletista")   // Dados celetistas
      oInfoCeletista:tpRegJor := 1   // Tipo de regime de jornada: 1 = Submetido a horário
      oInfoCeletista:natAtividade := 1   // Natureza da atividade: 1 = Urbana
      oInfoCeletista:dtBase := 1   // Mês base para cálculo
      oInfoCeletista:cnpjSindCategProf := "12345678901234"   // CNPJ do sindicato da categoria

      oTrabTemporario := CreateObject("Unimake.Business.DFe.Xml.ESocial.TrabTemporario")   // Dados de trabalho temporário
      oTrabTemporario:justProrr := "str1234"   // Justificativa para prorrogação
      oInfoCeletista:TrabTemporario := oTrabTemporario   // Atribuir trabalho temporário ao regime celetista

      oAprend := CreateObject("Unimake.Business.DFe.Xml.ESocial.Aprend")   // Dados de aprendiz
      oAprend:indAprend := 1   // Indicador de aprendiz: 1 = Aprendiz
      oAprend:tpInsc := 1   // Tipo de inscrição da entidade: 1 = CNPJ
      oAprend:nrInsc := "00000000000000"   // Número de inscrição da entidade
      oInfoCeletista:Aprend := oAprend   // Atribuir aprendiz ao regime celetista

      oInfoEstatutario := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoEstatutario")   // Dados estatutários
      oInfoEstatutario:tpPlanRP := 0   // Tipo de plano de aposentadoria: 0 = Não se aplica
      oInfoEstatutario:indTetoRGPS := 1   // Indicador de teto do RGPS: S = Sim
      oInfoEstatutario:indAbonoPerm := 1   // Indicador de abono de permanência: S = Sim
      oInfoRegimeTrab:InfoEstatutario := oInfoEstatutario   // Atribuir dados estatutários ao regime de trabalho

      oInfoRegimeTrab:InfoCeletista := oInfoCeletista   // Atribuir regime celetista ao regime de trabalho
      oVinculo:InfoRegimeTrab := oInfoRegimeTrab   // Atribuir regime de trabalho ao vínculo

      oInfoContrato := CreateObject("Unimake.Business.DFe.Xml.ESocial.InfoContrato")   // Informações do contrato
      oInfoContrato:nmCargo := "str1234"   // Nome do cargo
      oInfoContrato:CBOCargo := "123456"   // Código CBO do cargo
      oInfoContrato:nmFuncao := "str1234"   // Nome da função
      oInfoContrato:CBOFuncao := "123456"   // Código CBO da função
      oInfoContrato:acumCargo := 1   // Acumulação de cargo: S = Sim
      oInfoContrato:codCateg := 101   // Código da categoria

      oRemuneracao := CreateObject("Unimake.Business.DFe.Xml.ESocial.Remuneracao2206")   // Dados da remuneração
      oRemuneracao:vrSalFx := 123.45   // Valor do salário fixo
      oRemuneracao:undSalFixo := 1   // Unidade do salário fixo: 1 = Mensal
      oRemuneracao:dscSalVar := "str1234"   // Descrição do salário variável
      oInfoContrato:Remuneracao := oRemuneracao   // Atribuir remuneração ao contrato

      oDuracao := CreateObject("Unimake.Business.DFe.Xml.ESocial.Duracao")   // Dados da duração do contrato
      oDuracao:tpContr := 1   // Tipo de contrato: 1 = Indeterminado
      oDuracao:dtTerm := "2012-12-13"   // Data de término (se determinado)
      oInfoContrato:Duracao := oDuracao   // Atribuir duração ao contrato

      oLocalTrabalho := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalTrabalho")   // Dados do local de trabalho
      oLocalTrabGeral := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2206")   // Local de trabalho geral
      oLocalTrabGeral:tpInsc := 1   // Tipo de inscrição: 1 = CNPJ
      oLocalTrabGeral:nrInsc := "00000000000000"   // Número de inscrição
      oLocalTrabGeral:descComp := "str1234"   // Descrição complementar
      oLocalTrabalho:LocalTrabGeral := oLocalTrabGeral   // Atribuir local geral ao local de trabalho

      oLocalTempDom := CreateObject("Unimake.Business.DFe.Xml.ESocial.LocalTempDom")   // Local temporário no domicílio
      oLocalTempDom:tpLograd := "A"   // Tipo de logradouro: A = Avenida
      oLocalTempDom:dscLograd := "str1234"   // Descrição do logradouro
      oLocalTempDom:nrLograd := "str1234"   // Número do logradouro
      oLocalTempDom:complemento := "str1234"   // Complemento do endereço
      oLocalTempDom:bairro := "str1234"   // Bairro
      oLocalTempDom:cep := "87707000"   // CEP
      oLocalTempDom:codMunic := "1234567"   // Código do município (IBGE)
      oLocalTempDom:uf := "AC"   // Unidade federativa
      oLocalTrabalho:LocalTempDom := oLocalTempDom   // Atribuir local temporário ao local de trabalho
      oInfoContrato:LocalTrabalho := oLocalTrabalho   // Atribuir local de trabalho ao contrato

      oHorContratual := CreateObject("Unimake.Business.DFe.Xml.ESocial.HorContratual")   // Dados da jornada contratual
      oHorContratual:qtdHrsSem := 0.12   // Quantidade de horas semanais
      oHorContratual:tpJornada := 2   // Tipo de jornada: 2 = Jornada 12x36
      oHorContratual:tmpParc := 1   // Tempo parcial: 1 = Sim
      oHorContratual:horNoturno := 1   // Horário noturno: S = Sim
      oHorContratual:dscJorn := "12312312312"   // Descrição da jornada
      oInfoContrato:HorContratual := oHorContratual   // Atribuir jornada ao contrato

      oAlvaraJudicial := CreateObject("Unimake.Business.DFe.Xml.ESocial.AlvaraJudicial")   // Dados de alvará judicial
      oAlvaraJudicial:nrProcJud := "nrProcJud__________1"   // Número do processo judicial
      oInfoContrato:AlvaraJudicial := oAlvaraJudicial   // Atribuir alvará ao contrato

      oObservacoes := CreateObject("Unimake.Business.DFe.Xml.ESocial.Observacoes2306")   // Observações do contrato
      oObservacoes:observacao := "str1234"   // Texto da observação
      oInfoContrato:AddObservacoes(oObservacoes)   // Adicionar observação ao contrato	  

      oVinculo:InfoContrato := oInfoContrato   // Atribuir informações do contrato ao vínculo
      oAltContratual:Vinculo := oVinculo   // Atribuir vínculo à alteração contratual
      oEvtAltContratual:AltContratual := oAltContratual   // Atribuir alteração contratual ao evento

      oESocial2206:EvtAltContratual := oEvtAltContratual   // Atribuir evento de alteração ao S-2206
      	  
      oEventoESocial:ESocial2206 := oESocial2206   // Adicionar evento ao lote
      oEventosESocial:AddEvento(oEventoESocial)   // Adicionar evento à coleção de eventos
      oEnvioLoteEventosESocial:Eventos := oEventosESocial   // Atribuir eventos ao lote
      oESocialEnvioLoteEventos:EnvioLoteEventos := oEnvioLoteEventosESocial   // Atribuir lote ao envio

      oEnviarLoteEventosESocial := CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")   // Criar objeto para consumir o serviço de envio
	
      oEnviarLoteEventosESocial:Executar(oESocialEnvioLoteEventos, oConfiguracao)   // Executar o serviço de envio
      
      stringXMLLoteAssinado := oEnviarLoteEventosESocial:GetConteudoXMLAssinado()   // Obter o XML assinado
      
      ? "StringXMLAssinado:", stringXMLLoteAssinado
      ?
      ?
      Wait
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos2206assinado.xml", stringXMLLoteAssinado)
	
      ? oEnviarLoteEventosESocial:RetornoWSString
      ?
      ?
      hb_MemoWrit("D:\testenfe\esocial\xmlloteeventos-2206-ret.xml", oEnviarLoteEventosESocial:RetornoWSString)   // Salvar retorno do serviço em arquivo
      Wait
      Cls

   Catch oErro
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar enviar o evento S-2206."
      ? oErro:Description
      ? oErro:Operation
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
      Wait
      Cls   
   End
Return (Nil)
