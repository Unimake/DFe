Dim consSitMDFe
Dim consultaProtocolo
Dim configuracao
Dim procEventoMDFe
Dim detEvento
Dim QtdeEventos
'Dim retConsSitMDFe

'Montar o objeto de configuração com informações mínimas 
'para ser utilizado na hora de consumir o serviço
Set configuracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
configuracao.TipoDFe = 4
configuracao.CertificadoSenha = "12345678"
configuracao.CertificadoArquivo = "D:\projetos\UnimakePV.pfx"

'Montar o XML de consulta status do serviço
Set consSitMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe")
consSitMDFe.Versao = "3.00"
consSitMDFe.TpAmb = 2 '1-Produção 2-Homologação
consSitMDFe.ChMDFe = "31220637920833000180580010000000111000000123"

'Consumir o serviço
Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.MDFe.ConsultaProtocolo")
consultaProtocolo.Executar (consSitMDFe),(configuracao)

'Demonstrar mensagens na tela com o retorno da SEFAZ
MsgBox consultaProtocolo.Result.CStat 'Recuperar o conteúdo da tag <cStat> retornada pela SEFAZ
MsgBox consultaProtocolo.Result.XMotivo 'Recuperar o conteúdo da tag <xMotivo> retornada pela SEFAZ

'Set retConsSitMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.RetConsSitMDFe")
'Set retConsSitMDFe = consultaProtocolo.Result

'Resgatar a quantidade de eventos retornados na consulta
QtdeEventos = consultaProtocolo.Result.GetProcEventoMDFeCount 
MsgBox QtdeEventos

'Demonstrar mensagens com os dados dos eventos retornados
For i = 1 To QtdeEventos
   Set procEventoMDFe = consultaProtocolo.Result.GetProcEventoMDFe(i-1)

   'Se for evento de encerramento    
   if procEventoMDFe.EventoMDFe.InfEvento.TpEvento = "110112" then 'Evento de encerramento
      MsgBox "Chave do MDFe do evento: " + procEventoMDFe.EventoMDFe.InfEvento.ChMDFe
	  MsgBox "Tipo do evento: " + CStr(procEventoMDFe.EventoMDFe.InfEvento.TpEvento)
	  MsgBox "Protocolo de autorização do Evento de Encerramento: " + procEventoMDFe.RetEventoMDFe.InfEvento.NProt
      MsgBox "Chave do MDFe encerrado: " + procEventoMDFe.RetEventoMDFe.InfEvento.ChMDFe
      MsgBox "CStat do encerramento: " + CStr(procEventoMDFe.RetEventoMDFe.InfEvento.CStat)
      MsgBox "XMotivo do encerramento: " + procEventoMDFe.RetEventoMDFe.InfEvento.XMotivo
      MsgBox "Data do registro do evento na SEFAZ: " + CStr(procEventoMDFe.RetEventoMDFe.InfEvento.DhRegEvento)
      MsgBox "XEvento: " + procEventoMDFe.RetEventoMDFe.InfEvento.XEvento
   end if	  
   
   'Se for evento de cancelamento
   if procEventoMDFe.EventoMDFe.InfEvento.TpEvento = "110111" then 'Evento de cancelamento
      MsgBox "Chave do MDFe do evento: " + procEventoMDFe.EventoMDFe.InfEvento.ChMDFe
	  MsgBox "Tipo do evento: " + CStr(procEventoMDFe.EventoMDFe.InfEvento.TpEvento)
	  MsgBox "Protocolo de autorização do Evento de Cancelamento: " + procEventoMDFe.RetEventoMDFe.InfEvento.NProt
      MsgBox "Chave do MDFe cancelado: " + procEventoMDFe.RetEventoMDFe.InfEvento.ChMDFe
      MsgBox "CStat do cancelamento: " + CStr(procEventoMDFe.RetEventoMDFe.InfEvento.CStat)
      MsgBox "XMotivo do cancelamento: " + procEventoMDFe.RetEventoMDFe.InfEvento.XMotivo
      MsgBox "Data do registro do evento na SEFAZ: " + CStr(procEventoMDFe.RetEventoMDFe.InfEvento.DhRegEvento)
      MsgBox "XEvento: " + procEventoMDFe.RetEventoMDFe.InfEvento.XEvento       
   end if
Next
