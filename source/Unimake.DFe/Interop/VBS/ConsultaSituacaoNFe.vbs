Dim consSitNFe
Dim consultaProtocolo
Dim configuracao
Dim QtdeEventos
Dim procEventoNFe

'Montar o objeto de configuração com informações mínimas 
'para ser utilizado na hora de consumir o serviço
Set configuracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
configuracao.TipoDFe = 0 'NFe
configuracao.CertificadoSenha = "12345678"
configuracao.CertificadoArquivo = "D:\projetos\UnimakePV.pfx"

'Montar o XML de consulta status do serviço
Set consSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
consSitNFe.Versao = "4.00"
consSitNFe.TpAmb = 2 '1-Produção 2-Homologação
consSitNFe.ChNFe = "31221025975590000107550010000083851000841778"

'Consumir o serviço
Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
consultaProtocolo.Executar (consSitNFe),(configuracao)

'Demonstrar mensagens na tela com o retorno da SEFAZ
MsgBox consultaProtocolo.Result.CStat 'Recuperar o conteúdo da tag <cStat> retornada pela SEFAZ
MsgBox consultaProtocolo.Result.XMotivo 'Recuperar o conteúdo da tag <xMotivo> retornada pela SEFAZ

'Resgatar a quantidade de eventos retornados na consulta
QtdeEventos = consultaProtocolo.Result.GetProcEventoNFeCount 
MsgBox QtdeEventos

'Demonstrar mensagens com os dados dos eventos retornados
For i = 1 To QtdeEventos
   Set procEventoNFe = consultaProtocolo.Result.GetProcEventoNFe(i-1)

  'Se for evento de CCe
   if procEventoNFe.Evento.InfEvento.TpEvento = "110110" then 'Evento de CCe
      MsgBox "Chave da NFe do evento: " + procEventoNFe.Evento.InfEvento.ChNFe
	  MsgBox "Tipo do evento: " + CStr(procEventoNFe.Evento.InfEvento.TpEvento)
	  MsgBox "Protocolo de autorizacao do Evento de CCe: " + procEventoNFe.RetEvento.InfEvento.NProt
      MsgBox "CStat da CCe: " + CStr(procEventoNFe.RetEvento.InfEvento.CStat)
      MsgBox "XMotivo do cancelamento: " + procEventoNFe.RetEvento.InfEvento.XMotivo
      MsgBox "Data do registro do evento na SEFAZ: " + CStr(procEventoNFe.RetEvento.InfEvento.DhRegEvento)
      MsgBox "XEvento: " + procEventoNFe.RetEvento.InfEvento.XEvento       
   end if
Next
