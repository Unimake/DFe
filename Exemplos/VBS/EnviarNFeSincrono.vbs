Dim enviNFe
Dim autorizacao
Dim config
Dim consReciNFe
Dim retAutorizacao
Dim ProtNFe

'----------------------------------------------------
'Definir configurações básicas
'----------------------------------------------------
Set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
config.TipoDfe = 0
config.CertificadoArquivo = "d:\projetos\UnimakePV.pfx"
config.CertificadoSenha = "12345678"

'----------------------------------------------------
'Montar o XML da NFe
'----------------------------------------------------
Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")

enviNFe.Versao = "4.00"
enviNFe.IdLote = "000000000000001"
enviNFe.IndSinc = 0
enviNFe.AddNFe (GetFromFileNFe())

MsgBox "1) Carregou o XML"

'----------------------------------------------------
'Enviar a NFe
'----------------------------------------------------
Set autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
autorizacao.SetXMLConfiguracao (enviNFe), (config)

MsgBox "2) Setou o XML e Config" 
MsgBox "3) XML assinado" + autorizacao.GetConteudoNFeAssinada(0)

autorizacao.Executar (enviNFe), (config)

MsgBox "4) Enviou o XML" 

'----------------------------------------------------
'Consultar recibo
'----------------------------------------------------
Set consReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
consReciNFe.Versao = "4.00"
consReciNFe.TpAmb = 1
consReciNFe.NRec = "411001723503442" 'autorizacao.Result.InfRec.NRec
MsgBox autorizacao.Result.InfRec.NRec

Set retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
retAutorizacao.Executar (consReciNFe), (config)

if retAutorizacao.Result.CStat = 104 Then
   if retAutorizacao.Result.GetProtNFeCount > 0 Then
      Set protNFe = retAutorizacao.Result.GetProtNFe(0)
      MsgBox protNFe.InfProt.CStat 'Verificar se é status de nota autorizada ou não
      MsgBox protNFe.InfProt.XMotivo 'Motivo se autorizado ou rejeitado
   Else 
      MsgBox "Algo ocorreu na consulta recibo que não retornou o protocolo da nota, talvez tenha consultado um recibo que não existe."
   End if   
else   
   MsgBox retAutorizacao.Result.CStat + " " + retAutorizacao.Result.XMotivo
End if   

MsgBox retAutorizacao.Result.GetProtNFe

'----------------------------------------------------
'Serializar o XML da NFe
'----------------------------------------------------
Function GetFromFileNFe()
   Dim NFe: Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   Set GetFromFileNFe = NFe.LoadFromFile("C:\Users\Wandrey\Downloads\Telegram Desktop\35220564968613000108550010001348351512572804-nfe.xml")
End Function
