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
config.CertificadoSenha = "12345678"
config.CertificadoArquivo = "d:\projetos\UnimakePV.pfx"
config.TipoDfe = 0
config.TipoAmbiente = 2

'----------------------------------------------------
'Montar o XML da NFe
'----------------------------------------------------
Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")

enviNFe.Versao = "4.00"
enviNFe.IdLote = "000000000000001"
enviNFe.IndSinc = 0
enviNFe.AddNFe (GetFromFileNFe("C:\Users\Wandrey\Downloads\qqq\41220206117473000150550010000715971203756054-nfe.xml"))
enviNFe.AddNFe (GetFromFileNFe("C:\Users\Wandrey\Downloads\qqq\41220206117473000150550010000715981503756073-nfe.xml"))

'----------------------------------------------------
'Enviar a NFe
'----------------------------------------------------
'Set autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
'autorizacao.Executar (enviNFe), (config)

'MsgBox "1) " + autorizacao.RetornoWSString

'******** Preciso resolver o cara abaixo
'MsgBox "1) " + autorizacao.ConteudoXMLAssinado.OuterXml

'----------------------------------------------------
'Consultar recibo
'----------------------------------------------------
Set consReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
consReciNFe.Versao = "4.00"
consReciNFe.TpAmb = 2
consReciNFe.NRec = "411110221029758" 'autorizacao.Result.InfRec.NRec '411110221029758
'MsgBox "2) " + autorizacao.Result.InfRec.NRec

Set retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")
retAutorizacao.Executar (consReciNFe), (config)

MsgBox "3) " + retAutorizacao.RetornoWSString

if retAutorizacao.Result.CStat = 104 Then
   if retAutorizacao.Result.GetProtNFeCount > 0 Then
      Set protNFe = retAutorizacao.Result.GetProtNFe(0)
      MsgBox protNFe.InfProt.CStat 'Verificar se é status de nota autorizada ou não
      MsgBox protNFe.InfProt.XMotivo 'Motivo se autorizado ou rejeitado

      Set protNFe = retAutorizacao.Result.GetProtNFe(1)
      MsgBox protNFe.InfProt.CStat 'Verificar se é status de nota autorizada ou não
      MsgBox protNFe.InfProt.XMotivo 'Motivo se autorizado ou rejeitado
   Else 
      MsgBox "Algo ocorreu na consulta recibo que não retornou o protocolo da nota, talvez tenha consultado um recibo que não existe."
   End if   
else   
   MsgBox retAutorizacao.Result.CStat + " " + retAutorizacao.Result.XMotivo
End if   

'MsgBox retAutorizacao.Result.GetProtNFe

'----------------------------------------------------
'Serializar o XML da NFe
'----------------------------------------------------
Function GetFromFileNFe(arqXML)
   Dim NFe: Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   Set GetFromFileNFe = NFe.LoadFromFile(arqXML)
End Function
