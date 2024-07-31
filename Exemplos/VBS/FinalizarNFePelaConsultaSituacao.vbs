Dim enviNFe
Dim autorizacao
Dim config
Dim consSitNFe
Dim consultaProtocolo

'----------------------------------------------------
'Definir configurações básicas
'----------------------------------------------------
Set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
config.CertificadoSenha = "12345678"
config.CertificadoArquivo = "d:\projetos\UnimakePV.pfx"
config.CSC = "456a65d4654s65as4d654"
config.CSCIDToken = 1
config.TipoDfe = 1
config.TipoAmbiente = 2

'----------------------------------------------------
'Desserializar o XML da NFCe já existente e assinado
'----------------------------------------------------
Set enviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")

enviNFe.Versao = "4.00"
enviNFe.IdLote = "000000000000001"
enviNFe.IndSinc = 1
enviNFe.AddNFe (GetFromFileNFe("D:\testenfe\41230206117473000150650010000590061954332302-nfe.xml"))

'----------------------------------------------------
'Criar o objeto do serviço da NFCe
'----------------------------------------------------
Set autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")
autorizacao.SetXMLConfiguracao (enviNFe), (config)
autorizacao.SetNullRetConsReciNFe()

'----------------------------------------------------
'Consultar situação da NFCe
'----------------------------------------------------
'Montar o XML de consulta status do serviço
Set consSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
consSitNFe.Versao = "4.00"
consSitNFe.TpAmb = 2 '1-Produção 2-Homologação
consSitNFe.ChNFe = "41230206117473000150650010000590061954332302"

'Consumir o serviço
Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo")
consultaProtocolo.Executar (consSitNFe),(config)

if consultaProtocolo.Result.CStat = 100 Then
   'Nota esta autorizaca, vou somente gerar o XML de distribuição
   autorizacao.AddRetConsSitNFes(consultaProtocolo.Result)
   autorizacao.GravarXmlDistribuicao("d:\testenfe")		 
else 'Nota não está autorizada
   MsgBox consultaProtocolo.Result.CStat	  
   MsgBox consultaProtocolo.Result.XMotivo
end if

'----------------------------------------------------
'Serializar o XML da NFe
'----------------------------------------------------
Function GetFromFileNFe(arqXML)
   Dim NFe: Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
   Set GetFromFileNFe = NFe.LoadFromFile(arqXML)
End Function
