Dim oErro, oExceptionInterop
Dim oConfiguracao, oConsSitNfe, oProcEventoNFe, X, oConsultaProtocolo, xmlEvento, nHandle, nomeArqDistribEvento
Dim oConfig, oConsSitNfeChNfe

'Criar configuraçao básica para consumir o serviço
Set oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
oConfig.TipoDFe = 0 '// 0=nfe
oConfig.CertificadoSenha = "12345678"
oConfig.CertificadoArquivo = "D:\projetos\UnimakePV.pfx"

'Criar XML da consulta protocolo
Set oConsSitNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNfe")
oConsSitNfe.Versao = "4.00"
oConsSitNfe.TpAmb = 2   'Homologação
oConsSitNfe.ChNfe = "31240402046163000130550010000570591001308457"  'Chave da NFE
  
'Enviar a consulta
Set oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
oConsultaProtocolo.Executar (oConsSitNfe), (oConfig)
      
MsgBox (oConsultaProtocolo.RetornoWSString)     
      
If oConsultaProtocolo.Result.GetProcEventoNFeCount > 0 Then
   For X = 1 To oConsultaProtocolo.Result.GetProcEventoNFeCount
       Set oProcEventoNFe = oConsultaProtocolo.Result.GetProcEventoNFe(X - 1)
                             
       nomeArqDistribEvento = "d:\testenfe\" + oProcEventoNFe.NomeArquivoDistribuicao
	   
       MsgBox (nomeArqDistribEvento)
	   
       xmlEvento = oProcEventoNFe.GerarXMLString() 'String do xml do evento

       MsgBox (xmlEvento)

	   
       'Se quiser pegar um evento específico, só comparar o tipo de evento, conforme segue
       If oProcEventoNFe.Evento.InfEvento.tpEvento = 110111 Then ' //Cancelamento
          'Aqui dentro do IF só fazer o que foi feito fora dele, nas linhas anteriores,
          'mas só vai fazer se for um cancelamento.
       End If
     Next
End If  
