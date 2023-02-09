'---------------------------------------------------------
'Evento CCe CTe com Desserialização
'---------------------------------------------------------
 Dim oConfiguracao
 Dim oEventoCTe

 'Criar configuraçao básica para consumir o serviço
 Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
 oConfiguracao.TipoDFe = 2 '2=CTe
 oConfiguracao.CertificadoSenha = "12345678"
 oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 'Criar tag do lote de eventos <eventoCTe>
 Set oEventoCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCTe")
 
 'Desserializar o XML, que já existe no HD, no objeto.
 Set oEventoCTe = oEventoCTe.LoadFromFile("C:\projetos\uninfe\exemplos\CTe 3.00\cce35150107565416000104570000000012301000012300-ped-eve.xml")

 'Demonstrar alguns valores informados no XML para ficar o modelo de como resgatar a informação
 MsgBox oEventoCTe.Versao 'Demonstrar a versão informada no XML
 MsgBox oEventoCTe.InfEvento.COrgao 'Demonstrar o cOrgao informado no XML
 MsgBox oEventoCTe.InfEvento.CNPJ 'Demonstrar o CNPJ infomrado no XML
 MsgBox oEventoCTe.InfEvento.DhEvento 'Demonstrar o CNPJ infomrado no XML
 
 'Enviar carta de correcao
 Set oRecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento")
 oRecepcaoEvento.Executar (oEventoCTe),  (oConfiguracao)
  
 MsgBox oRecepcaoEvento.RetornoWSString
 MsgBox oRecepcaoEvento.Result.InfEvento.CStat 'Status retornado pela SEFAZ
 MsgBox oRecepcaoEvento.Result.InfEvento.XMotivo 'XMotivo
 
 if oRecepcaoEvento.Result.InfEvento.CStat = 134 Then
    oRecepcaoEvento.GravarXmlDistribuicao ("tmp\testenfe") 'Grava o XML de distribuição
 end if