'---------------------------------------------------------
'Evento cancelamento CTe
'---------------------------------------------------------
 Dim oConfiguracao
 Dim oEventoCTe
 Dim oDetEventoCanc
 Dim oInfEvento
 Dim oRecepcaoEvento

 'Criar configuraçao básica para consumir o serviço
 Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
 oConfiguracao.TipoDFe = 2 '2=CTe
 oConfiguracao.CertificadoSenha = "12345678"
 oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 'Criar tag <eventoCTe>
 Set oEventoCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCTe")
 oEventoCTe.Versao = "3.00"

 'Criar tag <detEvento>
 Set oDetEventoCanc = CreateObject("Unimake.Business.DFe.Xml.CTe.DetEventoCanc")
 oDetEventoCanc.VersaoEvento = "3.00"
 oDetEventoCanc.NProt = "141190000660363"
 oDetEventoCanc.XJust = "Justificativa para cancelamento da NFe de teste"

 'Criar tag <infEvento>
 Set oInfEvento = CreateObject("Unimake.Business.DFe.Xml.CTe.InfEvento")
 
 'Adicionar o Objeto DetEventoCanc dentro do objeto DetEvento
 oInfEvento.DetEvento = oDetEventoCanc
 
 'Atualizar propriedades da oInfEvento
 'IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCanc para que funcione sem erro
 oInfEvento.COrgao = 41 'UFBrasil.PR
 oInfEvento.ChCTe = "41191006117473000150550010000579281779843610"
 oInfEvento.CNPJ = "06117473000150"
 oInfEvento.DhEvento = Now
 oInfEvento.TpEvento = 110111 'TipoEventoCTe.Cancelamento
 oInfEvento.NSeqEvento = 1
 oInfEvento.TpAmb = 2 'TipoAmbiente.Homologacao

 'Adicionar a tag <infEvento> dentro da tag <eventoCTe>
 oEventoCTe.InfEvento = oInfEvento

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
 
 
 

