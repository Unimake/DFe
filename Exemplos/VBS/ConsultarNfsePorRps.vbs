Dim oConsultarNfsePorRps
Dim oConfigConsulta
Dim xmlConsultaRPS

'Montar o objeto de configuração com informações mínimas 
'para ser utilizado na hora de consumir o serviço
SET oConfigConsulta = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
oConfigConsulta.TipoDFe = 5 'TipoDFe.NFSe
oConfigConsulta.CertificadoSenha = "12345678"
oConfigConsulta.CertificadoArquivo = "D:\projetos\UnimakePV.pfx"
oConfigConsulta.CodigoMunicipio = 3170701
oConfigConsulta.TipoAmbiente =  1 'Homologação
oConfigConsulta.Servico = 36 'Servico.NFSeConsultarNfsePorRps
oConfigConsulta.PadraoNFSe = 1 'PadraoNFSe.BETHA
oConfigConsulta.SchemaVersao = "1.00"
  
xmlConsultaRPS = ""  
  
xmlConsultaRPS = xmlConsultaRPS + Trim("<e:ConsultarNfsePorRpsEnvio xmlns:e=""http://www.betha.com.br/e-nota-contribuinte-ws"">")
xmlConsultaRPS = xmlConsultaRPS + Trim("  <IdentificacaoRps>")
xmlConsultaRPS = xmlConsultaRPS + Trim("    <Numero>6097</Numero>")
xmlConsultaRPS = xmlConsultaRPS + Trim("    <Serie>1</Serie>")
xmlConsultaRPS = xmlConsultaRPS + Trim("    <Tipo>1</Tipo>")
xmlConsultaRPS = xmlConsultaRPS + Trim("  </IdentificacaoRps>")
xmlConsultaRPS = xmlConsultaRPS + Trim("  <Prestador>")
xmlConsultaRPS = xmlConsultaRPS + Trim("    <Cnpj>17408253000100</Cnpj>")
xmlConsultaRPS = xmlConsultaRPS + Trim("    <InscricaoMunicipal>4013</InscricaoMunicipal>")
xmlConsultaRPS = xmlConsultaRPS + Trim("  </Prestador>")
xmlConsultaRPS = xmlConsultaRPS + Trim("</e:ConsultarNfsePorRpsEnvio>")
  
'xmlConsultaRPS = xmlConsultaRPS + Trim("<?xml version=""1.0"" encoding=""utf-8""?>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("<ConsultarNfseRpsEnvio xmlns=""http://www.betha.com.br/e-nota-contribuinte-ws"">")
'xmlConsultaRPS = xmlConsultaRPS + Trim("  <IdentificacaoRps>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    <Numero>6097</Numero>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    <Serie>1</Serie>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    <Tipo>1</Tipo>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("  </IdentificacaoRps>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("  <Prestador>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    <CpfCnpj>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("      <Cnpj>17408253000100</Cnpj>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    </CpfCnpj>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("    <InscricaoMunicipal>123498</InscricaoMunicipal>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("  </Prestador>")
'xmlConsultaRPS = xmlConsultaRPS + Trim("</ConsultarNfseRpsEnvio>")

MsgBox xmlConsultaRPS

'Consumir o serviço
SET oConsultarNfsePorRps = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
oConsultarNfsePorRps.Executar (xmlConsultaRPS), (oConfigConsulta)

'Demonstrar mensagens na tela com o retorno da SEFAZ
MsgBox oConsultarNfsePorRps.RetornoWSSTring 'Recuperar o XML retornado pela prefeitura