* ---------------------------------------------------------------------------------
* Enivar Nfse - Serviço GerarNfse
* ---------------------------------------------------------------------------------
Function EnviarNfseGerarNFse()
 Local oConfiguracao
 Local oGerarNfse, oExceptionInterop
 Local xml
 
 * Criar o XML
 xml = [<?xml version="1.0" encoding="utf-8"?><GerarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws"><Rps><InfDeclaracaoPrestacaoServico Id="lote1"><Competencia>2014-12-02</Competencia><Servico><Valores><ValorServicos>110</ValorServicos></Valores><IssRetido>2</IssRetido><ItemListaServico>0702</ItemListaServico><Discriminacao>Prog.</Discriminacao><CodigoMunicipio>4204608</CodigoMunicipio><ExigibilidadeISS>1</ExigibilidadeISS><MunicipioIncidencia>4204608</MunicipioIncidencia></Servico><Prestador><CpfCnpj><Cnpj>45111111111100</Cnpj></CpfCnpj></Prestador><OptanteSimplesNacional>2</OptanteSimplesNacional><IncentivoFiscal>2</IncentivoFiscal></InfDeclaracaoPrestacaoServico></Rps></GerarNfseEnvio>]

 * Criar configuraçao básica para consumir o serviço
 oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
 oConfiguracao:TipoDfe = 5 // 0=nfse
 oConfiguracao:Servico = 27 // 27=NFSeGerarNfse
 oConfiguracao:CodigoMunicipio = 4204350
 oConfiguracao:SchemaVersao = "2.02" 
 oConfiguracao:TipoAmbiente = 2
 oConfiguracao:CertificadoSenha = "12345678"
 oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar objeto para pegar exceção do CSHARP
 oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
 
 Try
  * Enviar xml para prefeitura
    oGerarNfse = CreateObject("Unimake.Business.DFe.Servicos.NFSe.GerarNfse")
    ? "aqui 1"
    Wait
    oGerarNfse:Executar(xml, oConfiguracao)
    ? "aqui 2"
    Wait
    ? "Retorno", oGerarNfse:RetornoWSString
 Catch oErro
    //Demonstrar a exceção do CSHARP
    ?
    ? "Excecao do CSHARP: ", oExceptionInterop:GetMessage()
    ?
 End	   

 Wait
RETURN