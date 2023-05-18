* ---------------------------------------------------------------------------------
* Enviar NFSe padrão Paulistana (Município de São Paulo)
* ---------------------------------------------------------------------------------
FUNCTION EnviarNFSePadraoPAULISTANA()
   LOCAL oConfiguracao, oErro, oExceptionInterop, oConfigConsulta
   LOCAL oGerarNfse, oEnvioRPS, xmlNfse, xmlConsultaRPS, oConsultarNfse
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
   oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
   oConfiguracao.CertificadoSenha = "12345678"   
  
   oConfiguracao.TipoAmbiente = 1 && Produção
   
   oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
   oConfiguracao.Servico = 45 && Servico.NFSeEnvioRps
   oConfiguracao.SchemaVersao = "2.00"
   
*   oConfiguracao.MunicipioSenha = "123456"
*   oConfiguracao.MunicipioUsuario = "01001001000113"
      
 * Montar a string do XML
   xmlNfse = ""
   xmlNfse = xmlNfse + AllTrim([<?xml version="1.0" encoding="UTF-8"?>])
   xmlNfse = xmlNfse + AllTrim([<PedidoEnvioRPS xmlns="http://www.prefeitura.sp.gov.br/nfe">])
   xmlNfse = xmlNfse + AllTrim([	<Cabecalho Versao="1" xmlns="">])
   xmlNfse = xmlNfse + AllTrim([		<CPFCNPJRemetente>])
   xmlNfse = xmlNfse + AllTrim([			<CNPJ>99999997000100</CNPJ>])
   xmlNfse = xmlNfse + AllTrim([		</CPFCNPJRemetente>])
   xmlNfse = xmlNfse + AllTrim([	</Cabecalho>])
   xmlNfse = xmlNfse + AllTrim([	<RPS xmlns="">])
   
   xmlNfse = xmlNfse + AllTrim([		<Assinatura>d8Pg/jdA7t5tSaB8Il1d/CMiLGgfFAXzTL9o5stv6TNbhm9I94DIo0/ocqJpGx0KzoEeIQz4RSn99pWX4fiW/aETlNT3u5woqCAyL6U2hSyl/eQfWRYrqFu2zcdc4rsAG/])
   xmlNFse = xmlNfse + [wJbDjNO8y0Pz9b6rlTwkIJ+kMdLo+EWXMnB744olYE721g2O9CmUTvjtBgCfVUgvuN1MGjgzpgyussCOSkLpGbrqtM5+pYMXZsTaEVIIck1baDkoRpLmZ5Y/mcn1/Om1fMyhJVUAkgI5xBrORuotIP7e3+HLJn]
   xmlNFse = xmlNfse + [KgzQQPWCtLyEEyAqUk9Gq64wMayITua5FodaJsX+Eic/ie3kS5m50Q==</Assinatura>]
   
   xmlNfse = xmlNfse + AllTrim([		<ChaveRPS>])
   xmlNfse = xmlNfse + AllTrim([			<InscricaoPrestador>39616924</InscricaoPrestador>])
   xmlNfse = xmlNfse + AllTrim([			<SerieRPS>BB</SerieRPS>])
   xmlNfse = xmlNfse + AllTrim([			<NumeroRPS>4105</NumeroRPS>])
   xmlNfse = xmlNfse + AllTrim([		</ChaveRPS>])
   xmlNfse = xmlNfse + AllTrim([		<TipoRPS>RPS-M</TipoRPS>])
   xmlNfse = xmlNfse + AllTrim([		<DataEmissao>2015-01-20</DataEmissao>])
   xmlNfse = xmlNfse + AllTrim([		<StatusRPS>N</StatusRPS>])
   xmlNfse = xmlNfse + AllTrim([		<TributacaoRPS>T</TributacaoRPS>])
   xmlNfse = xmlNfse + AllTrim([		<ValorServicos>20500</ValorServicos>])
   xmlNfse = xmlNfse + AllTrim([		<ValorDeducoes>5000</ValorDeducoes>])
   xmlNfse = xmlNfse + AllTrim([		<ValorPIS>10</ValorPIS>])
   xmlNfse = xmlNfse + AllTrim([		<ValorCOFINS>10</ValorCOFINS>])
   xmlNfse = xmlNfse + AllTrim([		<ValorINSS>10</ValorINSS>])
   xmlNfse = xmlNfse + AllTrim([		<ValorIR>10</ValorIR>])
   xmlNfse = xmlNfse + AllTrim([		<ValorCSLL>10</ValorCSLL>])
   xmlNfse = xmlNfse + AllTrim([		<CodigoServico>7617</CodigoServico>])
   xmlNfse = xmlNfse + AllTrim([		<AliquotaServicos>0.05</AliquotaServicos>])
   xmlNfse = xmlNfse + AllTrim([		<ISSRetido>false</ISSRetido>])
   xmlNfse = xmlNfse + AllTrim([		<CPFCNPJTomador>])
   xmlNfse = xmlNfse + AllTrim([			<CPF>12345678909</CPF>])
   xmlNfse = xmlNfse + AllTrim([		</CPFCNPJTomador>])
   xmlNfse = xmlNfse + AllTrim([		<RazaoSocialTomador>TOMADOR PF</RazaoSocialTomador>])
   xmlNfse = xmlNfse + AllTrim([		<EnderecoTomador>])
   xmlNfse = xmlNfse + AllTrim([			<TipoLogradouro>Av</TipoLogradouro>])
   xmlNfse = xmlNfse + AllTrim([			<Logradouro>Paulista</Logradouro>])
   xmlNfse = xmlNfse + AllTrim([			<NumeroEndereco>100</NumeroEndereco>])
   xmlNfse = xmlNfse + AllTrim([			<ComplementoEndereco>Cj 35</ComplementoEndereco>])
   xmlNfse = xmlNfse + AllTrim([			<Bairro>Bela Vista</Bairro>])
   xmlNfse = xmlNfse + AllTrim([			<Cidade>3550308</Cidade>])
   xmlNfse = xmlNfse + AllTrim([			<UF>SP</UF>])
   xmlNfse = xmlNfse + AllTrim([			<CEP>1310100</CEP>])
   xmlNfse = xmlNfse + AllTrim([		</EnderecoTomador>])
   xmlNfse = xmlNfse + AllTrim([		<EmailTomador>tomador@teste.com.br</EmailTomador>])
   xmlNfse = xmlNfse + AllTrim([		<Discriminacao>Desenvolvimento de Web Site Pessoal.</Discriminacao>])
   xmlNfse = xmlNfse + AllTrim([	</RPS>])
   xmlNfse = xmlNfse + AllTrim([</PedidoEnvioRPS>])
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")     

   TRY 
      oEnvioRPS = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.EnvioRps")
      oEnvioRPS.Executar(xmlnfse, oConfiguracao)
      
      MESSAGEBOX(oEnvioRPS.RetornoWSString)  
      
    * Criar o XML de consulta nfse por RPS
      xmlConsultaRPS = ""
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([<?xml version="1.0" encoding="UTF-8"?>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([<p1:PedidoConsultaNFe xmlns:p1="http://www.prefeitura.sp.gov.br/nfe" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([	<Cabecalho Versao="1">])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([		<CPFCNPJRemetente>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([			<CNPJ>99999997000100</CNPJ>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([		</CPFCNPJRemetente>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([	</Cabecalho>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([	<Detalhe>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([		<ChaveRPS>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([			<InscricaoPrestador>39616924</InscricaoPrestador>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([			<SerieRPS>BB</SerieRPS>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([			<NumeroRPS>4105</NumeroRPS>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([		</ChaveRPS>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([	</Detalhe>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([</p1:PedidoConsultaNFe>])
      
    * Consumir o serviço de consulta nfse por RPS        
      oConfigConsulta = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfigConsulta.TipoDFe = 5 && TipoDFe.NFSe
      oConfigConsulta.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfigConsulta.CertificadoSenha = "12345678"   
      oConfigConsulta.TipoAmbiente =  1 && Produção
      oConfigConsulta.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
      oConfigConsulta.Servico = 32 && Servico.NFSeConsultarNfse
      oConfigConsulta.SchemaVersao = "2.00"
      
      oConsultarNfse = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse")
      oConsultarNfse.Executar(xmlConsultaRPS, oConfigConsulta)
      
      MESSAGEBOX(oConsultarNfse.RetornoWSString)
     
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY
RETURN

