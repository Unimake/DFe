* ---------------------------------------------------------------------------------
* Enviar CTe de forma síncrona
* ---------------------------------------------------------------------------------
FUNCTION EnviarNFSePadraoPAULISTANA()
   LOCAL oConfiguracao, oErro, oExceptionInterop
   LOCAL oGerarNfse, oEnvioRPS, xmlNfse
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
   oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
   oConfiguracao.CertificadoSenha = "12345678"   
  
   oConfiguracao.TipoAmbiente = 1 && Produção

*   oConfiguracao.CodigoMunicipio = 4118402 && Código do IBGE de Paranavaí-PR
*   oConfiguracao.Servico = 27 && Servico.NFSeGerarNfse
*   oConfiguracao.SchemaVersao = "1.20"
*   oConfiguracao.MunicipioSenha = "123456"
*   oConfiguracao.MunicipioUsuario = "01001001000113"
   
   oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
   oConfiguracao.Servico = 45 && Servico.NFSeEnvioRps
   oConfiguracao.SchemaVersao = "2.00"
   
      
 * Montar a string do XML
   xmlNfse = ""
*   xmlNfse = xmlNfse + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
*   xmlNfse = xmlNfse + AllTrim([<nfse>])
*   xmlNfse = xmlNfse + AllTrim([	<nfse_teste>1</nfse_teste>])
*   xmlNfse = xmlNfse + AllTrim([	<nf>])
*   xmlNfse = xmlNfse + AllTrim([		<serie_nfse>1</serie_nfse>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_total>0,10</valor_total>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_desconto>0,00</valor_desconto>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_ir>0,00</valor_ir>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_inss>0,00</valor_inss>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_contribuicao_social>0,00</valor_contribuicao_social>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_rps>0,00</valor_rps>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_pis>0,00</valor_pis>])
*   xmlNfse = xmlNfse + AllTrim([		<valor_cofins>0,00</valor_cofins>])
*   xmlNfse = xmlNfse + AllTrim([		<observacao/>])
*   xmlNfse = xmlNfse + AllTrim([	</nf>])
*   xmlNfse = xmlNfse + AllTrim([	<prestador>])
*   xmlNfse = xmlNfse + AllTrim([		<cpfcnpj>06117473000150</cpfcnpj>])
*   xmlNfse = xmlNfse + AllTrim([		<cidade>7749</cidade>])
*   xmlNfse = xmlNfse + AllTrim([	</prestador>])
*   xmlNfse = xmlNfse + AllTrim([	<tomador>])
*   xmlNfse = xmlNfse + AllTrim([		<tipo>F</tipo>])
*   xmlNfse = xmlNfse + AllTrim([		<cpfcnpj>77988006037</cpfcnpj>])
*   xmlNfse = xmlNfse + AllTrim([		<ie/>])
*   xmlNfse = xmlNfse + AllTrim([		<nome_razao_social>EMPRESA TESTE</nome_razao_social>])
*   xmlNfse = xmlNfse + AllTrim([		<sobrenome_nome_fantasia>EMPRESA TESTE</sobrenome_nome_fantasia>])
*   xmlNfse = xmlNfse + AllTrim([		<logradouro>KM47</logradouro>])
*   xmlNfse = xmlNfse + AllTrim([		<email/>])
*   xmlNfse = xmlNfse + AllTrim([		<complemento/>])
*   xmlNfse = xmlNfse + AllTrim([		<ponto_referencia/>])
*   xmlNfse = xmlNfse + AllTrim([		<bairro>RURAL</bairro>])
*   xmlNfse = xmlNfse + AllTrim([		<cidade>7829</cidade>])
*   xmlNfse = xmlNfse + AllTrim([		<cep>87800000</cep>])
*   xmlNfse = xmlNfse + AllTrim([		<ddd_fone_comercial>044</ddd_fone_comercial>])
*   xmlNfse = xmlNfse + AllTrim([		<fone_comercial>36721722</fone_comercial>])
*   xmlNfse = xmlNfse + AllTrim([		<ddd_fone_residencial/>])
*   xmlNfse = xmlNfse + AllTrim([		<fone_residencial/>])
*   xmlNfse = xmlNfse + AllTrim([		<ddd_fax/>])
*   xmlNfse = xmlNfse + AllTrim([		<fone_fax/>])
*   xmlNfse = xmlNfse + AllTrim([	</tomador>])
*   xmlNfse = xmlNfse + AllTrim([	<itens>])
*   xmlNfse = xmlNfse + AllTrim([		<lista>])
*   xmlNfse = xmlNfse + AllTrim([			<codigo_local_prestacao_servico>7749</codigo_local_prestacao_servico>])
*   xmlNfse = xmlNfse + AllTrim([			<codigo_item_lista_servico>0101</codigo_item_lista_servico>])
*   xmlNfse = xmlNfse + AllTrim([			<descritivo>SERVICO DESENVOLVIMENTO DE SISTEMAS</descritivo>])
*   xmlNfse = xmlNfse + AllTrim([			<aliquota_item_lista_servico>2,00</aliquota_item_lista_servico>])
*   xmlNfse = xmlNfse + AllTrim([			<situacao_tributaria>0</situacao_tributaria>])
*   xmlNfse = xmlNfse + AllTrim([			<valor_tributavel>0,10</valor_tributavel>])
*   xmlNfse = xmlNfse + AllTrim([			<valor_deducao>0,00</valor_deducao>])
*   xmlNfse = xmlNfse + AllTrim([			<valor_issrf>0,00</valor_issrf>])
*   xmlNfse = xmlNfse + AllTrim([			<tributa_municipio_prestador>N</tributa_municipio_prestador>])
*   xmlNfse = xmlNfse + AllTrim([			<unidade_codigo/>])
*   xmlNfse = xmlNfse + AllTrim([			<unidade_quantidade/>])
*   xmlNfse = xmlNfse + AllTrim([			<unidade_valor_unitario/>])
*   xmlNfse = xmlNfse + AllTrim([		</lista>])
*   xmlNfse = xmlNfse + AllTrim([	</itens>])
*   xmlNfse = xmlNfse + AllTrim([	<forma_pagamento>])
*   xmlNfse = xmlNfse + AllTrim([		<tipo_pagamento>2</tipo_pagamento>])
*   xmlNfse = xmlNfse + AllTrim([		<parcelas>])
*   xmlNfse = xmlNfse + AllTrim([			<parcela>])
*   xmlNfse = xmlNfse + AllTrim([				<numero>1</numero>])
*   xmlNfse = xmlNfse + AllTrim([				<valor>0,10</valor>])
*   xmlNfse = xmlNfse + AllTrim([				<data_vencimento>10/04/2023</data_vencimento>])
*   xmlNfse = xmlNfse + AllTrim([			</parcela>])
*   xmlNfse = xmlNfse + AllTrim([		</parcelas>])
*   xmlNfse = xmlNfse + AllTrim([	</forma_pagamento>])
*   xmlNfse = xmlNfse + AllTrim([</nfse>])

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
*      oGerarNfse = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.GerarNfse")
*      oGerarNfse.Executar(xmlnfse, oConfiguracao)

      oEnvioRPS = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.EnvioRps")
      oEnvioRPS.Executar(xmlnfse, oConfiguracao)

      
      MESSAGEBOX(oEnvioRPS.RetornoWSString)
	     
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY
RETURN

