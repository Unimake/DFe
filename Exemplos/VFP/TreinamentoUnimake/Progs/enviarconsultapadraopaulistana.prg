* ---------------------------------------------------------------------------------
* Enviar consultas diversas de NFSe padrão Paulistana (Município de São Paulo)
* ---------------------------------------------------------------------------------
FUNCTION EnviarConsultaPadraoPAULISTANA()
   LOCAL xmlConsulta, oConfiguracao 
   LOCAL oErro, oConsultaNFeEmitidas, oConsultarNfse, oConsultaNFeRecebidas   
      
   TRY
    * ------------------------------------------------------   
    * Consulta da NFSe por RPS
    * ------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<?xml version="1.0" encoding="UTF-8"?>])
      xmlConsulta = xmlConsulta + AllTrim([<p1:PedidoConsultaNFe xmlns:p1="http://www.prefeitura.sp.gov.br/nfe" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">])
      xmlConsulta = xmlConsulta + AllTrim([	<Cabecalho Versao="1">])
      xmlConsulta = xmlConsulta + AllTrim([		<CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([			<CNPJ>99999997000100</CNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		</CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([	</Cabecalho>])
      xmlConsulta = xmlConsulta + AllTrim([	<Detalhe>])
      xmlConsulta = xmlConsulta + AllTrim([		<ChaveRPS>])
      xmlConsulta = xmlConsulta + AllTrim([			<InscricaoPrestador>39616924</InscricaoPrestador>])
      xmlConsulta = xmlConsulta + AllTrim([			<SerieRPS>BB</SerieRPS>])
      xmlConsulta = xmlConsulta + AllTrim([			<NumeroRPS>4105</NumeroRPS>])
      xmlConsulta = xmlConsulta + AllTrim([		</ChaveRPS>])
      xmlConsulta = xmlConsulta + AllTrim([	</Detalhe>])
      xmlConsulta = xmlConsulta + AllTrim([</p1:PedidoConsultaNFe>])
   
    * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  1 && Produção
      oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
      oConfiguracao.Servico = 32 && Servico.NFSeConsultarNfse
      oConfiguracao.SchemaVersao = "2.00"

      oConsultarNfse = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse")
      oConsultarNfse.Executar(xmlConsulta, oConfiguracao)  
      
      MESSAGEBOX(oConsultarNfse.RetornoWSString)
      
    * ------------------------------------------------------   
    * Consulta de NFSe´s Emitidas por período
    * ------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlConsulta = xmlConsulta + AllTrim([<p1:PedidoConsultaNFePeriodo xmlns:p1="http://www.prefeitura.sp.gov.br/nfe">])
      xmlConsulta = xmlConsulta + AllTrim([	<Cabecalho Versao="1">])
      xmlConsulta = xmlConsulta + AllTrim([		<CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([			<CNPJ>04642554000143</CNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		</CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([		<CPFCNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([			<CNPJ>04642554000143</CNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		</CPFCNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		<Inscricao>31000000</Inscricao>])
      xmlConsulta = xmlConsulta + AllTrim([		<dtInicio>2022-01-01</dtInicio>])
      xmlConsulta = xmlConsulta + AllTrim([		<dtFim>2022-01-30</dtFim>])
      xmlConsulta = xmlConsulta + AllTrim([		<NumeroPagina>1</NumeroPagina>])
      xmlConsulta = xmlConsulta + AllTrim([	</Cabecalho>])
      xmlConsulta = xmlConsulta + AllTrim([</p1:PedidoConsultaNFePeriodo>])  
      
   * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  1 && Produção
      oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
      oConfiguracao.Servico = 42 && Servico.NFSeConsultaNFeEmitidas
      oConfiguracao.SchemaVersao = "2.00"
      
      oConsultaNFeEmitidas = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultaNFeEmitidas")
      oConsultaNFeEmitidas.Executar(xmlConsulta, oConfiguracao)
       
      MESSAGEBOX(oConsultaNFeEmitidas.RetornoWSString)
            
    * ------------------------------------------------------   
    * Consulta de NFSe´s Recebidas por período
    * ------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlConsulta = xmlConsulta + AllTrim([<p1:PedidoConsultaNFePeriodo xmlns:p1="http://www.prefeitura.sp.gov.br/nfe" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">])
      xmlConsulta = xmlConsulta + AllTrim([	<Cabecalho Versao="1">])
      xmlConsulta = xmlConsulta + AllTrim([		<CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([			<CNPJ>04642554000143</CNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		</CPFCNPJRemetente>])
      xmlConsulta = xmlConsulta + AllTrim([		<CPFCNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([			<CNPJ>04642554000143</CNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		</CPFCNPJ>])
      xmlConsulta = xmlConsulta + AllTrim([		<Inscricao>31000000</Inscricao>])
      xmlConsulta = xmlConsulta + AllTrim([		<dtInicio>2007-01-01</dtInicio>])
      xmlConsulta = xmlConsulta + AllTrim([		<dtFim>2007-01-31</dtFim>])
      xmlConsulta = xmlConsulta + AllTrim([		<NumeroPagina>1</NumeroPagina>])
      xmlConsulta = xmlConsulta + AllTrim([	</Cabecalho>])
      xmlConsulta = xmlConsulta + AllTrim([</p1:PedidoConsultaNFePeriodo>])
      
    * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  1 && Produção
      oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
      oConfiguracao.Servico = 41 && Servico.NFSeConsultaNFeRecebidas
      oConfiguracao.SchemaVersao = "2.00"        
      
      oConsultaNFeRecebidas = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultaNFeRecebidas")
      oConsultaNFeRecebidas.Executar(xmlConsulta, oConfiguracao)
       
      MESSAGEBOX(oConsultaNFeRecebidas.RetornoWSString)      


   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      