* ---------------------------------------------------------------------------------
* Enviar consultas diversas de NFSe padrão BETHA (Município de Itapema-SC)
* ---------------------------------------------------------------------------------
FUNCTION EnviarConsultaPadraoBETHA()
   LOCAL xmlConsulta, oConfiguracao 
   LOCAL oErro, oConsultarNfseServicoPrestado, oConsultarNfsePorRps, oConsultarNfseServicoTomado
      
   TRY
    * -------------------------------------------------------------
    * Consulta da NFSe por RPS
    * -------------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlConsulta = xmlConsulta + AllTrim([<ConsultarNfseRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
      xmlConsulta = xmlConsulta + AllTrim([  <IdentificacaoRps>])
      xmlConsulta = xmlConsulta + AllTrim([    <Numero>24</Numero>])
      xmlConsulta = xmlConsulta + AllTrim([    <Serie>A1</Serie>])
      xmlConsulta = xmlConsulta + AllTrim([    <Tipo>1</Tipo>])
      xmlConsulta = xmlConsulta + AllTrim([  </IdentificacaoRps>])
      xmlConsulta = xmlConsulta + AllTrim([  <Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([    <CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([      <Cnpj>45111111111100</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    </CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    <InscricaoMunicipal>123498</InscricaoMunicipal>])
      xmlConsulta = xmlConsulta + AllTrim([  </Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([</ConsultarNfseRpsEnvio>])
         
    * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  2 && Produção
      oConfiguracao.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC 
      oConfiguracao.Servico = 36 && Servico.NFSeConsultarNfsePorRps
      oConfiguracao.SchemaVersao = "2.02"

      oConsultarNfsePorRps = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
      oConsultarNfsePorRps.Executar(xmlConsulta, oConfiguracao)  
      
      MESSAGEBOX(oConsultarNfsePorRps.RetornoWSString)
      
    * -------------------------------------------------------------   
    * Consulta de NFSe´s Emitidas por período (Servicos prestados)
    * -------------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlConsulta = xmlConsulta + AllTrim([<ConsultarNfseServicoPrestadoEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
      xmlConsulta = xmlConsulta + AllTrim([	<Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([		<CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([			<Cnpj>45111111111100</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([		</CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([	</Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([	<NumeroNfse>61</NumeroNfse>])
      xmlConsulta = xmlConsulta + AllTrim([	<PeriodoEmissao>])
      xmlConsulta = xmlConsulta + AllTrim([		<DataInicial>2014-12-01</DataInicial>])
      xmlConsulta = xmlConsulta + AllTrim([		<DataFinal>2014-12-31</DataFinal>])
      xmlConsulta = xmlConsulta + AllTrim([	</PeriodoEmissao>])
      xmlConsulta = xmlConsulta + AllTrim([	<Pagina>1</Pagina>])
      xmlConsulta = xmlConsulta + AllTrim([</ConsultarNfseServicoPrestadoEnvio>])
            
   * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  2 && Produção
      oConfiguracao.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC 
      oConfiguracao.Servico = 33 && Servico.NFSeConsultarNfseServicoPrestado
      oConfiguracao.SchemaVersao = "2.02"
      
      oConsultarNfseServicoPrestado = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfseServicoPrestado")
      oConsultarNfseServicoPrestado.Executar(xmlConsulta, oConfiguracao)
       
      MESSAGEBOX(oConsultarNfseServicoPrestado.RetornoWSString)
            
    * -------------------------------------------------------------
    * Consulta de NFSe´s Recebidas por período (Serviços tomados)
    * -------------------------------------------------------------
    * Criar o XML
      xmlConsulta = ""
      xmlConsulta = xmlConsulta + AllTrim([<ConsultarNfseServicoTomadoEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
      xmlConsulta = xmlConsulta + AllTrim([  <Consulente>])
      xmlConsulta = xmlConsulta + AllTrim([    <CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([      <Cnpj>06067665000107</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    </CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    <InscricaoMunicipal>000</InscricaoMunicipal>])
      xmlConsulta = xmlConsulta + AllTrim([  </Consulente>])
      xmlConsulta = xmlConsulta + AllTrim([  <PeriodoCompetencia>])
      xmlConsulta = xmlConsulta + AllTrim([    <DataInicial>2019-01-01</DataInicial>])
      xmlConsulta = xmlConsulta + AllTrim([    <DataFinal>2019-03-21</DataFinal>])
      xmlConsulta = xmlConsulta + AllTrim([  </PeriodoCompetencia>])
      xmlConsulta = xmlConsulta + AllTrim([  <Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([    <CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([      <Cnpj>60250776000191</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    </CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    <InscricaoMunicipal>000</InscricaoMunicipal>])
      xmlConsulta = xmlConsulta + AllTrim([  </Prestador>])
      xmlConsulta = xmlConsulta + AllTrim([  <Tomador>])
      xmlConsulta = xmlConsulta + AllTrim([    <CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([      <Cnpj>06067665000107</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    </CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    <InscricaoMunicipal>000</InscricaoMunicipal>])
      xmlConsulta = xmlConsulta + AllTrim([  </Tomador>])
      xmlConsulta = xmlConsulta + AllTrim([  <Intermediario>])
      xmlConsulta = xmlConsulta + AllTrim([    <CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([      <Cnpj>00000000000000</Cnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    </CpfCnpj>])
      xmlConsulta = xmlConsulta + AllTrim([    <InscricaoMunicipal>000</InscricaoMunicipal>])
      xmlConsulta = xmlConsulta + AllTrim([  </Intermediario>])
      xmlConsulta = xmlConsulta + AllTrim([  <Pagina>1</Pagina>])
      xmlConsulta = xmlConsulta + AllTrim([</ConsultarNfseServicoTomadoEnvio>])
            
    * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  2 && Produção
      oConfiguracao.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC
      oConfiguracao.Servico = 34 && Servico.NFSeConsultarNfseServicoTomado
      oConfiguracao.SchemaVersao = "2.02"
      
      oConsultarNfseServicoTomado = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfseServicoTomado")
      oConsultarNfseServicoTomado.Executar(xmlConsulta, oConfiguracao)
       
      MESSAGEBOX(oConsultarNfseServicoTomado.RetornoWSString)      


   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      