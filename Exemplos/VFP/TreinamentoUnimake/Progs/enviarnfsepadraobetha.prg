* ---------------------------------------------------------------------------------
* Enviar NFSe padrão Betha (Município de São Paulo)
* ---------------------------------------------------------------------------------
FUNCTION EnviarNFSePadraoBetha()
   LOCAL oConfiguracao, oErro, oExceptionInterop, oConfigConsulta
   LOCAL oRecepcionarLoteRpsSincrono, xmlNfse, xmlConsultaRPS, oConsultarNfsePorRps   
      
 * Criar o objeto de configuração mínima
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
   oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
   oConfiguracao.CertificadoSenha = "12345678"   
   oConfiguracao.TipoAmbiente = 2 && Homologação

   oConfiguracao.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC ###
   oConfiguracao.Servico = 29 && Servico.NFSeRecepcionarLoteRpsSincrono ###
   oConfiguracao.SchemaVersao = "2.02" && ###
    
* Montar a string do XML ###
   xmlNfse = ""
   xmlNfse = xmlNfse + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
   xmlNfse = xmlNfse + AllTrim([<EnviarLoteRpsSincronoEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
   xmlNfse = xmlNfse + AllTrim([  <LoteRps Id="LOTE11628" versao="2.02">])
   xmlNfse = xmlNfse + AllTrim([    <NumeroLote>11628</NumeroLote>])
   xmlNfse = xmlNfse + AllTrim([    <CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([      <Cnpj>00000000000000</Cnpj>])
   xmlNfse = xmlNfse + AllTrim([    </CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([    <InscricaoMunicipal>1111</InscricaoMunicipal>])
   xmlNfse = xmlNfse + AllTrim([    <QuantidadeRps>1</QuantidadeRps>])
   xmlNfse = xmlNfse + AllTrim([    <ListaRps>])
   xmlNfse = xmlNfse + AllTrim([      <Rps>])
   xmlNfse = xmlNfse + AllTrim([        <InfDeclaracaoPrestacaoServico Id="RPS1551">])
   xmlNfse = xmlNfse + AllTrim([          <Rps>])
   xmlNfse = xmlNfse + AllTrim([            <IdentificacaoRps>])
   xmlNfse = xmlNfse + AllTrim([              <Numero>1551</Numero>])
   xmlNfse = xmlNfse + AllTrim([              <Serie>1</Serie>])
   xmlNfse = xmlNfse + AllTrim([              <Tipo>1</Tipo>])
   xmlNfse = xmlNfse + AllTrim([            </IdentificacaoRps>])
   xmlNfse = xmlNfse + AllTrim([            <DataEmissao>2021-07-27</DataEmissao>])
   xmlNfse = xmlNfse + AllTrim([            <Status>1</Status>])
   xmlNfse = xmlNfse + AllTrim([          </Rps>])
   xmlNfse = xmlNfse + AllTrim([          <Competencia>2021-07-27</Competencia>])
   xmlNfse = xmlNfse + AllTrim([          <Servico>])
   xmlNfse = xmlNfse + AllTrim([            <Valores>])
   xmlNfse = xmlNfse + AllTrim([              <ValorServicos>50.00</ValorServicos>])
   xmlNfse = xmlNfse + AllTrim([              <ValorDeducoes>0.00</ValorDeducoes>])
   xmlNfse = xmlNfse + AllTrim([              <ValorPis>0.00</ValorPis>])
   xmlNfse = xmlNfse + AllTrim([              <ValorCofins>0.00</ValorCofins>])
   xmlNfse = xmlNfse + AllTrim([              <ValorInss>0.00</ValorInss>])
   xmlNfse = xmlNfse + AllTrim([              <ValorIr>0.00</ValorIr>])
   xmlNfse = xmlNfse + AllTrim([              <ValorCsll>0.00</ValorCsll>])
   xmlNfse = xmlNfse + AllTrim([              <OutrasRetencoes>0.00</OutrasRetencoes>])
   xmlNfse = xmlNfse + AllTrim([              <DescontoIncondicionado>0.00</DescontoIncondicionado>])
   xmlNfse = xmlNfse + AllTrim([              <DescontoCondicionado>0.00</DescontoCondicionado>])
   xmlNfse = xmlNfse + AllTrim([            </Valores>])
   xmlNfse = xmlNfse + AllTrim([            <IssRetido>2</IssRetido>])
   xmlNfse = xmlNfse + AllTrim([            <ItemListaServico>1401</ItemListaServico>])
   xmlNfse = xmlNfse + AllTrim([            <Discriminacao>SERVICO DE CORTE E DOBRA A/c</Discriminacao>])
   xmlNfse = xmlNfse + AllTrim([            <CodigoMunicipio>1111111</CodigoMunicipio>])
   xmlNfse = xmlNfse + AllTrim([            <ExigibilidadeISS>1</ExigibilidadeISS>])
   xmlNfse = xmlNfse + AllTrim([            <MunicipioIncidencia>1111111</MunicipioIncidencia>])
   xmlNfse = xmlNfse + AllTrim([          </Servico>])
   xmlNfse = xmlNfse + AllTrim([          <Prestador>])
   xmlNfse = xmlNfse + AllTrim([            <CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([              <Cnpj>00000000000000</Cnpj>])
   xmlNfse = xmlNfse + AllTrim([            </CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([            <InscricaoMunicipal>1111</InscricaoMunicipal>])
   xmlNfse = xmlNfse + AllTrim([          </Prestador>])
   xmlNfse = xmlNfse + AllTrim([          <Tomador>])
   xmlNfse = xmlNfse + AllTrim([            <IdentificacaoTomador>])
   xmlNfse = xmlNfse + AllTrim([              <CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([                <Cnpj>00000000000000</Cnpj>])
   xmlNfse = xmlNfse + AllTrim([              </CpfCnpj>])
   xmlNfse = xmlNfse + AllTrim([            </IdentificacaoTomador>])
   xmlNfse = xmlNfse + AllTrim([            <RazaoSocial>xxxxxxxx ALIMENTOS LTDA</RazaoSocial>])
   xmlNfse = xmlNfse + AllTrim([            <Endereco>])
   xmlNfse = xmlNfse + AllTrim([              <Endereco>ROD. xxxxx xxxxxxxxx</Endereco>])
   xmlNfse = xmlNfse + AllTrim([              <Numero>sn</Numero>])
   xmlNfse = xmlNfse + AllTrim([              <Bairro>ZONA RURAL</Bairro>])
   xmlNfse = xmlNfse + AllTrim([              <CodigoMunicipio>1111111</CodigoMunicipio>])
   xmlNfse = xmlNfse + AllTrim([              <Uf>PR</Uf>])
   xmlNfse = xmlNfse + AllTrim([              <Cep>87706060</Cep>])
   xmlNfse = xmlNfse + AllTrim([            </Endereco>])
   xmlNfse = xmlNfse + AllTrim([            <Contato>])
   xmlNfse = xmlNfse + AllTrim([              <Telefone>1111111111111</Telefone>])
   xmlNfse = xmlNfse + AllTrim([              <Email>teste@hotmail.com</Email>])
   xmlNfse = xmlNfse + AllTrim([            </Contato>])
   xmlNfse = xmlNfse + AllTrim([          </Tomador>])
   xmlNfse = xmlNfse + AllTrim([          <RegimeEspecialTributacao>1</RegimeEspecialTributacao>])
   xmlNfse = xmlNfse + AllTrim([          <OptanteSimplesNacional>2</OptanteSimplesNacional>])
   xmlNfse = xmlNfse + AllTrim([          <IncentivoFiscal>2</IncentivoFiscal>])
   xmlNfse = xmlNfse + AllTrim([        </InfDeclaracaoPrestacaoServico>])
   xmlNfse = xmlNfse + AllTrim([      </Rps>])
   xmlNfse = xmlNfse + AllTrim([    </ListaRps>])
   xmlNfse = xmlNfse + AllTrim([  </LoteRps>])
   xmlNfse = xmlNfse + AllTrim([</EnviarLoteRpsSincronoEnvio>])      
      
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")     

   TRY 
      oRecepcionarLoteRpsSincrono = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.RecepcionarLoteRpsSincrono") && ###
      oRecepcionarLoteRpsSincrono.Executar(xmlnfse, oConfiguracao) && ###
      
      MESSAGEBOX(oRecepcionarLoteRpsSincrono.RetornoWSString)
      
    * Criar o XML de consulta nfse por RPS ###
      xmlConsultaRPS = ""
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([<ConsultarNfseRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([  <IdentificacaoRps>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    <Numero>24</Numero>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    <Serie>A1</Serie>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    <Tipo>1</Tipo>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([  </IdentificacaoRps>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([  <Prestador>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    <CpfCnpj>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([      <Cnpj>45111111111100</Cnpj>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    </CpfCnpj>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([    <InscricaoMunicipal>123498</InscricaoMunicipal>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([  </Prestador>])
      xmlConsultaRPS = xmlConsultaRPS + AllTrim([</ConsultarNfseRpsEnvio>])
      
    * Consumir o serviço de consulta nfse por RPS        
      oConfigConsulta = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfigConsulta.TipoDFe = 5 && TipoDFe.NFSe
      oConfigConsulta.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfigConsulta.CertificadoSenha = "12345678"   
      oConfigConsulta.TipoAmbiente =  2 && Homologação
      oConfigConsulta.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC ###
      oConfigConsulta.Servico = 36  && Servico.NFSeConsultarNfsePorRps ###
      oConfigConsulta.SchemaVersao = "2.02" && ###
      
      oConsultarNfsePorRps = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
      oConsultarNfsePorRps.Executar(xmlConsultaRPS, oConfigConsulta)
      
      MESSAGEBOX(oConsultarNfsePorRps.RetornoWSString)
  
     
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY
RETURN

