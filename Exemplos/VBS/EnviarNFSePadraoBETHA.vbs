' ---------------------------------------------------------------------------------
' Enviar NFSe padrão Betha (Município de Itapema-SC)
' ---------------------------------------------------------------------------------
Dim oConfiguracao
Dim xmlNfse
Dim oRecepcionarLoteRpsSincrono
Dim xmlConsultaRPS
Dim oConsultarNfsePorRps
      
' Criar o objeto de configuração mínima para envio da NFSe
  Set oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
  oConfiguracao.TipoDFe = 5 ' TipoDFe.NFSe
  oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
  oConfiguracao.CertificadoSenha = "12345678"   
  oConfiguracao.TipoAmbiente = 2 ' Homologação
  oConfiguracao.CodigoMunicipio = 4208302 ' Código do IBGE de Itapema-SC
  oConfiguracao.Servico = 29 ' Servico.NFSeRecepcionarLoteRpsSincrono
  oConfiguracao.SchemaVersao = "2.02" 
    
' Montar a string do XML
  xmlNfse = ""
  xmlNfse = xmlNfse & Trim("<?xml version=""1.0"" encoding=""utf-8""?>")
  xmlNfse = xmlNfse & Trim("<EnviarLoteRpsSincronoEnvio xmlns=""http://www.betha.com.br/e-nota-contribuinte-ws"">")
  xmlNfse = xmlNfse & Trim("  <LoteRps Id=""LOTE11628"" versao=""2.02"">")
  xmlNfse = xmlNfse & Trim("    <NumeroLote>11628</NumeroLote>")
  xmlNfse = xmlNfse & Trim("    <CpfCnpj>")
  xmlNfse = xmlNfse & Trim("      <Cnpj>00000000000000</Cnpj>")
  xmlNfse = xmlNfse & Trim("    </CpfCnpj>")
  xmlNfse = xmlNfse & Trim("    <InscricaoMunicipal>1111</InscricaoMunicipal>")
  xmlNfse = xmlNfse & Trim("    <QuantidadeRps>1</QuantidadeRps>")
  xmlNfse = xmlNfse & Trim("    <ListaRps>")
  xmlNfse = xmlNfse & Trim("      <Rps>")
  xmlNfse = xmlNfse & Trim("        <InfDeclaracaoPrestacaoServico Id=""RPS1551"">")
  xmlNfse = xmlNfse & Trim("          <Rps>")
  xmlNfse = xmlNfse & Trim("            <IdentificacaoRps>")
  xmlNfse = xmlNfse & Trim("              <Numero>1551</Numero>")
  xmlNfse = xmlNfse & Trim("              <Serie>1</Serie>")
  xmlNfse = xmlNfse & Trim("              <Tipo>1</Tipo>")
  xmlNfse = xmlNfse & Trim("            </IdentificacaoRps>")
  xmlNfse = xmlNfse & Trim("            <DataEmissao>2021-07-27</DataEmissao>")
  xmlNfse = xmlNfse & Trim("            <Status>1</Status>")
  xmlNfse = xmlNfse & Trim("          </Rps>")
  xmlNfse = xmlNfse & Trim("          <Competencia>2021-07-27</Competencia>")
  xmlNfse = xmlNfse & Trim("          <Servico>")
  xmlNfse = xmlNfse & Trim("            <Valores>")
  xmlNfse = xmlNfse & Trim("              <ValorServicos>50.00</ValorServicos>")
  xmlNfse = xmlNfse & Trim("              <ValorDeducoes>0.00</ValorDeducoes>")
  xmlNfse = xmlNfse & Trim("              <ValorPis>0.00</ValorPis>")
  xmlNfse = xmlNfse & Trim("              <ValorCofins>0.00</ValorCofins>")
  xmlNfse = xmlNfse & Trim("              <ValorInss>0.00</ValorInss>")
  xmlNfse = xmlNfse & Trim("              <ValorIr>0.00</ValorIr>")
  xmlNfse = xmlNfse & Trim("              <ValorCsll>0.00</ValorCsll>")
  xmlNfse = xmlNfse & Trim("              <OutrasRetencoes>0.00</OutrasRetencoes>")
  xmlNfse = xmlNfse & Trim("              <DescontoIncondicionado>0.00</DescontoIncondicionado>")
  xmlNfse = xmlNfse & Trim("              <DescontoCondicionado>0.00</DescontoCondicionado>")
  xmlNfse = xmlNfse & Trim("            </Valores>")
  xmlNfse = xmlNfse & Trim("            <IssRetido>2</IssRetido>")
  xmlNfse = xmlNfse & Trim("            <ItemListaServico>1401</ItemListaServico>")
  xmlNfse = xmlNfse & Trim("            <Discriminacao>SERVICO DE CORTE E DOBRA A/c</Discriminacao>")
  xmlNfse = xmlNfse & Trim("            <CodigoMunicipio>1111111</CodigoMunicipio>")
  xmlNfse = xmlNfse & Trim("            <ExigibilidadeISS>1</ExigibilidadeISS>")
  xmlNfse = xmlNfse & Trim("            <MunicipioIncidencia>1111111</MunicipioIncidencia>")
  xmlNfse = xmlNfse & Trim("          </Servico>")
  xmlNfse = xmlNfse & Trim("          <Prestador>")
  xmlNfse = xmlNfse & Trim("            <CpfCnpj>")
  xmlNfse = xmlNfse & Trim("              <Cnpj>00000000000000</Cnpj>")
  xmlNfse = xmlNfse & Trim("            </CpfCnpj>")
  xmlNfse = xmlNfse & Trim("            <InscricaoMunicipal>1111</InscricaoMunicipal>")
  xmlNfse = xmlNfse & Trim("          </Prestador>")
  xmlNfse = xmlNfse & Trim("          <Tomador>")
  xmlNfse = xmlNfse & Trim("            <IdentificacaoTomador>")
  xmlNfse = xmlNfse & Trim("              <CpfCnpj>")
  xmlNfse = xmlNfse & Trim("                <Cnpj>00000000000000</Cnpj>")
  xmlNfse = xmlNfse & Trim("              </CpfCnpj>")
  xmlNfse = xmlNfse & Trim("            </IdentificacaoTomador>")
  xmlNfse = xmlNfse & Trim("            <RazaoSocial>xxxxxxxx ALIMENTOS LTDA</RazaoSocial>")
  xmlNfse = xmlNfse & Trim("            <Endereco>")
  xmlNfse = xmlNfse & Trim("              <Endereco>ROD. xxxxx xxxxxxxxx</Endereco>")
  xmlNfse = xmlNfse & Trim("              <Numero>sn</Numero>")
  xmlNfse = xmlNfse & Trim("              <Bairro>ZONA RURAL</Bairro>")
  xmlNfse = xmlNfse & Trim("              <CodigoMunicipio>1111111</CodigoMunicipio>")
  xmlNfse = xmlNfse & Trim("              <Uf>PR</Uf>")
  xmlNfse = xmlNfse & Trim("              <Cep>87706060</Cep>")
  xmlNfse = xmlNfse & Trim("            </Endereco>")
  xmlNfse = xmlNfse & Trim("            <Contato>")
  xmlNfse = xmlNfse & Trim("              <Telefone>1111111111111</Telefone>")
  xmlNfse = xmlNfse & Trim("              <Email>teste@hotmail.com</Email>")
  xmlNfse = xmlNfse & Trim("            </Contato>")
  xmlNfse = xmlNfse & Trim("          </Tomador>")
  xmlNfse = xmlNfse & Trim("          <RegimeEspecialTributacao>1</RegimeEspecialTributacao>")
  xmlNfse = xmlNfse & Trim("          <OptanteSimplesNacional>2</OptanteSimplesNacional>")
  xmlNfse = xmlNfse & Trim("          <IncentivoFiscal>2</IncentivoFiscal>")
  xmlNfse = xmlNfse & Trim("        </InfDeclaracaoPrestacaoServico>")
  xmlNfse = xmlNfse & Trim("      </Rps>")
  xmlNfse = xmlNfse & Trim("    </ListaRps>")
  xmlNfse = xmlNfse & Trim("  </LoteRps>")
  xmlNfse = xmlNfse & Trim("</EnviarLoteRpsSincronoEnvio>")
      
' Criar objeto para enviar a NFSe para a prefeiturar
  SET oRecepcionarLoteRpsSincrono = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.RecepcionarLoteRpsSincrono")
  
' Enviar NFSe para a prefeitura
  oRecepcionarLoteRpsSincrono.Executar (xmlnfse), (oConfiguracao) 
  
  MsgBox oRecepcionarLoteRpsSincrono.RetornoWSString
	  
' Criar o XML de consulta nfse por RPS
  xmlConsultaRPS = ""
  xmlConsultaRPS = xmlConsultaRPS + Trim("<?xml version=""1.0"" encoding=""utf-8""?>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("<ConsultarNfseRpsEnvio xmlns=""http://www.betha.com.br/e-nota-contribuinte-ws"">")
  xmlConsultaRPS = xmlConsultaRPS + Trim("  <IdentificacaoRps>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    <Numero>24</Numero>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    <Serie>A1</Serie>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    <Tipo>1</Tipo>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("  </IdentificacaoRps>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("  <Prestador>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    <CpfCnpj>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("      <Cnpj>45111111111100</Cnpj>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    </CpfCnpj>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("    <InscricaoMunicipal>123498</InscricaoMunicipal>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("  </Prestador>")
  xmlConsultaRPS = xmlConsultaRPS + Trim("</ConsultarNfseRpsEnvio>")
      
' Consumir o serviço de consulta nfse por RPS        

' Criar objeto de configurações mínimas
  SET oConfigConsulta = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
  oConfigConsulta.TipoDFe = 5 ' TipoDFe.NFSe
  oConfigConsulta.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
  oConfigConsulta.CertificadoSenha = "12345678"   
  oConfigConsulta.TipoAmbiente =  2 ' Homologação
  oConfigConsulta.CodigoMunicipio = 4208302 ' Código do IBGE de Itapema-SC
  oConfigConsulta.Servico = 36  ' Servico.NFSeConsultarNfsePorRps 
  oConfigConsulta.SchemaVersao = "2.02"
      
  SET oConsultarNfsePorRps = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
  oConsultarNfsePorRps.Executar (xmlConsultaRPS), (oConfigConsulta)
      
  MsgBox oConsultarNfsePorRps.RetornoWSString