* ---------------------------------------------------------------------------------
* Enviar Consulta Lote RPS para prefeitura
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultarURLNfse()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oConsultarUrlNfse, cStringXML

 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 5 //5=NFSe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 58 //Servico.NFSeConsultarUrlNfse
   oConfiguracao:CodigoMunicipio = 5300108 //Brasilia - DF
   oConfiguracao:SchemaVersao = "2.04"
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      cStringXML = ""
      cStringXML = cStringXML + AllTrim([<ConsultarUrlNfseEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">])
      cStringXML = cStringXML + AllTrim([	<Pedido>])
      cStringXML = cStringXML + AllTrim([		<Prestador>])
      cStringXML = cStringXML + AllTrim([			<CpfCnpj>])
      cStringXML = cStringXML + AllTrim([				<Cnpj>72582257000100</Cnpj>])
      cStringXML = cStringXML + AllTrim([			</CpfCnpj>])
      cStringXML = cStringXML + AllTrim([			<InscricaoMunicipal>0734507700154</InscricaoMunicipal>])
      cStringXML = cStringXML + AllTrim([		</Prestador>])
      cStringXML = cStringXML + AllTrim([		<IdentificacaoRps>])
      cStringXML = cStringXML + AllTrim([			<Numero>65</Numero>])
      cStringXML = cStringXML + AllTrim([			<Serie>2</Serie>])
      cStringXML = cStringXML + AllTrim([			<Tipo>1</Tipo>])
      cStringXML = cStringXML + AllTrim([		</IdentificacaoRps>])
      cStringXML = cStringXML + AllTrim([		<Pagina>1</Pagina>])
      cStringXML = cStringXML + AllTrim([	</Pedido>])
      cStringXML = cStringXML + AllTrim([</ConsultarUrlNfseEnvio>])
     
	  ? "String do XML:"
	  ?
	  ?
	  ? cStringXML
	  ?
	  ?
	  wait
	  cls 
	  
	  oConsultarUrlNfse := CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarUrlNfse")
      oConsultarUrlNfse:Executar(cStringXML, oConfiguracao)
	  
	  ? oConsultarUrlNfse:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar a NFSe."
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
	  Wait
	  cls   
   End
Return 