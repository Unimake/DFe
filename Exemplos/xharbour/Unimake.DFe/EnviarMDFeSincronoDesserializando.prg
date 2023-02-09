* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarMDFeSincronoDesserializando()
   Local oConfiguracao, oErro
   Local oMDFe, oInfMDFe, oIDE, oInfMunCarrega, oEmit, oEnderEmit
   Local oInfModal, oRodo, oInfANTT, oInfContratante, oVeicTracao, oCondutor
   Local oInfDoc, oInfMunDescarga, oInfCTe, oInfNFe, oInfUnidTransp, oLacUnidTransp
   Local oInfUnidCarga, oLacUnidCarga, oSeg, oInfResp, oInfSeg, oProdPred
   Local oInfLotacao, oInfLocalCarrega, oInfLocalDescarrega, oTot, oLacres, oLacre
   Local oInfAdic, oInfRespTec, oExceptionInterop, oAutorizacao, oConfigRec, oConsReciMDFe
   Local oRetAutorizacao, oProtMDFe
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 4 //4=MDFe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"   
  
 * Criar o XML do MDFe
   oMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   oMDFe = oMDFe:LoadFromFile("D:\testenfe\xharbour\Unimake.DFe\mdfe.xml")
   
   //stringXml = [<?xml version="1.0" encoding="utf-8"?><MDFe xmlns="http://www.portalfiscal.inf.br/mdfe"><infMDFe versao="3.00" Id="MDFe41220706117473000150580010000008611017220673"><ide><cUF>41</cUF><tpAmb>2</tpAmb><tpEmit>1</tpEmit><mod>58</mod><serie>1</serie><nMDF>861</nMDF><cMDF>01722067</cMDF><cDV>3</cDV><modal>1</modal><dhEmi>2022-07-20T17:57:31-03:00</dhEmi><tpEmis>1</tpEmis><procEmi>0</procEmi><verProc>UNICO V8.0</verProc><UFIni>PR</UFIni><UFFim>SP</UFFim><infMunCarrega><cMunCarrega>4118402</cMunCarrega><xMunCarrega>PARANAVAI</xMunCarrega></infMunCarrega><dhIniViagem>2022-07-20T17:57:31-03:00</dhIniViagem></ide><emit><CNPJ>06117473000150</CNPJ><IE>9456656656</IE><xNome>XXXXXX XXXXXX XXXXXX</xNome><xFant>XXXXXX XXXXXX</xFant><enderEmit><xLgr>RUA XXXXXXX X. XX XXXXX</xLgr><nro>01112</nro><xBairro>VILA XXXXXXXXX</xBairro><cMun>4118402</cMun><xMun>PARANAVAI</xMun><CEP>87706000</CEP><UF>PR</UF><fone>04433333333</fone></enderEmit></emit><infModal versaoModal="3.00"><rodo><infANTT><RNTRC>44556666</RNTRC><infContratante><CNPJ>06117473000150</CNPJ></infContratante><infContratante><CNPJ>06117473000150</CNPJ></infContratante></infANTT><veicTracao><cInt>AXF0000</cInt><placa>AXF0000</placa><tara>0</tara><capKG>5000</capKG><prop><CNPJ>06117443000150</CNPJ><RNTRC>44556666</RNTRC><xNome>XXXXXX XXXXXX XXXXXX</xNome><IE>5545546656</IE><UF>PR</UF><tpProp>2</tpProp></prop><condutor><xNome>XXXXXXXXX XXXXX XX XXXXX</xNome><CPF>02133333333</CPF></condutor><tpRod>02</tpRod><tpCar>02</tpCar><UF>PR</UF></veicTracao></rodo></infModal><infDoc><infMunDescarga><cMunDescarga>3505708</cMunDescarga><xMunDescarga>BARUERI</xMunDescarga><infCTe><chCTe>41000000000000000000000000000000000000000006</chCTe></infCTe><infCTe><chCTe>41000000000000000000000000000000000000000004</chCTe></infCTe><infNFe><chNFe>12345678901234567890123456789012345678901234</chNFe><infUnidTransp><tpUnidTransp>2</tpUnidTransp><idUnidTransp>122</idUnidTransp><lacUnidTransp><nLacre>12334</nLacre></lacUnidTransp><infUnidCarga><tpUnidCarga>1</tpUnidCarga><idUnidCarga>123</idUnidCarga><lacUnidCarga><nLacre>3333333</nLacre></lacUnidCarga></infUnidCarga></infUnidTransp></infNFe></infMunDescarga><infMunDescarga><cMunDescarga>3550308</cMunDescarga><xMunDescarga>SAO PAULO</xMunDescarga><infCTe><chCTe>41000000000000000000000000000000000000000000</chCTe></infCTe></infMunDescarga></infDoc><seg><infResp><respSeg>1</respSeg><CNPJ>06117473000150</CNPJ></infResp><infSeg><xSeg>PORTO SEGURO</xSeg><CNPJ>06117473000150</CNPJ></infSeg><nApol>033666565656</nApol><nAver>0000000000000000000000000000000000000000</nAver><nAver>0000000000000000000000000000000000000000</nAver></seg><prodPred><tpCarga>05</tpCarga><xProd>TESTE DE PRODUTO PREDOMINANTE</xProd><infLotacao><infLocalCarrega><CEP>87302080</CEP></infLocalCarrega><infLocalDescarrega><CEP>25650208</CEP></infLocalDescarrega></infLotacao></prodPred><tot><qCTe>3</qCTe><vCarga>56599.09</vCarga><cUnid>01</cUnid><qCarga>2879.0000</qCarga></tot><lacres><nLacre>1111111</nLacre></lacres><lacres><nLacre>2222222</nLacre></lacres><infAdic><infCpl>DATA/HORA PREVISTA PARA O INICO DA VIAGEM: 10/08/2020 as 08:00</infCpl></infAdic><infRespTec><CNPJ>99999999999999</CNPJ><xContato>Teste de Responsavel Tecnico</xContato><email>testey@teste.com.br</email><fone>04431414900</fone></infRespTec></infMDFe></MDFe>]
   stringXml = [<MDFe xmlns="http://www.portalfiscal.inf.br/mdfe">	<infMDFe versao="3.00"	         Id="MDFe43220928066772000145580000000001141000002858"> 		<ide>			<cUF>43</cUF>			<tpAmb>2</tpAmb>			<tpEmit>1</tpEmit>			<mod>58</mod>			<serie>0</serie>			<nMDF>114</nMDF>			<cMDF>00000285</cMDF>			<cDV>8</cDV>			<modal>1</modal>			<dhEmi>2022-09-21T14:07:42-03:00</dhEmi>			<tpEmis>1</tpEmis>			<procEmi>0</procEmi>			<verProc>3.0.14</verProc>			<UFIni>SP</UFIni>			<UFFim>RS</UFFim>			<infMunCarrega>				<cMunCarrega>3510005</cMunCarrega>				<xMunCarrega>CANDIDO MOTA</xMunCarrega>			</infMunCarrega>			<infPercurso>				<UFPer>PR</UFPer>			</infPercurso>			<infPercurso>				<UFPer>SC</UFPer>			</infPercurso>		</ide>		<emit>			<CNPJ>28066772000145</CNPJ>			<IE>0420073353</IE>			<xNome>LION COMERCIO E DISTRIBUIDORA DE BEBIDAS LTDA EPP</xNome>			<enderEmit>				<xLgr>RUA SAO PEDRO</xLgr>				<nro>207</nro>				<xBairro>CENTRO</xBairro>				<cMun>4307609</cMun>				<xMun>ESTANCIA VELHA</xMun>				<CEP>93600000</CEP>				<UF>RS</UF>				<fone>35616681</fone>				<email>faturamento@casadiconti.com.br</email>			</enderEmit>		</emit>		<infModal versaoModal="3.00">			<rodo>				<infANTT>					<RNTRC>51813822</RNTRC>					<infContratante>						<CNPJ>46842894000591</CNPJ>					</infContratante>				</infANTT>				<veicTracao>					<placa>IZI9D67</placa>					<RENAVAM>744632765</RENAVAM>					<tara>19800</tara>					<capKG>19800</capKG>					<condutor>						<xNome>ALEX LUIS DULLIUS</xNome>						<CPF>57681333053</CPF>					</condutor>					<tpRod>01</tpRod>					<tpCar>05</tpCar>					<UF>RS</UF>				</veicTracao>			</rodo>		</infModal>		<infDoc>			<infMunDescarga>				<cMunDescarga>4310801</cMunDescarga>				<xMunDescarga>IVOTI</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037541167183214</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4306403</cMunDescarga>				<xMunDescarga>DOISIRMAOS</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037551176119008</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037561185054786</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4318705</cMunDescarga>				<xMunDescarga>SAOLEOPOLDO</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037571193990565</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037681211862202</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4313409</cMunDescarga>				<xMunDescarga>NOVOHAMBURGO</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037581202926353</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037591211862130</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037611149311723</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037691220797997</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037701149311790</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037721167183360</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037791229733840</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037801158247647</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037811167183432</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037831185055003</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037861211862359</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4319901</cMunDescarga>				<xMunDescarga>SAPIRANGA</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037601140375934</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037621158247509</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037821176119218</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4311643</cMunDescarga>				<xMunDescarga>LINHANOVA</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037631167183280</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4307708</cMunDescarga>				<xMunDescarga>ESTEIO</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037641176119074</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037891238669705</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4316501</cMunDescarga>				<xMunDescarga>SALVADORDOSUL</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037651185054858</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4307609</cMunDescarga>				<xMunDescarga>ESTANCIAVELHA</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037661193990645</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037751193990717</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037761202926491</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037871220798134</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4318481</cMunDescarga>				<xMunDescarga>SAOJOSEDOHORTENCIO</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037671202926425</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4319505</cMunDescarga>				<xMunDescarga>SAOSEBASTIAODOCAI</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037711158247575</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037771211862287</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4320008</cMunDescarga>				<xMunDescarga>SAPUCAIADOSUL</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037731176119146</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037741185054920</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4314050</cMunDescarga>				<xMunDescarga>PAROBE</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037781220798062</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037881229733920</chCTe>				</infCTe>			</infMunDescarga>			<infMunDescarga>				<cMunDescarga>4313060</cMunDescarga>				<xMunDescarga>NOVAHARTZ</xMunDescarga>				<infCTe>					<chCTe>43220928066772000145570010000037841193990783</chCTe>				</infCTe>				<infCTe>					<chCTe>43220928066772000145570010000037851202926563</chCTe>				</infCTe>			</infMunDescarga>		</infDoc>		<seg>			<infResp>				<respSeg>1</respSeg>			</infResp>			<infSeg>				<xSeg>MAPFRE SEGUROS GERAIS SA</xSeg>				<CNPJ>61074175000138</CNPJ>			</infSeg>			<nApol>00212103836001126701</nApol>			<nAver>0000000000000000000000000000000000000000</nAver>		</seg>		<prodPred>			<tpCarga>01</tpCarga>			<xProd>BEBIDAS</xProd>			<cEAN>78982953001505</cEAN>			<NCM>22089000</NCM>		</prodPred>		<tot>			<qCTe>36</qCTe>			<qNFe>0</qNFe>			<qMDFe>0</qMDFe>			<vCarga>62871.86</vCarga>			<cUnid>02</cUnid>			<qCarga>11706.0100</qCarga>		</tot>		<infAdic>			<infAdFisco>LEONARDO</infAdFisco>			<infCpl>192.168.15.7</infCpl>		</infAdic>	</infMDFe></MDFe>]
   oMDFe = oMDFe:LoadFromXML(stringXml)
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   ? "CNPJ Emitente:", oMDFe:InfMDFe:Emit:CNPJ
   ? "Razao Emitente:", oMDFe:InfMDFe:Emit:XNome
   ? "Data Emissao:", oMDFe:InfMDFe:IDE:DhEmi
   ? "Chave do MDFe:", oMDFe:InfMDFe:Chave
   ?
   ? 
   Wait
   ?
   ?
   ?
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try
    * Criar o objeto para consumir o serviço de autorização do MDFe
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")
	  oAutorizacao:SetXMLConfiguracao(oMDFe, oConfiguracao)

	  //O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
	  //caso seja necessário. Imagine que de um problema no envio do MDFe e vc precise resgatar para enviar novamente.
	  ? "Demonstrar o XML do MDFe assinadom: "
	  ?
	  ? oAutorizacao:GetConteudoMDFeAssinado()
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?

      oAutorizacao:Executar(oMDFe, oConfiguracao)   
   
	  ? "XML retornado pela SEFAZ no envio do XML de MDFe:"
	  ?
	  ? oAutorizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
   
	  If oAutorizacao:Result <> NIL
	     ? "Status envio:", oAutorizacao:Result:CStat, oAutorizacao:Result:XMotivo
		 ?
		 ?
		 Wait
		 ?
		 ?
		 ?

         If oAutorizacao:Result:CStat == 104 //Lote processado
            If oAutorizacao:Result:ProtMDFe:InfProt:CStat == 100 //MDFe autorizado
  		       ? "Status da de autorizacao/rejeicao do MDFe:", oAutorizacao:Result:ProtMDFe:InfProt:CStat, oAutorizacao:Result:ProtMDFe:InfProt:CStat
			   ? "Protocolo de autorizacao: ", oAutorizacao:Result:ProtMDFe:InfProt:NProt
			   ?
			   Wait
			   ?
			   ?
			   ?
  	           oAutorizacao:GravarXmlDistribuicao("d:\testenfe")			   			   
			Else   
               //Rejeitado, fazer devidos tratamentos
			Endif
		 Else
             //Rejeitado, fazer devidos tratamentos		 
		 Endif		 
	  Endif   
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
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