// ------------------------------------------------------------------
// Enviar NFe no modo síncrono com Desserialização do XML
// ------------------------------------------------------------------

unit EnviarNFeSincronoDesserializacao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarNFeSincronoDesserializacao = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarNFeSincronoDesserializacao.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  oEnviNFe: olevariant;
  oNfe: olevariant;
  xmlString: string;

  oNFeObj, oInfNFeObj, oRespTecObj: olevariant;

  oConteudoNFe: olevariant;
  oConteudoInfNFe: olevariant;
  chaveNFe: string;

  oAutorizacao: olevariant;

  notaAssinada: string;
  caminhoArquivo: string;

  xmlRetornado: string;
  statusRetorno, motivoRetorno: string;
  docProcNFe: string;
  numeroProtocolo: string;

  i: integer;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.TipoEmissao := 1; //1=Normal
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Criar a tag <enviNFe>
    oEnviNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnviNFe');
    oEnviNFe.Versao := '4.00';
    oEnviNFe.IdLote := '000000000000001';
    oEnviNFe.IndSinc := 1; // 1=Sim 0=Nao

    //Criar a tag <NFe>
    oNfe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.NFe');

    //Desserializar o XML a partir de um arquivo no HD/SSD
    //oEnviNFe.AddNFe(IUnknown(oNFe.LoadFromFile('D:\testenfe\testewandrey-nfe.xml')));

    //Desserializar o XML a partir de uma string recuperada, por exemplo, do banco de dados ou montada no código
    xmlString := '<NFe xmlns="http://www.portalfiscal.inf.br/nfe"><infNFe Id="NFe41250706117473000150550590000000331966278621" versao="4.00"><ide><cUF>41</cUF><cNF>96627862</cNF><natOp>VENDA PRODUC.DO ESTABELEC</natOp><mod>55</mod><serie>59</serie>';
    xmlString := xmlString + '<nNF>36</nNF><dhEmi>2025-07-03T09:44:01-03:00</dhEmi><dhSaiEnt>2025-07-03T09:44:01-03:00</dhSaiEnt><tpNF>1</tpNF><idDest>2</idDest><cMunFG>4118402</cMunFG><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>1</cDV><tpAmb>2</tpAmb>';
    xmlString := xmlString + '<finNFe>1</finNFe><indFinal>1</indFinal><indPres>1</indPres><procEmi>0</procEmi><verProc>TESTE 1.00</verProc></ide><emit><CNPJ>06117473000150</CNPJ><xNome>UNIMAKE SOLUCOES CORPORATIVAS LTDA</xNome><xFant>UNIMAKE - PARANAVAI</xFant><enderEmit>';
    xmlString := xmlString + '<xLgr>RUA PAULO ANTONIO COSTA</xLgr><nro>575</nro><xBairro>CENTRO</xBairro><cMun>4118402</cMun><xMun>PARANAVAI</xMun><UF>PR</UF><CEP>87707210</CEP><cPais>1058</cPais><xPais>BRASIL</xPais><fone>04431421010</fone></enderEmit><IE>9032000301</IE>';
    xmlString := xmlString + '<IM>14018</IM><CNAE>6202300</CNAE><CRT>1</CRT></emit><dest><CNPJ>04218457000128</CNPJ><xNome>NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome><enderDest><xLgr>AVENIDA DA SAUDADE</xLgr><nro>1555</nro><xBairro>CAMPOS ELISEOS</xBairro>';
    xmlString := xmlString + '<cMun>3543402</cMun><xMun>RIBEIRAO PRETO</xMun><UF>SP</UF><CEP>14080000</CEP><cPais>1058</cPais><xPais>BRASIL</xPais><fone>01639611500</fone></enderDest><indIEDest>1</indIEDest><IE>582614838110</IE><email>janelaorp@janelaorp.com.br</email></dest>';
    xmlString := xmlString + '<det nItem="1"><prod><cProd>00001</cProd><cEAN>SEM GTIN</cEAN><xProd>NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd><NCM>84714900</NCM><CFOP>6101</CFOP><uCom>LU</uCom><qCom>1</qCom><vUnCom>84.9</vUnCom><vProd>84.90</vProd>';
    xmlString := xmlString + '<cEANTrib>SEM GTIN</cEANTrib><uTrib>LU</uTrib><qTrib>1</qTrib><vUnTrib>84.9</vUnTrib><indTot>1</indTot><xPed>300474</xPed><nItemPed>1</nItemPed></prod><imposto><vTotTrib>12.63</vTotTrib><ICMS><ICMSSN101><orig>0</orig><CSOSN>101</CSOSN>';
    xmlString := xmlString + '<pCredSN>2.8255</pCredSN><vCredICMSSN>2.40</vCredICMSSN></ICMSSN101></ICMS><PIS><PISOutr><CST>99</CST><vBC>0.00</vBC><pPIS>0.0000</pPIS><vPIS>0.00</vPIS></PISOutr></PIS><COFINS><COFINSOutr><CST>99</CST><vBC>0.00</vBC><pCOFINS>0.0000</pCOFINS>';
    xmlString := xmlString + '<vCOFINS>0.00</vCOFINS></COFINSOutr></COFINS></imposto></det><det nItem="2"><prod><cProd>00002</cProd><cEAN>SEM GTIN</cEAN><xProd>NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd><NCM>84714900</NCM><CFOP>6101</CFOP>';
    xmlString := xmlString + '<uCom>LU</uCom><qCom>1</qCom><vUnCom>84.9</vUnCom><vProd>84.90</vProd><cEANTrib>SEM GTIN</cEANTrib><uTrib>LU</uTrib><qTrib>1</qTrib><vUnTrib>84.9</vUnTrib><indTot>1</indTot><xPed>300474</xPed><nItemPed>1</nItemPed></prod><imposto>';
    xmlString := xmlString + '<vTotTrib>12.63</vTotTrib><ICMS><ICMSSN101><orig>0</orig><CSOSN>101</CSOSN><pCredSN>2.8255</pCredSN><vCredICMSSN>2.40</vCredICMSSN></ICMSSN101></ICMS><PIS><PISOutr><CST>99</CST><vBC>0.00</vBC><pPIS>0.0000</pPIS><vPIS>0.00</vPIS></PISOutr>';
    xmlString := xmlString + '</PIS><COFINS><COFINSOutr><CST>99</CST><vBC>0.00</vBC><pCOFINS>0.0000</pCOFINS><vCOFINS>0.00</vCOFINS></COFINSOutr></COFINS></imposto></det><det nItem="3"><prod><cProd>00003</cProd><cEAN>SEM GTIN</cEAN><xProd>NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd>';
    xmlString := xmlString + '<NCM>84714900</NCM><CFOP>6101</CFOP><uCom>LU</uCom><qCom>1</qCom><vUnCom>84.9</vUnCom><vProd>84.90</vProd><cEANTrib>SEM GTIN</cEANTrib><uTrib>LU</uTrib><qTrib>1</qTrib><vUnTrib>84.9</vUnTrib><indTot>1</indTot><xPed>300474</xPed>';
    xmlString := xmlString + '<nItemPed>1</nItemPed></prod><imposto><vTotTrib>12.63</vTotTrib><ICMS><ICMSSN101><orig>0</orig><CSOSN>101</CSOSN><pCredSN>2.8255</pCredSN><vCredICMSSN>2.40</vCredICMSSN></ICMSSN101></ICMS><PIS><PISOutr><CST>99</CST><vBC>0.00</vBC>';
    xmlString := xmlString + '<pPIS>0.0000</pPIS><vPIS>0.00</vPIS></PISOutr></PIS><COFINS><COFINSOutr><CST>99</CST><vBC>0.00</vBC><pCOFINS>0.0000</pCOFINS><vCOFINS>0.00</vCOFINS></COFINSOutr></COFINS></imposto></det><total><ICMSTot><vBC>0.00</vBC><vICMS>0.00</vICMS>';
    xmlString := xmlString + '<vICMSDeson>0.00</vICMSDeson><vFCP>0.00</vFCP><vBCST>0.00</vBCST><vST>0.00</vST><vFCPST>0.00</vFCPST><vFCPSTRet>0.00</vFCPSTRet><vProd>254.70</vProd><vFrete>0.00</vFrete><vSeg>0.00</vSeg><vDesc>0.00</vDesc><vII>0.00</vII><vIPI>0.00</vIPI>';
    xmlString := xmlString + '<vIPIDevol>0.00</vIPIDevol><vPIS>0.00</vPIS><vCOFINS>0.00</vCOFINS><vOutro>0.00</vOutro><vNF>254.70</vNF><vTotTrib>37.89</vTotTrib></ICMSTot></total><transp><modFrete>0</modFrete><vol><qVol>1</qVol><esp>LU</esp><marca>UNIMAKE</marca></vol>';
    xmlString := xmlString + '<vol><qVol>1</qVol><esp>LU</esp><marca>UNIMAKE</marca></vol><vol><qVol>1</qVol><esp>LU</esp><marca>UNIMAKE</marca></vol></transp><cobr><fat><nFat>057910</nFat><vOrig>254.70</vOrig><vDesc>0.00</vDesc><vLiq>254.70</vLiq></fat><dup><nDup>001</nDup>';
    xmlString := xmlString + '<dVenc>2025-07-03</dVenc><vDup>127.35</vDup></dup><dup><nDup>002</nDup><dVenc>2025-07-03</dVenc><vDup>127.35</vDup></dup></cobr><pag><detPag><indPag>0</indPag><tPag>01</tPag><vPag>254.70</vPag></detPag></pag><infAdic>';
    xmlString := xmlString + '<infCpl>Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008</infCpl></infAdic><infRespTec><CNPJ>06117473000150</CNPJ><xContato>Ze das Couves</xContato><email>zedascouves@gmail.com</email><fone>04430000000</fone>';
    xmlString := xmlString + '</infRespTec></infNFe></NFe>';
    oEnviNFe.AddNFe(IUnknown(oNFe.LoadFromXml(xmlString)));

    //Atualizar o CSRT para gerar corretamente o hashCSRT
    oNFeObj := oEnviNFe.GetNFe(0);
    oInfNFeObj := oNFeObj.GetInfNFe(0);
    oRespTecObj := oInfNFeObj.InfRespTec;
    oRespTecObj.hashCSRT := '';
    oRespTecObj.IdCSRT := '01';
    oRespTecObj.CSRT := '8WCARAO9D8P00R845TARUPPTGY5CL40WS3J1';

    //Recuperar a chave da NFe
    oConteudoNFe := oEnviNFe.GetNFe(0);
    oConteudoInfNFe := olevariant(oConteudoNFe.GetInfNFe(0));
    chaveNFe := VarToStr(oConteudoInfNFe.Chave);

    ShowMessage('Chave NFe: ' + chaveNFe);

    //Recuperar outras informações da NFe
    ShowMessage(VarToStr(oConteudoInfNFe.Ide.cUF));
    ShowMessage(VarToStr(oConteudoInfNFe.Emit.XNome));

    //Consumir o serviço
    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.Autorizacao');
    oAutorizacao.SetXMLConfiguracao(IUnknown(oEnviNFe), IUnknown(oConfiguracao));

    //Recuperar o conteúdo do XML assinado
    notaAssinada := VarToStr(oAutorizacao.GetConteudoNFeAssinada(0));

    //Exibir o XML assinado
    ShowMessage(notaAssinada);

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + chaveNFe + '-nfe.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
      try
        WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
      finally
        Free;
      end;

    oAutorizacao.Executar(IUnknown(oEnviNFe), IUnknown(oConfiguracao));

    // XML Retornado pela SEFAZ
    xmlRetornado := VarToStr(oAutorizacao.RetornoWSString);
    ShowMessage(xmlRetornado);

    // Código de Status e Motivo
    statusRetorno := Trim(IntToStr(oAutorizacao.Result.CStat)) + ' ' + VarToStr(oAutorizacao.Result.XMotivo);
    ShowMessage(statusRetorno);

    // Verifica se o lote foi processado (CStat = 104)
    if oAutorizacao.Result.CStat = 104 then
    begin
      // Verifica se a NF-e foi autorizada (CStat = 100)
      if oAutorizacao.Result.ProtNFe.InfProt.CStat = 100 then
      begin
        // Gravar XML de distribuição em uma pasta (NFe com o protocolo anexado)
        oAutorizacao.GravarXmlDistribuicao('d:\testenfe');

        // Pegar a string do XML de distribuição
        docProcNFe := VarToStr(oAutorizacao.GetNFeProcResults(chaveNFe));
        ShowMessage(docProcNFe);

        // Pegar o número do protocolo de autorização
        numeroProtocolo := VarToStr(oAutorizacao.Result.ProtNFe.InfProt.NProt);
        ShowMessage(numeroProtocolo);
      end
      else
      begin
        // Rejeitada ou Denegada - Fazer devidos tratamentos
        ShowMessage('NF-e rejeitada ou denegada. Verifique os detalhes.');
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
