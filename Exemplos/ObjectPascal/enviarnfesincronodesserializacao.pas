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
    //oEnviNFe.AddNFe(IUnknown(oNFe.LoadFromFile('D:\testenfe\41250206117473000150550590000000301399002691-nfe.xml')));

    //Desserializar o XML a partir de uma string recuperada, por exemplo, do banco de dados ou montada no código
    xmlString := '<?xml version="1.0" encoding="UTF-8" ?><NFe xmlns="http://www.portalfiscal.inf.br/nfe"><infNFe Id="NFe43230492797901000174650200000000021000000577" versao="4.00"><ide><cUF>43</cUF><cNF>00000057</cNF>';
    xmlString := xmlString + '<natOp>VENDA DE MERCADORIA</natOp><mod>65</mod><serie>20</serie><nNF>2</nNF><dhEmi>2023-04-26T10:08:03-03:00</dhEmi><tpNF>1</tpNF><idDest>1</idDest><cMunFG>4314902</cMunFG><tpImp>4</tpImp><tpEmis>1</tpEmis>';
    xmlString := xmlString + '<cDV>7</cDV><tpAmb>2</tpAmb><finNFe>1</finNFe><indFinal>1</indFinal><indPres>1</indPres><procEmi>0</procEmi><verProc>1.4.229</verProc></ide><emit><CNPJ>92797901000174</CNPJ>';
    xmlString := xmlString + '<xNome>GREMIO FOOT-BALL PORTO ALEGRENSE</xNome><xFant>GREMIOMANIA ARENA</xFant><enderEmit><xLgr>AV PADRE LEOPOLDO BRENTANO</xLgr><nro>110/2100</nro><xBairro>HUMAITA</xBairro>';
    xmlString := xmlString + '<cMun>4314902</cMun><xMun>PORTO ALEGRE</xMun><UF>RS</UF><CEP>90250590</CEP><cPais>1058</cPais><xPais>BRASIL</xPais><fone>5132182000</fone></enderEmit><IE>0960578617</IE><CRT>3</CRT></emit>';
    xmlString := xmlString + '<det nItem="1"><prod><cProd>2050000017377</cProd><cEAN>SEM GTIN</cEAN><xProd>NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xProd><NCM>61052000</NCM><CFOP>5102</CFOP>';
    xmlString := xmlString + '<uCom>UN</uCom><qCom>1.0000</qCom><vUnCom>169.9000</vUnCom><vProd>169.90</vProd>';
    xmlString := xmlString + '<cEANTrib>SEM GTIN</cEANTrib><uTrib>UN</uTrib><qTrib>1.0000</qTrib><vUnTrib>169.9000</vUnTrib><indTot>1</indTot></prod><imposto><vTotTrib>37.72</vTotTrib><ICMS><ICMS00><orig>0</orig>';
    xmlString := xmlString + '<CST>00</CST><modBC>0</modBC><vBC>169.90</vBC><pICMS>17.00</pICMS><vICMS>28.88</vICMS></ICMS00></ICMS><PIS><PISNT><CST>07</CST></PISNT></PIS><COFINS><COFINSNT><CST>07</CST></COFINSNT>';
    xmlString := xmlString + '</COFINS></imposto></det><total><ICMSTot><vBC>169.90</vBC><vICMS>28.88</vICMS><vICMSDeson>0.00</vICMSDeson><vFCP>0.00</vFCP><vBCST>0.00</vBCST><vST>0.00</vST><vFCPST>0.00</vFCPST>';
    xmlString := xmlString + '<vFCPSTRet>0.00</vFCPSTRet><vProd>169.90</vProd><vFrete>0.00</vFrete><vSeg>0.00</vSeg><vDesc>0.00</vDesc><vII>0.00</vII><vIPI>0.00</vIPI><vIPIDevol>0.00</vIPIDevol><vPIS>0.00</vPIS>';
    xmlString := xmlString + '<vCOFINS>0.00</vCOFINS><vOutro>0.00</vOutro><vNF>169.90</vNF><vTotTrib>37.72</vTotTrib></ICMSTot></total><transp><modFrete>9</modFrete></transp><pag><detPag><tPag>01</tPag><vPag>169.90</vPag>';
    xmlString := xmlString + '</detPag></pag><infAdic><infCpl>OPERADOR: EDUARDO PERES CX-020;VENDEDOR: MEGASTORE *ARENA;</infCpl></infAdic></infNFe></NFe>';

    oEnviNFe.AddNFe(IUnknown(oNFe.LoadFromXml(xmlString)));

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
