// ------------------------------------------------------------------
// Enviar NFCom no modo síncrono
// ------------------------------------------------------------------

unit EnviarNFComSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarNFComSincrono = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarNFComSincrono.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  oNFCom: olevariant;
  oDet: olevariant;

  oAutorizacaoSinc: olevariant;

  notaAssinada: string;
  caminhoArquivo: string;

  xmlRetornado: string;
  statusRetorno: string;
  docProcNFe: string;
  numeroProtocolo: string;

  i: integer;
begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 15; //15=NFCom ###
  oConfiguracao.TipoEmissao := 1; //1=Normal
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar a tag <enviNFe>
    oNFCom := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.NFCom');

    // Criar a tag <NFe>
    oNFCom.InfNFCom := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.InfNFCom');
    oNFCom.InfNFCom.Versao := '1.00';

    // Cria tag Ide
    oNFCom.InfNFCom.Ide := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Ide');

    oNFCom.InfNFCom.Ide.CUF := 41; // Brasil.PR
    oNFCom.InfNFCom.Ide.TpAmb := 2; // TipoAmbiente.Homologacao
    oNFCom.InfNFCom.Ide.&Mod := 62; // ModeloDFe.NFCom
    oNFCom.InfNFCom.Ide.Serie := 59;
    oNFCom.InfNFCom.Ide.NNF := 5;
    oNFCom.InfNFCom.Ide.DhEmi := Now;
    oNFCom.InfNFCom.Ide.TpEmis := 1; // TipoEmissao.Normal
    oNFCom.InfNFCom.Ide.NSiteAutoriz := string('0');
    oNFCom.InfNFCom.Ide.CMunFG := 3132404;
    oNFCom.InfNFCom.Ide.FinNFCom := 0; // FinalidadeNFCom.Normal
    oNFCom.InfNFCom.Ide.TpFat := 0; // TipoFaturamentoNFCom.FaturamentoNormal
    oNFCom.InfNFCom.Ide.VerProc := 'TESTE 1.00';

    // criar tag Emit
    oNFCom.InfNFCom.Emit := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Emit');
    oNFCom.InfNFCom.Emit.CNPJ := '06117473000150';
    oNFCom.InfNFCom.Emit.IE := '9032000301';
    oNFCom.InfNFCom.Emit.XNome := 'UNIMAKE SOLUCOES CORPORATIVAS LTDA';
    oNFCom.InfNFCom.Emit.XFant := 'UNIMAKE - PARANAVAI';
    oNFCom.InfNFCom.Emit.CRT := 1; //1=Simples Nacional

    oNFCom.InfNFCom.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.EnderEmit');
    oNFCom.InfNFCom.Emit.EnderEmit.XLgr := 'RUA PAULO ANTONIO COSTA';
    oNFCom.InfNFCom.Emit.EnderEmit.Nro := '575';
    oNFCom.InfNFCom.Emit.EnderEmit.XCpl := 'teste';
    oNFCom.InfNFCom.Emit.EnderEmit.XBairro := 'CENTRO';
    oNFCom.InfNFCom.Emit.EnderEmit.CMun := 4118402;
    oNFCom.InfNFCom.Emit.EnderEmit.XMun := 'PARANAVAI';
    oNFCom.InfNFCom.Emit.EnderEmit.UF := 41; // UFBrasil.PR
    oNFCom.InfNFCom.Emit.EnderEmit.CEP := '87707210';
    oNFCom.InfNFCom.Emit.EnderEmit.Fone := '04431421010';

    // criar tag Dest
    oNFCom.InfNFCom.Dest := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Dest');
    oNFCom.InfNFCom.Dest.CNPJ := '11111111111111';
    oNFCom.InfNFCom.Dest.XNome := 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oNFCom.InfNFCom.Dest.IndIEDest := 9; // IndicadorIEDestinatario.NaoContribuinte

    oNFCom.InfNFCom.Dest.EnderDest := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.EnderDest');
    oNFCom.InfNFCom.Dest.EnderDest.XLgr := 'XXXXXXX XX XXXXXXX';
    oNFCom.InfNFCom.Dest.EnderDest.Nro := '1111';
    oNFCom.InfNFCom.Dest.EnderDest.XBairro := 'XXXXXX XXXXXX';
    oNFCom.InfNFCom.Dest.EnderDest.CMun := 3543402;
    oNFCom.InfNFCom.Dest.EnderDest.XMun := 'XXXXXXXX XXXXX';
    oNFCom.InfNFCom.Dest.EnderDest.UF := 35; // UFBrasil.SP
    oNFCom.InfNFCom.Dest.EnderDest.CEP := '14080000';
    oNFCom.InfNFCom.Dest.EnderDest.Fone := '01231111111';
    oNFCom.InfNFCom.Dest.EnderDest.Email := 'teste@teste.com';

    //Criar tag assinante
    oNFCom.InfNFCom.Assinante := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Assinante');
    oNFCom.InfNFCom.Assinante.ICodAssinante := '0095311';
    oNFCom.InfNFCom.Assinante.TpAssinante := 99; //TipoAssinante.Outros
    oNFCom.InfNFCom.Assinante.TpServUtil := 4; //TipoServicoUtilizado.ProvimentoAcessoInternet
    oNFCom.InfNFCom.Assinante.NContrato := '20250095311';
    oNFCom.InfNFCom.Assinante.DContratoIni := now;
    oNFCom.InfNFCom.Assinante.DContratoFim := now;

    for i := 1 to 1 do
    begin
      // criar tag Det
      oDet := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Det');
      oDet.NItem := i;

      oDet.Prod := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Prod');
      oDet.Prod.CProd := '0000' + TrimRight(IntToStr(i));
      oDet.Prod.XProd := 'ACESSO INTERNET';
      oDet.Prod.CClass := '0400501';
      oDet.Prod.CFOP := '5307';
      oDet.Prod.uMed := 2; // UnidadeBasicaMedida.MB
      oDet.Prod.qFaturada := 500;
      oDet.Prod.vItem := 0.2098;
      oDet.Prod.VProd := 104.12345679;

      // criar tag Imposto
      oDet.Imposto := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Imposto');

      // criar tag IcmsSN
      oDet.Imposto.ICMSSN := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.ICMSSN');
      oDet.Imposto.ICMSSN.CST := '90';
      oDet.Imposto.ICMSSN.IndSN := 1; //SimNao.Sim

      // criar tag PIS
      oDet.Imposto.PIS := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.PIS');
      oDet.Imposto.PIS.CST := 49; //CSTPisCofins.OutrasOperacoesDeSaida
      oDet.Imposto.PIS.VBC := olevariant(0.00);
      oDet.Imposto.PIS.PPIS := olevariant(0.00);
      oDet.Imposto.PIS.VPIS := olevariant(0.00);

      // criar tag COFINS
      oDet.Imposto.COFINS := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.COFINS');
      oDet.Imposto.COFINS.CST := 49; //CSTPisCofins.OutrasOperacoesDeSaida;
      oDet.Imposto.COFINS.VBC := olevariant(0.00);
      oDet.Imposto.COFINS.PCOFINS := olevariant(0.00);
      oDet.Imposto.COFINS.VCOFINS := olevariant(0.00);

      // criar tag IBSCBS
      oDet.Imposto.IBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.IBSCBS');
      oDet.Imposto.IBSCBS.CST := '000';
      oDet.Imposto.IBSCBS.CClassTrib := '000001';

      oDet.Imposto.IBSCBS.GIBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.GIBSCBS');
      oDet.Imposto.IBSCBS.GIBSCBS.VBC := 104.90;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.GIBSUF');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.PIBSUF := 0.1000;
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.VIBSUF := 0.10;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.GIBSMUN');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN.PIBSMUN := olevariant(0.00);
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN.VIBSMUN := olevariant(0.00);

      oDet.Imposto.IBSCBS.GIBSCBS.VIBS := 0.10;

      oDet.Imposto.IBSCBS.GIBSCBS.GCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.GCBS');
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.PCBS := 0.9000;
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.VCBS := 0.94;

      // adicionar a tag Det dentro da tag InfNFCom
      oNFCom.InfNFCom.AddDet(IUnknown(oDet));
    end;

    // Criar tag Total
    oNFCom.InfNFCom.Total := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.Total');
    oNFCom.InfNFCom.Total.VProd := FormatFloat('0.00', 104.90);

    oNFCom.InfNFCom.Total.ICMSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.ICMSTot');
    oNFCom.InfNFCom.Total.ICMSTot.vBC := olevariant(0.00);
    oNFCom.InfNFCom.Total.ICMSTot.vICMS := olevariant(0.00);
    oNFCom.InfNFCom.Total.ICMSTot.vICMSDeson := olevariant(0.00);
    oNFCom.InfNFCom.Total.ICMSTot.vFCP := olevariant(0.00);

    oNFCom.InfNFCom.Total.vCOFINS := olevariant(0.00);
    oNFCom.InfNFCom.Total.vPIS := olevariant(0.00);
    oNFCom.InfNFCom.Total.vFUNTTEL := olevariant(0.00);
    oNFCom.InfNFCom.Total.vFUST := olevariant(0.00);

    oNFCom.InfNFCom.Total.vRetTribTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.vRetTribTot');
    oNFCom.InfNFCom.Total.vRetTribTot.vRetPIS := olevariant(0.00);
    oNFCom.InfNFCom.Total.vRetTribTot.vRetCofins := olevariant(0.00);
    oNFCom.InfNFCom.Total.vRetTribTot.vRetCSLL := olevariant(0.00);
    oNFCom.InfNFCom.Total.vRetTribTot.vIRRF := olevariant(0.00);

    oNFCom.InfNFCom.Total.vDesc := olevariant(0.00);
    oNFCom.InfNFCom.Total.vOutro := olevariant(0.00);
    oNFCom.InfNFCom.Total.vNF := currency(104.90);

    oNFCom.InfNFCom.Total.IBSCBSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.IBSCBSTot');
    oNFCom.InfNFCom.Total.IBSCBSTot.vBCIBSCBS := 104.90;

    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.gIBSTot');

    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.gIBSUFTot');
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vDif := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vDevTrib := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vIBSUF := 0.10;

    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.gIBSMunTot');
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vDif := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vDevTrib := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vIBSMun := olevariant(0.00);

    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.vIBS := 0.10;

    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.gCBSTot');
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vDif := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vDevTrib := olevariant(0.00);
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vCBS := 0.94;

    oNFCom.InfNFCom.Total.vTotDFe := 105.95;

    // Criar tag gFat
    oNFCom.InfNFCom.GFat := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.GFat');
    oNFCom.InfNFCom.GFat.CompetFat := '202508';
    oNFCom.InfNFCom.GFat.dVencFat := now;
    oNFCom.InfNFCom.GFat.codBarras := '11111111111111111111111111111111111111111111111';

    // Criar tag infAdic
    oNFCom.InfNFCom.infAdic := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.infAdic');
    oNFCom.InfNFCom.infAdic.AddInfCpl('Optante pelo Simples Nacional conforme Lei Complementar Nº 123/2006.');
    oNFCom.InfNFCom.infAdic.AddInfCpl('Optante pelo Simples Nacional conforme Lei Complementar Nº 123/2006-2');

    // Criar a tag GRestTec
    oNFCom.InfNFCom.gRespTec := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.gRespTec');
    oNFCom.InfNFCom.gRespTec.CNPJ := '99999999999999';
    oNFCom.InfNFCom.gRespTec.xContato := 'XXXXXXX XXXXXX XXXXXXXX';
    oNFCom.InfNFCom.gRespTec.email := 'teste@teste.com.br';
    oNFCom.InfNFCom.gRespTec.fone := '99999999999';

    ShowMessage(oNFCom.InfNFCom.Id);
    ShowMessage(oNFCom.InfNFCom.Chave);

    //Consumir o serviço
    oAutorizacaoSinc := CreateOleObject('Unimake.Business.DFe.Servicos.NFCom.AutorizacaoSinc');
    oAutorizacaoSinc.SetXMLConfiguracao(IUnknown(oNFCom), IUnknown(oConfiguracao));

    //Recuperar o conteúdo do XML assinado
    notaAssinada := VarToStr(oAutorizacaoSinc.GetConteudoXMLAssinado());

    //Exibir o XML assinado
    ShowMessage(notaAssinada);

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oNFCom.InfNFCom.Chave + '-nfcom.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
    finally
      Free;
    end;

    oAutorizacaoSinc.Executar(IUnknown(oNFCom), IUnknown(oConfiguracao));

    // XML Retornado pela SEFAZ
    xmlRetornado := VarToStr(oAutorizacaoSinc.RetornoWSString);
    ShowMessage(xmlRetornado);

    // Código de Status e Motivo
    statusRetorno := Trim(IntToStr(oAutorizacaoSinc.Result.CStat)) + ' ' + VarToStr(oAutorizacaoSinc.Result.XMotivo);
    ShowMessage(statusRetorno);

    // Verifica se a NFCom foi autorizada
    if oAutorizacaoSinc.Result.CStat = 100 then
    begin
      if oAutorizacaoSinc.Result.ProtNFCom.InfProt.CStat = 100 then
      begin
        // Gravar XML de distribuição em uma pasta (NFCom com o protocolo anexado)
        oAutorizacaoSinc.GravarXmlDistribuicao('d:\testenfe');

        // Pegar a string do XML de distribuição
        docProcNFe := VarToStr(oAutorizacaoSinc.GetNFeProcResults(oNFCom.InfNFCom.Chave));
        ShowMessage(docProcNFe);

        // Pegar o número do protocolo de autorização
        numeroProtocolo := VarToStr(oAutorizacaoSinc.Result.ProtNFCom.InfProt.NProt);
        ShowMessage(numeroProtocolo);
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

