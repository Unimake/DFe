// ------------------------------------------------------------------
// Enviar BP-e TM modo síncrono
// ------------------------------------------------------------------
unit EnviarBPeTMSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarBPeTMSincrono = class
  private
    function CriarBPeTM: OleVariant;
  public
    procedure Executar;
  end;

implementation

function TEnviarBPeTMSincrono.CriarBPeTM: OleVariant;
var
  oBPeTM: OleVariant;
  oDetBPeTM: OleVariant;
  oDet: OleVariant;
  oComp: OleVariant;
  oPgto: OleVariant;
begin
  oBPeTM := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.BPeTM');
  oBPeTM.InfBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.InfBPe');
  oBPeTM.InfBPe.Versao := '1.00';

  oBPeTM.InfBPe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Ide');
  oBPeTM.InfBPe.Ide.CUF := 35; // São Paulo
  oBPeTM.InfBPe.Ide.TpAmb := 2; // Homologação
  oBPeTM.InfBPe.Ide.&Mod := 63; // BP-e
  oBPeTM.InfBPe.Ide.Serie := 1;
  oBPeTM.InfBPe.Ide.NBP := 1;
  oBPeTM.InfBPe.Ide.CBP := '12345678';
  oBPeTM.InfBPe.Ide.Modal := 1;
  oBPeTM.InfBPe.Ide.DhEmi := EncodeDate(2026, 7, 6) + EncodeTime(8, 0, 0, 0);
  oBPeTM.InfBPe.Ide.DCompet := EncodeDate(2026, 7, 6);
  oBPeTM.InfBPe.Ide.TpEmis := 1;
  oBPeTM.InfBPe.Ide.VerProc := '1.0';
  oBPeTM.InfBPe.Ide.TpBPe := 4;
  oBPeTM.InfBPe.Ide.CFOP := '5353';

  oBPeTM.InfBPe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Emit');
  oBPeTM.InfBPe.Emit.CNPJ := '12345678000195';
  oBPeTM.InfBPe.Emit.IE := '123456789012';
  oBPeTM.InfBPe.Emit.XNome := 'EMPRESA BP-E TM';
  oBPeTM.InfBPe.Emit.CRT := 3; // Regime normal
  oBPeTM.InfBPe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.EnderEmit');
  oBPeTM.InfBPe.Emit.EnderEmit.XLgr := 'RUA TESTE';
  oBPeTM.InfBPe.Emit.EnderEmit.Nro := '100';
  oBPeTM.InfBPe.Emit.EnderEmit.XBairro := 'CENTRO';
  oBPeTM.InfBPe.Emit.EnderEmit.CMun := '3550308';
  oBPeTM.InfBPe.Emit.EnderEmit.XMun := 'SAO PAULO';
  oBPeTM.InfBPe.Emit.EnderEmit.UF := 35; // São Paulo

  oDetBPeTM := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.DetBPeTM');
  oDetBPeTM.IdEqpCont := 1;
  oDetBPeTM.UFIniViagem := 35; // São Paulo

  oDet := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Det');
  oDet.NViagem := 1;
  oDet.CMunIni := '3550308';
  oDet.QPass := String('1');
  oDet.VBP := 10.00;
  oDet.Imp := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Imp');
  oDet.Imp.ICMS := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.ICMS');
  oDet.Imp.ICMS.ICMS00 := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.ICMS00');
  oDet.Imp.ICMS.ICMS00.CST := 0;
  oDet.Imp.ICMS.ICMS00.VBC := 10.00;
  oDet.Imp.ICMS.ICMS00.PICMS := 18.00;
  oDet.Imp.ICMS.ICMS00.VICMS := 1.80;

  oComp := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Comp');
  oComp.XNome := 'TARIFA';
  oComp.QComp := '00001';
  oDet.AddComp(IUnknown(oComp));
  oDetBPeTM.AddDet(IUnknown(oDet));
  oBPeTM.InfBPe.AddDetBPeTM(IUnknown(oDetBPeTM));

  oBPeTM.InfBPe.Total := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Total');
  oBPeTM.InfBPe.Total.QPass := 1;
  oBPeTM.InfBPe.Total.VBP := 10.00;
  oBPeTM.InfBPe.Total.ICMSTot := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.ICMSTot');
  oBPeTM.InfBPe.Total.ICMSTot.VBC := 10.00;
  oBPeTM.InfBPe.Total.ICMSTot.VICMS := 1.80;
  oBPeTM.InfBPe.Total.VTotDFe := olevariant(10.00);

  oBPeTM.InfBPe.PgtoVinc := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.PgtoVinc');
  oPgto := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Pgto');
  oPgto.NPag := String('1');
  oPgto.IdTransacao := 'TRANSACAO123';
  oPgto.TpMeioPgto := 17;
  oPgto.CNPJReceb := '12345678000195';
  oPgto.CNPJBasePSP := '12345678';
  oBPeTM.InfBPe.PgtoVinc.AddPgto(IUnknown(oPgto));

  Result := oBPeTM;
end;

procedure TEnviarBPeTMSincrono.Executar;
var
  oConfiguracao: OleVariant;
  oBPeTM: OleVariant;
  oAutorizacao: OleVariant;
  oExceptionInterop: OleVariant;
  xmlDistribuicao: string;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 22; // 22 = BPe
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oBPeTM := CriarBPeTM;
    ShowMessage(oBPeTM.GerarXMLString());

    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM');
    oAutorizacao.Executar(IUnknown(oBPeTM), IUnknown(oConfiguracao));

    ShowMessage(oAutorizacao.RetornoWSString);

    if (oAutorizacao.Result.CStat = 100) and (IUnknown(oAutorizacao.Result.ProtBPe) <> nil) and
       (oAutorizacao.Result.ProtBPe.InfProt.CStat = 100) then
    begin
      ShowMessage(oAutorizacao.Result.ProtBPe.InfProt.NProt);
      oAutorizacao.GravarXmlDistribuicao('d:\testenfe');
      xmlDistribuicao := VarToStr(oAutorizacao.GetBPeTMProcResults(oBPeTM.InfBPe.Chave));
      ShowMessage(xmlDistribuicao);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erro Lazarus: ' + E.Message);
      ShowMessage('CSHARP - ErrorCode: ' + IntToStr(oExceptionInterop.GetErrorCode));
      ShowMessage('CSHARP - Message: ' + oExceptionInterop.GetMessage);
    end;
  end;
end;

end.
