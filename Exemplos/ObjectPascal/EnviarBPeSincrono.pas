// ------------------------------------------------------------------
// Enviar BP-e modo síncrono
// ------------------------------------------------------------------
unit EnviarBPeSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarBPeSincrono = class
  private
    function CriarBPe: OleVariant;
  public
    procedure Executar;
  end;

implementation

function TEnviarBPeSincrono.CriarBPe: OleVariant;
var
  oBPe: OleVariant;
  oInfViagem: OleVariant;
  oCompValor: OleVariant;
  oPag: OleVariant;
begin
  oBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.BPe');
  oBPe.InfBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfBPe');
  oBPe.InfBPe.Versao := '1.00';

  oBPe.InfBPe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.BPe.Ide');
  oBPe.InfBPe.Ide.CUF := 35; // São Paulo
  oBPe.InfBPe.Ide.TpAmb := 2; // Homologação
  oBPe.InfBPe.Ide.&Mod := 63; // BP-e
  oBPe.InfBPe.Ide.Serie := 1;
  oBPe.InfBPe.Ide.NBP := 1;
  oBPe.InfBPe.Ide.CBP := '12345678';
  oBPe.InfBPe.Ide.Modal := 1;
  oBPe.InfBPe.Ide.DhEmi := EncodeDate(2026, 7, 6) + EncodeTime(8, 0, 0, 0);
  oBPe.InfBPe.Ide.TpEmis := 1;
  oBPe.InfBPe.Ide.VerProc := '1.0';
  oBPe.InfBPe.Ide.TpBPe := 0;
  oBPe.InfBPe.Ide.IndPres := 1;
  oBPe.InfBPe.Ide.UFIni := 35; // São Paulo
  oBPe.InfBPe.Ide.CMunIni := '3550308';
  oBPe.InfBPe.Ide.UFFim := 33; // Rio de Janeiro
  oBPe.InfBPe.Ide.CMunFim := '3304557';

  oBPe.InfBPe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Emit');
  oBPe.InfBPe.Emit.CNPJ := '12345678000195';
  oBPe.InfBPe.Emit.IE := '123456789012';
  oBPe.InfBPe.Emit.XNome := 'EMPRESA BP-E';
  oBPe.InfBPe.Emit.CRT := 3; // Regime normal
  oBPe.InfBPe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.EnderEmit');
  oBPe.InfBPe.Emit.EnderEmit.XLgr := 'RUA TESTE';
  oBPe.InfBPe.Emit.EnderEmit.Nro := '100';
  oBPe.InfBPe.Emit.EnderEmit.XBairro := 'CENTRO';
  oBPe.InfBPe.Emit.EnderEmit.CMun := '3550308';
  oBPe.InfBPe.Emit.EnderEmit.XMun := 'SAO PAULO';
  oBPe.InfBPe.Emit.EnderEmit.UF := 35; // São Paulo

  oBPe.InfBPe.InfPassagem := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfPassagem');
  oBPe.InfBPe.InfPassagem.CLocOrig := '3550308';
  oBPe.InfBPe.InfPassagem.XLocOrig := 'SAO PAULO';
  oBPe.InfBPe.InfPassagem.CLocDest := '3304557';
  oBPe.InfBPe.InfPassagem.XLocDest := 'RIO DE JANEIRO';
  oBPe.InfBPe.InfPassagem.DhEmb := EncodeDate(2026, 7, 6) + EncodeTime(10, 0, 0, 0);
  oBPe.InfBPe.InfPassagem.DhValidade := EncodeDate(2026, 7, 7) + EncodeTime(10, 0, 0, 0);
  oBPe.InfBPe.InfPassagem.InfPassageiro := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfPassageiro');
  oBPe.InfBPe.InfPassagem.InfPassageiro.XNome := 'PASSAGEIRO TESTE';
  oBPe.InfBPe.InfPassagem.InfPassageiro.CPF := '12345678901';
  oBPe.InfBPe.InfPassagem.InfPassageiro.TpDoc := 1;
  oBPe.InfBPe.InfPassagem.InfPassageiro.NDoc := '12345678';

  oInfViagem := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfViagem');
  oInfViagem.CPercurso := '001';
  oInfViagem.XPercurso := 'SAO PAULO - RIO DE JANEIRO';
  oInfViagem.TpViagem := 0;
  oInfViagem.TpServ := 1;
  oInfViagem.TpAcomodacao := 1;
  oInfViagem.TpTrecho := 1;
  oInfViagem.DhViagem := EncodeDate(2026, 7, 6) + EncodeTime(10, 0, 0, 0);
  oBPe.InfBPe.AddInfViagem(IUnknown(oInfViagem));

  oBPe.InfBPe.InfValorBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfValorBPe');
  oBPe.InfBPe.InfValorBPe.VBP := 100.00;
  oBPe.InfBPe.InfValorBPe.VDesconto := 0.00;
  oBPe.InfBPe.InfValorBPe.VPgto := 100.00;
  oBPe.InfBPe.InfValorBPe.VTroco := 0.00;

  oCompValor := CreateOleObject('Unimake.Business.DFe.Xml.BPe.CompValor');
  oCompValor.TpComp := 1;
  oCompValor.VComp := 100.00;
  oBPe.InfBPe.InfValorBPe.AddComp(IUnknown(oCompValor));

  oBPe.InfBPe.Imp := CreateOleObject('Unimake.Business.DFe.Xml.BPe.Imp');
  oBPe.InfBPe.Imp.ICMS := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.ICMS');
  oBPe.InfBPe.Imp.ICMS.ICMS00 := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.ICMS00');
  oBPe.InfBPe.Imp.ICMS.ICMS00.CST := 0;
  oBPe.InfBPe.Imp.ICMS.ICMS00.VBC := 100.00;
  oBPe.InfBPe.Imp.ICMS.ICMS00.PICMS := 18.00;
  oBPe.InfBPe.Imp.ICMS.ICMS00.VICMS := 18.00;

  oPag := CreateOleObject('Unimake.Business.DFe.Xml.BPe.Pag');
  oPag.TPag := 1;
  oPag.VPag := 100.00;
  oBPe.InfBPe.AddPag(IUnknown(oPag));

  Result := oBPe;
end;

procedure TEnviarBPeSincrono.Executar;
var
  oConfiguracao: OleVariant;
  oBPe: OleVariant;
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
    oBPe := CriarBPe;
    ShowMessage(oBPe.GerarXMLString());

    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe');
    oAutorizacao.Executar(IUnknown(oBPe), IUnknown(oConfiguracao));

    ShowMessage(oAutorizacao.RetornoWSString);

    if (oAutorizacao.Result.CStat = 100) and (IUnknown(oAutorizacao.Result.ProtBPe) <> nil) and
       (oAutorizacao.Result.ProtBPe.InfProt.CStat = 100) then
    begin
      ShowMessage(oAutorizacao.Result.ProtBPe.InfProt.NProt);
      oAutorizacao.GravarXmlDistribuicao('d:\testenfe');
      xmlDistribuicao := VarToStr(oAutorizacao.GetBPeProcResults(oBPe.InfBPe.Chave));
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
