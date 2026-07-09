// ------------------------------------------------------------------
// Enviar BP-e TA modo síncrono
// ------------------------------------------------------------------
unit EnviarBPeTASincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarBPeTASincrono = class
  private
    function CriarBPeTA: OleVariant;
  public
    procedure Executar;
  end;

implementation

function TEnviarBPeTASincrono.CriarBPeTA: OleVariant;
var
  oBPeTA: OleVariant;
  oInfViagem: OleVariant;
  oCompValor: OleVariant;
  oPag: OleVariant;
begin
  oBPeTA := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.BPeTA');
  oBPeTA.InfBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.InfBPe');
  oBPeTA.InfBPe.Versao := '1.00';

  oBPeTA.InfBPe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.Ide');
  oBPeTA.InfBPe.Ide.CUF := 35; // São Paulo
  oBPeTA.InfBPe.Ide.TpAmb := 2; // Homologação
  oBPeTA.InfBPe.Ide.&Mod := 63; // BP-e
  oBPeTA.InfBPe.Ide.Serie := 1;
  oBPeTA.InfBPe.Ide.NBP := 1;
  oBPeTA.InfBPe.Ide.CBP := '12345678';
  oBPeTA.InfBPe.Ide.Modal := 2;
  oBPeTA.InfBPe.Ide.DhEmi := EncodeDate(2026, 7, 6) + EncodeTime(8, 0, 0, 0);
  oBPeTA.InfBPe.Ide.TpEmis := 1;
  oBPeTA.InfBPe.Ide.VerProc := '1.0';
  oBPeTA.InfBPe.Ide.TpBPe := 0;
  oBPeTA.InfBPe.Ide.TpCompra := 0;
  oBPeTA.InfBPe.Ide.IndPres := 1;
  oBPeTA.InfBPe.Ide.UFIni := 35; // São Paulo
  oBPeTA.InfBPe.Ide.CMunIni := '3550308';
  oBPeTA.InfBPe.Ide.UFFim := 33; // Rio de Janeiro
  oBPeTA.InfBPe.Ide.CMunFim := '3304557';

  oBPeTA.InfBPe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.Emit');
  oBPeTA.InfBPe.Emit.CNPJ := '12345678000195';
  oBPeTA.InfBPe.Emit.IE := '123456789012';
  oBPeTA.InfBPe.Emit.XNome := 'EMPRESA BP-E TA';
  oBPeTA.InfBPe.Emit.CRT := 3; // Regime normal
  oBPeTA.InfBPe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.EnderEmit');
  oBPeTA.InfBPe.Emit.EnderEmit.XLgr := 'RUA TESTE';
  oBPeTA.InfBPe.Emit.EnderEmit.Nro := '100';
  oBPeTA.InfBPe.Emit.EnderEmit.XBairro := 'CENTRO';
  oBPeTA.InfBPe.Emit.EnderEmit.CMun := '3550308';
  oBPeTA.InfBPe.Emit.EnderEmit.XMun := 'SAO PAULO';
  oBPeTA.InfBPe.Emit.EnderEmit.UF := 35; // São Paulo

  oBPeTA.InfBPe.InfPassagem := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.InfPassagem');
  oBPeTA.InfBPe.InfPassagem.DhEmb := EncodeDate(2026, 7, 6) + EncodeTime(10, 0, 0, 0);
  oBPeTA.InfBPe.InfPassagem.DhValidade := EncodeDate(2026, 7, 7) + EncodeTime(10, 0, 0, 0);
  oBPeTA.InfBPe.InfPassagem.InfPassageiro := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.InfPassageiro');
  oBPeTA.InfBPe.InfPassagem.InfPassageiro.XNome := 'PASSAGEIRO TESTE';
  oBPeTA.InfBPe.InfPassagem.InfPassageiro.CPF := '12345678901';
  oBPeTA.InfBPe.InfPassagem.InfPassageiro.TpDoc := 1;
  oBPeTA.InfBPe.InfPassagem.InfPassageiro.NDoc := '12345678';

  oInfViagem := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.InfViagem');
  oInfViagem.NroVoo := '1234';
  oInfViagem.SiglaCiaOperVoo := 'UMK';
  oInfViagem.TpViagem := 0;
  oInfViagem.CAeroOrig := 'GRU';
  oInfViagem.CAeroDest := 'GIG';
  oInfViagem.TpServ := 12;
  oInfViagem.TpAcomodacao := 6;
  oInfViagem.TpTrecho := 1;
  oInfViagem.DhViagem := EncodeDate(2026, 7, 6) + EncodeTime(10, 0, 0, 0);
  oBPeTA.InfBPe.AddInfViagem(IUnknown(oInfViagem));

  oBPeTA.InfBPe.InfValorBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.InfValorBPe');
  oBPeTA.InfBPe.InfValorBPe.VBP := 100.00;
  oBPeTA.InfBPe.InfValorBPe.VDesconto := 0.00;
  oBPeTA.InfBPe.InfValorBPe.VPgto := 100.00;
  oBPeTA.InfBPe.InfValorBPe.VTroco := 0.00;

  oCompValor := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.CompValor');
  oCompValor.TpComp := 1;
  oCompValor.VComp := 100.00;
  oBPeTA.InfBPe.InfValorBPe.AddComp(IUnknown(oCompValor));

  oBPeTA.InfBPe.Imp := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.Imp');
  oBPeTA.InfBPe.Imp.IBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.IBSCBS');
  oBPeTA.InfBPe.Imp.IBSCBS.CST := '000';
  oBPeTA.InfBPe.Imp.IBSCBS.CClassTrib := '000001';
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.BPeTM.GIBSCBS');
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.VBC := 100.00;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSUF := CreateOleObject('Unimake.Business.DFe.Xml.BPe.GIBSUF');
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSUF.PIBSUF := 0.1000;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSUF.VIBSUF := 0.10;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSMun := CreateOleObject('Unimake.Business.DFe.Xml.BPe.GIBSMun');
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSMun.PIBSMun := 0.1000;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GIBSMun.VIBSMun := 0.10;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.VIBS := 0.20;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GCBS := CreateOleObject('Unimake.Business.DFe.Xml.BPe.GCBS');
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GCBS.PCBS := 0.1000;
  oBPeTA.InfBPe.Imp.IBSCBS.GIBSCBS.GCBS.VCBS := 0.10;
  oBPeTA.InfBPe.Imp.VTotDFe := 100.30;

  oPag := CreateOleObject('Unimake.Business.DFe.Xml.BPeTA.Pag');
  oPag.TPag := 1;
  oPag.VPag := 100.00;
  oBPeTA.InfBPe.AddPag(IUnknown(oPag));

  Result := oBPeTA;
end;

procedure TEnviarBPeTASincrono.Executar;
var
  oConfiguracao: OleVariant;
  oBPeTA: OleVariant;
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
    oBPeTA := CriarBPeTA;
    ShowMessage(oBPeTA.GerarXMLString());

    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA');
    oAutorizacao.Executar(IUnknown(oBPeTA), IUnknown(oConfiguracao));

    ShowMessage(oAutorizacao.RetornoWSString);

    if (oAutorizacao.Result.CStat = 100) and (IUnknown(oAutorizacao.Result.ProtBPe) <> nil) and
       (oAutorizacao.Result.ProtBPe.InfProt.CStat = 100) then
    begin
      ShowMessage(oAutorizacao.Result.ProtBPe.InfProt.NProt);
      oAutorizacao.GravarXmlDistribuicao('d:\testenfe');
      xmlDistribuicao := VarToStr(oAutorizacao.GetBPeTAProcResults(oBPeTA.InfBPe.Chave));
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
