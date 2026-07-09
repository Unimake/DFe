// ------------------------------------------------------------------
// Consulta situação do BP-e
// ------------------------------------------------------------------
unit ConsultarSituacaoBPe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarSituacaoBPe = class
  public
    procedure Executar;
  end;

implementation

procedure TConsultarSituacaoBPe.Executar;
var
  oConfiguracao: OleVariant;
  oConsSitBPe: OleVariant;
  oConsultaProtocolo: OleVariant;
  oExceptionInterop: OleVariant;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 22; // 22 = BPe
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 41; // Paraná
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oConsSitBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.ConsSitBPe');
  oConsSitBPe.ChBPe := '35260712345678000195630010000000011123456780';
  oConsSitBPe.TpAmb := 2; // Homologação
  oConsSitBPe.Versao := '1.00';

  ShowMessage(oConsSitBPe.ChBPe);
  ShowMessage(oConsSitBPe.TpAmb);
  ShowMessage(oConsSitBPe.Versao);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConsultaProtocolo := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo');
    oConsultaProtocolo.Executar(IUnknown(oConsSitBPe), IUnknown(oConfiguracao));

    ShowMessage(oConsultaProtocolo.RetornoWSString);
    ShowMessage(IntToStr(oConsultaProtocolo.Result.CStat) + ' - ' + oConsultaProtocolo.Result.XMotivo);
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
