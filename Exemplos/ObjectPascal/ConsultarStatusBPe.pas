// ------------------------------------------------------------------
// Consulta status do serviço do BP-e
// ------------------------------------------------------------------
unit ConsultarStatusBPe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarStatusBPe = class
  public
    procedure Executar;
  end;

implementation

procedure TConsultarStatusBPe.Executar;
var
  oConfiguracao: OleVariant;
  oConsStatServBPe: OleVariant;
  oStatusServico: OleVariant;
  oExceptionInterop: OleVariant;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 22; // 22 = BPe
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 41; // Paraná
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oConsStatServBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.ConsStatServBPe');
  oConsStatServBPe.TpAmb := 2; // Homologação
  oConsStatServBPe.Versao := '1.00';

  ShowMessage(oConsStatServBPe.TpAmb);
  ShowMessage(oConsStatServBPe.Versao);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.StatusServico');
    oStatusServico.Executar(IUnknown(oConsStatServBPe), IUnknown(oConfiguracao));

    ShowMessage(oStatusServico.RetornoWSString);
    ShowMessage(IntToStr(oStatusServico.Result.CStat) + ' - ' + oStatusServico.Result.XMotivo);
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
