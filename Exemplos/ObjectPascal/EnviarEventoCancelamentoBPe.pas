// ------------------------------------------------------------------
// Enviar evento de cancelamento do BP-e
// ------------------------------------------------------------------
unit EnviarEventoCancelamentoBPe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TEnviarEventoCancelamentoBPe = class
  private
    function CriarEventoBPe: OleVariant;
  public
    procedure Executar;
  end;

implementation

function TEnviarEventoCancelamentoBPe.CriarEventoBPe: OleVariant;
var
  oEventoBPe: OleVariant;
begin
  oEventoBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.EventoBPe');
  oEventoBPe.Versao := '1.00';
  oEventoBPe.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.BPe.InfEventoBPe');
  oEventoBPe.InfEvento.COrgao := 41; // Paraná
  oEventoBPe.InfEvento.TpAmb := 2; // Homologação
  oEventoBPe.InfEvento.CNPJ := '00000000000199';
  oEventoBPe.InfEvento.ChBPe := '35260712345678000195630010000000011123456780';
  oEventoBPe.InfEvento.DhEvento := Now;
  oEventoBPe.InfEvento.TpEvento := 110111; // Cancelamento
  oEventoBPe.InfEvento.NSeqEvento := 1;
  oEventoBPe.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.BPe.DetEventoBPe');
  oEventoBPe.InfEvento.DetEvento.VersaoEvento := '1.00';
  oEventoBPe.InfEvento.DetEvento.EvCancBPe := CreateOleObject('Unimake.Business.DFe.Xml.BPe.EvCancBPe');
  oEventoBPe.InfEvento.DetEvento.EvCancBPe.NProt := '123456789012345';
  oEventoBPe.InfEvento.DetEvento.EvCancBPe.XJust := 'Justificativa de teste valida';

  Result := oEventoBPe;
end;

procedure TEnviarEventoCancelamentoBPe.Executar;
var
  oConfiguracao: OleVariant;
  oEventoBPe: OleVariant;
  oRecepcaoEvento: OleVariant;
  oExceptionInterop: OleVariant;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 22; // 22 = BPe
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oEventoBPe := CriarEventoBPe;
    ShowMessage(oEventoBPe.GerarXMLString());

    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.BPe.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEventoBPe), IUnknown(oConfiguracao));

    ShowMessage(oRecepcaoEvento.RetornoWSString);
    ShowMessage(IntToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

    case oRecepcaoEvento.Result.InfEvento.CStat of
      134, 135, 136:
        oRecepcaoEvento.GravarXmlDistribuicao('d:\testenfe');
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
