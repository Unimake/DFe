// ------------------------------------------------------------------
// CIOT - Consultar CIOT gerado
// ------------------------------------------------------------------
unit CIOTConsultarCIOTGerado;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTConsultarCIOTGerado = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTConsultarCIOTGerado.Executar;
var
  oConfiguracao: OleVariant;
  oXmlCIOT: OleVariant;
  oServico: OleVariant;
  oExceptionInterop: OleVariant;
  I: Integer;
  mensagens: string;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 19; // 19 = CIOT
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 91; // AN
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.ConsultarCIOTGerado');
  oXmlCIOT.CodigoIdentificacaoOperacao := '123456789012';
  oXmlCIOT.AnoDeclaracao := 2026;

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    if IUnknown(oServico.Result.Temp) <> nil then
    begin
      ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
    end
    else
    begin
      mensagens := '';

      for I := 0 to oServico.Result.GetMensagemCount - 1 do
      begin
        mensagens := mensagens + oServico.Result.GetMensagem(I) + #13#10;
      end;

      ShowMessage('Código Identificação Operação: ' + oServico.Result.CodigoIdentificacaoOperacao + #13#10 + 'Mensagens: ' + #13#10 + mensagens);
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
