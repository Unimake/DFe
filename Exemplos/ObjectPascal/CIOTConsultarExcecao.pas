// ------------------------------------------------------------------
// CIOT - Consultar exceção
// ------------------------------------------------------------------
unit CIOTConsultarExcecao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTConsultarExcecao = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTConsultarExcecao.Executar;
var
  oConfiguracao: OleVariant;
  oXmlCIOT: OleVariant;
  oServico: OleVariant;
  oExceptionInterop: OleVariant;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 19; // 19 = CIOT
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 91; // AN
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.ConsultarExcecao');
  oXmlCIOT.CpfCnpjTransportador := '12345678901';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    if IUnknown(oServico.Result.Temp) <> nil then
        begin
          ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
        end
        else
            ShowMessage('Código: ' + oServico.Result.Codigo + #13#10 + 'Mensagem: ' + oServico.Result.Mensagem);

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
