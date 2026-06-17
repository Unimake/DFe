// ------------------------------------------------------------------
// CIOT - Consultar situação do transportador
// ------------------------------------------------------------------
unit CIOTConsultarSituacaoTransportador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTConsultarSituacaoTransportador = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTConsultarSituacaoTransportador.Executar;
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

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador');
  oXmlCIOT.CpfCnpjInteressado := '12345678000195';
  oXmlCIOT.CpfCnpjTransportador := '12345678901';
  oXmlCIOT.RNTRCTransportador := '012345678';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    if IUnknown(oServico.Result.Temp) <> nil then
        begin
          ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
        end
        else
        ShowMessage('CPF/CNPJ Transportador: ' + oServico.Result.CpfCnpjTransportador + #13#10 +
                    'RNTRC Transportador: ' + oServico.Result.RNTRCTransportador + #13#10 +
                    'Nome/Razão Social: ' + oServico.Result.NomeRazaoSocialTransportador + #13#10 +
                    'Mensagem: ' + oServico.Result.Mensagem);

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
