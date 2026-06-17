// ------------------------------------------------------------------
// CIOT - Cancelamento da operação de transporte
// ------------------------------------------------------------------
unit CIOTCancelamentoOperacaoTransporte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTCancelamentoOperacaoTransporte = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTCancelamentoOperacaoTransporte.Executar;
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

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporte');
  oXmlCIOT.CodigoIdentificacaoOperacao := '1234567890123456';
  oXmlCIOT.MotivoCancelamento := 'Operacao nao realizada';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    if IUnknown(oServico.Result.Temp) <> nil then
    begin
      ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
    end
    else
    begin
      ShowMessage('Protocolo: ' + oServico.Result.Protocolo + #13#10 +
                      'Código Identificação Operação: ' + oServico.Result.CodigoIdentificacaoOperacao + #13#10 +
                      'Mensagem: ' + oServico.Result.Mensagem);

          oServico.GravarXmlDistribuicao('d:\testenfe\xmlciot');
          ShowMessage(oServico.GetCancelamentoOperacaoTransporteProcResult());
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
