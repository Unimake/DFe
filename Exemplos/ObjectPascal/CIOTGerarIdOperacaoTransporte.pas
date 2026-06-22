// ------------------------------------------------------------------
// CIOT - Gerar identificador da operação de transporte
// ------------------------------------------------------------------
unit CIOTGerarIdOperacaoTransporte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TCIOTGerarIdOperacaoTransporte = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTGerarIdOperacaoTransporte.Executar;
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

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte');
  oXmlCIOT.CpfCnpj := '06117473000150';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    ShowMessage('Id Operação Transporte: ' + oServico.Result.IdOperacaoTransporte);

    oServico.GravarXmlDistribuicao('d:\testenfe\xmlciot');
    ShowMessage(oServico.GetGerarIdOperacaoTransporteProcResult());
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
