// ------------------------------------------------------------------
// CIOT - Encerramento da operação de transporte
// ------------------------------------------------------------------
unit CIOTEncerramentoOperacaoTransporte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTEncerramentoOperacaoTransporte = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTEncerramentoOperacaoTransporte.Executar;
var
  oConfiguracao: OleVariant;
  oXmlCIOT: OleVariant;
  oOrigemDestino: OleVariant;
  oDadosCarga: OleVariant;
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

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporte');
  oXmlCIOT.CodigoIdentificacaoOperacao := '1234567890123456';

  oOrigemDestino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.OrigemDestino');
  oOrigemDestino.Origem := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Origem');
  oOrigemDestino.Origem.CodigoMunicipioOrigem := '4118402';
  oOrigemDestino.Origem.CepOrigem := '87700000';
  oOrigemDestino.Destino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Destino');
  oOrigemDestino.Destino.CodigoMunicipioDestino := '4106902';
  oOrigemDestino.Destino.CepDestino := '80000000';
  oOrigemDestino.DistanciaPercorrida := '500';
  oOrigemDestino.QtdViagens := String('1');
  oXmlCIOT.AddOrigemDestino(IUnknown(oOrigemDestino));

  oDadosCarga := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.DadosCargaEncerramento');
  oDadosCarga.PesoTotalCarga := '1000.00';
  oXmlCIOT.DadosCarga := IUnknown(oDadosCarga);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte');
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
      ShowMessage(oServico.GetEncerramentoOperacaoTransporteProcResult());
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
