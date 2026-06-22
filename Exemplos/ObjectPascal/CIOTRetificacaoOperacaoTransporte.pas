// ------------------------------------------------------------------
// CIOT - Retificação da operação de transporte
// ------------------------------------------------------------------
unit CIOTRetificacaoOperacaoTransporte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTRetificacaoOperacaoTransporte = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTRetificacaoOperacaoTransporte.Executar;
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

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporte');
  oXmlCIOT.CodigoIdentificacaoOperacao := '1234567890123456';
  oXmlCIOT.ValorFrete := 1550.75;
  oXmlCIOT.DataFimViagem := EncodeDate(2026, 05, 27);

  oOrigemDestino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.OrigemDestino');
  oOrigemDestino.Origem := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Origem');
  oOrigemDestino.Origem.CodigoMunicipioOrigem := '4118402';
  oOrigemDestino.Origem.CepOrigem := '87700000';
  oOrigemDestino.Destino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Destino');
  oOrigemDestino.Destino.CodigoMunicipioDestino := '4106902';
  oOrigemDestino.Destino.CepDestino := '80000000';
  oXmlCIOT.AddOrigemDestino(IUnknown(oOrigemDestino));

  oDadosCarga := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.DadosCarga');
  oDadosCarga.CodigoNaturezaCarga := '0001';
  oDadosCarga.PesoCarga := '1100.00';
  oDadosCarga.CodigoTipoCarga := 5; // Carga geral
  oXmlCIOT.DadosCarga := IUnknown(oDadosCarga);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte');
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
      ShowMessage(oServico.GetRetificacaoOperacaoTransporteProcResult());
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
