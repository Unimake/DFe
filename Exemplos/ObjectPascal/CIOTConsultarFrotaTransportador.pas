// ------------------------------------------------------------------
// CIOT - Consultar frota do transportador
// ------------------------------------------------------------------
unit CIOTConsultarFrotaTransportador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TCIOTConsultarFrotaTransportador = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTConsultarFrotaTransportador.Executar;
var
  oConfiguracao: OleVariant;
  oXmlCIOT: OleVariant;
  oServico: OleVariant;
  oExceptionInterop: OleVariant;
  oVeiculoFrota: OleVariant;
  I: Integer;
  frota: string;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 19; // 19 = CIOT
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 91; // AN
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.ConsultarFrotaTransportador');
  oXmlCIOT.CpfCnpjInteressado := '12345678000195';
  oXmlCIOT.CpfCnpjTransportador := '12345678901';
  oXmlCIOT.RNTRCTransportador := '012345678';
  oXmlCIOT.AddPlacas('ABC1D23');

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.ConsultarFrotaTransportador');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

     if IUnknown(oServico.Result.Temp) <> nil then
        begin
          ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
        end
        else
        begin
          frota := '';
          for I := 0 to oServico.Result.GetFrotaCount - 1 do
          begin
            oVeiculoFrota := oServico.Result.GetFrota(I);
            frota := frota + oVeiculoFrota.PlacaVeiculo + ' - ' + BoolToStr(oVeiculoFrota.SituacaoVeiculoFrotaTransportador, True) + #13#10;
          end;

          ShowMessage('CPF/CNPJ Transportador: ' + oServico.Result.CpfCnpjTransportador + #13#10 +
                      'RNTRC Transportador: ' + oServico.Result.RNTRCTransportador + #13#10 +
                      'Nome/Razão Social: ' + oServico.Result.NomeRazaoSocialTransportador + #13#10 +
                      'Frota: ' + #13#10 + frota);
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
