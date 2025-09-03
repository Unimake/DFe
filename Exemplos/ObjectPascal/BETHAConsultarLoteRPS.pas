// ------------------------------------------------------------------
// Consultar Lote RPS
// ------------------------------------------------------------------
unit BETHAConsultarLoteRPS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TBETHAConsultarLoteRPS = class
  public
    procedure Executar;
  end;

implementation

procedure TBETHAConsultarLoteRPS.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarLoteRps: OleVariant;
  oExceptionInterop: OleVariant;
  XML: string;
begin
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';
    oConfiguracao.CodigoMunicipio := 9999903;
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 30; // NFSeConsultarLoteRps
    oConfiguracao.SchemaVersao := '2.02';

    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<ConsultarLoteRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">' +
      '  <Prestador>' +
      '    <CpfCnpj>' +
      '      <Cnpj>99999999999999</Cnpj>' +
      '    </CpfCnpj>' +
      '    <InscricaoMunicipal>99999999999999</InscricaoMunicipal>' +
      '  </Prestador>' +
      '  <Protocolo>9999999999999999999999999999</Protocolo>' +
      '</ConsultarLoteRpsEnvio>';

    oConsultarLoteRps := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarLoteRps');
    oConsultarLoteRps.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarLoteRps.RetornoWSString);

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao consultar lote RPS: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
