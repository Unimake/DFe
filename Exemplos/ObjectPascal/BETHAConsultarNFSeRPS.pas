// ------------------------------------------------------------------
// Consultar NFSe por RPS
// ------------------------------------------------------------------
unit BETHAConsultarNFSeRPS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TBETHAConsultarNFSeRPS = class
  public
    procedure Executar;
  end;

implementation

procedure TBETHAConsultarNFSeRPS.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfsePorRps: OleVariant;
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
    oConfiguracao.Servico := 29; // NFSeConsultarNfsePorRps
    oConfiguracao.SchemaVersao := '2.02';

    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<ConsultarNfseRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">' +
      '  <IdentificacaoRps>' +
      '    <Numero>24</Numero>' +
      '    <Serie>A1</Serie>' +
      '    <Tipo>1</Tipo>' +
      '  </IdentificacaoRps>' +
      '  <Prestador>' +
      '    <CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj>' +
      '    <InscricaoMunicipal>99999999999999</InscricaoMunicipal>' +
      '  </Prestador>' +
      '</ConsultarNfseRpsEnvio>';

    oConsultarNfsePorRps := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps');
    oConsultarNfsePorRps.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfsePorRps.RetornoWSString);

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao consultar NFSe por RPS: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
