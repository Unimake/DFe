// ------------------------------------------------------------------
// Consultar NFSe por RPS - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarNFSeRPS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALConsultarNFSeRPS = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarNFSeRPS.Executar;
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

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 36; // NFSeConsultarNfsePorRps
    oConfiguracao.SchemaVersao := '1.00';

    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<DPS versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">' +
      '	<infDPS Id="DPS999999999999999999999999999999999999999999"/>' +
      '</DPS>';

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

