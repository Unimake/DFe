// ------------------------------------------------------------------
// Consultar NFSe por RPS - Padrão ELOTECH
// ------------------------------------------------------------------
unit ELOTECHConsultarNFSeRPS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TELOTECHConsultarNFSeRPS = class
  public
    procedure Executar;
  end;

implementation

procedure TELOTECHConsultarNFSeRPS.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfsePorRps: OleVariant;
  oExceptionInterop: OleVariant;
  XML: string;
begin
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; //5=NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 4119905; //Codigo IBGE de Ponta Grossa
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 36; // NFSeConsultarNfsePorRps
    oConfiguracao.SchemaVersao := '2.03'; //Versão de schema deles é a 2.03	 

    XML := ''; //Modelos do XML da NFSe Elotech: https://www.unimake.com.br/uninfe/modelos.php?p=NFSe%2FELOTECH%2F2.03

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
