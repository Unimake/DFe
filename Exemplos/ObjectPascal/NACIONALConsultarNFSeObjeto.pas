// ------------------------------------------------------------------
// Consultar NFSe gerando XML a partir de uma classe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarNFSeObjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALConsultarNFSeObjeto = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarNFSeObjeto.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfse: OleVariant;
  oExceptionInterop: OleVariant;
  XML: string;
  oNFSe: OleVariant;
begin
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 32; // NFSeConsultarNfse
    oConfiguracao.SchemaVersao := '1.01';

    oNFSe := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.NFSe');
    oNFSe.Versao := '1.01';
    oNFSe.InfNFSe := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.InfNFSe');
    oNFSe.InfNFSe.Id := 'NFS99999999999999999999999999999999999999999999999999';

    XML := oNFSe.GerarXMLString();

    oConsultarNfse := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse');
    oConsultarNfse.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfse.RetornoWSString);

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

