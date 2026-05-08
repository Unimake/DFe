// ------------------------------------------------------------------
// Cancelar NFSe - Padrão ELOTECH
// ------------------------------------------------------------------
unit ELOTECHCancelarNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TELOTECHCancelarNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TELOTECHCancelarNFSe.Executar;
var
  oConfiguracao: olevariant;
  oCancelarNFSe: olevariant;
  oExceptionInterop: olevariant;
  XML: string;
begin
  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; //5=NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 4119905; //Codigo IBGE de Ponta Grossa
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 24; //NFSeCancelarNfse;
    oConfiguracao.SchemaVersao := '2.03'; //Versão de schema deles é a 2.03

    // Montar o XML como string (evita problema com namespace ns0)
    XML := ''; //Modelos do XML da NFSe Elotech: https://www.unimake.com.br/uninfe/modelos.php?p=NFSe%2FELOTECH%2F2.03

    oCancelarNFSe := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.CancelarNFSe');
    oCancelarNFSe.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oCancelarNFSe.RetornoWSString);
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao cancelar NFSe: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
