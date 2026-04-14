// ------------------------------------------------------------------
// Consultar NFSe por Faixa - Padrão ELOTECH
// ------------------------------------------------------------------
unit ELOTECHConsultarNFSeFaixa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TELOTECHConsultarNFSeFaixa = class
  public
    procedure Executar;
  end;

implementation

procedure TELOTECHConsultarNFSeFaixa.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfseFaixa: OleVariant;
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
    oConfiguracao.Servico := 35; // NFSeConsultarNfseFaixa
    oConfiguracao.SchemaVersao := '2.03'; //Versão de schema deles é a 2.03	 

    XML := ''; //Modelos do XML da NFSe Elotech: https://www.unimake.com.br/uninfe/modelos.php?p=NFSe%2FELOTECH%2F2.03

    oConsultarNfseFaixa := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfseFaixa');
    oConsultarNfseFaixa.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfseFaixa.RetornoWSString);

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
