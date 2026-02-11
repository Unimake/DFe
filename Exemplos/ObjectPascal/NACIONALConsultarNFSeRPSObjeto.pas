// ------------------------------------------------------------------
// Consultar NFSe por RPS gerando o XML a partir da classe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarNFSeRPSObjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALConsultarNFSeRPSObjeto = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarNFSeRPSObjeto.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfsePorRps: OleVariant;
  oExceptionInterop: OleVariant;
  oDPS: OleVariant;
  XML: string;
  ChaveAcesso: string;
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
    oConfiguracao.SchemaVersao := '1.01';

    oDPS := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.DPS');
    oDPS.Versao := '1.01';
    oDPS.InfDPS := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.InfDPS');
    oDPS.InfDPS.Id := 'DPS999999999999999999999999999999999999999999';

    XML := oDPS.GerarXmlString();

    ShowMessage(XML);

    oConsultarNfsePorRps := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps');
    oConsultarNfsePorRps.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfsePorRps.RetornoWSString);

    if IUnknown(oConsultarNfsePorRps.Result) <> nil then //Evento foi homologado na Receita
    begin
      if IUnknown(oConsultarNfsePorRps.Result.Erro) <> nil then
      begin
        ShowMessage(oConsultarNfsePorRps.Result.Erro.Descricao + ' - ' + oConsultarNfsePorRps.Result.Erro.Codigo);
      end
      else
      begin
       ChaveAcesso := oConsultarNfsePorRps.Result.ChaveAcesso;

       ShowMessage('Chave de acesso da NFSe: ' + ChaveAcesso);
      end;
    end;
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

