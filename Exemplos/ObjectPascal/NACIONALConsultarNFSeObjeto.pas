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
    oConfiguracao.CertificadoArquivo := 'C:\projetos\certificados\XXX.pfx';
    oConfiguracao.CertificadoSenha := 'Mh26';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 1; // Homologação
    oConfiguracao.Servico := 32; // NFSeConsultarNfse
    oConfiguracao.SchemaVersao := '1.01';

    oNFSe := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.NFSe');
    oNFSe.Versao := '1.01';
    oNFSe.InfNFSe := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.InfNFSe');
    oNFSe.InfNFSe.ID := 'NFS41055082234180727000110000000000017026025846920130';

    XML := oNFSe.GerarXmlString();

    oConsultarNfse := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse');
    oConsultarNfse.Executar(XML, IUnknown(oConfiguracao));

    // Gravar XML Retornado no HD
    DeleteFile('d:\testenfe\ConsultaNFSeNacional_retorno.xml');
    with TStringList.Create do
    try
      Text := oConsultarNfse.RetornoWSString;
      SaveToFile('d:\testenfe\ConsultaNFSeNacional_retorno.xml');
    finally
      Free;
    end;

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfse.RetornoWSString);

    if IUnknown(oConsultarNfse.Result) <> nil then
        begin
          if oConsultarNfse.Result.InfNFSe.CStat = 102 then //100 é NFSe normal e 102 é NFSe com autorização judicial de isenção de imposto.
          begin
             ShowMessage(oConsultarNfse.Result.InfNFSe.Id);
             ShowMessage(oConsultarNfse.Result.InfNFSe.xTribNac);
             ShowMessage(oConsultarNfse.Result.InfNFSe.nNFSe);
          end;
        end
        else
        begin
          if IUnknown(oConsultarNfse.ResultErro) <> nil then
          begin
            if IUnknown(oConsultarNfse.ResultErro.Erro) <> nil then
            begin
              ShowMessage(oConsultarNfse.ResultErro.Erro.Descricao + ' - ' + oConsultarNfse.ResultErro.Erro.Codigo);
            end;
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

