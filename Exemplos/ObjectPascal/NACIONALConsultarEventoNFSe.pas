// ------------------------------------------------------------------
// Consultar Eventos NFSe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarEventoNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALConsultarEventoNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarEventoNFSe.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarEvento: OleVariant;
  oExceptionInterop: OleVariant;
  oConsPedRegEvento: OleVariant;
  XML: string;
begin
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\projetos\certificados\XXX.pfx';
    oConfiguracao.CertificadoSenha := 'Mh26';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 1; // Produção
    oConfiguracao.Servico := 92; //NFSeConsultarEventosDiversos
    oConfiguracao.SchemaVersao := '1.01';

    oConsPedRegEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento');
    oConsPedRegEvento.Versao := '1.01';
    oConsPedRegEvento.InfConsPedRegEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfConsPedRegEvento');
    oConsPedRegEvento.InfConsPedRegEvento.ChNFSe := '41055082234180727000110000000000017026025846920130';
    oConsPedRegEvento.InfConsPedRegEvento.TipoEvento := '101101'; //Cancelamento
    oConsPedRegEvento.InfConsPedRegEvento.NumSeqEvento := '001';

    XML := oConsPedRegEvento.GerarXmlString();

    // Gravar XML Retornado no HD
    DeleteFile('d:\testenfe\ConsultaEventoNFSeNacional.xml');
    with TStringList.Create do
    try
      Text := XML;
      SaveToFile('d:\testenfe\ConsultaEventoNFSeNacional.xml');
    finally
      Free;
    end;

    ShowMessage(XML);

    oConsultarEvento := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarEvento');
    oConsultarEvento.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarEvento.RetornoWSString);

    // Gravar XML Retornado no HD
    DeleteFile('d:\testenfe\ConsultaEventoNFSeNacional_retorno.xml');
    with TStringList.Create do
    try
      Text := oConsultarEvento.RetornoWSString;
      SaveToFile('d:\testenfe\ConsultaEventoNFSeNacional_retorno.xml');
    finally
      Free;
    end;

    if IUnknown(oConsultarEvento.Result) <> nil then //Evento foi homologado na Receita
    begin
      if IUnknown(oConsultarEvento.Result.Erro) <> nil then
      begin
        ShowMessage('Algo deu errado: ' + oConsultarEvento.Result.Erro.Descricao + ' - ' + oConsultarEvento.Result.Erro.Codigo);
      end
      else
      begin
       ShowMessage('Chave de acesso da NFSe: ' + oConsultarEvento.Result.Eventos.ChaveAcesso);
       ShowMessage('Tipo Evento: ' + oConsultarEvento.Result.Eventos.TipoEvento);
       ShowMessage('VerAplic: ' + oConsultarEvento.Result.Eventos.ArquivoXml.Evento.InfEvento.VerAplic);
       ShowMessage('Id Evento: ' + oConsultarEvento.Result.Eventos.ArquivoXml.Evento.InfEvento.Id);
       ShowMessage('Id Pedido de registro do Evento: ' + oConsultarEvento.Result.Eventos.ArquivoXml.Evento.InfEvento.PedRegEvento.InfPedReg.Id);
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
