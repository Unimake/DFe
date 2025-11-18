// ------------------------------------------------------------------
// Consulta status do serviço da CTe
// ------------------------------------------------------------------
unit ConsultaResultadoLoteGNRE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultaResultadoLoteGNRE = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultaResultadoLoteGNRE.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oXml: olevariant;
  oConsultaLote: olevariant;
  oExceptionInterop: olevariant;
  sSituacaoProcess:olevariant;

begin
  try
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 8;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.TipoEmissao := 1;
  oConfiguracao.TipoAmbiente := 2;
  oConfiguracao.CodigoUF:= 41;

  //Criar objeto do XML
  oXml := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE');
  oXml.Ambiente := 2;
  oXml.NumeroRecibo := '1234567890';
  oXml.IncluirPDFGuias := 1;
  oXml.IncluirArquivoPagamento := 0;
  oXml.IncluirNoticias := 0;

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  oConsultaLote := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLote');
  oConsultaLote.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

  sSituacaoProcess := oConsultaLote.Result.SituacaoProcess.Codigo;
  ShowMessage('Situação Processamento: ' + sSituacaoProcess);

    if (sSituacaoProcess = '400') or (sSituacaoProcess = '401') then
    begin
      ShowMessage('O lote ainda está sendo processado. Tente novamente em alguns segundos.');
    end
    else if (sSituacaoProcess = '402') then
    begin
      try
        oConsultaLote.GravarXmlRetorno('D:\testenfe', oXml.NumeroRecibo + '-ret-gnre.xml');

        oConsultaLote.GravarPDFGuia('D:\testenfe', 'GuiaGNRE.pdf');

        ShowMessage('Sucesso! Arquivos salvos em D:\testenfe');
      except
        on E: Exception do
          ShowMessage('Lote processado (402), mas erro ao salvar arquivos: ' + E.Message);
      end;
    end
    else if (sSituacaoProcess = '403') then
    begin
       ShowMessage('Lote processado com pendências (403). Verifique o XML de retorno.');
       oConsultaLote.GravarXmlRetorno('D:\testenfe', oXml.NumeroRecibo + '-ret-gnre.xml');
    end
    else
    begin
      ShowMessage('Processamento concluído com situação: ' + sSituacaoProcess);
    end;

  except
    on E: Exception do
      ShowMessage('Erro ao consultar lote: ' + E.Message);
  end;
end;

end.
