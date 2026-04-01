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
  oTConsLoteGNRE: olevariant;
  oConsultaLote: olevariant;
  oExceptionInterop: olevariant;
  situacaoProcess: olevariant;

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
  oConfiguracao.Servico := 22; //Servico.GNREConsultaResultadoLote

  //Criar objeto do XML
  oTConsLoteGNRE := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE');
  oTConsLoteGNRE.Ambiente := 2;
  oTConsLoteGNRE.NumeroRecibo := '1234567890';
  oTConsLoteGNRE.IncluirPDFGuias := 1;
  oTConsLoteGNRE.IncluirArquivoPagamento := 0;
  oTConsLoteGNRE.IncluirNoticias := 0;

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  oConsultaLote := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLote');
  oConsultaLote.Executar(IUnknown(oTConsLoteGNRE), IUnknown(oConfiguracao));

  situacaoProcess := oConsultaLote.Result.SituacaoProcess.Codigo;
  ShowMessage('Situação Processamento: ' + situacaoProcess);

    if (situacaoProcess = '400') or (situacaoProcess = '401') then
    begin
      ShowMessage('O lote ainda está sendo processado. Tente novamente em alguns segundos.');
    end
    else if (situacaoProcess = '402') then
    begin
      try
        oConsultaLote.GravarXmlRetorno('D:\testenfe', oTConsLoteGNRE.NumeroRecibo + '-ret-gnre.xml');

        oConsultaLote.GravarPDFGuia('D:\testenfe', 'GuiaGNRE.pdf');

        ShowMessage('Sucesso! Arquivos salvos em D:\testenfe');
      except
        on E: Exception do
          ShowMessage('Lote processado (402), mas erro ao salvar arquivos: ' + E.Message);
      end;
    end
    else if (situacaoProcess = '403') then
    begin
       ShowMessage('Lote processado com pendências (403). Verifique o XML de retorno.');
       oConsultaLote.GravarXmlRetorno('D:\testenfe', oTConsLoteGNRE.NumeroRecibo + '-ret-gnre.xml');
    end
    else
    begin
      ShowMessage('Processamento concluído com situação: ' + situacaoProcess);
    end;

  except
    on E: Exception do
      ShowMessage('Erro ao consultar lote: ' + E.Message);
  end;
end;

end.
