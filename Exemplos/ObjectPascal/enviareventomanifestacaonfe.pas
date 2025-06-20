// ------------------------------------------------------------------
// Enviar evento de manifestação da NFe
// ------------------------------------------------------------------
unit EnviarEventoManifestacaoNFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarEventoManifestacaoNFe = class
  public
    procedure Executar;
  end;

implementation

procedure TEnviarEventoManifestacaoNFe.Executar;
var
  oConfiguracao, oEnvEvento, oEvento, oDetEventoManif, oInfEvento: OleVariant;
  oRecepcaoEvento, oRetEvento: OleVariant;
  oExceptionInterop: OleVariant;
  oTagEvento: OleVariant;
  eventoAssinado: string;
  I: Integer;
begin
  // Criar configuração básica para consumir o serviço
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; // 0 = NFe
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';

  // Criar tag EnvEvento
  oEnvEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnvEvento');
  oEnvEvento.Versao := '1.00';
  oEnvEvento.IdLote := '000000000000001';

  // Criar evento - sequência 1
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Evento');
  oEvento.Versao := '1.00';

  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfEvento');

  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.DetEventoManif');
  oEvento.InfEvento.DetEvento.Versao := '1.00';
  oEvento.InfEvento.DetEvento.DescEvento := 'Ciencia da Operacao';
  //oEvento.InfEvento.DetEvento.XJust := 'Justificativa...' // Apenas para alguns tipos de evento

  oEvento.InfEvento.COrgao := 91; // Ambiente Nacional
  oEvento.InfEvento.ChNFe := '41191006117473000150550010000579281779843610';
  oEvento.InfEvento.CNPJ := '06117473000150';
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 210210; // Ciência da operação
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.VerEvento := '1.00';
  oEvento.InfEvento.TpAmb := 2; // Homologação

  oEnvEvento.AddEvento(IUnknown(oEvento));

  // Mostrar informações do evento
  ShowMessage(oEnvEvento.Versao + ' ' + oEnvEvento.IdLote);
  ShowMessage('Qde eventos: ' + IntToStr(oEnvEvento.GetEventoCount()));

  for I := 1 to oEnvEvento.GetEventoCount do
  begin
    oTagEvento := oEnvEvento.GetEvento(I - 1);
    ShowMessage('Sequência evento: ' + IntToStr(oTagEvento.InfEvento.NSeqEvento) +
                ' - Órgão: ' + IntToStr(oTagEvento.InfEvento.COrgao));
  end;

  // Criar objeto para exceção C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Enviar evento
    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEnvEvento), IUnknown(oConfiguracao));

    eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado;
    ShowMessage(eventoAssinado);

    // Gravar XML assinado no HD
    DeleteFile('d:\testenfe\ManifestacaoNFe.xml');
    with TStringList.Create do
    try
      Text := eventoAssinado;
      SaveToFile('d:\testenfe\ManifestacaoNFe.xml');
    finally
      Free;
    end;

    ShowMessage('CStat do lote retornado: ' + IntToStr(oRecepcaoEvento.Result.CStat) +
                ' - xMotivo: ' + oRecepcaoEvento.Result.XMotivo);
    ShowMessage(oRecepcaoEvento.RetornoWSString);

    if oRecepcaoEvento.Result.CStat = 128 then
    begin
      for I := 1 to oRecepcaoEvento.Result.GetRetEventoCount do
      begin
        oRetEvento := oRecepcaoEvento.Result.GetRetEvento(I - 1);

        case oRetEvento.InfEvento.CStat of
          135, 136, 155:
            begin
              oRecepcaoEvento.GravarXmlDistribuicao('d:\testenfe');
            end;
        else
          // Evento rejeitado, tomar ação apropriada se necessário
        end;

        ShowMessage('CStat do evento ' + IntToStr(I) + ': ' +
                    IntToStr(oRetEvento.InfEvento.CStat) +
                    ' - xMotivo: ' + oRetEvento.InfEvento.XMotivo);
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro Lazarus: ' + E.Message);
      ShowMessage('CSHARP - ErrorCode: ' + IntToStr(oExceptionInterop.GetErrorCode));
      ShowMessage('CSHARP - Message: ' + oExceptionInterop.GetMessage);
    end;
  end;
end;

end.
