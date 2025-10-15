// ------------------------------------------------------------------
// Enviar evento de encerramento do MDFe
// ------------------------------------------------------------------
unit EnviarEventoEncerramentoMDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarEventoEncerramentoMDFe = class
  public
    procedure Executar;
  end;

implementation

procedure TEnviarEventoEncerramentoMDFe.Executar;
var
  oConfiguracao, oEventoMDFe: olevariant;
  oRecepcaoEvento: olevariant;
  oExceptionInterop: olevariant;
  eventoAssinado: string;
begin
  // Criar configuração básica para consumir o serviço
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; // 0 = MDFe
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';

  // Criar o XML
  oEventoMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.EventoMDFe');
  oEventoMDFe.Versao := '3.00';

  oEventoMDFe.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfEvento');
  oEventoMDFe.InfEvento.COrgao := 41; // UFBrasil.PR
  oEventoMDFe.InfEvento.ChMDFe := '41200210859283000185570010000005671227070615';
  oEventoMDFe.InfEvento.CNPJ := '10859283000185';
  oEventoMDFe.InfEvento.DhEvento := Now;
  oEventoMDFe.InfEvento.TpEvento := 110112; // TipoEventoNFe.Encerramento ###
  oEventoMDFe.InfEvento.NSeqEvento := 1;
  oEventoMDFe.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  oEventoMDFe.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.DetEventoEncMDFe'); //###
  oEventoMDFe.InfEvento.DetEvento.VersaoEvento := '3.00';
  oEventoMDFe.InfEvento.DetEvento.NProt := '141200000007987';
  oEventoMDFe.InfEvento.DetEvento.CMun := 3106200;
  oEventoMDFe.InfEvento.DetEvento.CUF := 41;
  oEventoMDFe.InfEvento.DetEvento.DtEnc := now;

  // Mostrar informações do evento
  ShowMessage('<versao>: ' + oEventoMDFe.Versao);
  ShowMessage('<cOrgao>: ' + IntToStr(oEventoMDFe.InfEvento.COrgao));
  ShowMessage('<chMDFe>: ' + oEventoMDFe.InfEvento.ChMDFe);
  ShowMessage('<cUF>: ' + IntToStr(oEventoMDFe.InfEvento.DetEvento.CUF));

  // Criar objeto para exceção C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Enviar evento
    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEventoMDFe), IUnknown(oConfiguracao));

    eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();
    ShowMessage(eventoAssinado);

    // Gravar XML assinado no HD
    DeleteFile('d:\testenfe\EncerramentoMDFe.xml');
    with TStringList.Create do
    try
      Text := eventoAssinado;
      SaveToFile('d:\testenfe\EncerramentoMDFe.xml');
    finally
      Free;
    end;

    ShowMessage(oRecepcaoEvento.RetornoWSString);

    ShowMessage('CStat do Lote Retornado: ' + IntToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

    if oRecepcaoEvento.Result.InfEvento.CStat = 135 then // Evento recebido pelo sistema e registrado
    begin
      // Grava o XML de distribuição na pasta informada
      oRecepcaoEvento.GravarXmlDistribuicao('d:\testenfe');
    end
    else
    begin
      // Foi rejeitado, fazer tratamentos.
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
