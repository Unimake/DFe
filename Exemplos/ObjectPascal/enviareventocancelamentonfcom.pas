// ------------------------------------------------------------------
// Enviar evento de cancelamento do NFCom
// ------------------------------------------------------------------
unit EnviarEventoCancelamentoNFCom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarEventoCancelamentoNFCom = class
  public
    procedure Executar;
  end;

implementation

procedure TEnviarEventoCancelamentoNFCom.Executar;
var
  oConfiguracao, oEventoNFCom: olevariant;
  oRecepcaoEvento: olevariant;
  oExceptionInterop: olevariant;
  eventoAssinado: string;
begin
  // Criar configuração básica para consumir o serviço
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 15; // NFCom
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.TipoEmissao:= 1;

  // Criar o XML
  oEventoNFCom := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.EventoNFCom');
  oEventoNFCom.Versao := '1.00';

  oEventoNFCom.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.InfEvento');
  oEventoNFCom.InfEvento.COrgao := 41; // UFBrasil.PR
  oEventoNFCom.InfEvento.ChNFCom := '12345678901234567890123456789012345678901234';
  oEventoNFCom.InfEvento.CNPJ := '10859283000185';
  oEventoNFCom.InfEvento.DhEvento := Now;
  oEventoNFCom.InfEvento.TpEvento := 110111; // TipoEventoNFe.Cancelamento
  oEventoNFCom.InfEvento.NSeqEvento := 1;
  oEventoNFCom.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  oEventoNFCom.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.DetEventoCanc');
  oEventoNFCom.InfEvento.DetEvento.VersaoEvento := '1.00';
  oEventoNFCom.InfEvento.DetEvento.NProt := '1234567890123456';
  oEventoNFCom.InfEvento.DetEvento.XJust := 'Erro na criacao do produto';
  oEventoNFCom.InfEvento.DetEvento.DescEvento:= 'Cancelamento';

  // Criar objeto para exceção C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Enviar evento
    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.NFCom.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEventoNFCom), IUnknown(oConfiguracao));

    eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();
    ShowMessage(eventoAssinado);

    ShowMessage(oRecepcaoEvento.RetornoWSString);

    ShowMessage('CStat do Lote Retornado: ' + IntToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

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
