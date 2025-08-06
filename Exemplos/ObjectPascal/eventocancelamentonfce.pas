// ------------------------------------------------------------------
// Enviar o evento de cancelamento da NFCe
// ------------------------------------------------------------------
unit EventoCancelamentoNFCe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEventoCancelamentoNFCe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoCancelamentoNFCe.Executar;
var
  oConfiguracao: olevariant;
  oEnvEvento: olevariant;
  oEvento: olevariant;
  oExceptionInterop: olevariant;

  I: integer;
  oTagEvento: olevariant;
  oRecepcaoEvento: olevariant;
  oRetEvento: olevariant;
  eventoAssinado: string;
  nHandle: TFileStream;
  nomeArquivoEvento: string;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 1; //1=NFCe ###
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oEnvEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnvEvento');
  oEnvEvento.Versao := '1.00';
  oEnvEvento.IdLote := '000000000000001';

  //Criar tag Evento - Sequencia 1
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Evento');
  oEvento.Versao := '1.00';

  //Criar tag InfEvento
  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfEvento');
  oEvento.InfEvento.COrgao := 41; // UFBrasil.PR
  oEvento.InfEvento.ChNFe := '41250706117473000150650590000000061771093890';
  oEvento.InfEvento.CNPJ := '06117473000150';
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 110111; // TipoEventoNFe.Cancelamento
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.VerEvento := '1.00';
  oEvento.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  //Criar a tag DetEvento
  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFe.DetEventoCanc');
  oEvento.InfEvento.DetEvento.Versao := '1.00';
  oEvento.InfEvento.DetEvento.NProt := '141250000245318';
  oEvento.InfEvento.DetEvento.XJust := 'Justificativa para cancelamento da NFCe de teste';

  //Adicionar a o objeto oEvento dentro do oEnvEvento
  oEnvEvento.AddEvento(IUnknown(oEvento));

  ShowMessage(oEnvEvento.Versao + ' ' + oEnvEvento.IdLote);
  ShowMessage('Qde eventos: ' + IntToStr(oEnvEvento.GetEventoCount()));

  for I := 1 to oEnvEvento.GetEventoCount() do
  begin
    oTagEvento := oEnvEvento.GetEvento(I - 1);
    ShowMessage(IntToStr(I) + ' ' + VarToStr(oTagEvento.InfEvento.NSeqEvento) + ' ' + VarToStr(oTagEvento.InfEvento.COrgao));
  end;

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      // Enviar evento
      oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento'); //###
      oRecepcaoEvento.Executar(IUnknown(oEnvEvento), IUnknown(oConfiguracao));

      eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

      // Criar e salvar o arquivo do evento assinado
      nomeArquivoEvento := 'd:\testenfe\CancelamentoNFe.xml';
      nHandle := TFileStream.Create(nomeArquivoEvento, fmCreate);
      try
        nHandle.WriteBuffer(Pointer(eventoAssinado)^, Length(eventoAssinado));
      finally
        nHandle.Free;
      end;

      ShowMessage('CStat do Lote Retornado: ' + VarToStr(oRecepcaoEvento.Result.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.XMotivo);

      if oRecepcaoEvento.Result.CStat = 128 then // 128 = Lote de evento processado com sucesso
      begin
        // Loop para verificar cada evento retornado
        for I := 1 to oRecepcaoEvento.Result.GetRetEventoCount() do
        begin
          oRetEvento := oRecepcaoEvento.Result.GetRetEvento(I - 1);

          ShowMessage(VarToStr(oRetEvento.InfEvento.CStat) + ' ' +  oRetEvento.InfEvento.XMotivo);
          case oRetEvento.InfEvento.CStat of
            135, 136, 155: // Eventos homologados
            begin
              oRecepcaoEvento.GravarXmlDistribuicao('d:\testenfe'); // Grava o XML de distribuição
              Break;
            end;
            else
              // Evento rejeitado - Realizar as ações necessárias
              Break;
          end;

          ShowMessage('CStat do evento ' + IntToStr(I) + ' retornado: ' + oRetEvento.InfEvento.CStat + ' - xMotivo: ' + oRetEvento.InfEvento.XMotivo);
        end;
      end;
    end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
