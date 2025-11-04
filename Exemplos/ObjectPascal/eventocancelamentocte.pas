// ------------------------------------------------------------------
// Enviar o evento de cancelamento da CTe
// ------------------------------------------------------------------
unit EventoCancelamentoCTe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEventoCancelamentoCTe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoCancelamentoCTe.Executar;
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
  oConfiguracao.TipoDFe := 2; //CTe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe');
  oEvento.Versao := '4.00';

  //Criar tag InfEvento
  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento');
  oEvento.InfEvento.COrgao := 41; // UFBrasil.PR
  oEvento.InfEvento.ChCTe := '41200211111111111111111111111111111111111115';
  oEvento.InfEvento.CNPJ := '06117473000150';
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 110111; // TipoEventoCte.Cancelamento
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  //Criar a tag DetEvento
  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoCanc');
  oEvento.InfEvento.DetEvento.VersaoEvento := '4.00';
  oEvento.InfEvento.DetEvento.NProt := '141190000660363';
  oEvento.InfEvento.DetEvento.XJust := 'Justificativa de teste de cancelamento';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      // Enviar evento
      oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento');
      oRecepcaoEvento.Executar(IUnknown(oEvento), IUnknown(oConfiguracao));

      eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();
      ShowMessage(eventoAssinado);

      ShowMessage(oRecepcaoEvento.RetornoWSString);

      ShowMessage('CStat do Lote Retornado: ' + IntToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);
    end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
