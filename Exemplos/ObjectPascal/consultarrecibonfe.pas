// ------------------------------------------------------------------
// Consulta recibo NFe
// ------------------------------------------------------------------
unit ConsultarReciboNFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarReciboNFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarReciboNFe.Executar;
var
  oConfiguracao: olevariant;
  oConsReciNFe: olevariant;
  oRetAutorizacao: olevariant;
  oExceptionInterop: olevariant;

  X: Integer;
  oProtNFe: OleVariant;
  nHandle: TFileStream;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsReciNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ConsReciNFe');
  oConsReciNFe.Versao := '4.00';
  oConsReciNFe.TpAmb  := 2;  // Homologação
  oConsReciNFe.NRec  := '351210140351219'; // Número do recibo

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oRetAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.RetAutorizacao');
    oRetAutorizacao.Executar(IUnknown(oConsReciNFe), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    ShowMessage(oRetAutorizacao.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oRetAutorizacao.Result.CStat) + ' - ' + oRetAutorizacao.Result.XMotivo);

    //Extrair os eventos retornados na consulta situação da nota
    if oRetAutorizacao.Result.GetProtNFeCount() > 0 then
      begin
        for X := 1 to oRetAutorizacao.Result.GetProtNFeCount() do
        begin
          oProtNFe := oRetAutorizacao.Result.GetProtNFe(X - 1);

          ShowMessage('Status: ' + oProtNFe.InfProt.CStat + ' - ' + oProtNFe.InfProt.XMotivo);
          ShowMessage('Protocolo de autorização: ' + oProtNFe.InfProt.NProt);
        end;
      end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
