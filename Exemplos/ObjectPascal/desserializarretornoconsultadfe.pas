// ------------------------------------------------------------------
// Desserializar o Retorno da Consulta DFe
// ------------------------------------------------------------------
unit DesserializarRetornoConsultaDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TDesserializarRetornoConsultaDFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TDesserializarRetornoConsultaDFe.Executar;
var
  // Declarar objetos
  oRetDistDFeInt: olevariant;
  oDocZip, oExceptionInterop: olevariant;
  I: integer;
begin
  //Criar objeto do XML
  oRetDistDFeInt := CreateOleObject('Unimake.Business.DFe.Xml.NFe.RetDistDFeInt');
  oRetDistDFeInt := oRetDistDFeInt.LoadFromFile('D:\testenfe\RetornoDfe-000000000000000.xml');

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
     if oRetDistDFeInt.CStat = 138 then
      begin
        for I := 1 to oRetDistDFeInt.LoteDistDFeInt.GetDocZipCount do
        begin
          oDocZip := oRetDistDFeInt.LoteDistDFeInt.GetDocZip(I - 1);

          ShowMessage(oDocZip.NSU); // Número do NSU onde tem o XML
          ShowMessage(IntToStr(oDocZip.TipoXML)); // Tipo do XML

          if oDocZip.TipoXML = 1 then // TipoXMLDocZip.ResEvento
            ShowMessage('Resumo de evento')
          else if oDocZip.TipoXML = 2 then // TipoXMLDocZip.ResNFe
            ShowMessage('Resumo de NFe')
          else if oDocZip.TipoXML = 3 then // TipoXMLDocZip.ProcEventoNFe
            ShowMessage('Evento completo')
          else if oDocZip.TipoXML = 4 then // TipoXMLDocZip.ProcNFe
            ShowMessage('NFe Completa');

          ShowMessage(oDocZip.ConteudoXML); // XML decodificado
        end;
      end

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
