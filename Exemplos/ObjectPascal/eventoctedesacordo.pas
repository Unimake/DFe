// ------------------------------------------------------------------
// Evento de Prestação de Serviço em Desacordo (CTe)
// ------------------------------------------------------------------
unit EventoCTeDesacordo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TEventoCTeDesacordo = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoCTeDesacordo.Executar;
var
  oConfiguracao: olevariant;
  oRecepcaoEvento: olevariant;
  oExceptionInterop: olevariant;
  eventoAssinado:string;
  oEventoCTe: olevariant;   // Objeto principal (EventoCTe)

begin
  try

    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 2;
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';


    oEventoCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe');
    oEventoCTe.Versao := '4.00';

    // 3.b. Criar as informações do evento (InfEvento)
    oEventoCTe.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento');
    oEventoCTe.InfEvento.COrgao := 41;      // C# UFBrasil.PR = 41
    oEventoCTe.InfEvento.ChCTe := '41200211111111111111111111111111111111111115';
    oEventoCTe.InfEvento.CNPJ := '11111111111111';
    oEventoCTe.InfEvento.DhEvento := Now;  // C# DateTime.Now
    oEventoCTe.InfEvento.TpEvento := 610110;
    oEventoCTe.InfEvento.TpAmb := 2;     // C# TipoAmbiente.Homologacao = 2

    // 3.c. Criar o objeto raiz do evento (EventoCTe)
    oEventoCTe.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoPrestDesacordo');
    oEventoCTe.InfEvento.DetEvento.VersaoEvento := '4.00';
    oEventoCTe.InfEvento.DetEvento.IndDesacordoOper:= string('1');
    oEventoCTe.InfEvento.DetEvento.XObs:= 'Observação do desacordo';

    oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEventoCTe), IUnknown(oConfiguracao)) ;


    eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

      DeleteFile('d:\testenfe\EventoDesacordoCte.xml');
      with TStringList.Create do
      try
        Text := eventoAssinado;
        SaveToFile('d:\testenfe\EventoDesacordoCte.xml');
      finally
        Free;
      end;


      ShowMessage(eventoAssinado);

      ShowMessage(oRecepcaoEvento.RetornoWSString);

      ShowMessage('CStat do Lote Retornado: ' + IntToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

  except
    on E: Exception do
    begin
      ShowMessage('Erro Delphi: ' + E.Message);
      begin
        ShowMessage('Erro (Unimake): ' + oExceptionInterop.GetMessage());
        ShowMessage('ErrorCode: ' + IntToStr(oExceptionInterop.GetErrorCode()));
      end;
    end;
  end;
end;

end.
