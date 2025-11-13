// ------------------------------------------------------------------
// Evento de Prestação de Serviço em Desacordo (CTe)
// ------------------------------------------------------------------
unit EventoEpecCte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TEventoEpecCte = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoEpecCte.Executar;
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
    oConfiguracao.TipoEmissao := 4;


    oEventoCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe');
    oEventoCTe.Versao := '4.00';

    // 3.b. Criar as informações do evento (InfEvento)
    oEventoCTe.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento');
    oEventoCTe.InfEvento.COrgao := 41;      // C# UFBrasil.PR = 41
    oEventoCTe.InfEvento.ChCTe := '41200211111111111111111111111111111111111115';
    oEventoCTe.InfEvento.CNPJ := '11111111111111';
    oEventoCTe.InfEvento.DhEvento := Now;  // C# DateTime.Now
    oEventoCTe.InfEvento.TpEvento := 110113;
    oEventoCTe.InfEvento.TpAmb := 2;     // C# TipoAmbiente.Homologacao = 2

    // 3.c. Criar o objeto raiz do evento (EventoCTe)
    oEventoCTe.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoEPEC');
    oEventoCTe.InfEvento.DetEvento.VersaoEvento := '4.00';
    oEventoCTe.InfEvento.DetEvento.XJust:= 'Teste de EPEC do CTE para ver se tudo está funcionando';
    oEventoCTe.InfEvento.DetEvento.VICMS:= 100;
    oEventoCTe.InfEvento.DetEvento.VICMSST:= 100;
    oEventoCTe.InfEvento.DetEvento.VTPrest:= 1000;
    oEventoCTe.InfEvento.DetEvento.VCarga:= 1000;
    oEventoCTe.InfEvento.DetEvento.Toma4:= CreateOleObject('Unimake.Business.DFe.Xml.CTe.EvEPECCTeToma4');

    oEventoCTe.InfEvento.DetEvento.Toma4.UF:= 41;
    oEventoCTe.InfEvento.DetEvento.Toma4.CNPJ:= '06117473000150';
    oEventoCTe.InfEvento.DetEvento.Toma4.IE:= '1234567890';

    oEventoCTe.InfEvento.DetEvento.Modal:= 1;
    oEventoCTe.InfEvento.DetEvento.UFIni:= 41;
    oEventoCTe.InfEvento.DetEvento.UFFim:= 41;
    oEventoCTe.InfEvento.DetEvento.TpCTe:= 0 ;
    oEventoCTe.InfEvento.DetEvento.DhEmi := Now;


    oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

    oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento');
    oRecepcaoEvento.Executar(IUnknown(oEventoCTe), IUnknown(oConfiguracao)) ;


    eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

      DeleteFile('c:\testenfe\EventoDesacordoCte.xml');
      with TStringList.Create do
      try
        Text := eventoAssinado;
        SaveToFile('c:\testenfe\EventoDesacordoCte.xml');
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
