// ------------------------------------------------------------------
// Consulta situação CTe
// ------------------------------------------------------------------
unit ConsultarSituacaoCTe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarSituacaoCTe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarSituacaoCTe.Executar;
var
  oConfiguracao: olevariant;
  oConsSitCTe: olevariant;
  oConsultaProtocolo: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 2; //CTE
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsSitCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ConsSitCTe');
  oConsSitCTe.Versao := '4.00';
  oConsSitCTe.TpAmb  := 2;  // Homologação
  oConsSitCTe.ChCTe  := '11111111111111111111111111111111111111111111'; // Chave do MDFe

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsultaProtocolo := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.ConsultaProtocolo');
    oConsultaProtocolo.Executar(IUnknown(oConsSitCTe), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    ShowMessage(oConsultaProtocolo.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oConsultaProtocolo.Result.CStat) + ' - ' + oConsultaProtocolo.Result.XMotivo);

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
