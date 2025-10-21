// ------------------------------------------------------------------
// Consulta situação MDFe
// ------------------------------------------------------------------
unit ConsultarSituacaoMDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarSituacaoMDFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarSituacaoMDFe.Executar;
var
  oConfiguracao: olevariant;
  oConsSitMDFe: olevariant;
  oConsultaProtocolo: olevariant;
  oExceptionInterop: olevariant;

  X: Integer;
  oProcEventoNFe: OleVariant;
  xmlEvento, nomeArqDistribEvento: string;
  nHandle: TFileStream;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; //MDFE
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsSitMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe');
  oConsSitMDFe.Versao := '3.00';
  oConsSitMDFe.TpAmb  := 2;  // Homologação
  oConsSitMDFe.ChMDFe  := '41201280568835000181580010000010411406004656'; // Chave do MDFe

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsultaProtocolo := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.ConsultaProtocolo');
    oConsultaProtocolo.Executar(IUnknown(oConsSitMDFe), IUnknown(oConfiguracao));

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
