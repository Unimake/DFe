// ------------------------------------------------------------------
// Consulta situação MDFe
// ------------------------------------------------------------------
unit ConsultarSituacaoNFCom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultaSituacaoNFCom = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultaSituacaoNFCom.Executar;
var
  oConfiguracao: olevariant;
  oConsSitNFCom: olevariant;
  oConsultaProtocolo: olevariant;
  oExceptionInterop: olevariant;

  X: Integer;
  oProcEventoNFe: OleVariant;
  xmlEvento, nomeArqDistribEvento: string;
  nHandle: TFileStream;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 15; //NFCom
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := '41';

  //Criar objeto do XML
  oConsSitNFCom := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.ConsSitNFCom');
  oConsSitNFCom.Versao := '1.00';
  oConsSitNFCom.TpAmb  := 2;  // Homologação
  oConsSitNFCom.ChNFCom  := '12345678901234567890123456789012345678901234'; // Chave do MDFe

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsultaProtocolo := CreateOleObject('Unimake.Business.DFe.Servicos.NFCom.ConsultaProtocolo');
    oConsultaProtocolo.Executar(IUnknown(oConsSitNFCom), IUnknown(oConfiguracao));

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
