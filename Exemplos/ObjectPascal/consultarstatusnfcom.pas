// ------------------------------------------------------------------
// Consulta status do serviço da NFCom
// ------------------------------------------------------------------
unit ConsultarStatusNFCom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarStatusNFCom = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarStatusNFCom.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsStatServNFCom: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 15; //15=NFCom
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 35; //São Paulo

  //Criar objeto do XML
  oConsStatServNFCom := CreateOleObject('Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom');
  oConsStatServNFCom.Versao := '1.00';
  oConsStatServNFCom.TpAmb := 2; //2=Homologação

  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsStatServNFCom.TpAmb);
  ShowMessage(oConsStatServNFCom.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.NFCom.StatusServico');
    oStatusServico.Executar(oConsStatServNFCom, oConfiguracao);

    //String do XML retornado pela SEFAZ
    ShowMessage(oStatusServico.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oStatusServico.Result.CStat) + ' - ' + oStatusServico.Result.XMotivo);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
