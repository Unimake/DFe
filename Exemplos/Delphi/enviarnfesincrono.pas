// ------------------------------------------------------------------
// Enviar NFe no modo síncrono
// ------------------------------------------------------------------

unit EnviarNFeSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TEnviarNFeSincrono = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarNFeSincrono.Executar;
var
  oConfiguracao: olevariant;
  oConsStatServ: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsStatServ := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ConsStatServ');
  oConsStatServ.Versao := '4.00';
  oConsStatServ.TpAmb := 2; //2=Homologação
  oConsStatServ.CUF := 41; //41=Paraná

  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsStatServ.cUF);
  ShowMessage(oConsStatServ.TpAmb);
  ShowMessage(oConsStatServ.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.StatusServico');
    oStatusServico.Executar(oConsStatServ, oConfiguracao);

    //String do XML retornado pela SEFAZ
    ShowMessage(oStatusServico.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oStatusServico.Result.CStat) + ' - ' + oStatusServico.Result.XMotivo);
  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
