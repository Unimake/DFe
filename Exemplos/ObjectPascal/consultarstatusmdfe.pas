// ------------------------------------------------------------------
// Consulta status do serviço da MDFe
// ------------------------------------------------------------------
unit ConsultarStatusMDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarStatusMDFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarStatusMDFe.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsStatServ: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; //4=MDFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 41;

  //Criar objeto do XML
  oConsStatServ := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.ConsStatServMDFe');
  oConsStatServ.Versao := '3.00';
  oConsStatServ.TpAmb := 2; //2=Homologação

  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsStatServ.TpAmb);
  ShowMessage(oConsStatServ.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.StatusServico');
    oStatusServico.Executar(IUnknown(oConsStatServ), IUnknown(oConfiguracao));

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
