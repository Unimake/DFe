// ------------------------------------------------------------------
// Consulta status do serviço da DCe
// ------------------------------------------------------------------
unit ConsultarStatusDCe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarStatusDCe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarStatusDCe.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsStatServDCe: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 16; //16=DCe
  oConfiguracao.TipoAmbiente := 2; //2=Homologação
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsStatServDCe := CreateOleObject('Unimake.Business.DFe.Xml.DCe.ConsStatServDCe');
  oConsStatServDCe.Versao := '1.00';
  oConsStatServDCe.TpAmb := 2; //2=Homologação
  oConsStatServDCe.CUF := 41; //41=Paraná

  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsStatServDCe.cUF);
  ShowMessage(oConsStatServDCe.TpAmb);
  ShowMessage(oConsStatServDCe.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.DCe.StatusServico');
    oStatusServico.Executar(oConsStatServDCe, oConfiguracao);

    //String do XML retornado pela SEFAZ
    ShowMessage(oStatusServico.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oStatusServico.Result.CStat) + ' - ' + oStatusServico.Result.XMotivo);
    ShowMessage(oStatusServico.Result.XMotivo);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
