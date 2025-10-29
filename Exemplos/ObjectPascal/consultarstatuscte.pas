// ------------------------------------------------------------------
// Consulta status do serviço da CTe
// ------------------------------------------------------------------
unit ConsultarStatusCte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultaStatusCte = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultaStatusCte.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsStatServ: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 2; //cte
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsStatServ := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ConsStatServCte');
  oConsStatServ.Versao := '4.00';
  oConsStatServ.TpAmb := 2; //2=Homologação
  oConsStatServ.CUF := 41; //41=Paraná

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.StatusServico');
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
