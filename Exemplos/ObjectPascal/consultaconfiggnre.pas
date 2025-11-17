// ------------------------------------------------------------------
// Consulta status do serviço da CTe
// ------------------------------------------------------------------
unit ConsultaConfigGNRE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultaConfigGNRE = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultaConfigGNRE.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oXml: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 8;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.TipoEmissao := 1 ;


  //Criar objeto do XML
  oXml := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TConsultaConfigUf');
  oXml.Ambiente := 2;
  oXml.UF := 43;
  oXml.Receita := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Receita');
  oXml.Receita.Courier := 0;
  oXml.Receita.Value := 100064;

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.ConsultaConfigUF');
    oStatusServico.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    ShowMessage(oStatusServico.RetornoWSString);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
