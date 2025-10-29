// ------------------------------------------------------------------
// Consultar MDFe´s não encerrados
// ------------------------------------------------------------------
unit ConsultarMDFeNaoEncerrado;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarMDFeNaoEncerrado = class
  public
    procedure Executar;
  end;

implementation

procedure TConsultarMDFeNaoEncerrado.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsMDFeNaoEnc: olevariant;
  oConsNaoEnc: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; //MDFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 41;

  //Criar objeto do XML
  oConsMDFeNaoEnc := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc');
  oConsMDFeNaoEnc.Versao := '3.00';
  oConsMDFeNaoEnc.TpAmb := 2; //2=Homologação
  oConsMDFeNaoEnc.CNPJ := '06117473000150';
  //oConsMDFeNaoEnc.XServ := 'CONSULTAR NÃO ENCERRADOS';


  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsMDFeNaoEnc.TpAmb);
  ShowMessage(oConsMDFeNaoEnc.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsNaoEnc := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.ConsNaoEnc');
    oConsNaoEnc.Executar(IUnknown(oConsMDFeNaoEnc), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    ShowMessage(oConsNaoEnc.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oConsNaoEnc.Result.CStat) + ' - ' + oConsNaoEnc.Result.XMotivo);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
