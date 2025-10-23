// ------------------------------------------------------------------
// Consulta status do serviço da MDFe
// ------------------------------------------------------------------
unit ConsultarMDFeNaoEncerrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarMDFeNaoEncerrada = class
  public
    procedure Executar;
  end;

implementation

procedure TConsultarMDFeNaoEncerrada.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsStatServ: olevariant;
  oStatusServico: olevariant;
  oExceptionInterop: olevariant;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 41;

  //Criar objeto do XML
  oConsStatServ := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc');
  oConsStatServ.Versao := '3.00';
  oConsStatServ.TpAmb := 2; //2=Homologação
  oConsStatServ.CNPJ := '06117473000150';
  //oConsStatServ.XServ := 'CONSULTAR NÃO ENCERRADOS';


  //Resgatar algumas informações do objeto do XML
  ShowMessage(oConsStatServ.TpAmb);
  ShowMessage(oConsStatServ.Versao);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oStatusServico := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.ConsNaoEnc');
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
