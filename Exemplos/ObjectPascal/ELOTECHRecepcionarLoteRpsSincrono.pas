// ------------------------------------------------------------------
// RecepcionarLoteRpsSincrono - Padrão ELOTECH
// ------------------------------------------------------------------
unit ELOTECHRecepcionarLoteRpsSincrono;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TELOTECHRecepcionarLoteRpsSincrono = class
  public
    procedure Executar;
  end;

implementation

procedure TELOTECHRecepcionarLoteRpsSincrono.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oRecepcionarLoteRpsSincrono: olevariant;
  oExceptionInterop: olevariant;

  XML: string;
begin
  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; //5=NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 4119905; //Codigo IBGE de Ponta Grossa
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 29; //NFSeRecepcionarLoteRpsSincrono;
    oConfiguracao.SchemaVersao := '2.03'; //Versão de schema deles é a 2.03

    // XML montado manualmente (você pode gerar via string ou ler de arquivo também)
    XML := ''; //Modelos do XML da NFSe Elotech: https://www.unimake.com.br/uninfe/modelos.php?p=NFSe%2FELOTECH%2F2.03    
		
    oRecepcionarLoteRpsSincrono is Unimake.Business.DFe.Servicos.NFSe.RecepcionarLoteRpsSincrono
    oRecepcionarLoteRpsSincrono.Executar(XML, configuracao)

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oRecepcionarLoteRpsSincrono.RetornoWSString);

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao gerar NFSe: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.