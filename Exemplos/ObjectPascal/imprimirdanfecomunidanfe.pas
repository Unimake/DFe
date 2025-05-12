// ------------------------------------------------------------------
// Imprimir DANFE com o UNIDANFE
// Lista de parametros/propriedades que podem ser utilizadas:
// https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
// ------------------------------------------------------------------
unit ImprimirDANFEcomUniDANFE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Windows, SysUtils, ShellAPI, Dialogs, ComObj, Variants;

type
  TImprimirDANFEcomUniDANFE = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TImprimirDANFEcomUniDANFE.Executar;
var
  ShellExecResult: Integer;
  oUnidanfeConfiguration, oUnidanfeServices: OleVariant;

  begin
    try
      // Impressão via DLL
      oUnidanfeConfiguration := CreateOleObject('Unimake.Unidanfe.Configurations.UnidanfeConfiguration');
      oUnidanfeConfiguration.WaitProcess := False;
      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml';
      oUnidanfeConfiguration.Visualizar := True;
      oUnidanfeConfiguration.Imprimir := False;
      oUnidanfeConfiguration.EnviaEmail := False;

      oUnidanfeServices := CreateOleObject('Unimake.Unidanfe.UnidanfeServices');
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\41220706117473000150650010000580151230845952-procnfe.xml';
      oUnidanfeConfiguration.AddComprovanteTEF('D:\testenfe\foxpro\unimake.dfe\testeComprovanteTEF1.txt');
      oUnidanfeConfiguration.AddComprovanteTEF('D:\testenfe\foxpro\unimake.dfe\testeComprovanteTEF2.txt');
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\Backup\Autorizados\202012\41201200000000000000570010000001111001111111-procCTe.xml';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'C:\projetos\uninfe\exemplos\CTe 3.00\CTeOS\41170899999999999999670010000000131000000041-cte.xml';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\41201280568835000181580010000010401406004659-procMDFe.xml';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'C:\projetos\uninfe\exemplos\SAT\SATAutorizado-sat.xml';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\35221046105508000155550000000051471230408814_110111_01-proceventonfe.xml';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml';
      oUnidanfeConfiguration.Configuracao := 'DANFE_SIMPL';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := 'D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml';
      oUnidanfeConfiguration.Configuracao := 'DANFE_ETIQ';
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeConfiguration.Arquivo := '\\192.168.0.250\app\adm\NFe\Emp0001_Fil002_Envio\Enviadas\Autorizados\202202\41220206117473000150550010000717301715751703-procNFe.xml';
      oUnidanfeConfiguration.NomePDF := 'D:\testenfe\pdf\41220206117473000150550010000717301715751703.pdf';
      oUnidanfeConfiguration.Visualizar := False;
      oUnidanfeConfiguration.Imprimir := False;
      oUnidanfeServices.Execute(oUnidanfeConfiguration);
      ShowMessage('Aguarde!');

      oUnidanfeServices.ShowConfigurationScreen();
      ShowMessage('Aguarde!');

      oUnidanfeServices.ShowConfigurationScreenPastaConfig('C:\Unimake\Unimake.UniDANFe\Unimake_EXE');
      ShowMessage('Aguarde!');

      oUnidanfeServices.ShowEmailScreen();
      ShowMessage('Aguarde!');

      oUnidanfeServices.ShowLicencaScreen();
      ShowMessage('Aguarde!');

      // Executar o UniDANFe.EXE sem parâmetros
      ShellExecResult := ShellExecute(0, 'open', 'D:\testenfe\unidanfe\unidanfe.exe', nil, 'D:\testenfe\unidanfe', SW_SHOWNORMAL);
      ShowMessage('Aguarde!');

      // Executar o UniDANFe.EXE com parâmetro
      ShellExecResult := ShellExecute(0, 'open', 'D:\testenfe\unidanfe\unidanfe.exe',
                                      PChar('A=D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml'),
                                      'D:\testenfe\unidanfe', SW_SHOWNORMAL);
      ShowMessage('Aguarde!');
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao imprimir DFe: ' + E.Message);
      end;
    end;
  end;

end.
