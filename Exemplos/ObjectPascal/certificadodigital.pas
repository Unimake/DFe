// ------------------------------------------------------------------
// Testes diversos com certificado digital (A1 e A3)
// ------------------------------------------------------------------
unit CertificadoDigital;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TCertificadoDigital = class
  private
    procedure MostrarDados(const oCert, oCertSel: Variant);
  public
    procedure Executar;
  end;

implementation

procedure TCertificadoDigital.MostrarDados(const oCert, oCertSel: Variant);
var
  msg: string;
begin
  msg :=
    'ID do Certificado'     + LineEnding + oCert.GetThumbPrint(oCertSel)   + LineEnding + LineEnding +
    'Dados do proprietário' + LineEnding + oCert.GetSubject(oCertSel)      + LineEnding + LineEnding +
    'Número de Série'       + LineEnding + oCert.GetSerialNumber(oCertSel) + LineEnding + LineEnding +
    'Validade Inicial'      + LineEnding + oCert.GetNotBefore(oCertSel)    + LineEnding + LineEnding +
    'Validade Final'        + LineEnding + oCert.GetNotAfter(oCertSel)     + LineEnding + LineEnding +
    'Certificado vencido?'  + LineEnding;

  If oCert.Vencido(oCertSel) then
     msg := msg + 'SIM'
  Else
     msg := msg + 'NÃO';

  ShowMessage(msg);
end;

procedure TCertificadoDigital.Executar;
var
  // Objetos (COM/OLE) como Variant para facilitar Automação
  oCertificado, oCertSel1, oCertSel2, oCertSel3, oCertSel4, oCertSel5, oCertA3, oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  // Dados
  thumbPrint, serialNumber, certBase64: string;
begin
  // Helper para capturar mensagens de exceção geradas do lado C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // ------------------------------------------------------------------
    // Criar objeto para trabalhar com certificados digitais (A1 e A3)
    // ------------------------------------------------------------------
    oCertificado := CreateOleObject('Unimake.Security.Platform.CertificadoDigital');

    // ------------------------------------------------------------------
    // Abrir tela para selecionar certificado instalado no repositório do
    // Windows (A1 ou A3)
    // ------------------------------------------------------------------
    oCertSel1 := oCertificado.AbrirTelaSelecao;

    // Guardar Thumbprint e SerialNumber (podem ser persistidos no BD)
    thumbPrint   := oCertificado.GetThumbPrint(IUnknown(oCertSel1));
    serialNumber := oCertificado.GetSerialNumber(IUnknown(oCertSel1));

    ShowMessage('Thumbprint: '   + thumbPrint);
    ShowMessage('Serial Number: ' + serialNumber);

    MostrarDados(oCertificado, oCertSel1);

    // ------------------------------------------------------------------
    // Somente A1 - Testar senha (capturando exceção caso seja inválida)
    // ------------------------------------------------------------------
    oCertificado := CreateOleObject('Unimake.Business.Security.CertificadoDigital');

    try
      oCertificado.CarregarCertificadoDigitalA1('c:\projetos\certificados\UnimakePV.pfx', '1234567'); //Forcei uma senha errada para demonstrar a exceção
    except
      on E: Exception do
      begin
        // Erro ao carregar (ex.: senha incorreta) – mensagem clara ao usuário
        ShowMessage(E.Message);
      end;
    end;

    // ------------------------------------------------------------------
    // Somente A1 - Carregar certificado direto do .PFX
    // ------------------------------------------------------------------
    oCertSel2 := oCertificado.CarregarCertificadoDigitalA1('c:\projetos\certificados\UnimakePV.pfx', '12345678');
    MostrarDados(oCertificado, oCertSel2);

    // ------------------------------------------------------------------
    // A1/A3 - Buscar no repositório do Windows pelo Serial Number
    // ------------------------------------------------------------------
    oCertSel3 := oCertificado.BuscarCertificadoDigital(serialNumber);
    MostrarDados(oCertificado, oCertSel3);

    // ------------------------------------------------------------------
    // A1/A3 - Buscar no repositório do Windows pelo ThumbPrint
    // ------------------------------------------------------------------
    oCertSel4 := oCertificado.BuscarCertificadoDigital(thumbPrint);
    MostrarDados(oCertificado, oCertSel4);

    // ------------------------------------------------------------------
    // A1 - Converter arquivo PFX para Base64 (para gravar em banco)
    // ------------------------------------------------------------------
    certBase64 := string(oCertificado.ToBase64('C:\Projetos\certificados\UnimakePV.pfx'));
    ShowMessage(certBase64);

    // Recuperar certificado A1 a partir do Base64
    oCertSel5 := oCertificado.FromBase64(certBase64, '12345678');
    MostrarDados(oCertificado, oCertSel5);

    // ------------------------------------------------------------------
    // A3 - Definir PIN do A3 para não precisar informar manualmente
    // ------------------------------------------------------------------
    oCertA3 := CreateOleObject('Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop');

    if oCertA3.IsA3(IUnknown(oCertSel1)) then
    begin
      ShowMessage('É certificado A3');
      // Ajuste o PIN conforme necessário
      oCertA3.SetPinPrivateKey(IUnknown(oCertSel1), '123456');
    end
    else
      ShowMessage('É certificado A1');

    // ------------------------------------------------------------------
    // Configurações dos serviços, forma de tratar o certificado digital
    // ------------------------------------------------------------------
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');

    // Carregar o certificado A1 direto do .PFX
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    // Carregar o certificado pelo Thumbprint
    oConfiguracao.CertificadoSerialNumberOrThumbPrint := thumbPrint;

    // Carregar o certificado pelo SerialNumber
    oConfiguracao.CertificadoSerialNumberOrThumbPrint := serialNumber;

    // Carregar o certificado pelo Base64
    oConfiguracao.CertificadoBase64 := certBase64;

    // Informar um certificado digital já carregado anteriormente
    oConfiguracao.CertificadoDigital := IUnknown(oCertSel5);

  except
    on E: Exception do
    begin
      // Exceção do Lazarus/Free Pascal
      ShowMessage('Erro Lazarus: ' + E.Message);
      // Exceção vinda do C# (via ThrowHelper)
      ShowMessage('CSHARP - Message: ' + string(oExceptionInterop.GetMessage()));
      ShowMessage('CSHARP - ErrorCode: ' + IntToStr(Integer(oExceptionInterop.GetErrorCode())));
    end;
  end;
end;

end.
