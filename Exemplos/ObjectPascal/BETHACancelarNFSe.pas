// ------------------------------------------------------------------
// Cancelar NFSe - Padrão BETHA
// ------------------------------------------------------------------
unit BETHACancelarNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TBETHACancelarNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TBETHACancelarNFSe.Executar;
var
  oConfiguracao: olevariant;
  oCancelarNFSe: olevariant;
  oExceptionInterop: olevariant;
  XML: string;
begin
  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';
    oConfiguracao.CodigoMunicipio := 9999903;
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 24; // CancelarNFSe
    oConfiguracao.SchemaVersao := '2.02';

    // Montar o XML como string (evita problema com namespace ns0)
    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<CancelarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">' +
      '  <Pedido>' +
      '    <InfPedidoCancelamento Id="C7000">' +
      '      <IdentificacaoNfse>' +
      '        <Numero>58</Numero>' +
      '        <CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj>' +
      '        <InscricaoMunicipal>99999999999999</InscricaoMunicipal>' +
      '        <CodigoMunicipio>4204608</CodigoMunicipio>' +
      '      </IdentificacaoNfse>' +
      '      <CodigoCancelamento>1</CodigoCancelamento>' +
      '    </InfPedidoCancelamento>' +
      '  </Pedido>' +
      '</CancelarNfseEnvio>';

    oCancelarNFSe := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.CancelarNFSe');
    oCancelarNFSe.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oCancelarNFSe.RetornoWSString);
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao cancelar NFSe: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.

