// ------------------------------------------------------------------
// Cancelar NFSe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALCancelarNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALCancelarNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALCancelarNFSe.Executar;
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

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 24; // NFSeCancelarNFSe
    oConfiguracao.SchemaVersao := '1.01';

    // Montar o XML como string (evita problema com namespace ns0)
    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<pedRegEvento xmlns="http://www.sped.fazenda.gov.br/nfse" versao="1.01">' +
      '      <infPedReg xmlns="http://www.sped.fazenda.gov.br/nfse" Id="PRE14001591201761135000132000000000000022096100197260101101">' +
      '        <tpAmb>2</tpAmb>' +
      '        <verAplic>Teste_0.1.0</verAplic>' +
      '        <dhEvento>2022-09-28T13:50:29-03:00</dhEvento>' +
      '        <CNPJAutor>01761135000132</CNPJAutor>' +
      '        <chNFSe>14001591201761135000132000000000000022096100197260</chNFSe>' +
      '        <e101101>' +
      '          <xDesc>Cancelamento de NFS-e</xDesc>' +
      '          <cMotivo>1</cMotivo>' +
      '          <xMotivo>xMotivo___teste____1</xMotivo>' +
      '        </e101101>' +
      '      </infPedReg>' +
      '</pedRegEvento>';

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

