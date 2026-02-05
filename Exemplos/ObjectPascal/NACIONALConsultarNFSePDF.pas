// ------------------------------------------------------------------
// Consultar NFSe PDF - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarNFSePDF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALConsultarNFSePDF = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarNFSePDF.Executar;
var
  oConfiguracao: OleVariant;
  oConsultarNfsePDF: OleVariant;
  oExceptionInterop: OleVariant;
  XML: string;
begin
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 37; // NFSeConsultarNfsePDF
    oConfiguracao.SchemaVersao := '1.01';

    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' + 
      '<NFSe versao="1.01" xmlns="http://www.sped.fazenda.gov.br/nfse">' +
      '	<infNFSe Id="NFS35060032243673350000121000000000000623092546599173"/>' + 
      '</NFSe>';

    oConsultarNfsePDF := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePDF');
    oConsultarNfsePDF.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarNfsePDF.RetornoWSString);
	 
	 // Extrair o PDF
	 oConsultarNfsePDF.ExtrairPDF('d:\testenfe\pdf', 'nome_arquivo.pdf', 'Base64Pdf');

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao consultar NFSe por RPS: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.

