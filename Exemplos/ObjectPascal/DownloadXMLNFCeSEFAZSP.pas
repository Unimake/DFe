// ------------------------------------------------------------------
// Download do XML da NFCe SEFAZ SP
// ------------------------------------------------------------------
unit DownloadXMLNFCeSEFAZSP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants, DateUtils;

type
  TDownloadXMLNFCeSEFAZSP = class
  private

  public
    procedure Executar(Chave: string);
  end;

implementation

procedure TDownloadXMLNFCeSEFAZSP.Executar(Chave: string);
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oNFCeDownloadXML: olevariant;
  oDownloadXML: olevariant;
  oExceptionInterop: olevariant;
  caminhoArquivo: string;
  retorno: string;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 1; //1=NFCe
  oConfiguracao.CertificadoArquivo := 'D:\projetos\certificados\DosClientes\NFCeSPDownload_12345678.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 35; //São Paulo
  oConfiguracao.TipoAmbiente := 1; //Produção

  //Criar objeto do XML
  oNFCeDownloadXML := CreateOleObject('Unimake.Business.DFe.Xml.NFe.NFCeDownloadXML');
  oNFCeDownloadXML.Versao := '1.00';
  oNFCeDownloadXML.TpAmb := 1; //Produção
  oNFCeDownloadXML.ChNFCe := Chave;

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oDownloadXML := CreateOleObject('Unimake.Business.DFe.Servicos.NFCe.DownloadXML');
    oDownloadXML.Executar(IUnknown(oNFCeDownloadXML), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    retorno := oDownloadXML.RetornoWSString;

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\oDownloadXMLNFCeSP.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
      try
        WriteBuffer(Pointer(retorno)^, Length(retorno));
      finally
        Free;
      end;

    oDownloadXML.GravarXMLProc('D:\testenfe\NFCeSP');

    //Código de Status e Motivo
    ShowMessage(IntToStr(oDownloadXML.Result.CStat) + ' - ' + oDownloadXML.Result.XMotivo);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
