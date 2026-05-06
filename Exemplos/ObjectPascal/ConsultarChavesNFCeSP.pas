// ------------------------------------------------------------------
// Consulta chaves NFCe SEFAZ SP
// ------------------------------------------------------------------
unit ConsultarChavesNFCeSP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants, DateUtils;

type
  TConsultarChavesNFCeSP = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarChavesNFCeSP.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oNFCeListagemChaves: olevariant;
  oConsultaChaves: olevariant;
  oExceptionInterop: olevariant;
  caminhoArquivo: string;
  retorno: string;
  I: Integer;
  ChaveNFCe: string;


begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 1; //1=NFCe
  oConfiguracao.CertificadoArquivo := 'D:\projetos\certificados\DosClientes\NFCeSPDownload_12345678.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CodigoUF := 35; //São Paulo
  oConfiguracao.TipoAmbiente := 1; //Produção

  //Criar objeto do XML
  oNFCeListagemChaves := CreateOleObject('Unimake.Business.DFe.Xml.NFe.NFCeListagemChaves');
  oNFCeListagemChaves.Versao := '1.00';
  oNFCeListagemChaves.TpAmb := 1; //Produção
  oNFCeListagemChaves.DataHoraInicial := IncDay(Now, -11);
  oNFCeListagemChaves.DataHoraFinal := now;

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsultaChaves := CreateOleObject('Unimake.Business.DFe.Servicos.NFCe.ConsultaChaves');
    oConsultaChaves.Executar(IUnknown(oNFCeListagemChaves), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    retorno := oConsultaChaves.RetornoWSString;

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\ConsultaChavesNFCeSP.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
      try
        WriteBuffer(Pointer(retorno)^, Length(retorno));
      finally
        Free;
      end;

    //Abrir um FOR nas chaves retornadas.
    for I := 1 to oConsultaChaves.Result.GetChNFCeCount do
    begin
       chaveNFCe := oConsultaChaves.Result.GetChNFCe(I-1);
       //Aqui já poderia disparar o serviço para fazer downlod da NFCe.
       //Veja botão de Download para aprender o código
    end;

    //Código de Status e Motivo
    ShowMessage(IntToStr(oConsultaChaves.Result.CStat) + ' - ' + oConsultaChaves.Result.XMotivo);

  except
    //Demostrar a exceção
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
