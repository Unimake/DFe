// ------------------------------------------------------------------
// Consultar Distribuicao NFSe NSU - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarDistribuicaoNFSeNSU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TNACIONALConsultarDistribuicaoNFSeNSU = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarDistribuicaoNFSeNSU.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oConsultarDistribuicaoNFSeNSU: olevariant;
  oDistribuicaoNFSe: olevariant;
  oExceptionInterop: olevariant;
  oLoteDFe: olevariant;
  oNFSe: olevariant;

  XML: string;
  caminhoArquivo: string;
  proximoNSU: string;
  retorno: string;
  I: integer;
begin
  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; //5=NFSe
    oConfiguracao.CertificadoArquivo := 'D:\projetos\certificados\DosClientes\nfse.pfx';
    oConfiguracao.CertificadoSenha := 'Mh26';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 95; //NFSeConsultarDistribuicaoNFSeNSU;
    oConfiguracao.SchemaVersao := '1.01';

    // Montar o XML de consulta
    oDistribuicaoNFSe := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.DistribuicaoNFSe');
    oDistribuicaoNFSe.NSU := '000000000000001';
    oDistribuicaoNFSe.TipoNSU := 'DISTRIBUICAO';
    oDistribuicaoNFSe.Lote := 'false';

    XML := oDistribuicaoNFSe.GerarXMLString();

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oDistribuicaoNFSe.NSU + '-cons-nsunfse.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(XML)^, Length(XML));
    finally
      Free;
    end;

    ShowMessage(XML);

    oConsultarDistribuicaoNFSeNSU := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultarDistribuicaoNFSeNSU');
    oConsultarDistribuicaoNFSeNSU.Executar(XML, IUnknown(oConfiguracao));

    retorno := oConsultarDistribuicaoNFSeNSU.RetornoWSString;

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oDistribuicaoNFSe.NSU + '-ret-cons-nsunfse.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(retorno)^, Length(retorno));
    finally
      Free;
    end;

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oConsultarDistribuicaoNFSeNSU.RetornoWSString);

    if oConsultarDistribuicaoNFSeNSU.Result.StatusProcessamento = 'DOCUMENTOS_LOCALIZADOS' then
    begin
      if oConsultarDistribuicaoNFSeNSU.Result.GetLoteDFeCount > 0 then
      begin
        oLoteDFe := oConsultarDistribuicaoNFSeNSU.Result.GetLoteDFe(0);
        ShowMessage(oLoteDFe.ChaveAcesso);

        proximoNSU := oLoteDFe.NSU; //Proximo NSU a ser consultado
        ShowMessage(proximoNSU);
        oConsultarDistribuicaoNFSeNSU.GravarXMLNFSe('d:\testenfe');
      end;
    end;

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
