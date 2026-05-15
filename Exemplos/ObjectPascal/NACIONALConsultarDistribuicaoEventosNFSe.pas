// ------------------------------------------------------------------
// Consultar Distribuicao NFSe NSU - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALConsultarDistribuicaoEventosNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TNACIONALConsultarDistribuicaoEventosNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALConsultarDistribuicaoEventosNFSe.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oXMLConsultaEventosNFSeChaveAcesso: olevariant;
  oConsultaEventosNFSeChaveAcesso: olevariant;
  oExceptionInterop: olevariant;
  oLoteDFe: olevariant;

  XML: string;
  caminhoArquivo: string;
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
    oConfiguracao.Servico := 96; //NFSeConsultarEventosNFSeChaveAcesso;
    oConfiguracao.SchemaVersao := '1.01';

    // Montar o XML de consulta
    oXMLConsultaEventosNFSeChaveAcesso := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso');
    oXMLConsultaEventosNFSeChaveAcesso.ChaveNFSe := '12345678901234567890123456789012345678901234567890';

    XML := oXMLConsultaEventosNFSeChaveAcesso.GerarXMLString();

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oXMLConsultaEventosNFSeChaveAcesso.ChaveNFSe + '-cons-chaveacesso.xml';

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

    oConsultaEventosNFSeChaveAcesso := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.ConsultaEventosNFSeChaveAcesso');
    oConsultaEventosNFSeChaveAcesso.Executar(XML, IUnknown(oConfiguracao));

    retorno := oConsultaEventosNFSeChaveAcesso.RetornoWSString;

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oXMLConsultaEventosNFSeChaveAcesso.ChaveNFSe + '-ret-cons-chaveacesso.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(retorno)^, Length(retorno));
    finally
      Free;
    end;

    ShowMessage('XML retornado pela Receita Federal:' + sLineBreak + retorno);

    if oConsultaEventosNFSeChaveAcesso.Result.StatusProcessamento = 'DOCUMENTOS_LOCALIZADOS' then
    begin
      if oConsultaEventosNFSeChaveAcesso.Result.GetLoteDFeCount > 0 then
      begin
        //Pegar o conteúdo direto do XML retornado
        for I := 1 to oConsultaEventosNFSeChaveAcesso.Result.GetLoteDFeCount do
        begin
          oLoteDFe := oConsultaEventosNFSeChaveAcesso.Result.GetLoteDFe(I-1);
          ShowMessage(oLoteDFe.ChaveAcesso);
          ShowMessage(oLoteDFe.ArquivoXml.NFSe.Versao);
          ShowMessage(oLoteDFe.ArquivoXml.NFSe.InfNFSe.Id);
          ShowMessage(oLoteDFe.ArquivoXml.NFSe.InfNFSe.NNFSe);
        end;

        //Gravar XMLs retornados em uma pasta específica
        oConsultaEventosNFSeChaveAcesso.GravarXMLNFSe('d:\testenfe');
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
