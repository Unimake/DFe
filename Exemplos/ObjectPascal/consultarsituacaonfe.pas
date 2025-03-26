// ------------------------------------------------------------------
// Consulta situação NFe
// ------------------------------------------------------------------
unit ConsultarSituacaoNFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TConsultarSituacaoNFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TConsultarSituacaoNFe.Executar;
var
  oConfiguracao: olevariant;
  oConsSitNfe: olevariant;
  oConsultaProtocolo: olevariant;
  oExceptionInterop: olevariant;

  X: Integer;
  oProcEventoNFe: OleVariant;
  xmlEvento, nomeArqDistribEvento: string;
  nHandle: TFileStream;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oConsSitNfe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ConsSitNfe');
  oConsSitNfe.Versao := '4.00';
  oConsSitNfe.TpAmb  := 2;  // Homologação
  oConsSitNfe.ChNfe  := '35240110654122000155550010000085161700218900'; // Chave da NFE

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    //Consumir o serviço
    oConsultaProtocolo := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo');
    oConsultaProtocolo.Executar(IUnknown(oConsSitNfe), IUnknown(oConfiguracao));

    //String do XML retornado pela SEFAZ
    ShowMessage(oConsultaProtocolo.RetornoWSString);

    //Código de Status e Motivo
    ShowMessage(IntToStr(oConsultaProtocolo.Result.CStat) + ' - ' + oConsultaProtocolo.Result.XMotivo);

    //Extrair os eventos retornados na consulta situação da nota
    if oConsultaProtocolo.Result.GetProcEventoNFeCount() > 0 then
      begin
        for X := 1 to oConsultaProtocolo.Result.GetProcEventoNFeCount() do
        begin
          oProcEventoNFe := oConsultaProtocolo.Result.GetProcEventoNFe(X - 1);

          // Pegar a string do XML do evento e salvar em um arquivo no HD
          xmlEvento := oProcEventoNFe.GerarXMLString(); // String do XML do evento

          // Definir o nome do arquivo para salvar o evento
          nomeArqDistribEvento := 'd:\testenfe\' + oProcEventoNFe.NomeArquivoDistribuicao;

          // Criar e salvar o arquivo
          try
            nHandle := TFileStream.Create(nomeArqDistribEvento, fmCreate);
            try
              nHandle.WriteBuffer(Pointer(xmlEvento)^, Length(xmlEvento));
            finally
              nHandle.Free;
            end;
          except
            on E: Exception do
              ShowMessage('Erro ao salvar evento: ' + E.Message);
          end;

          // Se quiser pegar um evento específico, só comparar o tipo de evento
          if oProcEventoNFe.Evento.InfEvento.TpEvento = 110111 then
          begin
            // Aqui dentro do IF, só fazer o que foi feito fora dele, mas apenas para eventos de cancelamento.
          end;
        end;
      end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
