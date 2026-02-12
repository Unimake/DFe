// ------------------------------------------------------------------
// Cancelar NFSe gerando o XML a partir de uma classe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALCancelarNFSeObjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALCancelarNFSeObjeto = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALCancelarNFSeObjeto.Executar;
var
  oConfiguracao: olevariant;
  oPedRegEvento: olevariant;
  oRecepcionarEvento: olevariant;
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
    oConfiguracao.Servico := 91; // NFSeRecepcionarEventosDiversos
    oConfiguracao.SchemaVersao := '1.01';

    //Criar o XML do evento de cancelamento da NFSe
    oPedRegEvento := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.PedRegEvento');
    oPedRegEvento.Versao := '1.01';
    oPedRegEvento.InfPedReg := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfPedReg');
    oPedRegEvento.InfPedReg.TpAmb := 2; //Homologação
    oPedRegEvento.InfPedReg.VerAplic := 'Teste1';
    oPedRegEvento.InfPedReg.DhEvento := Now;
    oPedRegEvento.InfPedReg.CNPJAutor := '01761135000132';
    oPedRegEvento.InfPedReg.ChNFSe := '14001591201761135000132000000000000022096100197260';
    oPedRegEvento.InfPedReg.E101101 := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Eventos.E101101');
    oPedRegEvento.InfPedReg.E101101.XDesc := 'Cancelamento de NFS-e';
    oPedRegEvento.InfPedReg.E101101.CMotivo := 1; //CodigoJustificativaCancelamento.ErroNaEmissao
    oPedRegEvento.InfPedReg.E101101.XMotivo := 'Teste de cancelamento da NFSe Nacional';

    XML := oPedRegEvento.GerarXMLString();
    ShowMessage(XML);

    // Gravar XML assinado no HD
    DeleteFile('d:\testenfe\CancelamentoNFSeNacional.xml');
    with TStringList.Create do
    try
      Text := XML;
      SaveToFile('d:\testenfe\CancelamentoNFSeNacional.xml');
    finally
      Free;
    end;

    oRecepcionarEvento := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.RecepcionarEvento');
    oRecepcionarEvento.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oRecepcionarEvento.RetornoWSString);

    // Gravar XML assinado no HD
    DeleteFile('d:\testenfe\CancelamentoNFSeNacional_retorno.xml');
    with TStringList.Create do
    try
      Text := oRecepcionarEvento.RetornoWSString;
      SaveToFile('d:\testenfe\CancelamentoNFSeNacional_retorno.xml');
    finally
      Free;
    end;

    if IUnknown(oRecepcionarEvento.Result) <> nil then //Evento foi homologado na Receita
    begin
       oRecepcionarEvento.GravarXmlDistribuicao('d:\testenfe', oRecepcionarEvento.Result.InfNFSe.Id + '-procEventoNFSe.xml', oRecepcionarEvento.RetornoWSString);
    end
    else
    begin
      if IUnknown(oRecepcionarEvento.ResultErro) <> nil then
      begin
        if IUnknown(oRecepcionarEvento.ResultErro.Erro) <> nil then
        begin
          ShowMessage(oRecepcionarEvento.ResultErro.Erro.Descricao + ' - ' + oRecepcionarEvento.ResultErro.Erro.Codigo);
        end;
      end;
    end;

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

