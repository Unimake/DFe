unit EnviarXmlGNRe; // Nome da unit mantido do template

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarXmlGNRe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarXmlGNRe.Executar;
var
  oConfiguracao: OleVariant;
  oExceptionInterop: OleVariant;
  oTLoteGNRE: OleVariant;
  oDadosGNRE: OleVariant;
  oItem: OleVariant;
  oValor: OleVariant;
  oLoteRecepcao: OleVariant;
  oConsultaResultadoLote: OleVariant;
  oTConsLoteGNRE: OleVariant;
  situacaoRecepcao: string;
  numeroRecibo: string;
  situacaoProcessamento: string;
  oConfiguracaoConsulta: OleVariant;
begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 8;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.TipoAmbiente := 2;
  oConfiguracao.CodigoUF := 41;
  oConfiguracao.Servico := 23; //Servico.GNRELoteRecepcao

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // 1. Criar o objeto do XML
    oTLoteGNRE := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TLoteGNRE');
    oTLoteGNRE.Guias := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Guias');

    oDadosGNRE := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TDadosGNRE');
    oDadosGNRE.Versao := '2.00';
    oDadosGNRE.UfFavorecida := 41; // PR (Paraná)
    oDadosGNRE.TipoGNRE := 0;

    oDadosGNRE.ContribuinteEmitente := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.ContribuinteEmitente');
    oDadosGNRE.ContribuinteEmitente.Identificacao := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Identificacao');
    oDadosGNRE.ContribuinteEmitente.Identificacao.CNPJ := '07666666000166';
    oDadosGNRE.ContribuinteEmitente.Identificacao.IE := '9335665656';
    oDadosGNRE.ContribuinteEmitente.RazaoSocial := 'TESTE EMPRESA PARA ENVIO DA GNRE';
    oDadosGNRE.ContribuinteEmitente.Endereco := 'XXX XXXXXXX XXXXX';
    oDadosGNRE.ContribuinteEmitente.Municipio := '04808'; // Código IBGE do Município
    oDadosGNRE.ContribuinteEmitente.UF := 41; // PR
    oDadosGNRE.ContribuinteEmitente.CEP := '90399899';
    oDadosGNRE.ContribuinteEmitente.Telefone := '04456566566';

    oDadosGNRE.ItensGNRE := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.ItensGNRE');

    oItem := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Item');
    oItem.Receita := '100099';
    oItem.DocumentoOrigem := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.DocumentoOrigem');
    oItem.DocumentoOrigem.Tipo := '10'; // 10 = NF-e
    oItem.DocumentoOrigem.Value := '41210807666666000166550010001234551123455553'; // Chave da NF-e
    oItem.DataVencimento := Now;

    oValor := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Valor');
    oValor.Tipo := 0;
    oValor.ValorOriginal := 116.24;
    oItem.AddValor(IUnknown(oValor));

    oItem.ContribuinteDestinatario := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.ContribuinteDestinatario');
    oItem.ContribuinteDestinatario.Identificacao := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Identificacao');
    oItem.ContribuinteDestinatario.Identificacao.IE := '1236566556';

    oDadosGNRE.ItensGNRE.AddItem(IUnknown(oItem));

    oDadosGNRE.ValorGNRE := 30.00;
    oDadosGNRE.DataPagamento := Now;
    oTLoteGNRE.Guias.AddTDadosGNRE(IUnknown(oDadosGNRE));

    //Consumir o serviço
    oLoteRecepcao := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.LoteRecepcao');
    oLoteRecepcao.Executar(IUnknown(oTLoteGNRE), IUnknown(oConfiguracao));

    situacaoRecepcao := '100'; //oLoteRecepcao.Result.SituacaoRecepcao.Codigo;
    ShowMessage('Situação Recepção: ' + situacaoRecepcao);

    if (situacaoRecepcao = '100') then
    begin
      numeroRecibo := '4112345123'; //oLoteRecepcao.Result.Recibo.Numero;

      ShowMessage('Recibo: ' + numeroRecibo);

      // Criar a configuração mínima para consulta do lote de GNRE
      oConfiguracaoConsulta := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
      oConfiguracaoConsulta.TipoDFe := 8;
      oConfiguracaoConsulta.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
      oConfiguracaoConsulta.CertificadoSenha := '12345678';
      oConfiguracaoConsulta.TipoAmbiente := 2;
      oConfiguracaoConsulta.CodigoUF := 41;
      oConfiguracaoConsulta.Servico := 22; //Servico.GNREConsultaResultadoLote

      // Criar o XML de consulta
      oTConsLoteGNRE := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE');
      oTConsLoteGNRE.Ambiente := 2; // 2 = Homologação
      oTConsLoteGNRE.NumeroRecibo := numeroRecibo;
      oTConsLoteGNRE.IncluirPDFGuias := 1; // SimNaoLetra.Sim
      oTConsLoteGNRE.IncluirArquivoPagamento := 0; // SimNaoLetra.Nao
      oTConsLoteGNRE.IncluirNoticias := 0; // SimNaoLetra.Nao

      // Executar o serviço de Consulta
      oConsultaResultadoLote := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLote');
      oConsultaResultadoLote.Executar(IUnknown(oTConsLoteGNRE), IUnknown(oConfiguracaoConsulta)); // Reutiliza a mesma configuração

      situacaoProcessamento := oConsultaResultadoLote.Result.SituacaoProcess.Codigo;
      ShowMessage('Situação Processamento: ' + situacaoProcessamento);

      // Tratar o resultado da consulta
      if (situacaoProcessamento = '400') or (situacaoProcessamento = '401') then
      begin
        // 400: Lote recebido, aguardando processamento
        // 401: Lote em processamento
        ShowMessage('Lote em processamento, tente consultar mais tarde.');
      end
      else if (situacaoProcessamento = '402') then
      begin
        // 402: Lote processado com sucesso
        try
          oConsultaResultadoLote.GravarXmlRetorno('D:\testenfe', numeroRecibo + '-ret-gnre.xml');
          oConsultaResultadoLote.GravarPDFGuia('D:\testenfe', 'GuiaGNRE.pdf');
          ShowMessage('Sucesso! XML de retorno e PDF da guia salvos em D:\testenfe');
        except
          on E: Exception do
            ShowMessage('Erro ao salvar arquivos: ' + E.Message);
        end;
      end
      else if (situacaoProcessamento = '403') then
      begin
        // 403: Lote processado com pendências
        ShowMessage('Lote processado com pendências. Verifique o XML de retorno.');
        oConsultaResultadoLote.GravarXmlRetorno('D:\testenfe', numeroRecibo + '-ret-gnre.xml');
      end
      else if (situacaoProcessamento = '404') then
      begin
        // 404: Erro no processamento do lote
        ShowMessage('Erro no processamento do lote. Verifique o XML de retorno e tente enviar novamente.');
        oConsultaResultadoLote.GravarXmlRetorno('D:\testenfe', numeroRecibo + '-ret-gnre.xml');
      end
      else
      begin
        ShowMessage('Situação desconhecida: ' + situacaoProcessamento);
      end;
    end
    else
    begin
      // Lote não foi recebido (código != 100)
      ShowMessage('Lote não foi recebido. Motivo: ' + oLoteRecepcao.Result.SituacaoRecepcao.Descricao);
    end;

  except
    on E: Exception do
      // O oExceptionInterop captura detalhes da exceção interna da DLL .NET
      ShowMessage('Erro na execução: ' + E.Message + #13#10 +
                  'Detalhes Interop: ' + oExceptionInterop.Message);
  end;
end;
end.
