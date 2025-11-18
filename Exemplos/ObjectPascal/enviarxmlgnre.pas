
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
    oXml: OleVariant;
    oDadosGNRE: OleVariant;
    oItem: OleVariant;
    oValor: OleVariant;
    oServicoEnvio: OleVariant;
    oServicoConsulta: OleVariant;
    oXmlConsulta: OleVariant;
    sSituacaoRecepcao: string;
    sNumeroRecibo: string;
    sSituacaoProcessamento: string;


begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 8;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.TipoAmbiente := 2;
  oConfiguracao.CodigoUF := 41;

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // 1. Criar o objeto XML principal (CTeOS)
    oXml := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TLoteGNRE');
    oXml.Guias := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.Guias');

    oDadosGNRe := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TDadosGNRE');

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
    oXml.Guias.AddTDadosGNRE(IUnknown(oDadosGNRE));
    oServicoEnvio := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.LoteRecepcao');

    oServicoEnvio.Executar(IUnknown(oXml), IUnknown(oConfiguracao));
    //sSituacaoRecepcao := oServicoEnvio.Result.SituacaoRecepcao.Codigo;
    sSituacaoRecepcao := '100';
    ShowMessage('Situação Recepção: ' + sSituacaoRecepcao);

        if (sSituacaoRecepcao = '100') then
        begin


          sNumeroRecibo := oServicoEnvio.Result.Recibo.Numero;

          ShowMessage('Recibo: ' + sNumeroRecibo);

          // Criar o XML de consulta
          oXmlConsulta := CreateOleObject('Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE');
          oXmlConsulta.Ambiente := 2; // 2 = Homologação
          oXmlConsulta.NumeroRecibo := sNumeroRecibo;
          oXmlConsulta.IncluirPDFGuias := 1; // SimNaoLetra.Sim
          oXmlConsulta.IncluirArquivoPagamento := 0; // SimNaoLetra.Nao

          // Executar o serviço de Consulta
          oServicoConsulta := CreateOleObject('Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLote');
          oServicoConsulta.Executar(IUnknown(oXmlConsulta), IUnknown(oConfiguracao)); // Reutiliza a mesma configuração

          sSituacaoProcessamento := oServicoConsulta.Result.SituacaoProcess.Codigo;
          ShowMessage('Situação Processamento: ' + sSituacaoProcessamento);

          // Tratar o resultado da consulta
          if (sSituacaoProcessamento = '400') or (sSituacaoProcessamento = '401') then
          begin
            // 400: Lote recebido, aguardando processamento
            // 401: Lote em processamento
            ShowMessage('Lote em processamento, tente consultar mais tarde.');
          end
          else if (sSituacaoProcessamento = '402') then
          begin
            // 402: Lote processado com sucesso
            try
              oServicoConsulta.GravarXmlRetorno('D:\testenfe', sNumeroRecibo + '-ret-gnre.xml');
              oServicoConsulta.GravarPDFGuia('D:\testenfe', 'GuiaGNRE.pdf');
              ShowMessage('Sucesso! XML de retorno e PDF da guia salvos em D:\testenfe');
            except
              on E: Exception do
                ShowMessage('Erro ao salvar arquivos: ' + E.Message);
            end;
          end
          else if (sSituacaoProcessamento = '403') then
          begin
            // 403: Lote processado com pendências
            ShowMessage('Lote processado com pendências. Verifique o XML de retorno.');
            oServicoConsulta.GravarXmlRetorno('D:\testenfe', sNumeroRecibo + '-ret-gnre.xml');
          end
          else if (sSituacaoProcessamento = '404') then
          begin
            // 404: Erro no processamento do lote
            ShowMessage('Erro no processamento do lote. Verifique o XML de retorno e tente enviar novamente.');
            oServicoConsulta.GravarXmlRetorno('D:\testenfe', sNumeroRecibo + '-ret-gnre.xml');
          end
          else
          begin
             ShowMessage('Situação desconhecida: ' + sSituacaoProcessamento);
          end;
        end
        else
        begin
          // Lote não foi recebido (código != 100)
          ShowMessage('Lote não foi recebido. Motivo: ' + oServicoEnvio.Result.SituacaoRecepcao.Descricao);
        end;

      except
        on E: Exception do
          // O oExceptionInterop captura detalhes da exceção interna da DLL .NET
          ShowMessage('Erro na execução: ' + E.Message + #13#10 +
                      'Detalhes Interop: ' + oExceptionInterop.Message);
      end;
    end;

    end.
