// ------------------------------------------------------------------
// CIOT - Declaração da operação de transporte
// ------------------------------------------------------------------
unit CIOTDeclaracaoOperacaoTransporte;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TCIOTDeclaracaoOperacaoTransporte = class
  public
    procedure Executar;
  end;

implementation

procedure TCIOTDeclaracaoOperacaoTransporte.Executar;
var
  oConfiguracao: OleVariant;
  oXmlCIOT: OleVariant;
  oVeiculo: OleVariant;
  oOrigemDestino: OleVariant;
  oDadosCarga: OleVariant;
  oInfPagamento: OleVariant;
  oIndicadoresOperacionais: OleVariant;
  oServico: OleVariant;
  oExceptionInterop: OleVariant;
  oConfigID: OleVariant;
  oServicoID: OleVariant;
  oXmlID: OleVariant;
  xml: string;
  idOperacaoTransporte: string;
  xmlDistribuicao: string;
begin
  oConfigID := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfigID.TipoDFe := 19; // 19 = CIOT
  oConfigID.TipoEmissao := 1; // Normal
  oConfigID.TipoAmbiente := 2; // Homologação
  oConfigID.CodigoUF := 91; // AN
  oConfigID.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfigID.CertificadoSenha := '12345678';

  oXmlID := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte');
  oXmlID.CpfCnpj := '06117473000150';

  oServicoID := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte');
  oServicoID.Executar(IUnknown(oXmlID), IUnknown(oConfigID));

  ShowMessage('Id Operação Transporte: ' + oServicoID.Result.IdOperacaoTransporte);

  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 19; // 19 = CIOT
  oConfiguracao.TipoEmissao := 1; // Normal
  oConfiguracao.TipoAmbiente := 2; // Homologação
  oConfiguracao.CodigoUF := 91; // AN
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  oXmlCIOT := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte');
  oXmlCIOT.IdOperacaoTransporte := oServicoID.Result.IdOperacaoTransporte;
  oXmlCIOT.TipoOperacao := 1; // Carga lotação
  oXmlCIOT.CpfCnpjContratado := '12345678901';
  oXmlCIOT.RNTRCContratado := '012345678';
  oXmlCIOT.CpfCnpjContratante := '12345678000195';
  oXmlCIOT.RNTRCContratante := '987654321';
  oXmlCIOT.CpfCnpjDestinatario := '98765432000110';
  oXmlCIOT.ValorFrete := 1500.50;
  oXmlCIOT.DataDeclaracao := Now;
  oXmlCIOT.IndContingencia := False;
  oXmlCIOT.DataInicioViagem := EncodeDate(2026, 05, 25);
  oXmlCIOT.DataFimViagem := EncodeDate(2026, 05, 26);

  oVeiculo := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Veiculo');
  oVeiculo.Placa := 'ABC1D23';
  oVeiculo.RNTRCVeiculo := '012345678';
  oVeiculo.NumeroEixos := String('1');
  oXmlCIOT.AddVeiculos(IUnknown(oVeiculo));

  oOrigemDestino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.OrigemDestino');
  oOrigemDestino.Origem := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Origem');
  oOrigemDestino.Origem.CodigoMunicipioOrigem := '4118402';
  oOrigemDestino.Origem.CepOrigem := '87700000';
  oOrigemDestino.Origem.LatitudeOrigem := '-23.073300';
  oOrigemDestino.Origem.LongitudeOrigem := '-52.465300';
  oOrigemDestino.Destino := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.Destino');
  oOrigemDestino.Destino.CodigoMunicipioDestino := '4106902';
  oOrigemDestino.Destino.CepDestino := '80000000';
  oOrigemDestino.Destino.LatitudeDestino := '-25.428400';
  oOrigemDestino.Destino.LongitudeDestino := '-49.273300';
  oOrigemDestino.DistanciaPercorrida := '500';
  oXmlCIOT.AddOrigemDestino(IUnknown(oOrigemDestino));

  oDadosCarga := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.DadosCarga');
  oDadosCarga.CodigoNaturezaCarga := '0001';
  oDadosCarga.PesoCarga := '1000.00';
  oDadosCarga.CodigoTipoCarga := 5; // Carga geral
  oDadosCarga.AddContratantesCargFrac('12345678000195');
  oDadosCarga.AddContratantesCargFrac('98765432000110');
  oXmlCIOT.DadosCarga := IUnknown(oDadosCarga);

  oInfPagamento := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.InfPagamento');
  oInfPagamento.TipoPagamento := 6; // Conta corrente
  oInfPagamento.ChavePix := 'financeiro@example.com';
  oInfPagamento.CpfCnpjCreditado := '12345678901';
  oInfPagamento.IdentificadorPix := 'PIX123456789';
  oInfPagamento.IndPagamento := 0;
  oXmlCIOT.AddInfPagamento(IUnknown(oInfPagamento));

  oIndicadoresOperacionais := CreateOleObject('Unimake.Business.DFe.Xml.CIOT.IndicadoresOperacionais');
  oIndicadoresOperacionais.IndAltoDesempenho := False;
  oIndicadoresOperacionais.IndRetornoVazio := False;
  oIndicadoresOperacionais.ComposicaoVeicular := False;
  oXmlCIOT.InfIndicadoresOperacionais := IUnknown(oIndicadoresOperacionais);

  xml := oXmlCIOT.GerarXmlString();
  ShowMessage(xml);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    oServico := CreateOleObject('Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte');
    oServico.Executar(IUnknown(oXmlCIOT), IUnknown(oConfiguracao));

    if oServico.Result.Codigo = '110' then // 110 = Dados inseridos com sucesso
     begin
       // Alguns dados retornados
       ShowMessage('Protocolo: ' + oServico.Result.Protocolo + #13#10 +
                   'Aviso Transportador: ' + oServico.Result.AvisoTransportador + #13#10 +
                   'Código Verificador: ' + oServico.Result.CodigoVerificador + #13#10 +
                   'Id Operação Transporte: ' + oServico.Result.IdOperacaoTransporte);

       // Gerar o XML de Distribuição para gravar em uma pasta qualquer
       oServico.GravarXmlDistribuicao('d:\testenfe\xmlciot');

       // Pegar a string do XML de distribuição para gravar em banco de dados
       idOperacaoTransporte := oServico.Result.IdOperacaoTransporte;
       xmlDistribuicao := oServico.oServicoProcResults[idOperacaoTransporte].GerarXMLString();
     end
     else
     begin
       if IUnknown(oServico.Result.Temp) <> nil then
       begin
         ShowMessage(oServico.Result.Temp.Error + ' - ' + oServico.Result.Temp.Message);
       end
       else
       begin
         ShowMessage(oServico.Result.Codigo + ' - ' + oServico.Result.Mensagem);
       end;
     end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro Lazarus: ' + E.Message);
      ShowMessage('CSHARP - ErrorCode: ' + IntToStr(oExceptionInterop.GetErrorCode));
      ShowMessage('CSHARP - Message: ' + oExceptionInterop.GetMessage);
    end;
  end;
end;

end.
