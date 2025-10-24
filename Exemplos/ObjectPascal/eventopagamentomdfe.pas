unit EventoPagamentoMDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants, DateUtils;

type
  TEventoPagamentoMDFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoPagamentoMDFe.Executar;
var
  oConfiguracao: olevariant;
  oXml: olevariant;                 // Objeto principal: EventoMDFe
  oInfEvento: olevariant;
  oDetEvento: olevariant;
  oEventoPagto: olevariant;
  oInfViagens: olevariant;
  oInfPag: olevariant;            // Informações de Pagamento (item da lista)
  oComp: olevariant;              // Componente do Pagamento (item da lista)
  oInfPrazo: olevariant;          // Informação de Prazo (item da lista)
  oInfBanc: olevariant;
  oRecepcaoEvento: olevariant;
  oExceptionInterop: olevariant;
begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; // 4 = MDFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  // Ajuste seu certificado
  oConfiguracao.CertificadoSenha := '12345678'; // Ajuste sua senha

  // Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      // --- Início da Montagem do XML (baseado no C#) ---

      // 1. Criar o objeto raiz 'EventoMDFe'
      oXml := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.EventoMDFe');
      oXml.Versao := '3.00';

      // 2. Criar o objeto 'InfEvento'
      oXml.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfEvento');
      oXml.InfEvento.COrgao := 41; // UFBrasil.PR
      oXml.InfEvento.ChMDFe := '41200380568835000181580010000007171930099252'; // Chave do MDFe
      oXml.InfEvento.CNPJ := '80568835000181';
      oXml.InfEvento.DhEvento := Now;
      oXml.InfEvento.TpEvento := 110118; // 110118 = TipoEventoMDFe.PagamentoOperacao
      oXml.InfEvento.NSeqEvento := 1;
      oXml.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

      // 3. Criar o objeto 'DetEvento' (DetEventoPagtoOperMDFe)
      oXml.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.DetEventoPagtoOperMDFe');
      oXml.InfEvento.DetEvento.VersaoEvento := '3.00';

      // 4. Criar o objeto 'EventoPagtoOperMDFe' (dentro do DetEvento)
      //oXml.InfEvento.DetEvento.EventoPagtoOperMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.EventoPagtoOperMDFe');
      oXml.InfEvento.DetEvento.DescEvento := 'Pagamento Operacao MDF-e';
      oXml.InfEvento.DetEvento.NProt := '941190000014312'; // Protocolo do MDFe a ser vinculado

      // 4a. Criar 'InfViagens'
      oXml.InfEvento.DetEvento.InfViagens := IUnknown(CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfViagens'));
      oXml.InfEvento.DetEvento.InfViagens.NroViagem := '00001';
      oXml.InfEvento.DetEvento.InfViagens.QtdViagens := '00001';

      // 4b. Criar 'InfPag' (é uma lista, mas vamos adicionar um item)
      oInfPag := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.PagtoOperMDFeInfPag');
      oInfPag.XNome := 'TESTE TRANSPORTE E OPERACOES LTDA';
      oInfPag.CNPJ := '00000000000000';
      oInfPag.VContrato := 3000.00;
      oInfPag.IndPag := 1; // 1 = IndicadorPagamento.PagamentoPrazo
      oInfPag.VAdiant := 500.00;

      // 4c. Criar lista 'Comp' (dentro do InfPag)
      // Comp 1
      oComp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Comp');
      oComp.TpComp := 99; // 99 = TipoComponenteMDFe.Outros
      oComp.VComp := 2000.00;
      oComp.XComp := 'PAGAMENTO DE FRETE';
      oInfPag.AddComp(IUnknown(oComp));

      // Comp 2
      oComp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Comp');
      oComp.TpComp := 1; // 1 = TipoComponenteMDFe.ValePedagio
      oComp.VComp := 500.00;
      oInfPag.AddComp(IUnknown(oComp));

      // Comp 3
      oComp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Comp');
      oComp.TpComp := 99; // 99 = TipoComponenteMDFe.Outros
      oComp.VComp := 500.00;
      oComp.XComp := 'COMPRA DE PNEUS';
      oInfPag.AddComp(IUnknown(oComp));

      oInfPrazo := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfPrazo');
      oInfPrazo.NParcela := '001';
      oInfPrazo.DVenc := Now + 20;
      oInfPrazo.VParcela := 2000.00;
      oInfPag.AddInfPrazo(IUnknown(oInfPrazo));

      // Prazo 2
      oInfPrazo := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfPrazo');
      oInfPrazo.NParcela := '002';
      oInfPrazo.DVenc := Now + 40; // DateTime.Now.AddDays(40)
      oInfPrazo.VParcela := 500.00;
      oInfPag.AddInfPrazo(IUnknown(oInfPrazo));

      // 4e. Criar 'InfBanc' (dentro do InfPag)
      oInfPag.InfBanc := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfBanc');
      oInfPag.InfBanc.PIX := '+5544993333223';

      oXml.InfEvento.DetEvento.EventoPagtoOperMDFe.AddInfPag(IUnknown(oInfPag));

      // --- Fim da Montagem do XML ---

      oRecepcaoEvento := CreateOleObject(
        'Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento');
      oRecepcaoEvento.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

      // O resultado do MDFe é direto no 'Result.InfEvento'
      ShowMessage('CStat do Evento: ' +
        VarToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' +
        oRecepcaoEvento.Result.InfEvento.XMotivo);
    end;
  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
