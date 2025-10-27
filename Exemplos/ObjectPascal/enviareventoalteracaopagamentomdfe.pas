// ------------------------------------------------------------------
// Enviar evento de manifestação da NFe
// ------------------------------------------------------------------
unit EnviarEventoAlteracaoPagamentoMDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarEventoAlteracaoPagamentoMDFe = class
  public
    procedure Executar;
  end;

implementation

procedure TEnviarEventoAlteracaoPagamentoMDFe.Executar;
var
  oConfiguracao: olevariant;
  oXml: olevariant;
  oInfEvento: olevariant;
  oEventoAlteracaoPagto: olevariant;
  oDetEvento: olevariant;
  oExceptionInterop: olevariant;
  oInfPag: olevariant;
  oComp: olevariant;
  oInfPrazo: olevariant;
  oRecepcaoEvento: olevariant;
begin
  // Criar configuração básica para consumir o serviço
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; // 0 = NFe
  oConfiguracao.CertificadoSenha := '12345678';
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');
  try
      begin
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

  // 3. Criar o objeto 'DetEvento' (DetEventoAlteracaoPagtoServMDFe)
  oXml.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.DetEventoAlteracaoPagtoServMDFe');
  oXml.InfEvento.DetEvento.VersaoEvento := '3.00';

  // 4. Criar o objeto 'EventoAlteracaoPagtoServMDFe' (dentro do DetEvento)
  //oXml.InfEvento.DetEvento.EventoAlteracaoPagtoServMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.EventoAlteracaoPagtoServMDFe');
  oXml.InfEvento.DetEvento.DescEvento := 'Alteracao Pagamento Servico MDFe';
  oXml.InfEvento.DetEvento.NProt := '941190000014312'; // Protocolo do MDFe a ser vinculado

   // 4b. Criar 'InfPag' (é uma lista, mas vamos adicionar um item)
   oInfPag := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.AlteracaoPagtoServMDFeInfPag');
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

   //InfPrazo
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

   oXml.InfEvento.DetEvento.EventoAlteracaoPagtoServMDFe.AddInfPag(IUnknown(oInfPag));

   // --- Fim da Montagem do XML ---

   oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento');
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
