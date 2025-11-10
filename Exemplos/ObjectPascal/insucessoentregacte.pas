// ------------------------------------------------------------------
// Enviar o evento de Insucesso da CTe (Modelo 57)
// ------------------------------------------------------------------
unit InsucessoEntregaCTe; // Nome da Unit

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TInsucessoEntregaCTe = class // Nome da Classe
  private

  public
    procedure Executar;
  end;

implementation

procedure TInsucessoEntregaCTe.Executar;
var
  oConfiguracao: olevariant;
  oEvento: olevariant; // No CTe, o 'EventoCTe' é o XML raiz, não o 'EnvEvento'
  oInfEvento: olevariant;
  oDetEvento: olevariant;
  oEventoCCe: olevariant;
  oInfCorrecao: olevariant;
  oExceptionInterop: olevariant;
  oInfEntrega: olevariant;
  oRecepcaoEvento: olevariant;
  oRetEvento: olevariant; // Variável para o retorno
  I: integer;
  eventoAssinado: string;
  nHandle: TFileStream;
  nomeArquivoEvento: string;

begin
  // ... [Toda a criação do oEvento e oConfiguracao permanece igual] ...

  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 2; // 2=CTe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe'); //
  oEvento.Versao := '4.00';

  //Criar tag InfEvento
  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento'); //
  oEvento.InfEvento.COrgao := 41; // UFBrasil.PR
  oEvento.InfEvento.ChCTe := '41200210859283000185570010000005671227070615'; //
  oEvento.InfEvento.CNPJ := '11111111111111'; // CNPJ do emitente do CTe
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 110190;
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  //Criar a tag DetEvento (Estrutura do CTe)
  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoInsucessoEntrega');
  oEvento.InfEvento.DetEvento.VersaoEvento := '4.00';
  oEvento.InfEvento.DetEvento.DescEvento := 'Insucesso na Entrega do CT-e';
  oEvento.InfEvento.DetEvento.Nprot := '141200000007987';
  oEvento.InfEvento.DetEvento.DhTentativaEntrega := Now;
  oEvento.InfEvento.DetEvento.TpMotivo := 4;
  oEvento.InfEvento.DetEvento.XJustMotivo := 'Teste da justificativa do motivo da entrega quando é outro = 4';
  oEvento.InfEvento.DetEvento.Latitude := '37.774929';
  oEvento.InfEvento.DetEvento.Longitude := '122.419418';
  oEvento.InfEvento.DetEvento.HashTentativaEntrega := 'noauBnfaoS02PYxVm8ufox7OKww=';
  oEvento.InfEvento.DetEvento.DhHashTentativaEntrega := Now;

  oInfEntrega := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEntrega');
  oInfEntrega.ChNFe := '12345678901234567890123456789012345678901234';
  //oEvento.InfEvento.DetEvento.AddInfEntrega(IUnknown(oInfEntrega));

  ShowMessage(oEvento.InfEvento.TpAmb);

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  // **************************************************************************
  // *** INÍCIO: BLOCO DE EXECUÇÃO E VALIDAÇÃO CORRIGIDO PARA CTE ***
  // **************************************************************************
  try
    begin
      // Enviar evento
      oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento'); //
      oRecepcaoEvento.Executar(IUnknown(oEvento), IUnknown(oConfiguracao));

      eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

      // Salvar o XML de evento assinado (envio)
      nomeArquivoEvento := 'd:\testenfe\InsucessoCTe-env.xml';
      nHandle := TFileStream.Create(nomeArquivoEvento, fmCreate);
      try
        nHandle.WriteBuffer(Pointer(eventoAssinado)^, Length(eventoAssinado));
      finally
        nHandle.Free;
      end;

      // O 'Result' já é o 'retEvento'.
      // Vamos atribuí-lo a 'oRetEvento' para clareza (opcional)
      oRetEvento := oRecepcaoEvento.Result;

      // Exibe o CStat do EVENTO (retEvento)
      ShowMessage('CStat do Lote Retornado: ' + VarToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

      case oRetEvento.InfEvento.CStat of
        135: // 135 = Evento registrado e vinculado a CT-e (SUCESSO)
        begin
          oRecepcaoEvento.GravarXmlDistribuicao('d:\testenfe');
          ShowMessage('Evento homologado e XML (procEventoCTe) salvo!');
        end;
        else
          // Evento rejeitado - Realizar as ações necessárias
          ShowMessage('Evento REJEITADO. Motivo: ' + oRetEvento.InfEvento.XMotivo);
      end;
    end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
  // **************************************************************************
  // *** FIM DO BLOCO DE EXECUÇÃO E VALIDAÇÃO ***
  // **************************************************************************
end;

end.
