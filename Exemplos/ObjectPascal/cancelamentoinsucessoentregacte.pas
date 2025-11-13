// ------------------------------------------------------------------
// Enviar o evento de Insucesso da CTe (Modelo 57)
// ------------------------------------------------------------------
unit CancelamentoInsucessoEntregaCTe; // Nome da Unit

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TCancelamentoInsucessoEntregaCTe = class // Nome da Classe
  private

  public
    procedure Executar;
  end;

implementation

procedure TCancelamentoInsucessoEntregaCTe.Executar;
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
  oConfiguracao.TipoDFe := 2;
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe');
  oEvento.Versao := '4.00';

  //Criar tag InfEvento
  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento');
  oEvento.InfEvento.COrgao := 41;
  oEvento.InfEvento.ChCTe := '41200210859283000185570010000005671227070615';
  oEvento.InfEvento.CNPJ := '11111111111111';
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 110191;
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  //Criar a tag DetEvento (Estrutura do CTe)
  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoCancelamentoInsucessoEntrega');
  oEvento.InfEvento.DetEvento.VersaoEvento := '4.00';
  oEvento.InfEvento.DetEvento.DescEvento := 'Cancelamento do Insucesso de Entrega do CT-e';
  oEvento.InfEvento.DetEvento.Nprot := '141200000007987';
  oEvento.InfEvento.DetEvento.NprotIE := '141200000007982';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento'); //
      oRecepcaoEvento.Executar(IUnknown(oEvento), IUnknown(oConfiguracao));

      eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

      nomeArquivoEvento := 'c:\testenfe\CancelamentoInsucessoCTe-env.xml';
      nHandle := TFileStream.Create(nomeArquivoEvento, fmCreate);
      try
        nHandle.WriteBuffer(Pointer(eventoAssinado)^, Length(eventoAssinado));
      finally
        nHandle.Free;
      end;

      oRetEvento := oRecepcaoEvento.Result;

      ShowMessage('CStat do Lote Retornado: ' + VarToStr(oRecepcaoEvento.Result.InfEvento.CStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

      case oRetEvento.InfEvento.CStat of
        134,
        135,
        136: // 135 = Evento registrado e vinculado a CT-e (SUCESSO)
        begin
          oRecepcaoEvento.GravarXmlDistribuicao('c:\testenfe');
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
end;

end.
