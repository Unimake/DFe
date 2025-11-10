// ------------------------------------------------------------------
// Enviar o evento de CCE da CTe (Modelo 57)
// ------------------------------------------------------------------
unit EventoCCeCTe; // Nome da Unit

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEventoCCeCTe = class // Nome da Classe
  private

  public
    procedure Executar;
  end;

implementation

procedure TEventoCCeCTe.Executar;
var
  oConfiguracao: olevariant;
  oEvento: olevariant; // No CTe, o 'EventoCTe' é o XML raiz, não o 'EnvEvento'
  oInfEvento: olevariant;
  oDetEvento: olevariant;
  oEventoCCe: olevariant;
  oInfCorrecao: olevariant;
  oExceptionInterop: olevariant;

  I: integer;
  oRecepcaoEvento: olevariant;
  //oRetEvento: olevariant; // CTe não retorna um lote, o retorno é direto
  eventoAssinado: string;
  nHandle: TFileStream;
  nomeArquivoEvento: string;
  cStat: integer; // Para o CStat do CTe

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 2; // *** CORREÇÃO: 2=CTe ***
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCTe'); //
  oEvento.Versao := '4.00';

  //Criar tag InfEvento
  oEvento.InfEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfEvento'); //
  oEvento.InfEvento.COrgao := 41; // UFBrasil.PR
  oEvento.InfEvento.ChCTe := '41200211111111111111111111111111111111111115'; //
  oEvento.InfEvento.CNPJ := '11111111111111'; // CNPJ do emitente do CTe
  oEvento.InfEvento.DhEvento := Now;
  oEvento.InfEvento.TpEvento := 110110; // TipoEventoCTe.CartaCorrecao
  oEvento.InfEvento.NSeqEvento := 1;
  oEvento.InfEvento.TpAmb := 2; // TipoAmbiente.Homologacao

  //Criar a tag DetEvento (Estrutura do CTe)
  oEvento.InfEvento.DetEvento := CreateOleObject('Unimake.Business.DFe.Xml.CTe.DetEventoCCE'); // *** CORREÇÃO: DetEvento de CTe ***
  oEvento.InfEvento.DetEvento.VersaoEvento := '4.00';

  oEventoCCe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EventoCCeCTe');

  // Item 1 da Correção
  oInfCorrecao := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCorrecao');
  oInfCorrecao.GrupoAlterado := 'ide';
  oInfCorrecao.CampoAlterado := 'cfop';
  oInfCorrecao.ValorAlterado := '6353';
  oInfCorrecao.NroItemAlterado := '';
  oEventoCCe.AddInfCorrecao(IUnknown(oInfCorrecao));

  // Item 2 da Correção
  oInfCorrecao := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCorrecao');
  oInfCorrecao.GrupoAlterado := 'ide';
  oInfCorrecao.CampoAlterado := 'cfop';
  oInfCorrecao.ValorAlterado := '6353';
  oInfCorrecao.NroItemAlterado := '';
  oEventoCCe.AddInfCorrecao(IUnknown(oInfCorrecao));

  //Adicionar os objetos (Ligar o XML)
  oEvento.InfEvento.DetEvento.EventoCCeCTe := IUnknown(oEventoCCe);

  ShowMessage(oEvento.InfEvento.TpAmb);

  //(Os loops de debug do EnvEvento da NFe foram removidos pois CTe não usa EnvEvento)

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      // Enviar evento
      oRecepcaoEvento := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento'); //
      oRecepcaoEvento.SetXMLConfiguracao(IUnknown(oEvento), IUnknown(oConfiguracao));
      eventoAssinado := oRecepcaoEvento.GetConteudoXMLAssinado();

       // Gravar XML assinado no HD
    DeleteFile('d:\testenfe\CCeCTe.xml');
    with TStringList.Create do
    try
      Text := eventoAssinado;
      SaveToFile('d:\testenfe\CCeCTe.xml');
    finally
      Free;
    end;


      oRecepcaoEvento.Executar(IUnknown(oEvento), IUnknown(oConfiguracao)); //

      cStat := oRecepcaoEvento.Result.InfEvento.CStat;

      ShowMessage('CStat do Evento Retornado: ' + IntToStr(cStat) + ' - XMotivo: ' + oRecepcaoEvento.Result.InfEvento.XMotivo);

    end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.
