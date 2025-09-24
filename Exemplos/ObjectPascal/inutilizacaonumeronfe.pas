// ------------------------------------------------------------------
// Enviar a Inutilização de Números da NFe
// ------------------------------------------------------------------
unit InutilizacaoNumeroNFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TInutilizacaoNumeroNFe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TInutilizacaoNumeroNFe.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  I: integer;
  oInutNFe: olevariant;
  oInutilizacao: olevariant;
  xmlAssinado: string;
  nHandle: TFileStream;
  nomeArquivoXML: string;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto do XML
  oInutNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InutNFe');
  oInutNFe.Versao := '4.00';
  oInutNFe.InfInut := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InutNFeInfInut');
  oInutNFe.InfInut.TpAmb := 2;  // Homologação
  oInutNFe.InfInut.Ano := '25';
  oInutNFe.InfInut.CNPJ := '06117473000150';
  oInutNFe.InfInut.CUF := 33; // RJ
  oInutNFe.InfInut.&Mod := 55; // Modelo NFe
  oInutNFe.InfInut.NNFIni := 57919;
  oInutNFe.InfInut.NNFFin := 57919;
  oInutNFe.InfInut.Serie := 1;
  oInutNFe.InfInut.XJust := 'Justificativa da inutilizacao de teste';

  ShowMessage(oInutNFe.Versao);
  ShowMessage(oInutNFe.InfInut.CNPJ);

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    begin
      // Enviar
      oInutilizacao := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.Inutilizacao');
      oInutilizacao.Executar(IUnknown(oInutNFe), IUnknown(oConfiguracao));

      xmlAssinado := oInutilizacao.GetConteudoXMLAssinado();

      // Criar e salvar o arquivo do XML assinado
      nomeArquivoXML := 'd:\testenfe\InutilizacaoNFe.xml';
      nHandle := TFileStream.Create(nomeArquivoXML, fmCreate);
      try
        nHandle.WriteBuffer(Pointer(xmlAssinado)^, Length(xmlAssinado));
      finally
        nHandle.Free;
      end;

      ShowMessage(oInutilizacao.RetornoWSSTring);

      ShowMessage('CStat Retornado: ' + VarToStr(oInutilizacao.Result.InfInut.CStat) + ' - XMotivo: ' + oInutilizacao.Result.InfInut.XMotivo);

      if oInutilizacao.Result.InfInut.CStat = 102 then // Inutilização homologada/autorizada
      begin
         oInutilizacao.GravarXmlDistribuicao('d:\testenfe\');
      end;
    end;

  except
    ShowMessage(oExceptionInterop.GetMessage());
    ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
  end;
end;

end.

