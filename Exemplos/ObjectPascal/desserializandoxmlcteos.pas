// ------------------------------------------------------------------
// Consulta situação CTe
// ------------------------------------------------------------------
unit DesserializandoXmlCTeOS;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TDesserializandoXmlCTeOS = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TDesserializandoXmlCTeOS.Executar;
var
  oCTeOS: olevariant;          // O objeto XML desserializado
  oXMLUtility: olevariant;
  oExceptionInterop: olevariant;
  oStringList: TStringList;
  sXmlContent: string;
  sCaminhoXML: string;
  oConfiguracao: olevariant;
  oAutorizacao: olevariant;
  oXml: olevariant;

  begin
    // *** CORREÇÃO 1: Adicionado o 'try' para iniciar o bloco de exceção ***
    try
      // Caminho do arquivo XML de CTeOS (igual ao C#)
      sCaminhoXML := 'C:\Projetos\Uninfe\exemplos xml\CTe 4.00\CTeOS\35170799999999999999670000000000261309301440-cte.xml';

      // O objeto 'oXml' será criado e conterá o XML desserializado
      oXml := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.CTeOS');
      oXml := oXml.LoadFromFile(sCaminhoXML); // 'oXml' agora contém o objeto

      // Criar objeto para pegar exceção do lado do CSHARP
      oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

      //ShowMessage(oXml);

      ShowMessage('Emitente: ' + oXml.InfCTe.Emit.XNome);

      oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
      oConfiguracao.TipoDFe := 3;

      oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
      oConfiguracao.CertificadoSenha := '12345678';

      oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.CTeOS.Autorizacao');

      // *** CORREÇÃO 3: Passar 'oXml' (o objeto correto) para o método Executar ***
      oAutorizacao.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

      ShowMessage('CStat Lote: ' + VarToStr(oAutorizacao.Result.ProtCTe.InfProt.CStat) +
                ' - XMotivo: ' + oAutorizacao.Result.ProtCTe.InfProt.XMotivo);

    // *** CORREÇÃO 4: Sintaxe correta do bloco 'except' ***
    except
      on E: Exception do
      begin
        ShowMessage('Erro (Delphi): ' + E.Message);

        begin
          ShowMessage('Erro (Unimake): ' + oExceptionInterop.GetMessage());
          ShowMessage('ErrorCode: ' + IntToStr(oExceptionInterop.GetErrorCode()));
        end;
      end;
    end;
  end;

  end.
