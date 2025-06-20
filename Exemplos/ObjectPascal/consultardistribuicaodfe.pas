// ------------------------------------------------------------------
// Consulta documentos fiscais destinados / Consulta NFe contra CNPJ
// ------------------------------------------------------------------
unit ConsultarDistribuicaoDFe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TConsultarDistribuicaoDFe = class
  public
    procedure Executar;
  end;

implementation

procedure TConsultarDistribuicaoDFe.Executar;
var
  oConfiguracao: OleVariant;
  oDistDFeInt, oDistNSU: OleVariant;
  oDistribuicaoDFe: OleVariant;
  oExceptionInterop: OleVariant;
  oDocZip, oResEvento, oResNFe, oProcEventoNFe, oNfeProc, oInfNFe: OleVariant;
  nsu, folder: string;
  I: Integer;
begin
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  nsu := '000000000000000';

  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    while True do
    begin
      oDistDFeInt := CreateOleObject('Unimake.Business.DFe.Xml.NFe.DistDFeInt');
      oDistDFeInt.Versao := '1.35';
      oDistDFeInt.TpAmb := 1; // Produção
      oDistDFeInt.CNPJ := '06117473000150';
      oDistDFeInt.CUFAutor := 41;

      oDistDFeInt.DistNSU := CreateOleObject('Unimake.Business.DFe.Xml.NFe.DistNSU');
      oDistDFeInt.DistNSU.UltNSU := nsu;

      oDistribuicaoDFe := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe');
      oDistribuicaoDFe.Executar(IUnknown(oDistDFeInt), IUnknown(oConfiguracao));

      ShowMessage(oDistribuicaoDFe.RetornoWSString);

      DeleteFile('d:\testenfe\RetornoDfe-' + nsu + '.xml');
      with TStringList.Create do
      try
        Text := oDistribuicaoDFe.RetornoWSString;
        SaveToFile('d:\testenfe\RetornoDfe-' + nsu + '.xml');
      finally
        Free;
      end;

      if oDistribuicaoDFe.Result.CStat = 138 then
      begin
        folder := 'd:\testenfe\doczip';
        oDistribuicaoDFe.GravarXMLDocZIP(folder, True, True);

        for I := 1 to oDistribuicaoDFe.Result.LoteDistDFeInt.GetDocZipCount do
        begin
          oDocZip := oDistribuicaoDFe.Result.LoteDistDFeInt.GetDocZip(I - 1);
          ShowMessage(oDocZip.ConteudoXML);
          ShowMessage(IntToStr(oDocZip.TipoXML));
        end;

        for I := 1 to oDistribuicaoDFe.GetResEventosCount do
        begin
          oResEvento := oDistribuicaoDFe.GetResEvento(I - 1);
          ShowMessage(oResEvento.ChNFe);
          ShowMessage(oResEvento.CNPJ);
        end;

        for I := 1 to oDistribuicaoDFe.GetResNFeCount do
        begin
          oResNFe := oDistribuicaoDFe.GetResNFe(I - 1);
          ShowMessage(oResNFe.ChNFe);
          ShowMessage(oResNFe.CNPJ);
        end;

        for I := 1 to oDistribuicaoDFe.GetProcEventoNFesCount do
        begin
          oProcEventoNFe := oDistribuicaoDFe.GetProcEventoNFes(I - 1);
          ShowMessage(oProcEventoNFe.Evento.InfEvento.CNPJ);
          ShowMessage(oProcEventoNFe.Evento.InfEvento.ChNFe);
        end;

        for I := 1 to oDistribuicaoDFe.GetProcNFesCount do
        begin
          oNfeProc := oDistribuicaoDFe.GetProcNFes(I - 1);
          oInfNFe := oNfeProc.NFe.GetInfNFe(0);
          ShowMessage(oInfNFe.Id);
          ShowMessage(IntToStr(oInfNFe.IDE.CUF));
          ShowMessage(oInfNFe.IDE.CNF);
          ShowMessage(oNfeProc.ProtNFe.InfProt.ChNFe);
          ShowMessage(oNfeProc.ProtNFe.InfProt.NProt);
        end;
      end
      else if oDistribuicaoDFe.Result.CStat = 656 then
        Break;

      nsu := oDistribuicaoDFe.Result.UltNSU;

      if Trim(oDistribuicaoDFe.Result.UltNSU) >= Trim(oDistribuicaoDFe.Result.MaxNSU) then
        Break;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro Lazarus: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
