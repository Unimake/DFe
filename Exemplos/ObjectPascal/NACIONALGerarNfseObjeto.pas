// ------------------------------------------------------------------
// Gerar NFSe - Gerar o XML a partir de um objeto - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALGerarNFSeObjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALGerarNFSeObjeto = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALGerarNFSeObjeto.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oGerarNFSe: olevariant;
  oExceptionInterop: olevariant;

  XML: string;
  DPS: olevariant;
begin
  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; //5=NFSe
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';

    oConfiguracao.CodigoMunicipio := 1001058; //Padrão Nacional
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 27; //NFSeGerarNFSe;
    oConfiguracao.SchemaVersao := '1.01';

    // Criar o XML do DPS
    DPS := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.DPS');
    DPS.Versao := '1.01';

    DPS.InfDPS := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfDPS');
    DPS.InfDPS.TpAmb := 2; //Homologação
    DPS.InfDPS.DhEmi := Now;
    DPS.InfDPS.VerAplic := 'ERP teste 1.0';
    DPS.InfDPS.Serie := '900';
    DPS.InfDPS.NDPS := '11';
    DPS.InfDPS.DCompet := Now;
    DPS.InfDPS.TpEmit := 1;
    DPS.InfDPS.CLocEmi := 3304557;

    DPS.InfDPS.Prest := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Prest');
    DPS.InfDPS.Prest.CNPJ := '04216261000102';
    DPS.InfDPS.Prest.RegTrib := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.RegTrib');
    DPS.InfDPS.Prest.RegTrib.OpSimpNac := 3;
    DPS.InfDPS.Prest.RegTrib.RegApTribSN := 1;
    DPS.InfDPS.Prest.RegTrib.RegEspTrib := 0;

    DPS.InfDPS.Toma := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Toma');
    DPS.InfDPS.Toma.CNPJ := '05030084000120';
    DPS.InfDPS.Toma.xNome := 'AAE - ASSOCIACAO DE APOIO A ESCOLA DO CIEP 339 MAR';
    DPS.InfDPS.Toma.&End := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.End');
    DPS.InfDPS.Toma.&End.EndNac := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.EndNac');
    DPS.InfDPS.Toma.&End.EndNac.CMun := 3304557;
    DPS.InfDPS.Toma.&End.EndNac.CEP := '21235390';
    DPS.InfDPS.Toma.&End.XLgr := 'ESTRADA PEDRO BORGES DE FREITAS, S/Nº';
    DPS.InfDPS.Toma.&End.Nro := 'S/N';
    DPS.InfDPS.Toma.&End.XBairro := 'IRAJA';

    DPS.InfDPS.Serv := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Serv');
    DPS.InfDPS.Serv.LocPrest := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.LocPrest');
    DPS.InfDPS.Serv.LocPrest.cLocPrestacao := 3304557;
    DPS.InfDPS.Serv.CServ := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.CServ');
    DPS.InfDPS.Serv.CServ.cTribNac := '310101';
    DPS.InfDPS.Serv.CServ.cTribMun := '001';
    DPS.InfDPS.Serv.CServ.xDescServ := 'REFERENTE A MANUTENCAO';
    DPS.InfDPS.Serv.CServ.cNBS := '101011200';

    DPS.InfDPS.Valores := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Valores');
    DPS.InfDPS.Valores.VServPrest := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.VServPrest');
    DPS.InfDPS.Valores.VServPrest.VServ := 2.00;
    DPS.InfDPS.Valores.Trib := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.Trib');
    DPS.InfDPS.Valores.Trib.TribMun := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.TribMun');
    DPS.InfDPS.Valores.Trib.TribMun.TribISSQN := 1;
    DPS.InfDPS.Valores.Trib.TribMun.TpRetISSQN := 1;
    DPS.InfDPS.Valores.Trib.TotTrib := CreateOleObject('Unimake.Business.DFe.Xml.NFSe.NACIONAL.TotTrib');
    DPS.InfDPS.Valores.Trib.TotTrib.PTotTribSN := 16.16;

    XML := DPS.GerarXMLString();

    oGerarNFSe := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.GerarNFSe');
    oGerarNFSe.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oGerarNFSe.RetornoWSString);

    if IUnknown(oGerarNFSe.Result) <> nil then
    begin
      if oGerarNFSe.Result.InfNFSe.CStat = 100 then
      begin
        oGerarNFSe.GravarXmlDistribuicao('d:\testenfe', DPS.InfDPS.Id + '-procNFSe', oGerarNFSe.RetornoWSString);
      end;
    end
    else
    begin
      if IUnknown(oGerarNFSe.ResultErro) <> nil then
      begin
        if IUnknown(oGerarNFSe.ResultErro.Erros) <> nil then
        begin
          ShowMessage(oGerarNFSe.ResultErro.Erros.Descricao + ' - ' + oGerarNFSe.ResultErro.Erros.Codigo);
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao gerar NFSe: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
