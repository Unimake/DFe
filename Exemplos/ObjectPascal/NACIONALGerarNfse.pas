// ------------------------------------------------------------------
// Gerar NFSe - Padrão NACIONAL
// ------------------------------------------------------------------
unit NACIONALGerarNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TNACIONALGerarNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TNACIONALGerarNFSe.Executar;
var
  // Declarar objetos
  oConfiguracao: olevariant;
  oGerarNFSe: olevariant;
  oExceptionInterop: olevariant;

  XML: string;
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
    oConfiguracao.SchemaVersao := '1.00';

    // XML montado manualmente (você pode gerar via string ou ler de arquivo também)
    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' + 
      '<DPS versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">' + 
      '	<infDPS Id="DPS420240420000000000000000007000000000000002">' + 
      '		<tpAmb>1</tpAmb>' + 
      '		<dhEmi>2023-09-09T09:42:06-03:00</dhEmi>' + 
      '		<verAplic>20220719</verAplic>' + 
      '		<serie>00007</serie>' + 
      '		<nDPS>2</nDPS>' + 
      '		<dCompet>2023-09-09</dCompet>' + 
      '		<tpEmit>1</tpEmit>' + 
      '		<cLocEmi>4202404</cLocEmi>' + 
      '		<prest>' + 
      '			<CNPJ>00000000000000</CNPJ>' + 
      '			<IM>152422</IM>' + 
      '			<regTrib>' + 
      '				<opSimpNac>2</opSimpNac>' + 
      '				<regEspTrib>0</regEspTrib>' + 
      '			</regTrib>' + 
      '		</prest>' + 
      '		<toma>' + 
      '			<CNPJ>00000000000000</CNPJ>' + 
      '			<IM>00000</IM>' + 
      '			<xNome>XXXXXXX XXXXXXXXXXX LTDA ME</xNome>' + 
      '			<end>' + 
      '				<xLgr>ARY XXXXXXX XXXXX</xLgr>' + 
      '				<nro>79</nro>' + 
      '				<xBairro>XXXXXXXX XXXXX</xBairro>' + 
      '			</end>' + 
      '		</toma>' + 
      '		<serv>' + 
      '			<locPrest>' + 
      '				<cLocPrestacao>4202404</cLocPrestacao>' + 
      '			</locPrest>' + 
      '			<cServ>' + 
      '				<cTribNac>010101</cTribNac>' + 
      '				<xDescServ>Teste de NF Nacional com imposto</xDescServ>' + 
      '				<cNBS>111111111</cNBS>' + 
      '			</cServ>' + 
      '		</serv>' + 
      '		<valores>' + 
      '			<vServPrest>' + 
      '				<vServ>20.00</vServ>' + 
      '			</vServPrest>' + 
      '			<trib>' + 
      '				<tribMun>' + 
      '					<tribISSQN>1</tribISSQN>' + 
      '					<tpRetISSQN>1</tpRetISSQN>' + 
      '				</tribMun>' + 
      '				<tribFed>' + 
      '					<vRetCP>0.40</vRetCP>' + 
      '					<vRetIRRF>0.00</vRetIRRF>' + 
      '					<vRetCSLL>0.00</vRetCSLL>' + 
      '				</tribFed>' + 
      '				<totTrib>' + 
      '					<indTotTrib>0</indTotTrib>' + 
      '				</totTrib>' + 
      '			</trib>' + 
      '		</valores>' + 
      '	</infDPS>' + 
      '</DPS>';
		
      oGerarNFse := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.GerarNFSe');
      oGerarNFSe.Executar(XML, IUnknown(oConfiguracao));

      ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oGerarNFSe.RetornoWSString);

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
