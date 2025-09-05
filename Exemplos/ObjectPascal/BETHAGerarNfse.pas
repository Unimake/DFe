// ------------------------------------------------------------------
// Gerar NFSe - Padrão BETHA
// ------------------------------------------------------------------
unit BETHAGerarNFSe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TBETHAGerarNFSe = class
  public
    procedure Executar;
  end;

implementation

procedure TBETHAGerarNFSe.Executar;
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

    oConfiguracao.CodigoMunicipio := 9999903;
    oConfiguracao.TipoAmbiente := 2; //Homologacao;
    oConfiguracao.Servico := 27; //GerarNFSe;
    oConfiguracao.SchemaVersao := '2.02';

    // XML montado manualmente (você pode gerar via string ou ler de arquivo também)
    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<GerarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">' +
      '<Rps>' +
      '  <InfDeclaracaoPrestacaoServico Id="lote">' +
      '    <Rps>' +
      '      <IdentificacaoRps>' +
      '        <Numero>1</Numero>' +
      '        <Serie>A</Serie>' +
      '        <Tipo>1</Tipo>' +
      '      </IdentificacaoRps>' +
      '      <DataEmissao>2024-10-01</DataEmissao>' +
      '      <Status>1</Status>' +
      '    </Rps>' +
      '    <Competencia>2024-10-01</Competencia>' +
      '    <Servico>' +
      '      <Valores>' +
      '        <ValorServicos>1.00</ValorServicos>' +
      '        <ValorDeducoes>0.00</ValorDeducoes>' +
      '        <ValorPis>0.00</ValorPis>' +
      '        <ValorCofins>0.00</ValorCofins>' +
      '        <ValorInss>0.00</ValorInss>' +
      '        <ValorIr>0.00</ValorIr>' +
      '        <ValorCsll>0.00</ValorCsll>' +
      '        <OutrasRetencoes>0.00</OutrasRetencoes>' +
      '        <ValorIss>0.00</ValorIss>' +
      '        <Aliquota>0.00</Aliquota>' +
      '        <DescontoIncondicionado>0.00</DescontoIncondicionado>' +
      '        <DescontoCondicionado>0.00</DescontoCondicionado>' +
      '      </Valores>' +
      '      <IssRetido>1</IssRetido>' +
      '      <ResponsavelRetencao>1</ResponsavelRetencao>' +
      '      <ItemListaServico>14.01</ItemListaServico>' +
      '      <CodigoCnae>1234567</CodigoCnae>' +
      '      <CodigoTributacaoMunicipio>12345678</CodigoTributacaoMunicipio>' +
      '      <Discriminacao>TESTESTESTESTESTESTESTES</Discriminacao>' +
      '      <CodigoMunicipio>1234567</CodigoMunicipio>' +
      '      <CodigoPais>1058</CodigoPais>' +
      '      <ExigibilidadeISS>1</ExigibilidadeISS>' +
      '      <MunicipioIncidencia>1234567</MunicipioIncidencia>' +
      '    </Servico>' +
      '    <Prestador>' +
      '      <CpfCnpj><Cnpj>12345678901234</Cnpj></CpfCnpj>' +
      '      <InscricaoMunicipal>123456</InscricaoMunicipal>' +
      '    </Prestador>' +
      '    <Tomador>' +
      '      <IdentificacaoTomador>' +
      '        <CpfCnpj><Cnpj>06117473000150</Cnpj></CpfCnpj>' +
      '        <InscricaoMunicipal>987654</InscricaoMunicipal>' +
      '      </IdentificacaoTomador>' +
      '      <RazaoSocial>TESTE DE ENVIO</RazaoSocial>' +
      '      <Endereco>' +
      '        <Endereco>RUA TESTE</Endereco>' +
      '        <Numero>1</Numero>' +
      '        <Complemento>TESTE</Complemento>' +
      '        <Bairro>TESTE</Bairro>' +
      '        <CodigoMunicipio>1234567</CodigoMunicipio>' +
      '        <Uf>PR</Uf>' +
      '        <CodigoPais>1058</CodigoPais>' +
      '        <Cep>12345678</Cep>' +
      '      </Endereco>' +
      '      <Contato>' +
      '        <Telefone>123456789</Telefone>' +
      '        <Email>TESTE@TESTES.COM</Email>' +
      '      </Contato>' +
      '    </Tomador>' +
      '    <RegimeEspecialTributacao>1</RegimeEspecialTributacao>' +
      '    <OptanteSimplesNacional>2</OptanteSimplesNacional>' +
      '    <IncentivoFiscal>1</IncentivoFiscal>' +
      '  </InfDeclaracaoPrestacaoServico>' +
      '</Rps>' +
      '</GerarNfseEnvio>';

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
