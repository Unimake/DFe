// ------------------------------------------------------------------
// Enviar Lote RPS Síncrono - Padrão BETHA
// ------------------------------------------------------------------
unit BETHAEnviarLoteRPSSincrono;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs;

type
  TBETHAEnviarLoteRPSSincrono = class
  public
    procedure Executar;
  end;

implementation

procedure TBETHAEnviarLoteRPSSincrono.Executar;
var
  oConfiguracao: OleVariant;
  oRecepcionarLoteRpsSincrono: OleVariant;
  oExceptionInterop: OleVariant;
  XML: string;
begin
  // Criar objeto para capturar exceção C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Configuração do serviço
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 5; // NFSe
    oConfiguracao.CertificadoArquivo := 'C:\\Projetos\\certificados\\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';
    oConfiguracao.CodigoMunicipio := 9999903;
    oConfiguracao.TipoAmbiente := 2; // Homologação
    oConfiguracao.Servico := 29; // NFSeRecepcionarLoteRpsSincrono
    oConfiguracao.SchemaVersao := '2.02';

    // Montar XML manualmente (evita namespace ns0)
    XML :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<EnviarLoteRpsSincronoEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">' +
      '  <LoteRps Id="ID11628" versao="2.02">' +
      '    <NumeroLote>11628</NumeroLote>' +
      '    <CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj>' +
      '    <InscricaoMunicipal>99999999999999</InscricaoMunicipal>' +
      '    <QuantidadeRps>2</QuantidadeRps>' +
      '    <ListaRps>' +
      '      <!-- RPS 1 -->' +
      '      <Rps>' +
      '        <InfDeclaracaoPrestacaoServico Id="ID1">' +
      '          <Rps>' +
      '            <IdentificacaoRps>' +
      '              <Numero>1551</Numero>' +
      '              <Serie>1</Serie>' +
      '              <Tipo>1</Tipo>' +
      '            </IdentificacaoRps>' +
      '            <DataEmissao>2021-07-27</DataEmissao>' +
      '            <Status>1</Status>' +
      '          </Rps>' +
      '          <Competencia>2021-07-27</Competencia>' +
      '          <Servico>' +
      '            <Valores>' +
      '              <ValorServicos>50.00</ValorServicos>' +
      '              <ValorDeducoes>0.00</ValorDeducoes>' +
      '              <ValorPis>0.00</ValorPis>' +
      '              <ValorCofins>0.00</ValorCofins>' +
      '              <ValorInss>0.00</ValorInss>' +
      '              <ValorIr>0.00</ValorIr>' +
      '              <ValorCsll>0.00</ValorCsll>' +
      '              <OutrasRetencoes>0.00</OutrasRetencoes>' +
      '              <DescontoIncondicionado>0.00</DescontoIncondicionado>' +
      '              <DescontoCondicionado>0.00</DescontoCondicionado>' +
      '            </Valores>' +
      '            <IssRetido>2</IssRetido>' +
      '            <ItemListaServico>1401</ItemListaServico>' +
      '            <Discriminacao>SERVICO DE CORTE E DOBRA A/c</Discriminacao>' +
      '            <CodigoMunicipio>1111111</CodigoMunicipio>' +
      '            <ExigibilidadeISS>1</ExigibilidadeISS>' +
      '            <MunicipioIncidencia>1111111</MunicipioIncidencia>' +
      '          </Servico>' +
      '          <Prestador>' +
      '            <CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj>' +
      '            <InscricaoMunicipal>99999999999999</InscricaoMunicipal>' +
      '          </Prestador>' +
      '          <Tomador>' +
      '            <IdentificacaoTomador><CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj></IdentificacaoTomador>' +
      '            <RazaoSocial>xxxxxxxx ALIMENTOS LTDA</RazaoSocial>' +
      '            <Endereco>' +
      '              <Endereco>ROD. xxxxx xxxxxxxxx</Endereco>' +
      '              <Numero>sn</Numero>' +
      '              <Bairro>ZONA RURAL</Bairro>' +
      '              <CodigoMunicipio>1111111</CodigoMunicipio>' +
      '              <Uf>PR</Uf>' +
      '              <Cep>87706060</Cep>' +
      '            </Endereco>' +
      '            <Contato><Telefone>1111111111111</Telefone><Email>teste@hotmail.com</Email></Contato>' +
      '          </Tomador>' +
      '          <RegimeEspecialTributacao>1</RegimeEspecialTributacao>' +
      '          <OptanteSimplesNacional>2</OptanteSimplesNacional>' +
      '          <IncentivoFiscal>2</IncentivoFiscal>' +
      '        </InfDeclaracaoPrestacaoServico>' +
      '      </Rps>' +
      '      <!-- RPS 2: Repete mesma estrutura com ID2 -->' +
      '      <!-- ... Copiar e colar estrutura de RPS 1 ajustando Id="ID2" ... -->' +
      '    </ListaRps>' +
      '  </LoteRps>' +
      '</EnviarLoteRpsSincronoEnvio>';

    oRecepcionarLoteRpsSincrono := CreateOleObject('Unimake.Business.DFe.Servicos.NFSe.RecepcionarLoteRpsSincrono');
    oRecepcionarLoteRpsSincrono.Executar(XML, IUnknown(oConfiguracao));

    ShowMessage('XML retornado pela prefeitura:' + sLineBreak + oRecepcionarLoteRpsSincrono.RetornoWSString);

  except
    on E: Exception do
    begin
      ShowMessage('Erro ao enviar NFSe: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.

