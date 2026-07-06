---
name: inserir-municipio-nfse
description: Use quando Codex precisar adicionar um novo municipio NFSe na DLL Unimake.DFe, pedindo codigo IBGE, link de producao, link de homologacao opcional, padrao NFSe e versao, atualizando o arquivo Servicos/Config/Config.xml, criando o XML do municipio em Servicos/Config/NFSe e ajustando o .csproj para EmbeddedResource com base em municipios ja existentes do mesmo padrao e, de preferencia, da mesma versao.
---

# Inserir Municipio NFSe

## Objetivo

Adicionar um novo municipio NFSe na DLL `Unimake.DFe` sem inventar estrutura nova.

Sempre reaproveite o padrao ja existente no projeto:

1. localizar um municipio ja implementado com o mesmo `PadraoNFSe`;
2. preferir um XML da mesma `versao`;
3. copiar a estrutura existente;
4. trocar somente os dados especificos do novo municipio;
5. incluir o novo XML como recurso embutido no `.csproj`.

## Entrada obrigatoria

Antes de alterar arquivos, obtenha:

1. `codigo IBGE` do municipio;
2. `link de producao`;
3. `padrao NFSe`;
4. `versao`.

Tambem solicite:

1. `link de homologacao`, quando existir;
2. `nome do municipio`;
3. `UF`.

O link de homologacao nao e obrigatorio.

Exemplo de entrada:

```text
Codigo IBGE: 3550308
Municipio: Sao Paulo
UF: SP
Producao: https://producao.exemplo.gov.br/ws
Homologacao: https://homologacao.exemplo.gov.br/ws
Padrao NFSe: GINFES
Versao: 3.01
```

Se faltar qualquer item obrigatorio, peca somente os campos faltantes antes de editar arquivos.

## Arquivos envolvidos

Trabalhe principalmente nestes arquivos:

```text
C:\Projetos\GitHub\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\Config.xml
C:\Projetos\GitHub\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFSe\
C:\Projetos\GitHub\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj
```

## Fluxo

### 1. Verificar o padrao no Config.xml

Leia `Servicos/Config/Config.xml` e procure um municipio que ja use o `PadraoNFSe` informado.

Objetivo dessa etapa:

1. confirmar que o padrao ja existe no projeto;
2. descobrir como o grupo do municipio esta montado no `Config.xml`;
3. reaproveitar o valor exato de `PadraoNFSe`.

Se nao existir nenhum municipio com o padrao informado, aborte a operacao e informe ao usuario que o padrao ainda nao esta implementado no projeto.

### 2. Montar o grupo do municipio no Config.xml

Ao inserir o novo municipio em `Config.xml`, use esta estrutura:

```xml
<Arquivo ID="3505500">
    <Nome>Barretos - SP</Nome>
    <UF>SP</UF>
    <ArqConfig>BarretosSP.xml</ArqConfig>
    <PadraoNFSe>RLZ_INFORMATICA</PadraoNFSe>
</Arquivo>
```

Regras obrigatorias:

1. `ID`: usar o codigo IBGE do municipio.
2. `Nome`: usar o formato `Municipio - UF`.
3. `UF`: usar a sigla do estado.
4. `ArqConfig`: usar o nome do arquivo XML fisico do municipio.
5. `PadraoNFSe`: usar exatamente o padrao informado, reaproveitando a grafia existente no projeto.

### 3. Regra de nome do arquivo XML

O nome do arquivo em `Servicos/Config/NFSe` deve seguir o padrao observado no projeto:

```text
NomeMunicipioUF.xml
```

Regras:

1. remover espacos;
2. remover acentos;
3. concatenar o nome do municipio com a `UF`;
4. manter extensao `.xml`.

Exemplos:

```text
Sao Jose dos Campos - SP -> SaoJoseDosCamposSP.xml
Belem - PA -> BelemPA.xml
Rio das Ostras - RJ -> RioDasOstrasRJ.xml
```

Nao invente outro formato para `ArqConfig`.

### 4. Escolher o XML-base do municipio

Em `Servicos/Config/NFSe`, procure arquivos existentes do mesmo `PadraoNFSe`.

Ao escolher a base:

1. prefira um municipio da mesma `versao`;
2. se houver mais de um na mesma versao, escolha o mais aderente ao mesmo conjunto de servicos;
3. se nao houver mesma versao, use um XML do mesmo padrao, informe esse fato ao usuario e continue a implementacao.

Considere a versao pelos elementos do arquivo, principalmente:

1. atributo `versao` dos servicos;
2. tag `SchemaVersao`;
3. qualquer outra convencao explicita ja existente no XML-base.

Se nao existir XML da mesma versao:

1. continue a implementacao com um XML-base do mesmo padrao;
2. informe no resultado final que a versao exata nao foi encontrada;
3. deixe vazia a informacao de versao na configuracao do municipio sempre que houver um campo especifico de versao dependente desse mapeamento.

### 5. Montar o XML do novo municipio

Crie o novo arquivo XML a partir do XML-base escolhido.

Mantenha a estrutura do arquivo base, incluindo:

1. servicos existentes;
2. nomes de tags;
3. `SchemaArquivo`;
4. `WebActionHomologacao` e `WebActionProducao`;
5. `TagAssinatura`, `TagAtributoID`, `TagLoteAssinatura`, `TagLoteAtributoID`;
6. `WebSoapString`, `WebSoapStringHomologacao`, `WebSoapStringProducao`;
7. `WebTagRetorno`, `WebEncodingRetorno`, `WebSoapVersion`, `SchemaVersao` e `TargetNS`.

Troque somente o que for especifico do municipio novo:

1. `WebEnderecoProducao`;
2. `WebEnderecoHomologacao`, quando informado;
3. outros endpoints explicitamente dependentes do ambiente, quando o XML-base usar conteudo separado por ambiente;
4. a informacao de versao, quando a versao solicitada nao existir no padrao reaproveitado.

Se o XML-base usar strings SOAP separadas para producao e homologacao, atualize os dois blocos quando necessario.

Quando a versao informada pelo usuario nao existir entre os municipios implementados do mesmo padrao:

1. informe isso explicitamente ao usuario;
2. continue a criacao do municipio usando um XML-base do mesmo padrao;
3. deixe vazia a tag de versao na configuracao do municipio, em vez de inventar ou forcar uma versao nao encontrada.

Se o usuario nao informar homologacao:

1. nao invente URL de homologacao;
2. preserve a ausencia de homologacao de forma coerente com o padrao do arquivo base;
3. se o XML-base exigir a tag de homologacao, mantenha a tag vazia somente se esse for o padrao ja usado no projeto para aquele provedor.

Nao altere a estrutura do XML apenas para acomodar o novo municipio.

### 6. Incluir o arquivo no .csproj

Depois de criar o XML do municipio, ajuste `Unimake.Business.DFe.csproj` para embuti-lo como recurso.

Siga o mesmo padrao dos outros XMLs de `Servicos/Config/NFSe`, incluindo:

1. a remocao em bloco `None Remove="Servicos\Config\NFSe\NomeMunicipioUF.xml"`;
2. a inclusao como `EmbeddedResource Include="Servicos\Config\NFSe\NomeMunicipioUF.xml"`.

Nao crie um tratamento especial fora do padrao ja usado pelos demais municipios NFSe.

## Regras de decisao

### Quando o padrao ja existe no Config.xml

Monte o novo grupo do municipio em `Config.xml` copiando a estrutura de um municipio existente com o mesmo `PadraoNFSe`.

### Quando existem varios XMLs do mesmo padrao

Use a mesma versao primeiro.

### Quando existe o padrao, mas nao a mesma versao

Use um XML do mesmo padrao como base, informe isso ao usuario e deixe vazia a tag de versao na configuracao do municipio.

### Quando nao existe homologacao

Siga apenas com producao e nao crie URL ficticia.

## Restricoes

1. nao inventar `PadraoNFSe`;
2. nao inventar estrutura nova para `Config.xml`;
3. nao inventar nome de arquivo fora do padrao `NomeMunicipioUF.xml`;
4. nao reescrever manualmente um XML de municipio se ja existir um XML-base do mesmo padrao;
5. nao alterar outros municipios sem necessidade;
6. nao mudar servicos, schemas, actions ou payloads alem do necessario para o novo municipio;
7. nao incluir homologacao ficticia;
8. nao esquecer de ajustar o `.csproj`;
9. nao inventar valor de versao quando a versao solicitada nao existir.

## Checklist antes de finalizar

- [ ] Os dados obrigatorios foram obtidos.
- [ ] O `PadraoNFSe` foi localizado em `Config.xml`.
- [ ] O grupo do novo municipio no `Config.xml` usa `ID`, `Nome`, `UF`, `ArqConfig` e `PadraoNFSe`.
- [ ] `Nome` esta no formato `Municipio - UF`.
- [ ] `ArqConfig` esta no formato `NomeMunicipioUF.xml`.
- [ ] O XML-base foi escolhido a partir do mesmo `PadraoNFSe`.
- [ ] Foi priorizado municipio da mesma `versao`.
- [ ] Quando a versao nao existiu, o usuario foi informado e a tag de versao do municipio ficou vazia.
- [ ] O novo XML do municipio foi criado em `Servicos/Config/NFSe`.
- [ ] Os links de producao e homologacao foram substituidos corretamente.
- [ ] O `.csproj` foi atualizado com `None Remove` e `EmbeddedResource`.

## Saida esperada

Ao concluir, responda com um resumo objetivo contendo:

```text
Municipio:
- ...

Codigo IBGE:
- ...

Padrao NFSe:
- ...

Versao:
- ...

Arquivos alterados:
- ...

XML-base utilizado:
- ...

Resultado:
- ...
```
