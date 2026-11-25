# Preparo de DARF em Produção Beta

Em 25/11/2026, a AACBS-005 preparou uma proposta de RAD para revisão local pelo DEV. A emissão permanece pendente. Para PCONT, o DEV confirmou que o CNPJ disponível não possui cenário de teste. Toda a análise usou arquivos existentes e executou zero chamadas HTTP.

| Operação | Parâmetros propostos | Situação |
| --- | --- | --- |
| RAD-01, adquirente | CNPJ completo da matriz configurada, uma chave de NF-e do arquivo real de créditos, `PAGAMENTO_TOTAL`, campo parcial ausente | `PENDENTE_CONFIRMACAO`. O DEV precisa confirmar que a matriz é a adquirente, que há débito ainda não extinto nessa NF-e e que admite gerar uma guia pelo total em Produção Beta. A Receita decide a elegibilidade ao processar a solicitação. |
| PCONT, contribuinte | O contrato recebe CNPJ completo, um período no formato `MM/yyyy` e opção de pagamento. O valor é exigido na opção parcial. | Aguardando contribuinte com acesso autorizado e período com saldo a recolher, conforme informado pelo DEV. Preparação do XML e escolha da opção ficam para quando houver esses dados. |

## Como o documento da proposta foi escolhido

O CNPJ completo do arquivo original de créditos e do envelope XML coincide com o da matriz configurada. O tíquete coincide nos dois arquivos. Os dois períodos, os 27 registros e sua ordem foram conferidos entre JSON e XML, comparando período, origem, modelo, chave e crédito a apropriar. Na consulta de débitos da mesma raiz, a coleção de períodos estava vazia.

Os 27 registros de crédito têm origem normal, CBS apurada positiva e valor positivo a apropriar. Desses, dois são NF-e com uma única ocorrência da chave no arquivo, 44 dígitos, modelo 55 coerente com o campo de modelo da chave, dígito verificador correto e emitente de raiz distinta da matriz. A proposta selecionou a primeira dessas duas chaves na ordem original. O arquivo identifica o crédito da matriz. O campo `aApropriar` indica a parcela do crédito ainda indisponível para uso. A confirmação sobre quem adquiriu o documento e sobre o débito passível de RAD continua pendente. [Consulta de créditos](../../../fontes/2026-11-24/credito.html) · [DARF do adquirente](../../../fontes/2026-11-24/gerar-darf-ad.html)

A opção total submete à revisão do DEV o valor integral que a Receita calcular para a chave. Esse valor está em aberto. Nenhum valor do arquivo de créditos foi usado para preencher o DARF. O grupo de períodos vazio, em conjunto com a informação do DEV, deixa o PCONT sem base para escolher um período. [DARF do contribuinte](../../../fontes/2026-11-24/gerar-darf-contrib.html)

## Conferência pelo DEV

A partir da raiz do checkout, execute o comando a seguir em console privado da conta Windows que protegeu os arquivos. O visualizador confere o SHA-256 e mostra o CNPJ, a chave exata e a opção propostos. A opção `-SomenteVerificar` permite verificar o arquivo sem exibir os parâmetros. Foi essa a modalidade usada na conferência técnica.

```powershell
pwsh -NoProfile -File .local/aacbs/cenarios-darf005/Visualizar-RAD01.ps1
```

A proposta está em `.local/aacbs/cenarios-darf005/RAD-01.xml.dpapi`, fora do versionamento. O XML usa a raiz `gerarDarfAdquirente`, versão `1.00` e um único `solicitacoes/item`. Passou pela validação offline com o XSD de envio e pela comparação integral após releitura do arquivo cifrado. A referência para uma eventual autorização é **RAD-01** e o SHA-256 abaixo. O limite proposto para essa autorização é uma emissão em Produção Beta, com parada após o primeiro retorno ou falha ambígua. Nenhum pagamento integra o cenário. [Contrato público](../../../contratos/DARF.md)

| Artefato | SHA-256 dos bytes cifrados |
| --- | --- |
| Original de créditos | `13CD64C7D2FAEBEEFE2C606CCEF4623A52602D78E2200EE34FA956E0CE4F4474` |
| Envelope XML de créditos | `B9DC58F16A2DE54847F385D54C950365EEB40B3CAFAF9D8A2E136A317D2B502F` |
| Envelope XML de débitos | `CE1200EF13CBA43C4E1BD29021E973FBEE9F6DA0921E7EA4E2C35339095B50B7` |
| Proposta RAD-01 | `D663880BB7351232C3896C18BD5D60F16B1A42E41D3C259F3E6294F74056F4D1` |

Os dois hashes de créditos coincidem com a [verificação da tentativa anterior](../tentativa-0001/VERIFICACAO-REAL-CREDITOS.json). O preparo consumiu somente essas três fontes cifradas e o CNPJ de configuração. Nenhum XML fiscal em claro foi gravado.
