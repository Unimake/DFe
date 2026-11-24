# AACBS-002 — Modelos XML, schemas e validação

## Identificação e liberação

| Campo | Registro |
| --- | --- |
| Etapa / tentativa | AACBS-002 / tentativa-0001 |
| Data / estado | 24/11/2026, America/Sao_Paulo / DELIVERED_FOR_REVIEW |
| Base Git | main. Início em 451b90b04a8effe061e655b3eebba2cb96f96fbc; HEAD observado no fechamento 027c2abbf0aebf292ed1ddea4714ff6a3c31e921, após trabalho paralelo fora desta etapa. Alterações anteriores de documentação, acesso local, NFCe/NFe e versão do projeto preservadas; nenhum commit executado pela AACBS-002. |
| Ambiente | Windows, SDK 11.0.100; testes net10.0 no runtime .NET 10.0.15, xUnit v3 in-process 4.0.3; biblioteca netstandard2.0, C# 7.3. |
| Liberação do DEV | "Aprovo a AACBS-001 e libero a execução da AACBS-002. Pode seguir com as propostas documentadas para as datas e o retorno global do DARF RAD." |
| Escopo | Modelos XML, XSDs, recursos, enums de identificação, validação central e de conteúdo, testes offline. Sem serviços HTTP ou uso de credenciais. |
| Contrato | [Especificação aprovada](../../../contratos/ESPECIFICACAO.md), catálogo de 24 raízes e três matrizes de campos. |

## Fontes

Os 23 hashes locais da baseline da AACBS-001 foram conferidos. A [revalidação remota](REVALIDACAO-FONTES.csv) encontrou 21 arquivos byte a byte iguais. Os dois comunicados GOV.BR tiveram HTML diferente; a extração do texto dos elementos `article`, remoção de marcação, decodificação HTML e normalização de espaços confirmou texto integral igual: [comparação](REVALIDACAO-COMUNICADOS.csv), 2.460 e 4.924 caracteres. A baseline aprovada não foi substituída. Não houve alteração de contrato a incorporar nesta etapa.

## Responsabilidade e paralelismo

| Agente | Modelo / esforço | Responsabilidade |
| --- | --- | --- |
| Orquestrador | Modelo herdado da sessão | Comuns, catálogo/enums centrais, recursos de projeto, integração da validação, correções, PDCA e evidência. |
| aacbs002_arquivos | gpt-6.2-sol / medium | Quatro arquivos finais, seus grupos e quatro XSDs. |
| aacbs002_fluxo | gpt-6.2-sol / medium | Vinte demais raízes e XSDs, auxiliares DARF, dois enums explicitamente previstos. |
| aacbs002_testes | gpt-6.2-sol / medium | Autoria independente dos testes e 24 massas XML sintéticas. |
| aacbs002_revisao | gpt-6.2-astra / high, contexto independente | Revisão somente leitura contra matrizes/fontes e caminhos públicos. |
| aacbs002_executor | gpt-6-luna / low | Único executor dos builds e testes sobre os arquivos estabilizados. |

As responsabilidades foram comunicadas nos prompts dos agentes. Arquivos comuns e globais permaneceram com o orquestrador. Correções nos auxiliares DARF foram devolvidas ao autor; somente um agente executou builds/testes por vez. O [CHECKPOINT-0004.csv](CHECKPOINT-0004.csv) identifica os 94 arquivos de código, schemas, projetos e massas da validação final. A comparação após os comandos confirmou 94/94 hashes inalterados. Mudanças apenas documentais posteriores não alteram esse checkpoint.

## Entrega e critérios

| Critério AACBS-002 | Entrega verificável | Resultado |
| --- | --- | --- |
| Classes por contrato | 24 raízes em 32 arquivos de modelos, grupos explícitos, decimal/nullable, Field de instante e documentação pública; namespace/versão aprovados. | Atendido; round-trip de todas as raízes e ramos. |
| Schemas de envio e retorno | 24 XSDs de raiz e tiposComuns_v1.00.xsd, embutidos; escolhas de sucesso/erro/erroLocal e variantes de datas. | Atendido; todos localizados e exercitados pelo ValidarSchema. |
| Identificação e validação central | TipoDFe.AACBS=24, dez serviços 140–149, dois enums de domínio, 24 registros em ValidarConfig.xml; raiz qualificada e versão. | Atendido; incluindo prefixos XML e rejeição de raiz/namespace/versão indevidos. |
| Conteúdo | AACBSValidator registra ValidateTag e complementa o XSD em parciais, estados, URLs, datas e ramos RAD/PCONT. | Atendido, sem requisições HTTP. |
| Fidelidade e erros | Ausência/zero, duplicatas, coleções vazias/nulas, precisão, frações sub-tick, código aberto de erro e datas publicadas. Leitura XML AACBS valida antes de materializar, sem refletir o XML nas exceções. | Atendido; campos desconhecidos/incompatíveis rejeitados. |
| Compatibilidade | Caminhos novos da leitura/escrita exigem XmlRoot no namespace AACBS exato; XMLBase não foi alterado. Blocos COM, Fields e helpers de coleções. | Build normal e INTEROP aprovados; regressão dos pontos compartilhados aprovada. |

Arquivos para revisão: [modelos XML](<../../../../../source/.NET Standard/Unimake.Business.DFe/Xml/AACBS>), [XSDs](<../../../../../source/.NET Standard/Unimake.Business.DFe/Xml/Schemas/AACBS>), [validador de conteúdo](<../../../../../source/.NET Standard/Unimake.Business.DFe/Validator/AACBS/AACBSValidator.cs>) e [testes e massas](../../../../../source/Unimake.DFe.Test/AACBS).

## Verificação executada

Os autores fizeram conferências estáticas: 24 XSDs compilados individualmente por `XmlSchemaSet`, 22 verificações sintéticas estruturais, nomes e caminhos das matrizes. Esses checks não substituem os builds e testes da DLL.

O primeiro build principal passou com quatro avisos preexistentes de obsolescência em EnviNFe. A inicialização do CLI exigiu desativar telemetria somente no processo devido a acesso ao registro; também houve timeout do servidor MSBuild com fallback e aviso do pre-build `*Undefined*Build.bat`. O build do projeto de testes encontrou um `using Unimake.Business.DFe` ausente no helper de testes (CS0246), corrigido antes da nova execução. Os logs e checkpoints intermediários foram preservados apenas localmente e estão ignorados pelo Git; não contam como prova do estado final. O checkpoint 0004 e os quatro logs finais sanitizados referenciados abaixo são versionados.

A primeira execução xUnit teve 11 falhas em 226 casos: a coleção opcional dadosInvalidos reaparecia vazia e um texto opaco composto por espaço era perdido. Após a primeira correção, 81 casos expuseram a inclusão da indentação estrutural no InnerText. Os hooks de leitura dos dois retornos DARF restauram a ausência do grupo, sem omitir uma coleção vazia explícita; o leitor AACBS preserva whitespace e o escritor não acrescenta indentação estrutural. O checkpoint 0004 passou sem modificar as expectativas dos dados para ocultar esses defeitos.

| Comando / filtro do checkpoint 0004 | Resultado | Log |
| --- | --- | --- |
| `dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore` — inclui biblioteca normal netstandard2.0 | Exit 0; 4 avisos CS0618 de EnviNFe, 0 erros. | [build-testes-0004.log](build-testes-0004.log) |
| `dotnet Unimake.DFe.Test.dll -trait "DFe=AACBS"` | Exit 0; 226 aprovados, 0 falhas/erros/ignorados. | [testes-aacbs-0004.log](testes-aacbs-0004.log) |
| Mesma DLL, cinco filtros `-class` descritos abaixo | Exit 0; 66 aprovados, 0 falhas/erros/ignorados. | [regressao-0004.log](regressao-0004.log) |
| `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore -c INTEROP_Release` | Exit 0; 11 avisos CS0618 de EnviNFe, 0 erros. | [build-interop-0004.log](build-interop-0004.log) |

O runner foi executado em `source/Unimake.DFe.Test/bin/Debug/net10.0`, necessário para os caminhos relativos dos recursos legados. As cinco classes da regressão foram `Unimake.DFe.Test.Utility.Validacao.ValidarSchemaTest`, `Unimake.DFe.Test.Utility.Validacao.ValidarConfigTest`, `Unimake.DFe.Test.Utility.Validacao.NormalizacaoXMLTest`, `Unimake.DFe.Test.NFe.Serializacao.DesserializacaoSeguraTest` e `Unimake.DFe.Test.DCe.Serializacao.SerializacaoDesserializacaoTest`, cada uma em seu argumento `-class` na mesma chamada. Foram escolhidas pelos pontos compartilhados tocados, sem rodar toda a suíte ou serviços externos. `DOTNET_CLI_TELEMETRY_OPTOUT=1` ficou restrito aos processos de execução. As mensagens ambientais de pre-build/servidor MSBuild continuaram sem impedir os builds finais.

O fechamento documental reconferiu os 94 hashes do checkpoint, 19 arquivos Markdown e 112 links locais válidos, sem espaços finais nos documentos ou no código novo AACBS. `git diff --check` terminou com exit code 0. Os dois arquivos privados de acesso continuam ignorados pelo Git; seu conteúdo não foi lido nesta etapa.

## Revisão independente

A revisão confrontou 247 caminhos XML das matrizes e a ordem dos campos de 44 tipos complexos, sem omissões. Identificou e motivou correções de: faceta inválida em xs:date; arredondamento de decimais antes da validação; omissão silenciosa de itens nulos; aplicação de regras RAD à família de recolhimentos; bypass por representação lexical de inteiro; restrições de URL/texto mais amplas que o contrato; mensagens de erro de XSD/desserialização que podiam refletir valores sensíveis; seleção de raiz em schemas com includes; e preservação de espaços. O estado final foi reconferido por inspeção e reproduções isoladas no CLR: **sem achados materiais pendentes no escopo AACBS-002**. Os testes automatizados do mesmo checkpoint também passaram. Isso não equivale à aprovação humana.

## Limites, retomada e gate

Sem autenticação, obtenção de token, chamadas de negócio ou leitura das credenciais locais. O único tráfego foi a leitura de documentação pública. A AACBS-003 não foi iniciada.

Não houve empacotamento, publicação, alterações de consumidores ou teste COM/OLE por aplicação externa. O build INTEROP e os testes de contrato não comprovam consumo HTTP; serviços, mappers JSON, autenticação/cache/journal e provas reais permanecem nas etapas próprias. A ausência de rota PRR qualificada continua sendo restrição para o real da AACBS-003, sem fallback para produção.

Para retomar, ler PDCA e especificação aprovada, comparar as fontes e os arquivos do checkpoint e executar somente a etapa liberada. Reversão, se solicitada, deve retirar os arquivos novos AACBS e apenas os hunks de integração desta etapa, preservando o trabalho paralelo, as evidências aprovadas e todo acesso privado. Nenhuma reversão foi executada.

Entregue ao DEV em 24/11/2026. Decisão humana para a AACBS-002: **AGUARDANDO**. AACBS-003 a AACBS-005 não iniciadas. O PDCA é a fonte única de estado.
