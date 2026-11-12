# Agentes da integração AACBS

Este documento define papéis e instruções reutilizáveis para executar o plano de Apuração Assistida da CBS. Não instala configuração global nem cria agentes permanentes. O orquestrador cria agentes de trabalho somente para a etapa liberada pelo DEV e registra quais foram efetivamente usados.

## Limites comuns

- Ler `AGENTS.md` da raiz, `README.md`, `PLANO.md`, `DECISOES.md`, `PDCA.md`, as fontes da etapa e as instruções aplicáveis aos arquivos tocados.
- Executar apenas a etapa atual. A aprovação humana de uma etapa libera a seguinte; a aprovação e o início são registrados separadamente, sem nova pergunta ritual. Nenhum agente antecipa trabalho da sucessora antes dessa aprovação.
- Preservar C# 7.3, `netstandard2.0`, INTEROP/COM, documentação pública e os padrões de XML, validação, configuração e transporte existentes.
- Expor XML por operação. Transformação para a API e conversão de retorno pertencem à DLL; espera, tickets, retomada e sequência de chamadas pertencem ao consumidor.
- Cobrir somente a CBS documentada nas fontes qualificadas. IBS é expansão futura e não autoriza endpoints ou contratos inferidos.
- Não registrar certificados, senhas, PIN, tokens ou dados fiscais reais em código, massas versionadas, fixtures, logs ou evidências. Usar massas sintéticas ou anonimizadas. Configuração local protegida e gravação operacional dos XMLs reais são responsabilidades do consumidor e seguem o contrato da DLL; retornos com token ou URL temporária exigem proteção, sem cópia para o repositório ou para o dossiê.
- Não alterar arquivos de outro agente sem combinar a transferência com o orquestrador. Comunicar bloqueios e divergências de contrato com referência verificável.

## Papéis e divisão de trabalho

Há quatro slots concorrentes neste ambiente, incluindo o orquestrador. A configuração usual é um orquestrador e até três agentes: implementação, autoria de testes e execução. Revisão independente usa um slot liberado e contexto novo.

| Papel | Responsabilidade | Saída verificável |
|---|---|---|
| Orquestrador | Confirmar etapa liberada, qualificar fontes, resolver dependências, distribuir arquivos, integrar entregas e apresentar o gate humano | Escopo, contratos comuns, propriedade de arquivos, PDCA e entrega da etapa |
| Implementador | Produzir classes, XSDs, mappers, validadores, configurações e serviços atribuídos | Código e contratos implementados; lista de arquivos e limitações |
| Autor dos testes | Derivar cenários das fontes oficiais e critérios; produzir fixtures e testes que detectem erros do contrato | Testes positivos, negativos e de regressão adequados ao incremento |
| Executor | Executar comandos definidos sobre um incremento estável, acompanhar processos e resumir resultados | Comandos, exit codes, contagens, falhas e identidade do conteúdo testado |
| Revisor independente | Confrontar contrato, diff, testes e evidências sem depender da justificativa do implementador | Achados acionáveis ou parecer sem achados no escopo revisado |

O autor dos testes não deve apenas repetir a implementação. Deve verificar nomes e tipos, obrigatoriedade, opcionais, arrays, precisão, datas, erros e fidelidade de XML/JSON contra a documentação qualificada.

## Modelos e esforço

As opções abaixo estão disponíveis nas ferramentas da sessão de planejamento em 2026-11-12. Disponibilidade deve ser reconferida ao iniciar a execução. São recomendações por tarefa, sem estimativa de preço ou promessa de consumo.

| Perfil | Modelo sugerido | Esforço inicial | Uso e escalonamento |
|---|---|---|---|
| Econômico | `gpt-6-luna` | `low` | Rodar comandos já definidos, acompanhar processos e consolidar resultados objetivos; escalar análise técnica de falhas |
| Implementação | `gpt-6.1-sol` ou `gpt-6.1-terra` | `medium` | Código e testes delimitados por contratos estáveis; aumentar esforço se o contrato ou a falha exigir |
| Decisão complexa | `gpt-6-astra` | `high` | Arquitetura, contratos ambíguos, autenticação e riscos de compatibilidade que não forem resolvidos pelo perfil anterior |
| Revisão independente | `gpt-6.1-sol`, `gpt-6.1-terra` ou `gpt-6-astra` | `high` | Escolher capacidade adequada à complexidade; autenticação e contratos críticos exigem revisão experiente em contexto novo |

Registrar modelo e esforço efetivos por agente na evidência. Preferir o perfil econômico quando o trabalho for execução mecânica; não delegar decisões de contrato a um executor só para reduzir consumo. Modelos mais caros não precisam ficar aguardando comandos de longa duração. Espera de processo usa as ferramentas de sessão, com acompanhamento limitado e sem mensagens repetidas de estado inalterado.

Ao selecionar modelo ou esforço diferente na criação de um agente, usar contexto inicial independente (`fork_turns: "none"`) e fornecer o escopo, os caminhos e as decisões necessários no prompt. Não copiar catálogos antigos de outros planos como prova de disponibilidade atual. Se o perfil pretendido não estiver disponível, escolher alternativa disponível adequada e registrar a mudança; não inventar um modelo.

## Paralelismo e propriedade de arquivos

1. Antes de delegar, o orquestrador registra na evidência os agentes, os arquivos/subárvores sob sua responsabilidade e o contrato comum necessário.
2. Implementação e autoria dos testes podem ocorrer simultaneamente em arquivos distintos. O autor dos testes pode preparar casos e fixtures enquanto as classes são produzidas.
3. Enums compartilhados, `.csproj`, configurações globais e resolvedores centrais têm um único responsável por vez. Alterações pequenas nesses arquivos são integradas em sequência.
4. Há um único executor de builds e testes por checkout. `obj`, `bin`, `source/Unimake.DFe/Compilacao` e o XML de documentação podem ser compartilhados entre comandos; separar apenas o diretório de saída dos testes não elimina colisões.
5. O executor recebe um checkpoint estável: base Git, lista dos arquivos relevantes e SHA-256 de seus conteúdos. Não compilar enquanto esses arquivos estiverem sendo alterados. Gerar a DLL e executar os testes desse mesmo checkpoint, sem outro build substituindo os artefatos durante a execução.
6. Enquanto o executor testa um incremento pronto, outro agente pode produzir conteúdo independente da mesma etapa, sem alterar o checkpoint nem a saída em uso. O resultado vale para o incremento identificado, não para arquivos adicionados depois.
7. Ao integrar conteúdo posterior, repetir os checks afetados e produzir a validação final do estado entregue. Mudança relevante entre o início e o fim da validação invalida a evidência daquele check.
8. Agentes não fazem commits, publicações, atualização de consumidores ou chamadas reais por iniciativa própria. Essas ações seguem o escopo da etapa e as autorizações existentes.

Não criar worktrees ou fluxos de build paralelos apenas para ocupar slots. Usá-los somente se uma dependência concreta justificar o custo e os caminhos de saída estiverem comprovadamente isolados.

## Prompts iniciais reutilizáveis

Os campos entre `<...>` são preenchidos pelo orquestrador ao iniciar cada agente. Não delegar com uma decisão material em aberto escondida nesses campos.

### Implementador

> Execute somente `<etapa>` da integração AACBS. Leia as instruções da raiz e os documentos em `docsplan/aacbs`. Seu escopo é `<entrega>` e você pode editar somente `<arquivos/subárvores>`. O contrato qualificado está em `<fontes e seções>` e as decisões aplicáveis são `<decisões>`. Preserve XML público por endpoint, C# 7.3 e INTEROP; a DLL converte API/XML e o consumidor controla o fluxo assíncrono. Não execute builds, chamadas reais ou trabalho da etapa seguinte. Coordene assinaturas públicas com o autor dos testes. Entregue arquivos alterados, comportamento, pendências e riscos com referências. Não declare testes que não executou.

### Autor dos testes

> Produza os testes de `<etapa>` para `<contratos/cenários>`. Leia `docsplan/aacbs` e as fontes qualificadas `<referências>`. Edite somente `<arquivos de teste e recursos>`. Derive expectativas da documentação e dos critérios de aceitação; procure divergências entre XML, JSON, XSD e erros. Use fixtures sintéticas, teste o fluxo público com transporte controlado quando aplicável e mantenha traits/namespaces conforme o diretório. Não execute builds ou rede de negócio. Combine dependências públicas com o implementador e informe cobertura, lacunas e comandos focados sugeridos.

### Executor

> Verifique o checkpoint `<base Git, lista de arquivos e hashes>` de `<etapa>`. Você é o único executor de builds/testes neste checkout. Não edite código ou testes. Execute nesta ordem `<comandos exatos>`, com os filtros e limites definidos pelo orquestrador. Use o runner xUnit v3 da DLL e não substitua os filtros por toda a suíte. Acompanhe as sessões sem repetir mensagens de estado inalterado. Registre duração, exit code, contagens e falhas sanitizadas. Reconfira os arquivos do checkpoint antes de concluir. Se a fonte mudar ou aparecer falha fora do roteiro, comunique e aguarde orientação técnica; não afrouxe validação nem certificado.

### Revisor independente

> Revise a entrega `<etapa>` em contexto independente. Leia as fontes qualificadas, decisões, critérios, diff e evidência em `<caminhos>`. Não edite arquivos nem se baseie apenas no relato dos autores. Confira fidelidade dos contratos XML/API, XSDs, erros, autenticação, configurações, compatibilidade e validade das evidências no escopo entregue. Aponte apenas problemas técnicos verificáveis com arquivo, trecho, cenário e impacto. Diferencie falhas, limitações ambientais e melhorias opcionais. Informe o resultado da revisão sem aprovar a etapa: a aprovação pertence ao DEV.

## Encerramento da etapa

O orquestrador integra os resultados, resolve achados impeditivos, reúne a evidência mínima e muda o PDCA para `DELIVERED_FOR_REVIEW`. Entrega ao DEV e para. Quando o DEV aprovar, registra `APPROVED`; essa resposta libera a sucessora, cujo início é outra linha de histórico, sem exigir segunda confirmação.
