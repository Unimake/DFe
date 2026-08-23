---
name: grill-me
description: Submeter um plano, projeto ou decisão a uma entrevista adversarial e adaptativa antes de consolidar o plano. Usar quando o usuário pedir "grill me", quiser eliminar ambiguidades, testar premissas ou colocar uma abordagem sob pressão; não ativar em planejamentos comuns sem esse pedido explícito.
---

# Grill Me

Conduzir no Modo Planejamento uma investigação rigorosa, uma decisão por vez, até que outra pessoa consiga executar o plano sem inventar intenções relevantes. Ser incisivo com o problema, nunca hostil com o usuário. O objetivo não é prolongar a entrevista nem obter certeza absoluta, mas revelar decisões caras, premissas frágeis e critérios ausentes enquanto ainda é barato corrigi-los.

## 1. Investigar antes de entrevistar

Antes da primeira pergunta e sempre que uma resposta criar uma nova hipótese verificável:

1. Ler as instruções aplicáveis e inspecionar o estado real do repositório, documentação, testes, histórico e integrações disponíveis.
2. Separar em um registro mental ou no plano:
   - **fatos verificados**: sustentados por código, testes, configuração ou fonte autoritativa;
   - **decisões do usuário**: escolhas de produto, escopo, risco ou preferência;
   - **hipóteses**: inferências ainda não confirmadas;
   - **questões abertas**: decisões necessárias que não podem ser descobertas.
3. Resolver fatos com ferramentas. Não transferir ao usuário perguntas que o ambiente pode responder.
4. Apontar contradições entre o pedido e a evidência antes de pedir uma escolha. Citar o fato relevante de forma concisa.

Não modificar arquivos nem iniciar a implementação durante a entrevista. A investigação permanece somente leitura; a execução começa apenas depois do plano consolidado e fora desta skill.

## 2. Construir a árvore de decisões

Mapear apenas dimensões pertinentes ao caso. Considerar, sem transformar a lista em questionário obrigatório:

- resultado desejado e problema que justifica o trabalho;
- público, consumidores e contrato observável;
- critério de sucesso, evidência de conclusão e não objetivos;
- escopo, prioridades, dependências, prazos e restrições;
- alternativas reais, inclusive manter o desenho atual;
- interfaces, dados, migração, compatibilidade e operação;
- segurança, falhas, recuperação, concorrência e casos-limite;
- estratégia de testes, rollout, observabilidade e rollback.

Para cada questão aberta, estimar informalmente:

- **impacto**: quanto a resposta altera o objetivo, a arquitetura, o contrato, o risco ou o custo;
- **poder de desbloqueio**: quantas decisões posteriores ela condiciona;
- **urgência**: quão cedo precisa ser decidida para evitar trabalho desperdiçado;
- **descobribilidade**: se uma investigação adicional pode eliminá-la.

Perguntar primeiro a questão não descobrível com maior combinação de impacto, desbloqueio e urgência. Não seguir uma checklist em ordem fixa.

## 3. Perguntar com precisão

Usar `request_user_input` em todas as rodadas, pois esta skill pressupõe o Modo Planejamento.

- Fazer uma única pergunta decisória por rodada. Uma pergunta pode conter contexto curto, mas não várias decisões escondidas.
- Oferecer duas ou três opções concretas, mutuamente exclusivas e comparáveis.
- Colocar primeiro a recomendação atual e marcá-la como recomendada.
- Explicar em uma frase o efeito ou trade-off de cada opção, sem caricaturar alternativas.
- Formular opções no nível da decisão, não como respostas genéricas "sim/não".
- Preservar a resposta livre oferecida pela ferramenta.
- Quando ainda não houver base para recomendar, declarar a incerteza e apresentar as opções sem fingir convicção.

Não perguntar por confirmação ritual. Se uma opção é dominada, insegura ou incompatível com uma restrição verificada, explicar e removê-la em vez de oferecê-la por falsa neutralidade.

## 4. Adaptar após cada resposta

Depois de cada escolha:

1. Registrar a decisão e a justificativa relevante em até duas frases.
2. Atualizar fatos, hipóteses, questões abertas e ramos descartados.
3. Verificar consequências de segunda ordem: qual contrato mudou, qual risco surgiu e qual decisão foi desbloqueada.
4. Pesquisar qualquer novo fato verificável antes da próxima pergunta.
5. Testar respostas vagas, contraditórias ou baseadas em premissa falsa com um contraexemplo realista ou evidência, e então pedir a decisão mínima que resolve a divergência.
6. Fazer a próxima pergunta de maior valor.

Aceitar a decisão explícita do usuário mesmo quando divergir da recomendação, desde que seja viável e autorizada. Registrar o trade-off; não reabrir a mesma decisão sem evidência nova ou contradição material.

## 5. Manter a entrevista eficiente

- Não repetir perguntas respondidas no contexto, em arquivos ou em rodadas anteriores.
- Não explorar preferências cosméticas antes de decisões estruturais.
- Não exigir decisões reversíveis que possam ser deixadas com segurança para a implementação.
- Não abrir ramos hipotéticos sem efeito provável no plano.
- Não usar a skill para ampliar o escopo ou buscar autorização para ações externas.
- Se o usuário disser que não sabe, recomendar um padrão seguro e reversível, explicitar a premissa e seguir; perguntar novamente somente se o risco impedir isso.
- Se surgir um bloqueio externo real, registrar a condição e definir no plano como resolvê-la, sem entrevistar indefinidamente.

## 6. Critério de parada

Encerrar quando todas as condições forem verdadeiras:

- objetivo, resultado observável e critérios de aceitação estão claros;
- decisões de alto impacto estão resolvidas ou registradas como premissas explícitas;
- fatos críticos foram verificados e hipóteses restantes têm método de validação;
- escopo e fora de escopo são distinguíveis;
- abordagem, interfaces e dependências permitem ordenar a execução;
- riscos materiais possuem prevenção, detecção, recuperação ou aceitação consciente;
- testes e validação demonstram o resultado pedido;
- não resta pergunta cuja resposta alteraria materialmente o plano agora.

Não buscar exaustividade. Pare quando o valor esperado da próxima pergunta for menor que o custo de atrasar o plano.

## 7. Consolidar o plano

Ao concluir, entregar o artefato normal do Modo Planejamento, incorporando — sem despejar a transcrição da entrevista —:

1. objetivo e contexto verificado;
2. decisões tomadas e premissas ainda assumidas;
3. escopo e não objetivos;
4. passos implementáveis, ordenados por dependência, com arquivos ou componentes quando conhecidos;
5. contratos, dados, migrações e compatibilidade afetados;
6. falhas, riscos, mitigação e rollback pertinentes;
7. testes e critérios de aceitação observáveis;
8. questões realmente adiáveis, com responsável ou gatilho para decisão.

O plano final deve refletir as escolhas do usuário, não a preferência inicial do entrevistador. Se a investigação mostrar que não fazer a mudança é a melhor opção, apresentar essa conclusão claramente e explicar a evidência.
