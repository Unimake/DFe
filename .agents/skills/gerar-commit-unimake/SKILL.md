---
name: gerar-commit-unimake
description: Analisa somente alterações Git pendentes produzidas pela sessão atual do Codex e gera mensagens em português do Brasil integralmente compatíveis com Conventional Commits 1.0.0, mantendo o ticket como footer. Usar quando o usuário pedir mensagens, textos ou sugestões de commit para o trabalho realizado neste chat. Apenas inspeciona e devolve textos prontos para copiar; nunca executa commit nem modifica o repositório.
---

# Gerar mensagens Conventional Commits da sessão

Gerar mensagens para as alterações ainda pendentes que foram realizadas pelo Codex no chat atual. Seguir integralmente a especificação [Conventional Commits 1.0.0](https://www.conventionalcommits.org/en/v1.0.0/) e as regras locais compatíveis descritas abaixo.

## Limites obrigatórios

- Atuar somente em modo de leitura.
- Analisar somente as alterações elegíveis da sessão atual e devolver apenas as mensagens propostas.
- Nunca alterar, criar, excluir, corrigir ou formatar arquivos.
- Nunca executar comandos que modifiquem repositório, staging, working tree, branches, remotos ou histórico, incluindo `git add`, `git commit`, `git push`, `git reset`, `git checkout`, `git restore`, `git clean`, `git rebase`, `git merge`, `git cherry-pick` e `git stash`.
- Nunca criar branch, Pull Request ou qualquer artefato no repositório.
- Não executar o commit, mesmo quando o pedido usar expressões como "gerar commit" ou "gerar os commits"; interpretar esses pedidos como geração das mensagens.

Usar, quando necessário, somente comandos Git de leitura, como:

- `git status` e `git status --short`;
- `git diff`, `git diff --cached`, `git diff --stat` e `git diff HEAD`;
- `git log` e `git show`.

Abrir arquivos alterados, novos ou relacionados somente quando isso for necessário para compreender a mudança.

## Delimitar as alterações da sessão atual

A unidade de análise não é todo o working tree. Considerar exclusivamente a interseção entre:

1. alterações ainda pendentes no Git, estejam elas staged, unstaged ou untracked; e
2. alterações comprovadamente realizadas pelo Codex no chat atual, incluindo trabalho de agentes pertencentes a este mesmo chat quando houver.

Usar como fonte de proveniência o histórico disponível da conversa, seu eventual resumo após compactação, os patches aplicados e as saídas das ferramentas desta sessão. Não usar horário do arquivo, ordem do `git status`, branch atual ou simples presença no diff como prova de autoria da sessão.

Aplicar estas regras:

- Excluir alterações que já existiam antes desta sessão.
- Excluir alterações feitas pelo usuário, por outro chat, por outro processo ou por agentes que não pertençam a este chat.
- Excluir mudanças realizadas nesta sessão que já não estejam pendentes, por exemplo porque foram revertidas ou commitadas.
- Incluir arquivos novos somente quando sua criação nesta sessão estiver comprovada e eles continuarem untracked ou staged.
- Quando um arquivo contiver alterações da sessão misturadas com alterações anteriores ou externas, analisar somente os hunks atribuíveis à sessão. Usar o diff ou snapshot anterior à edição, quando disponível, e os patches registrados na conversa para separar a proveniência.
- Não abrir nem analisar o conteúdo de alterações externas à sessão além do mínimo necessário para confirmar que devem ser excluídas.
- Se a proveniência exata de uma mudança não puder ser demonstrada, não a incluir nem inferir que pertence à sessão.

Se não houver alteração pendente comprovadamente feita nesta sessão, retornar exatamente:

```text
Não foram encontradas alterações locais pendentes realizadas nesta sessão para gerar uma mensagem de commit.
```

## Analisar semanticamente

Antes de propor mensagens:

1. Verificar somente as alterações locais atribuíveis à sessão, incluindo staged, unstaged, untracked, exclusões e renomeações.
2. Ler os diffs elegíveis e, quando não bastarem, ler arquivos relacionados, chamadas, testes e histórico pertinente.
3. Identificar a finalidade do trabalho, o problema resolvido, a funcionalidade adicionada ou a melhoria realizada.
4. Diferenciar o objetivo funcional dos detalhes de implementação.
5. Identificar responsabilidades lógicas independentes e decidir se devem resultar em uma ou mais mensagens.
6. Basear cada afirmação em evidência do código, dos testes, da documentação ou do histórico; não inventar nem extrapolar funcionalidades.
7. Priorizar o que mudou e por que isso é relevante, não uma lista mecânica de arquivos, classes, métodos ou linhas.

Não gerar mensagem fictícia nem completar a análise com alterações externas à sessão.

Se uma incerteza relevante continuar impedindo a classificação depois de analisar diff, arquivos relacionados, chamadas, testes e histórico, informá-la brevemente. Não pedir esclarecimentos por questões triviais que possam ser resolvidas no repositório.

## Proteger informações sensíveis

Nunca reproduzir em mensagens de commit senhas, tokens, API keys, certificados, PINs, secrets, connection strings com credenciais, chaves privadas, dados pessoais sensíveis ou qualquer segredo encontrado. Ignorar o conteúdo sensível ao descrever a alteração.

## Estrutura Conventional Commits 1.0.0

Usar esta estrutura:

```text
<tipo>[escopo opcional][! opcional]: <descrição>

[corpo opcional]

[BREAKING CHANGE: descrição, quando aplicável]
ID #9999999
```

O footer `ID #9999999` é obrigatório e literal. Ele usa o token `ID`, o separador ` #` e o valor `9999999`, portanto deve ficar na seção de footers e nunca no título, na descrição ou no corpo. Nunca descobrir, inferir, perguntar ou aproveitar outro ticket encontrado em branch, código, comentário ou histórico.

### Tipo

- Usar `feat` obrigatoriamente quando o commit adicionar uma funcionalidade.
- Usar `fix` obrigatoriamente quando o commit corrigir um bug.
- Para outras intenções, escolher um tipo coerente. Os tipos convencionais usuais são `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test` e `revert`.
- Manter o tipo em inglês e minúsculo para consistência.
- Não usar um tipo local ou inventado quando um tipo convencional expressar corretamente a intenção.
- Classificar pela finalidade predominante, não pela extensão do arquivo. Uma alteração de segurança que corrige vulnerabilidade, por exemplo, é `fix`; uma nova proteção funcional pode ser `feat`.

### Escopo

- O escopo é opcional e deve aparecer entre parênteses imediatamente após o tipo: `feat(nfse): ...`.
- Quando usado, deve ser um substantivo curto que identifique uma seção coerente do código, como o DFe, componente ou subsistema afetado.
- Omitir o escopo quando ele não acrescentar contexto claro ou quando a mudança abranger várias áreas sem um escopo único.

### Descrição

- Inserir `: ` obrigatoriamente após o tipo, escopo e eventual `!`.
- Escrever imediatamente depois uma descrição curta, específica e compreensível isoladamente.
- Escrever em português do Brasil e descrever somente alterações já realizadas.
- Não incluir `ID #9999999` na descrição.
- Evitar ponto final, emoji, Markdown, aspas desnecessárias, tutorial e termos vagos como `alterações`, `ajustes gerais`, `melhorias` ou `correções diversas`.

Exemplo sem corpo:

```text
fix(certificados): corrige a aquisição da chave privada de certificados A3

ID #9999999
```

### Corpo

- O corpo é opcional, livre e pode ter vários parágrafos.
- Usá-lo apenas quando contexto relevante não couber claramente na descrição, como motivação, decisão arquitetural, compatibilidade, fallback ou consequência não evidente.
- Iniciá-lo exatamente uma linha em branco depois da descrição.
- Não repetir o título, produzir changelog, listar arquivos ou transformar a mensagem em tutorial.

### Footers e ticket

- Iniciar os footers uma linha em branco depois do corpo ou, sem corpo, uma linha em branco depois da descrição.
- Escrever cada footer como um token seguido de `: ` ou ` #` e de seu valor.
- Substituir espaços do token por `-`, exceto no token especial `BREAKING CHANGE`.
- Manter `ID #9999999` como footer obrigatório em todas as mensagens.
- Quando houver vários footers, colocá-los em linhas consecutivas na seção final.

Exemplo com corpo e mais de um footer:

```text
fix(servicos): evita concorrência entre consultas simultâneas

Mantém somente a resposta associada à requisição mais recente.

Refs: #123
ID #9999999
```

### Breaking changes

- Indicar toda mudança incompatível por `!` imediatamente antes de `:` ou por um footer `BREAKING CHANGE: <descrição>`.
- Permitir `!` depois do tipo (`feat!:`) ou do escopo (`feat(api)!:`).
- Preferir incluir o footer `BREAKING CHANGE:` quando ele explicar melhor o impacto e a migração; ele pode coexistir com `!`.
- Escrever `BREAKING CHANGE` exatamente em maiúsculas. `BREAKING-CHANGE` é um token sinônimo válido, mas preferir a grafia canônica com espaço.
- O footer do ticket continua obrigatório e deve permanecer na mesma seção.

Exemplo:

```text
feat(config)!: remove suporte ao formato legado de configuração

BREAKING CHANGE: arquivos no formato anterior devem ser convertidos antes da atualização
ID #9999999
```

## Agrupar ou separar responsabilidades

Gerar mais de uma mensagem quando houver responsabilidades logicamente distintas, por exemplo:

- funcionalidade e conjunto significativo de testes;
- correção e testes independentes;
- funcionalidade e documentação independente;
- refatoração e atualização de exemplos;
- código de produção e alteração independente de CI ou build;
- alteração visual independente e correção de regra de negócio.

Manter alterações juntas quando pertencerem ao mesmo objetivo, forem necessárias para a mesma correção ou funcionalidade, compartilharem a mesma intenção e a separação não melhorar o histórico.

Não misturar responsabilidades independentes para reduzir o número de commits. Não fragmentar artificialmente pequenas alterações inseparáveis. Tratar testes ou documentação como commit próprio quando constituírem responsabilidade clara; manter ajustes mínimos junto do trabalho principal quando a separação não trouxer clareza.

Não misturar mudanças externas à sessão para completar um grupo. Se um arquivo tiver hunks de origens diferentes, a mensagem deve descrever apenas os hunks desta sessão; a seleção correta desses hunks para staging permanece responsabilidade do usuário.

## Produzir a saída

Quando houver uma única responsabilidade lógica, retornar somente uma mensagem pronta para copiar, em bloco `text`. Não acrescentar introdução, explicação, justificativa, rótulos como `Sugestão:` ou `Commit:`, numeração, bullet ou conclusão.

```text
fix(sped): corrige a geração do registro 1010 que produzia XML inválido

ID #9999999
```

Quando houver várias responsabilidades, retornar uma mensagem por bloco `text`, separando visualmente os blocos. Não inserir números ou bullets dentro das mensagens.

```text
feat(nfe): adiciona suporte ao novo serviço de consulta

ID #9999999
```

```text
test(nfe): adiciona testes unitários para o novo serviço de consulta

ID #9999999
```

```text
docs(nfe): documenta os parâmetros do novo serviço de consulta

ID #9999999
```

Antes de responder, confirmar que cada mensagem:

1. contém somente trabalho pendente comprovadamente realizado nesta sessão;
2. segue a gramática `<tipo>[escopo opcional][! opcional]: <descrição>`;
3. usa `feat` para funcionalidade e `fix` para correção;
4. representa uma responsabilidade coerente e demonstrada;
5. contém o footer literal `ID #9999999`, separado do título;
6. identifica qualquer breaking change conforme a especificação;
7. não expõe informações sensíveis.
