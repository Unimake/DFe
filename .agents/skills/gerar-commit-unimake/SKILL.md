---
name: gerar-commit-unimake
description: Analisa semanticamente alterações Git locais pendentes e gera mensagens de commit em português do Brasil conforme o padrão Unimake, podendo separar responsabilidades lógicas em mensagens diferentes. Usar quando o usuário pedir para gerar commit, mensagem ou texto de commit, analisar alterações para commit, sugerir mensagens ou preparar commits pendentes. Apenas inspeciona o repositório e devolve textos prontos para copiar; nunca executa commit nem modifica arquivos, staging, branches, working tree ou histórico.
---

# Gerar mensagens de commit Unimake

## Limites obrigatórios

- Atuar somente em modo de leitura.
- Analisar o repositório Git atual e devolver apenas a análise estritamente necessária e as mensagens propostas.
- Nunca alterar, criar, excluir, corrigir ou formatar arquivos.
- Nunca executar comandos que modifiquem repositório, staging, working tree, branches, remotos ou histórico, incluindo `git add`, `git commit`, `git push`, `git reset`, `git checkout`, `git restore`, `git clean`, `git rebase`, `git merge`, `git cherry-pick` e `git stash`.
- Nunca criar branch, Pull Request ou qualquer artefato no repositório.
- Não executar o commit, mesmo quando o pedido usar expressões como "gerar commit" ou "gerar os commits"; interpretar esses pedidos como geração das mensagens.

Usar, quando necessário, somente comandos Git de leitura, como:

- `git status` e `git status --short`;
- `git diff`, `git diff --cached`, `git diff --stat` e `git diff HEAD`;
- `git log` e `git show`.

Abrir arquivos alterados, novos ou relacionados somente quando isso for necessário para compreender a mudança.

## Analisar as alterações

Antes de propor mensagens:

1. Verificar todas as alterações locais relevantes, incluindo staged, unstaged, untracked, exclusões e renomeações.
2. Ler os diffs necessários e, quando o diff não bastar, ler arquivos completos, chamadas relacionadas, testes e histórico pertinente.
3. Identificar a finalidade do trabalho, o problema resolvido, a funcionalidade adicionada ou a melhoria realizada.
4. Diferenciar o objetivo funcional dos detalhes de implementação.
5. Identificar responsabilidades lógicas independentes e decidir se devem resultar em uma ou mais mensagens.
6. Basear cada afirmação em evidência do código, dos testes, da documentação ou do histórico; não inventar nem extrapolar funcionalidades.
7. Priorizar o que mudou e por que isso é relevante, não uma lista mecânica de arquivos, classes, métodos ou linhas.

Se não houver alterações locais pendentes, retornar exatamente:

```text
Não foram encontradas alterações locais pendentes para gerar uma mensagem de commit.
```

Não gerar mensagem fictícia.

Se uma incerteza relevante continuar impedindo a classificação depois de analisar diff, arquivos relacionados, chamadas, testes e histórico, informá-la brevemente. Não pedir esclarecimentos por questões triviais que possam ser resolvidas no repositório.

## Proteger informações sensíveis

Nunca reproduzir em mensagens de commit senhas, tokens, API keys, certificados, PINs, secrets, connection strings com credenciais, chaves privadas, dados pessoais sensíveis ou qualquer segredo encontrado. Ignorar o conteúdo sensível ao descrever a alteração.

## Formar a primeira linha

Usar sempre:

```text
<tipo>: <descrição resumida da alteração>. ID #9999999
```

Aplicar todas estas regras:

- Escrever a descrição em português do Brasil.
- Manter o tipo em inglês, minúsculo e seguido imediatamente por `:`.
- Usar obrigatoriamente o identificador literal `ID #9999999`.
- Nunca descobrir, inferir, perguntar ou aproveitar outro número de ticket encontrado em branch, código, comentário ou histórico.
- Descrever algo já realizado, usando construções como `Adicionada`, `Implementado`, `Corrigida`, `Ajustado`, `Atualizada`, `Removido`, `Refatorada`, `Melhorado`, `Reorganizada` ou `Simplificado`.
- Não escrever no infinitivo, no presente como instrução ou no futuro.
- Tornar a mensagem concisa, específica, objetiva e compreensível isoladamente.
- Terminar a descrição com pontuação adequada antes de `ID #9999999`.
- Não usar emoji, Markdown dentro da mensagem, aspas desnecessárias, tutorial ou descrição genérica.
- Nunca usar descrições vagas como `Alterações`, `Ajustes`, `Melhorias`, `Correções`, `Mudanças diversas`, `Atualizações`, `Ajustes gerais` ou `Pequenas correções`.

Exemplo:

```text
fix: Corrigida validação do certificado digital A3 durante a aquisição da chave privada. ID #9999999
```

## Escolher o tipo

Usar somente um destes tipos, conforme o objetivo predominante:

- `security`: corrigir vulnerabilidade, proteger credenciais ou dados, aplicar criptografia, controle de acesso, validação de segurança ou hardening.
- `fix`: corrigir bug, falha, defeito ou comportamento incorreto.
- `feat`: adicionar funcionalidade ou comportamento novo.
- `perf`: melhorar desempenho ou eficiência, como memória, alocações, latência, throughput, disco, consultas, concorrência ou algoritmo.
- `test`: criar, corrigir ou ampliar testes, fixtures, mocks ou infraestrutura diretamente ligada aos testes.
- `docs`: alterar exclusivamente README, Markdown, comentários, XML Documentation, documentação técnica ou de API e exemplos textuais de documentação.
- `ui`: alterar predominantemente telas, layouts, controles, estilos visuais, posicionamento, aparência ou experiência visual.
- `refactor`: reorganizar ou simplificar internamente o código sem alteração funcional intencional.
- `style`: alterar exclusivamente formatação ou estilo do código sem mudar o funcionamento; não usar para interface gráfica.
- `build`: alterar diretamente compilação, dependências, NuGet, MSBuild, propriedades ou ferramentas de build.
- `ci`: alterar integração contínua, workflows, pipelines, validações ou publicação automatizada.
- `config`: alterar configuração da aplicação, parâmetros, defaults ou opções de runtime que não sejam build ou CI.
- `chore`: alterar exemplos, projetos de demonstração, XMLs de exemplo, artefatos auxiliares ou manutenção sem revisão funcional; também corresponde ao conceito interno `IGNORE-REVISION`.
- `revert`: reverter commit ou comportamento introduzido anteriormente.

Usar como precedência orientativa: `security`, `fix`, `feat`, `perf`, `test`, `docs`, `ui`, `refactor`, `style`, `build`, `ci`, `config`, `chore`, `revert`. Considerar sempre a intenção predominante, sem classificar mecanicamente pela extensão do arquivo.

Exemplos de decisões:

- Classificar um XAML que corrige defeito funcional como `fix`, não necessariamente `ui`.
- Classificar um `.csproj` que apenas adiciona projeto de demonstração como `chore`, não necessariamente `build`.
- Preferir `feat` quando o valor principal de uma nova tela for a funcionalidade; usar `ui` quando predominar apresentação ou experiência visual.

## Usar corpo somente quando necessário

Preferir a mensagem mais simples que explique corretamente a alteração. Omitir o corpo quando a primeira linha bastar.

Adicionar corpo apenas quando contexto histórico importante não couber claramente na primeira linha, como decisão arquitetural, compatibilidade legada, fallback, mudança de estratégia, consequência não evidente ou restrição técnica relevante.

Quando houver corpo:

- deixar uma linha em branco depois da primeira linha;
- escrever somente contexto realmente relevante;
- não repetir a primeira linha;
- não escrever tutorial, changelog, lista de arquivos ou detalhes irrelevantes;
- não transformar a mensagem em documentação.

Exemplo:

```text
fix: Corrigida aquisição da chave privada de certificados A3 compatíveis com CNG e CAPI. ID #9999999

Implementada preferência por CNG/KSP com fallback para CAPI/CSP para preservar a compatibilidade com dispositivos e middlewares legados.
```

Usar ponto e vírgula apenas para informações curtas e diretamente relacionadas. Preferir corpo a acumular muitas mudanças na primeira linha.

## Agrupar ou separar responsabilidades

Gerar mais de uma mensagem quando houver responsabilidades logicamente distintas, por exemplo:

- funcionalidade e conjunto significativo de testes;
- correção e testes independentes;
- funcionalidade e documentação independente;
- refatoração e atualização de exemplos;
- código de produção e alteração independente de CI ou build;
- UI independente e correção de regra de negócio.

Manter alterações juntas quando pertencerem ao mesmo objetivo, forem necessárias para a mesma correção ou funcionalidade, compartilharem a mesma intenção e a separação não melhorar o histórico.

Não misturar responsabilidades independentes para reduzir o número de commits. Não fragmentar artificialmente pequenas alterações inseparáveis. Tratar testes ou documentação como commit próprio quando constituírem responsabilidade clara; manter ajustes mínimos junto do trabalho principal quando a separação não trouxer clareza.

## Produzir a saída

Quando houver uma única responsabilidade lógica, retornar somente uma mensagem pronta para copiar, em bloco `text`. Não acrescentar introdução, explicação, justificativa, rótulos como `Sugestão:` ou `Commit:`, numeração, bullet ou conclusão.

```text
fix: Corrigida geração do registro 1010 do SPED que produzia XML inválido. ID #9999999
```

Quando houver várias responsabilidades, retornar uma mensagem por bloco `text`, separando visualmente os blocos. Não inserir números ou bullets dentro das mensagens.

```text
feat: Adicionado suporte ao novo serviço de consulta da NF-e. ID #9999999
```

```text
test: Adicionados testes unitários para o novo serviço de consulta da NF-e. ID #9999999
```

```text
docs: Atualizada documentação com os parâmetros do novo serviço de consulta da NF-e. ID #9999999
```

Antes de responder, confirmar que cada mensagem:

1. representa somente alterações demonstradas;
2. usa um tipo permitido e coerente com o objetivo principal;
3. está em português do Brasil e descreve trabalho já realizado;
4. contém literalmente `ID #9999999`;
5. não expõe informações sensíveis;
6. é concisa e útil para o histórico;
7. não mistura nem fragmenta responsabilidades indevidamente.
