---
name: manutencao-disponibilidade-dfe
description: Manter o diagnóstico de disponibilidade da Unimake.DFe para documentos, integrações e utilitários remotos, incluindo sessões passivas, contratos de resposta, autenticação, transporte, status oficial, cache, sanitização e integração com o PRO. Não usar para manutenção fiscal que não afete esses caminhos.
---

# Manutenção do diagnóstico de disponibilidade DFe

## Preparação obrigatória

1. Ler o [contrato e mapa de acesso](../../../.agents/instructions/disponibilidade-dfe.instructions.md) e [references/regras-diagnostico.md](references/regras-diagnostico.md). Consultar apenas as matrizes/fontes necessárias à dúvida atual.
2. Inspecionar `git status --short` e preservar mudanças alheias.
3. Rastrear serviço concreto → preparação/login → consumidor/executor → parser/normalizador → sessão/classificador. `TelemetriaDisponibilidade.cs` contém classificador/agregador compartilhados, além do histórico obsoleto; não confundir essas responsabilidades.
4. Tratar emissão como caminho crítico. O consumidor do diagnóstico é o PRO; o usuário decidiu retirar o recurso do legado. Essa decisão não retira outros fluxos fiscais do UniNFe nem autoriza editar consumidores fora da tarefa.

## Contrato recomendado

- Sessão por tentativa, envolvendo também construção/autenticação. `DefinirConfiguracao` antes do primeiro transporte quando a configuração inicial for provisória; `Concluir(xml,http)` ou `Falhar(ex,http)` sem reter XML/exceção.
- Verificação manual: `VerificarStatusServico()`; capacidades por `CapacidadesDiagnosticoDisponibilidade`/`PossuiConsultaOficialStatus`. Sem status oficial, manter observação real e informar ausência de sonda.
- Não usar nem ampliar APIs obsoletas como substitutas do canal. Preservar transição sem remover assinaturas/COM silenciosamente. Sessão da mesma configuração suprime histórico duplicado.
- Classificação/interpretação pertencem à DLL. Contexto durável, persistência, agregação temporal, UI e bloqueio entre reinícios pertencem ao PRO.

## Fluxo de manutenção

### 1. Delimitar a evidência

- Identificar `TipoDFe`, `Servico`, versão, ambiente e formato de retorno.
- Separar observação real, contrato reconhecido, falha de acesso/preparação, evidência de infraestrutura, status oficial e ausência de evidência. Enumerar desvios, tokens, downloads e componentes opacos; enum/herança não comprovam execução.
- Conferir o XSD oficial embutido em `Xml/Schemas/<DFe>` e a classe em `Xml/<DFe>`.
- Não deduzir a posição de `cStat` apenas por outro documento ou serviço.
- Confirmar se o retorno contém status principal e status internos de protocolo, documento ou evento.

### 2. Preservar a operação fiscal

- Manter a coleta passiva: observar somente a chamada que a aplicação já executaria.
- Nunca repetir autorização, evento, inutilização, distribuição ou outra mensagem fiscal para diagnosticar disponibilidade.
- Não introduzir leitura/gravação de arquivos, espera, bloqueio prolongado ou chamada externa no caminho da emissão.
- Manter falhas da telemetria isoladas: elas nunca podem mudar o resultado da operação fiscal observada.
- Permitir consulta explícita de `StatusServico` somente pelo fluxo protegido por cache e bloqueio de consumo indevido.

### 3. Extrair e classificar o retorno

- Escolher o reconhecedor da família/serviço. GNRE, CIOT, eSocial/Reinf e NFS-e não herdam o mapa cStat; conferir parser/modelo e contrato antes de acrescentar código. Catálogo estrutural não é interpretação de todos os erros.
- Ler reconhecimento junto com estado/falha/contrato/limitação. Rejeição normal pode provar processamento, sem provar autorização; contrato reconhecido com código desconhecido pode continuar inconclusivo. HTTP200 e SOAP Fault não são confirmação de negócio.
- As regras de cStat abaixo valem somente para as famílias aceitas por `UsaCStat`.
- Usar o primeiro `cStat` em ordem documental.
- Preservar o status principal do serviço ou lote quando existirem `protNFe`, `protCTe` ou `protMDFe` posteriores.
- Quando não houver status principal, alcançar o primeiro código em estruturas como `infInut`, `infCons` e `infEvento`.
- Não escolher o maior, o último ou o “pior” `cStat` do XML.
- Alterar códigos especiais somente na política central compartilhada pelo classificador, agregador, consulta explícita e cache.
- Manter qualquer `cStat` positivo não especial como prova de processamento fiscal.

### 4. Separar origem fiscal e local

- Não atribuir timeout isolado à SEFAZ.
- Distinguir HTTP de código de negócio e autenticação de indisponibilidade. Preservar regressões de timeout REST e ambiguidade de cancelamento entre runtimes.
- Correlacionar HTTP 5xx e timeouts por serviço e endpoint.
- Fazer DNS, conexão, TLS, proxy, certificado e configuração prevalecerem como causa local quando não houver indisponibilidade fiscal direta.
- Usar `Essencial` na agregação; falha apenas em serviço secundário não representa indisponibilidade total.
- Tratar serviço, UF, ambiente ou versão inexistente como `NaoAplicavel`.

### 5. Proteger dados e compatibilidade

- Nunca guardar XML completo, corpo fiscal, certificado, chave privada, senha, token ou credencial de proxy.
- Expor somente esquema/host/porta no endpoint e mensagens controladas. Manter cache/agrupamento por identidade derivada interna; não reintroduzir path ou exceção remota bruta.
- Preservar `netstandard2.0`, C# 7.3 e INTEROP/COM.
- Documentar APIs públicas e novos membros conforme o padrão do projeto.
- Manter a memória limitada e sem persistência em disco.
- Preservar o bloqueio por consumo indevido mesmo com fila cheia/sem histórico; não limpar cache para forçar sondagem. Verificação suprimida não deve preparar certificado ou fazer rede. Não criar chaves ilimitadas para enums inválidos/integrações sem status.

## Aprofundamento por tarefa

- O [mapa de acesso](../../../.agents/instructions/disponibilidade-dfe.instructions.md) localiza fontes e classes de teste por problema. Não carregar inventários/CSVs completos por padrão.
- Ao alterar contratos/catálogos, ler [INTERPRETACAO.md](../../../docsplan/diagnostico/INTERPRETACAO.md) e conferir suas evidências no código/XSD/manual. Regenerar pelos scripts e atualizar os registros de origem; não alterar só o gerado ou atualizar hash de fonte sem revisão semântica.
- Ao manter o consumo do PRO, ler [MIGRACAO-SESSOES.md](../../../docsplan/diagnostico/MIGRACAO-SESSOES.md). [COMANDO-PRO.md](../../../docsplan/diagnostico/COMANDO-PRO.md) é roteiro de migração, não autorização permanente para outro repositório. Versões/contagens em VALIDACAO são históricas.
- Ao tocar CIOT fiscal, normalização ou autenticação, aplicar também a skill `manutencao-ciot`, preservando baseline ANTT. Não tentar revelar HTTP/endpoints internos de GeradorCIOTShared por suposição.

## Validação

1. Criar teste determinístico sem internet para cada formato de retorno ou regra alterada.
2. Cobrir status principal e status interno quando o XML puder conter mais de um `cStat`.
3. Atravessar serviço/consumidor/executor real com transporte substituído quando possível. Conferir retorno/exceção intactos, coleta única, ausência de request extra e dados sensíveis, inclusive falhas do próprio diagnóstico.
4. Validar agregação, origem provável, cache, consumo indevido e sanitização quando afetados.
5. Rodar testes novos/alterados e regressões compartilhadas pertinentes. Cache/relógio usam `MemoriaDiagnosticoDisponibilidade`, com estado restaurado. Não substituir testes offline por suíte externa.
6. Executar o build principal com `--no-restore`.
7. Compilar INTEROP quando API/transporte/COM forem afetados. Validar o PRO apenas quando autorizado, com pacote offline novo e efetivamente restaurado, sobre base fiscal compatível. Não exigir testes do legado para o diagnóstico nem editar um consumidor somente leitura; registrar a pendência de integração nesse caso.

Comandos usuais:

```powershell
dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore
dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore
# Exemplo para sessões/contratos; selecionar outras classes pelo mapa conforme a alteração.
dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" `
  -class Unimake.DFe.Test.Utility.Rede.MigracaoDiagnosticoSessaoTest `
  -class Unimake.DFe.Test.Utility.Rede.InterpretacaoContratosDisponibilidadeTest `
  -trait- "Utility=DisponibilidadeIntegracao" -noLogo
```

## Entrega

- Informar quais evidências mudaram e como são agregadas.
- Explicar explicitamente o efeito sobre autorização e consumo indevido.
- Relatar testes aprovados, ignorados e falhas externas ou preexistentes separadamente.
- Não afirmar que a SEFAZ está indisponível sem evidência fiscal direta ou correlação suficiente.
- Atualizar a matriz/evidências quando mudar cobertura. Distinguir limitação de protocolo, componente opaco e interpretação ainda pendente; não declarar cobertura universal por catálogo ou enum.
