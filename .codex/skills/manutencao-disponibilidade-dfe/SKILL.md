---
name: manutencao-disponibilidade-dfe
description: Use quando Codex precisar implementar, corrigir, revisar ou otimizar o diagnóstico de disponibilidade e a telemetria passiva de NFe, NFCe, CTe, MDFe ou futuros DFe na Unimake.DFe, incluindo Utility/Disponibilidade, coleta em Servicos/ServicoBase, classificação de cStat, infraestrutura DNS/TCP/TLS/proxy, cache de status, agregação, sanitização, desempenho e testes relacionados.
---

# Manutenção do diagnóstico de disponibilidade DFe

## Preparação obrigatória

1. Ler integralmente [references/regras-diagnostico.md](references/regras-diagnostico.md) antes de alterar código.
2. Inspecionar `git status --short` e preservar mudanças alheias.
3. Localizar o fluxo completo da evidência: serviço fiscal, transporte, `TelemetriaDisponibilidade`, classificador, agregador e apresentação.
4. Tratar a autorização do documento como caminho crítico de desempenho.

## Fluxo de manutenção

### 1. Delimitar a evidência

- Identificar `TipoDFe`, `Servico`, versão, ambiente e formato de retorno.
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

- Usar o primeiro `cStat` em ordem documental.
- Preservar o status principal do serviço ou lote quando existirem `protNFe`, `protCTe` ou `protMDFe` posteriores.
- Quando não houver status principal, alcançar o primeiro código em estruturas como `infInut`, `infCons` e `infEvento`.
- Não escolher o maior, o último ou o “pior” `cStat` do XML.
- Alterar códigos especiais somente na política central compartilhada pelo classificador, agregador, consulta explícita e cache.
- Manter qualquer `cStat` positivo não especial como prova de processamento fiscal.

### 4. Separar origem fiscal e local

- Não atribuir timeout isolado à SEFAZ.
- Correlacionar HTTP 5xx e timeouts por serviço e endpoint.
- Fazer DNS, conexão, TLS, proxy, certificado e configuração prevalecerem como causa local quando não houver indisponibilidade fiscal direta.
- Usar `Essencial` na agregação; falha apenas em serviço secundário não representa indisponibilidade total.
- Tratar serviço, UF, ambiente ou versão inexistente como `NaoAplicavel`.

### 5. Proteger dados e compatibilidade

- Nunca guardar XML completo, corpo fiscal, certificado, chave privada, senha, token ou credencial de proxy.
- Sanitizar endpoint e mensagens vindas de transporte antes de armazená-los.
- Preservar `netstandard2.0`, C# 7.3 e INTEROP/COM.
- Documentar APIs públicas e novos membros conforme o padrão do projeto.
- Manter a memória limitada e sem persistência em disco.

## Pontos principais do código

- `source/.NET Standard/Unimake.Business.DFe/Servicos/ServicoBase.cs`
- `source/.NET Standard/Unimake.Business.DFe/Utility/Disponibilidade/DiagnosticoDisponibilidadeDFe.cs`
- `source/.NET Standard/Unimake.Business.DFe/Utility/Disponibilidade/TelemetriaDisponibilidade.cs`
- `source/.NET Standard/Unimake.Business.DFe/Utility/Disponibilidade/ModelosDisponibilidade.cs`
- `source/Unimake.DFe.Test/Utility/Rede/DiagnosticoDisponibilidadeTest.cs`
- Projeto `Unimake.Utils`, quando a mudança envolver classificação HTTP, DNS, conexão, timeout, TLS ou proxy.

## Validação

1. Criar teste determinístico sem internet para cada formato de retorno ou regra alterada.
2. Cobrir status principal e status interno quando o XML puder conter mais de um `cStat`.
3. Confirmar que a telemetria não executa transporte adicional nem lança exceção para a operação fiscal.
4. Validar agregação, origem provável, cache, consumo indevido e sanitização quando afetados.
5. Executar somente os testes novos/alterados e, para infraestrutura compartilhada, a classe completa de disponibilidade.
6. Executar o build principal com `--no-restore`.
7. Executar também `C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj` em `Debug`, conforme o `AGENTS.md`.

Comandos usuais:

```powershell
dotnet test "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore --filter "FullyQualifiedName~DiagnosticoDisponibilidadeTest"
dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore
dotnet test "C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj" --no-restore --configuration Debug
```

## Entrega

- Informar quais evidências mudaram e como são agregadas.
- Explicar explicitamente o efeito sobre autorização e consumo indevido.
- Relatar testes aprovados, ignorados e falhas externas ou preexistentes separadamente.
- Não afirmar que a SEFAZ está indisponível sem evidência fiscal direta ou correlação suficiente.
