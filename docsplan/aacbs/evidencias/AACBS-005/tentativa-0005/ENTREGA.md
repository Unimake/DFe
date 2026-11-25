# AACBS-005 — DryRun do pacote após as correções

Validação complementar autorizada pelo DEV em 25/11/2026: executar o empacotamento oficial em DryRun e registrar evidências, sem publicar pacote, alterar consumidores ou fazer chamadas à Receita. A etapa permanece BLOCKED nos critérios reais; esta prova não reconcilia RAD-01 nem aprova a etapa.

## Código e comando

HEAD da execução: `3efc0eae7e5b216a0f348387b8575f9e941679eb`. Inclui os reparos AACBS `30e979946`, `929a2b6bc` e a documentação `a0781d7ab`, além do teste NFe posteriormente commitado por outro trabalho. A árvore rastreada estava limpa; o exemplo eFrete não rastreado foi preservado. [Contexto](CONTEXTO.json).

Foi utilizado diretamente o fluxo oficial chamado pelo packDFeOffline.bat, sem outro empacotador ou modificação em Pack-DFe.ps1:

```powershell
$env:DOTNET_CLI_TELEMETRY_OPTOUT = '1'
powershell.exe -NoProfile -ExecutionPolicy Bypass -File source/Packaging/Pack-DFe.ps1 -Mode Offline -DryRun -StartDate "19/11/2026 00:00:00" -EndDate "25/11/2026 11:57:55"
```

O intervalo é interpretado em Brasília e serve somente às release notes obtidas do GitHub pelo fluxo oficial. O build usa o código local identificado pelo HEAD; as notas remotas não são prova de que todo commit local já foi publicado no GitHub. A primeira tentativa encontrou restrição de DNS para api.nuget.org, antes de qualquer build ou alteração do projeto; os sete hashes permaneceram iguais. A repetição autorizada com acesso de rede executou o fluxo integral. Nenhum acesso fiscal foi necessário.

## Resultado

O [log final](pacote-dryrun.log) registra exit 0 e pacote temporário `20261125.1158.32`, AssemblyVersion/FileVersion `2026.11.25.1158`. O script compilou Release/netstandard2.0, criou o nupkg temporário, extraiu-o e validou:

- Somente quatro assets em lib/netstandard2.0: Unimake.Business.DFe.dll, PDB, XML e GeradorCIOTShared.dll; sem duplicação net472 ou outros arquivos além dos metadados NuGet previstos.
- Identidade, versão, descrição institucional, release notes e grupo de dependências .NETStandard2.0 correspondentes ao projeto/template oficial, incluindo System.Net.Http.Json.
- Hash de GeradorCIOTShared.dll igual ao arquivo de origem.

Na DLL Release usada pelo empacotador, a inspeção de recursos confirmou **25 XSDs e duas configurações de serviço AACBS**: [recursos embutidos](RECURSOS-AACBS.txt). Os [hashes dos quatro assets de origem](ASSETS-COMPILADOS.csv) identificam os binários desta execução. Essa inspeção é de metadados e não executa serviços fiscais. O nupkg temporário foi removido pelo finally do fluxo oficial; não é pacote persistido para instalação.

## Preservação e limites

Os [sete hashes antes](CHECKPOINT-ANTES.csv) e [depois](INTEGRIDADE-DEPOIS.csv) não apresentam divergências: projeto, pacote rastreado, empacotador, template, origem CIOT e validadores corrigidos foram preservados. A versão transitória do projeto foi restaurada pelo fluxo oficial. O candidato não foi encontrado no feed offline; não houve push, alteração de NuGet.Config, consumidor, chave, cache, cota ou journal real. [Verificação consolidada](VERIFICACAO-FINAL.json).

O build teve zero erros, quatro avisos CS0618 já existentes de NFe e um NU1900: o índice do feed Azure configurado não ficou disponível para consulta de vulnerabilidades. Portanto, esta execução não comprova uma auditoria completa de vulnerabilidades das dependências. O pack também mantém os avisos existentes de iconUrl obsoleto e ausência de README de pacote. Nenhuma validação foi desabilitada para obter o resultado.

Não foram repetidos os testes unitários: não houve alteração de código desde as provas direcionadas da [tentativa-0004](../tentativa-0004/ENTREGA.md). O DryRun é prova de build/empacotamento, não de integração de consumidor, registro COM ou comportamento fiscal real. O estado BLOCKED e as condições de retomada do [encerramento técnico](../../../ENCERRAMENTO-TECNICO.md) permanecem vigentes. A publicação, instalação em consumidor e qualquer chamada à Receita continuam fora desta autorização. Nenhum commit foi executado nesta tentativa.

A [conferência independente](REVISAO.md) confirmou os hashes, o inventário e o resultado. O .gitattributes do dossiê preserva os bytes CRLF do log para manter seu SHA-256 após o versionamento. Os checkpoints dos arquivos de origem identificam o estado Windows desta execução.
