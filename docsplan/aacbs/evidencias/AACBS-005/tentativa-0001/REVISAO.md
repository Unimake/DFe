# Revisão independente da AACBS-005

Revisão estática em 25/11/2026 sobre o diff da AACBS-005 e os ajustes enviados para o checkpoint final. A execução dos testes e do empacotamento pertence ao executor da etapa.

## Contrato e exemplos

O mapper de créditos valida o tíquete, o comprimento de 8 ou 14 dígitos ASCII e a igualdade ordinal dos primeiros oito com a base do pedido. O modelo recebe o valor integral da resposta. O XSD restringe a mudança ao `ni` do arquivo de créditos, também usado no envelope de download. Os envios conservam a base de oito dígitos. Os retornos de pagamentos e recolhimentos conservam o limite de oito, e o retorno de débitos já aceitava oito ou 14. As assinaturas públicas .NET e INTEROP permanecem compatíveis.

Os testes contemplam zeros iniciais, raiz divergente, tipos, caracteres e comprimentos inválidos, espaços nas bordas, arquivo vazio e arquivo com itens. O smoke test lê os 24 exemplos publicados, verifica XSD e conteúdo e compara a árvore XML após a serialização. A equivalência entre a forma expandida vazia e a autocontida se limita à raiz `autenticar`, cujo schema admite apenas atributos. Os nomes, enums, construtores, ProgIds, acessores de coleção e métodos de gravação usados no guia correspondem às declarações da biblioteca.

## Empacotamento

O achado inicial era a aceitação de ativos `ref` e `runtimes` por um validador que enumerava somente `lib`. Na revisão final, `Test-GeneratedPackage` também enumera a raiz do pacote e rejeita arquivos fora dos quatro ativos e dos metadados NuGet previstos. O teste de regressão chama a função oficial com dois arquivos ZIP sintéticos, um de cada cenário, e exige a falha específica. O script preserva a conferência do hash de `GeradorCIOTShared.dll`, e o manifesto exporta `System.Net.Http.Json` na versão lida do projeto principal.

## Situação documental e limites

O orquestrador corrigiu a descrição das etapas no `PDCA.md`, linha 83. A releitura confirmou a aprovação da 004 e a execução da 005 conforme as transições registradas.

## Reconciliação do original de créditos

Examinei a rotina local de verificação (SHA-256 `D4FBE99C87EF2658293535BF6877783A489B71E6D72B816F3A9BDF44F1BA9127`), os métodos do harness e a [evidência sanitizada](VERIFICACAO-REAL-CREDITOS.json). O hash do original cifrado coincide com o [diagnóstico da AACBS-004](../../AACBS-004/tentativa-0001/DIAGNOSTICO-CREDITOS.json). O hash da biblioteca registrado na evidência coincide com o binário Debug conferido nesta revisão.

A rotina entrega o original em memória ao serviço público de download e confere que o executor selecionado é o transporte de teste. A evidência registra uma solicitação controlada e zero chamadas HTTP reais. O NI de 14 dígitos foi preservado integralmente, incluindo a correspondência com a base de oito dígitos do pedido. O tíquete coincide com o registro protegido. A comparação recursiva verifica a quantidade e os nomes dos campos, a ordem de cada lista, as strings por igualdade ordinal e os números pelo valor decimal. Ela percorre o XML independente, o filho do envelope e o envelope relido do texto público. O original contém dois períodos e 27 créditos. Os resultados incluem XSD, regras de conteúdo e nova serialização de arquivo e envelope.

A reconciliação toma o mesmo mutex global do harness antes da leitura do journal e o mantém até a releitura após a gravação. A rotina confere a integridade inicial do original e do journal, confirma o XML operacional relido sob DPAPI, altera a fase do registro para `Finalizada` e compara o estado relido com o esperado. As demais informações do journal foram preservadas. O dossiê contém resultados agregados e hashes. Os testes focados do executor registraram 499 casos Debug e 509 INTEROP, ambos com zero falhas e zero pulos.

## Verificação final de COM e do pacote

A [prova de ITypeInfo](INTEROP-ITYPEINFO.json) extraiu os descritores da DLL INTEROP com Windows PowerShell 5.1 x64. Conferi o hash do binário e os dez serviços. Cada serviço expõe `Executar` com o modelo da operação e `Configuracao`, além de `Envio` e `Result` tipados. A interface de configuração conserva o IID, as 204 funções e as posições de `ClientSecret` e `ProvedorCIOT` verificadas na AACBS-003. `AACBSToken` fica oculto. Esta prova verifica os metadados nativos em x64, sem registro COM nem invocação de negócio.

O [log do DryRun oficial](pacote-dryrun.log) confirma a versão temporária `20261125.655.17`, o fim do intervalo de release notes em `25/11/2026 06:55:17`, os quatro ativos em `lib/netstandard2.0`, a correspondência do hash de `GeradorCIOTShared.dll` com a origem e a saída zero. O pacote temporário serviu à validação, sem publicação. Recalculei os 142 hashes do [checkpoint](CHECKPOINT.csv), com zero divergências.

A revisão independente acompanhou as evidências sem executar as rotinas locais, descriptografar o original, compilar, testar, registrar COM ou empacotar. O executor documenta a execução no dossiê da tentativa. A revisão não abrange alterações externas de versão ou de diagnóstico eFrete. A aprovação da etapa cabe ao DEV.
