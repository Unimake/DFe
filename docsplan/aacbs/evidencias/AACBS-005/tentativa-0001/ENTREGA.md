# AACBS-005 — Consolidação e entrega final

Execução autorizada pelo DEV em 25/11/2026: “Aprovo a AACBS-004 e libero a execução da AACBS-005.” O DEV aprovou separadamente NI de oito ou 14 dígitos no retorno de créditos, preservando o valor integral e conferindo a base do pedido. Aprovações registradas no [PDCA](../../../PDCA.md). Estado DELIVERED_FOR_REVIEW: validação técnica concluída, com as limitações materiais abaixo submetidas ao DEV.

## Entrega e escopo

A compatibilidade aprovada foi integrada ao mapper, ao XSD isolado e ao ramo do envelope de créditos, mantendo tipo público string, ordem, namespace, versão e interface COM. Pagamentos/recolhimentos continuam com NI8, débitos mantêm a exceção anterior, os cinco envios de consultas/download continuam com CNPJ-base de oito dígitos. [Auditoria estrutural](AUDITORIA-NI-CREDITOS.md) e [grafo de schemas](GRAFO-XSD.csv).

O [guia de uso](../../../uso/README.md) cobre os dez serviços, abertura/retomada/situação/link/download por XML, exemplos .NET e COM, erros, armazenamento protegido e condições de emissão de DARF. Os 24 XMLs sintéticos publicados são testados diretamente, sem cópia divergente, por vínculo de conteúdo no projeto de testes. O teste compara XSD, validação de conteúdo, geração/leitura e gravação/releitura; a equivalência de forma vazia é restrita à raiz autenticar e não remove CR/LF de campos fiscais. Coleções COM também foram exercitadas.

Foi corrigida uma omissão comprovada do manifesto NuGet oficial: a referência System.Net.Http.Json usada pela DLL não era exportada pelo pacote. O nuspec agora lê a mesma versão do projeto, fornecendo também System.Text.Json por dependência transitiva. Nenhuma nova versão ou biblioteca foi introduzida no projeto principal. O validador oficial exige os quatro arquivos em lib/netstandard2.0 e rejeita outros ativos de compilação/execução; metadados padronizados NuGet são permitidos. A regressão usa as funções oficiais de manifesto e validação, com ZIPs sintéticos negativos ref/runtimes; não cria pacote distribuível por fluxo paralelo.

## Fontes, checkpoint e limites

As [23 fontes públicas reconsultadas](REVALIDACAO-FONTES.csv) mantêm os hashes da baseline qualificada de 24/11/2026. O checkpoint estável contém 142 fontes, testes, exemplos e arquivos de empacotamento, com SHA-256 antes/depois; a versão externa 2026.11.25.0620 do projeto principal foi preservada. A pasta externa Exemplos/DiagnosticoTlsEFrete não pertence à entrega. O diff de versão do csproj principal preexistia a esta etapa e não deve ser incluído em futuro commit AACBS.

Não houve chamada fiscal real nesta etapa, autenticação, nova abertura, repetição de 404, emissão ou pagamento de DARF, publicação, persistência de pacote offline ou alteração de UniNFe/PRO. O DryRun consulta somente fontes públicas NuGet/GitHub necessárias ao fluxo oficial. A [matriz final](MATRIZ-ACEITE-FINAL.md) distingue execução controlada, respostas reais históricas e lacunas que continuam bloqueadas.

## Testes executados

Windows, PowerShell 7.6, xUnit v3 pelo runner da DLL net10.0, biblioteca netstandard2.0/C# 7.3. Um único executor sequencial de builds/runners/pack por checkout.

| Verificação | Resultado | Evidência |
|---|---|---|
| Build dos testes Debug, --no-restore --no-incremental --disable-build-servers | Exit 0; 4 avisos preexistentes, zero erros | [build-debug.log](build-debug.log) |
| Runner Debug, -trait DFe=AACBS | 499 testes; zero falhas, erros, ignorados ou não executados | [testes-debug.log](testes-debug.log) |
| Executar-AACBS004.ps1 -Offline com DLL da saída completa de testes Debug | Exit 0; quatro famílias e regressões PowerShell, sem API ou acesso local real | [roteiro-offline.log](roteiro-offline.log) |
| Testar-EmpacotamentoAACBS005.ps1 | Exit 0; dependência na versão compilada e rejeição de assets extras ref/runtimes | [manifesto-pacote.log](manifesto-pacote.log) |
| Mesmo build em INTEROP_Release | Exit 0; 15 avisos preexistentes, zero erros | [build-interop.log](build-interop.log) |
| Runner INTEROP_Release, -trait DFe=AACBS | 509 testes; zero falhas, erros, ignorados ou não executados | [testes-interop.log](testes-interop.log) |

Foram executados somente os testes do domínio AACBS, não toda a suíte fiscal. O primeiro início do SDK encontrou bloqueio de Registro na telemetria; repetido com DOTNET_CLI_TELEMETRY_OPTOUT=1. O primeiro smoke acusou a forma expandida vazia do exemplo autenticar; o comparador foi corrigido de forma restrita e os 499/509 casos passaram depois. A primeira execução do roteiro apontou por engano a DLL do runner de testes, em vez da Unimake.Business.DFe.dll; a repetição com a DLL da biblioteca na pasta completa do build de testes passou. Os logs canônicos acima correspondem ao estado final; tentativas preliminares não são apresentadas como resultados verdes. Logs preliminares ficam ignorados, preservados localmente.

## Resposta real de créditos, sem novo consumo

A [prova sanitizada](VERIFICACAO-REAL-CREDITOS.json) processou o JSON original cifrado da 004, sem alteração de NI ou campos, com o binário Debug final pelo serviço público Executar e transporte exclusivamente em memória. O original veio de HTTP 200 no Beta na etapa anterior: 8.969 bytes, dois períodos e 27 créditos. A comparação independente confirmou todos os campos, ordem de arrays, identificadores, datas e números contra Result, XML do filho, envelope e RetornoWSString; XSD, conteúdo e ambos round-trips passaram.

O NI14 integral, a base do pedido, matriz configurada e tíquete do journal foram conferidos. O XML operacional do envelope foi cifrado por DPAPI em .local/aacbs/execucoes e relido. Sob o mutex do harness, o estado passou de DownloadIndisponivel para Finalizada, preservando integralmente os demais campos, inclusive tíquete e contagens. Os hashes dos dois artefatos protegidos e da DLL estão na prova; nenhum corpo fiscal, segredo ou identificador real foi copiado ao dossiê. Essa reconciliação não comprova novas chamadas do transporte HTTP nem consome cotas.

## Agentes e revisão

- Orquestrador: contratos, delta de produção/XSD, integração do projeto de testes, correção delimitada do fluxo oficial de empacotamento, matrizes, PDCA e dossiê.
- aacbs005_guia, gpt-6.2-sol/medium: guia e 24 exemplos XML; conferência de links e hashes.
- aacbs005_smoke, gpt-6.2-sol/medium: smoke dos exemplos, leitura/gravação e coleções INTEROP; correção do comparador da raiz vazia.
- aacbs005_ni_testes, gpt-6.2-sol/medium: regressões de NI no mapper, XML, XSD e envelope.
- aacbs005_executor, gpt-6-luna/low: único executor de builds, runners, roteiro offline e empacotamento oficial.
- aacbs005_credito_real, gpt-6.2-astra/high: comparação crítica do original cifrado, reconciliação sob mutex e compatibilidade COM.
- aacbs005_revisao, gpt-6.2-astra/high: revisão independente de produção, contratos, testes, empacotamento, documentação e prova original. [Parecer](REVISAO.md).

Autoria paralela em arquivos distintos; quatro slots no máximo. A implementação ficou congelada antes dos builds finais. Nenhum agente executou commit ou aprovou etapa.

## Limitações e gate humano

Pagamentos e recolhimentos: HTTP 404 com resposta incompatível na única abertura da 004, sem tíquete confirmado. Causa indeterminada; não atribuir falha a habilitação ou inexistência sem evidência. Permanecem bloqueados o ciclo e os arquivos reais dessas famílias. A retomada precisa de qualificação da rota/habilitação ou orientação explícita para nova tentativa controlada, preservando a ambiguidade e a cota local.

DARF RAD/PCONT: todos os fluxos controlados implementados/testados; nenhuma emissão real. Antes de emitir, receber em arquivo local protegido os parâmetros de cenário habilitado: RAD com chaves de DF-e e opção total/parcial, valor quando parcial; PCONT com PA MM/yyyy e opção/valor. Preparar o XML concreto e solicitar confirmação específica do DEV. A aprovação desta etapa não substitui aquela confirmação.

Débitos: o ciclo real validado veio vazio, portanto os grupos de itens têm prova sintética e cobertura real residual. Créditos: o original com itens foi validado, mas o ajuste não provocou novo download remoto; o transporte mantém a evidência real anterior.

A consolidação do pacote/COM está registrada abaixo. Limitações materiais são submetidas ao DEV; nenhum requisito real pendente é contado como sucesso e não há declaração de ausência total de bugs. Não existe etapa sucessora liberada automaticamente após esta entrega.

## Compatibilidade COM e empacotamento concluídos

A [conferência COM nativa x64](INTEROP-ITYPEINFO.json) usou Windows PowerShell 5.1/CLR .NET Framework e ITypeInfo/FUNCDESC, sem registro de componentes e sem invocar negócio. Configuracao mantém IID e 204 funções, os DISPIDs/offsets observados de ClientSecret/ProvedorCIOT e AACBSToken oculto. As dez operações expõem Envio, Result e Executar com os tipos COM concretos esperados. A prova é de metadados e não substitui uma instalação/execução em consumidor externo, que não foi solicitada.

O [DryRun oficial](pacote-dryrun.log) concluiu com exit 0 pelo comando `pwsh -NoProfile -File source/Packaging/Pack-DFe.ps1 -Mode Offline -DryRun -StartDate "19/11/2026 00:00:00" -EndDate "25/11/2026 06:55:17"`. PackageVersion temporária 20261125.655.17 e AssemblyVersion 2026.11.25.0655. Foram conferidos exclusivamente os quatro arquivos em lib/netstandard2.0, hash do CIOT contra ThirdParty, manifesto, dependências e metadados. As release notes foram extraídas dos commits elegíveis remotos no intervalo; os deltas locais AACBS não foram publicados e não se atribuem a esse histórico remoto.

A versão externa original 2026.11.25.0620 foi restaurada byte a byte pelo finally do DryRun; pacote rastreado e fontes de NuGet não foram alterados. Não houve gravação de pacote no feed nem publicação. Os hashes finais e o checkpoint de 142 arquivos sem divergência estão em [VERIFICACAO-FINAL.json](VERIFICACAO-FINAL.json). O NU1900 do restore registra indisponibilidade dos dados de vulnerabilidade de uma fonte configurada; não é um resultado de auditoria de vulnerabilidades aprovado. A obsolescência de PackageIconUrl também foi emitida pelo fluxo preexistente. Ambos foram preservados no log, sem desativar verificações para obter sucesso.

## Entrega para revisão humana

Validação técnica concluída e limitações materiais explicitamente submetidas ao DEV. Estado DELIVERED_FOR_REVIEW, aguardando aprovação humana da AACBS-005; isso não declara a prova real de pagamentos/recolhimentos ou DARFs concluída. Toda retomada real seguirá a matriz e a confirmação específica de emissão. Uma correção após esta entrega abrirá nova tentativa; uma reversão, se solicitada, deverá retirar somente os deltas desta etapa, preservando o trabalho externo, os journals e as evidências históricas. Nenhuma reversão foi executada.
