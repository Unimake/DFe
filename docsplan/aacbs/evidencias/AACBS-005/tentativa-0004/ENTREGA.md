# AACBS-005 — Correções da revisão consolidada do ticket

Retrabalho autorizado pelo DEV em 25/11/2026: analisar e corrigir os três achados da revisão do ticket ID #189855. Base da entrega anterior: `06c964f68`.

Escopo exclusivamente offline: consistência da atualização de credencial/metadados, unificação da verificação local e preservação de whitespace no validador AACBS. Nenhuma chamada à Receita, emissão de DARF, leitura ou alteração do acesso real, cache, cotas ou journals. As pendências reais continuam bloqueadas e não há nova etapa.

Responsáveis: orquestrador integra documentação e coordena a verificação; `fix_local_access` (gpt-6.2-astra/high) modifica somente os scripts de acesso e sua documentação; `fix_xml_whitespace` (gpt-6.2-sol/medium) modifica somente o validador AACBS e regressões XML. Não há builds concorrentes. A autoria de testes PowerShell e a revisão independente serão registradas antes da entrega.

## Correções e regressões

| Achado confirmado | Correção | Prova |
|---|---|---|
| Credencial nova podia ser combinada com CNPJ/ambiente antigos | Publicação exclusiva por escritor: preparar/validar temporários, retirar JSON ativo antes de substituir credencial e publicar novo JSON por último. Leitor mantém o JSON aberto com FileShare.Read durante a importação DPAPI. Interrupção deixa acesso indisponível, sem par misturado | Teste com escritor intercalado durante leitura, falha ao substituir credencial, rejeição do acesso incompleto e recuperação explícita |
| Validador descartava espaços e aceitava chave acima de 100 caracteres | Overload protegido de leitura mantém LoadOptions.None na base; AACBS usa PreserveWhitespace pelo mesmo pipeline de ValidateTag | Cinco famílias com 100/101 espaços, texto separado por comentário, Unicode suplementar, regra adicional registrada e controle do comportamento padrão da base |
| -Verificar aprovava metadados rejeitados pelo executor | Preparador e roteiro usam AACBS-AcessoLocal.ps1, com o mesmo leitor e regras de metadados | Entradas públicas -Verificar e Invoke-AACBSLocal rejeitam os mesmos limites, DARF habilitado, cenários e estruturas JSON inválidos, sem mostrar valores |

Os caminhos e formatos dos dois arquivos locais foram mantidos. Não foi realizada migração ou atualização do acesso real. A recuperação de uma publicação incompleta está documentada em [ACESSO-LOCAL.md](../../../ACESSO-LOCAL.md): executar o preparador com -AtualizarCredenciais e fornecer novamente o par pretendido. Leitores operacionais devem usar Read-LocalAccess. Essa recuperação não altera journals nem autoriza repetir a RAD-01.

O teste PowerShell foi produzido por `test_local_access` (gpt-6.2-sol/medium), em arquivo separado. Revisões independentes: `review_contracts` (gpt-6.2-astra/high), validador e compatibilidade; `review_scripts` (gpt-6.2-sol/high), publicação/leitura e testes. Sem achados remanescentes nos respectivos recortes. O orquestrador foi o único executor de builds/runners.

## Verificações executadas

| Comando | Resultado | Evidência |
|---|---|---|
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore --disable-build-servers -c Debug | Exit 0, quatro avisos preexistentes, zero erros; biblioteca netstandard2.0 | [build-debug.log](build-debug.log) |
| Runner direto Debug com -class Unimake.DFe.Test.AACBS.Validacao.AACBSLimitesContratoTest e -class Unimake.DFe.Test.AACBS.Validacao.AACBSValidacaoTest | 171 casos, zero falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| Mesmo build em INTEROP_Release | Exit 0, 15 avisos preexistentes, zero erros | [build-interop.log](build-interop.log) |
| Mesmo filtro no runner INTEROP_Release | 173 casos, zero falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |
| powershell.exe -NoProfile -ExecutionPolicy Bypass -File docsplan/aacbs/scripts/Testar-AcessoLocal.ps1 | Exit 0, acesso sintético/DPAPI e entradas públicas | [acesso-winps.log](acesso-winps.log) |
| pwsh -NoProfile -File docsplan/aacbs/scripts/Testar-AcessoLocal.ps1 | Exit 0, mesmos cenários no PowerShell 7.6 | [acesso-pwsh.log](acesso-pwsh.log) |
| pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Offline | Exit 0, regressões anteriores de journal/cotas/retomada em contexto sintético, sem carregar DLL ou importar acesso real | [roteiro-offline.log](roteiro-offline.log) |

O runner é xUnit v3/net10.0, executado com dotnet sobre a DLL gerada em bin/Debug/net10.0 ou bin/INTEROP_Release/net10.0. Não houve suíte integral, novo registro COM, pacote, publicação ou alteração de consumidor UniNFe/PRO. O comportamento padrão dos demais validadores foi preservado e testado; a AACBS não tem adaptação de consumidor incluída neste retrabalho. O PostBuild legado do build isolado continua com SolutionDir indefinido e IgnoreExitCode; a prova INTEROP aqui é compilação e runner, sem instalação COM.

Uma primeira tentativa de iniciar dotnet foi impedida pelo acesso ao registro usado pela telemetria do CLI; DOTNET_CLI_TELEMETRY_OPTOUT=1 permitiu os builds sem alterar o código. O primeiro teste WinPS identificou duplicação JSON com nome escapado não reconhecida no leitor nativo: o mapeamento do nome decodificado foi corrigido e ambos os runtimes passaram no checkpoint final. Os logs finais acima comprovam o estado entregue. As mensagens de InitializeDefaultDrives são do ambiente de execução e não impediram os comandos finais, todos com exit 0.

[Checkpoint C#](CHECKPOINT-CS.csv): 29 fontes do validador e testes AACBS, estáveis antes/depois dos builds/runners. [Checkpoint PowerShell](CHECKPOINT-PS.csv): quatro scripts de produção/teste, estáveis durante os checks finais. As alterações externas de versão do projeto, testes NFe e exemplo eFrete foram preservadas e não fazem parte do delta ou da revisão.

## Gate humano e pendências reais

Correções offline entregues para revisão humana. A AACBS-005 continua BLOCKED nos critérios reais: RAD-01 indeterminada, pagamentos/recolhimentos sem reconciliação operacional, PCONT sem massa elegível e PRR adiada. Nenhum destes critérios foi convertido em sucesso. Não houve consulta real, autenticação, emissão, pagamento, envio externo ou modificação de arquivos privados reais nesta tentativa. Não houve commit.
