# Diagnóstico de disponibilidade: contrato e acesso ao conhecimento

Aplicar ao manter `Utility/Disponibilidade`, instrumentação SOAP/REST, autenticações auxiliares, reconhecimento de respostas ou integração do diagnóstico com consumidores. Para tarefas fiscais de outros domínios, consultar esta instrução somente se esses caminhos forem afetados.

## Fontes e responsabilidades

- Consumidor do diagnóstico: PRO em `C:\projetos\UniNFe.Desktop`. O usuário decidiu retirar o recurso do UniNFe legado; não presumir a remoção concluída. Serviços fiscais do legado continuam fora dessa decisão.
- DLL: observar operações e interpretar protocolo/retorno, classificar falhas, oferecer capacidades e sondas seguras. PRO: persistência, contexto de empresa/acesso, idempotência, agregação temporal, apresentação e bloqueios persistentes.
- Código e testes atuais são a prova. Matriz ou comentário não comprova execução: rastrear serviço concreto → preparação/login → transporte → parser/normalizador → conclusão da sessão. Enumerar downloads, utilitários externos e desvios da base comum; separar validação/conversão local de acesso remoto.

## Escolher a API

| Finalidade | API/condição |
|---|---|
| Observar tentativa real | `CanalObservacoesDisponibilidade.IniciarOperacao(config, operacaoId, contextoId)`; habilitar `ColetarTelemetriaDisponibilidade` e abrir antes do construtor quando ele puder autenticar |
| Configuração provisória | `sessao.DefinirConfiguracao(config)` antes do primeiro transporte; operacaoId identifica a tentativa e contextoId é opaco, definido pelo consumidor |
| Retorno/falha | `Concluir(xml, http)` / `Falhar(ex, http)`; preservar a exceção original e descartar a sessão em ordem inversa de criação |
| Drenar sem I/O | `TentarLer(out observacao)` e `ObservacoesDescartadas`; não entregar XML/configuração/exceção à persistência |
| Consultar capacidade | `CapacidadesDiagnosticoDisponibilidade` e `PossuiConsultaOficialStatus`; capacidade não prova endpoint nem disponibilidade atual |
| Verificação manual | `VerificarStatusServico()`; não incorpora histórico passivo anterior. Reutiliza cache/correlação das próprias sondas e suprime consumo indevido antes de preparar certificado/infraestrutura |
| APIs anteriores | `Executar`, `ObterDiagnosticoPassivo`, `ConsultarStatusServico`, `HistoricoLegado` e `Concluir(xml,http,bool)` estão obsoletos, preservados para transição; não ampliar nem removê-los silenciosamente |

Sessão ativa para a mesma instância de Configuracao impede coleta duplicada no histórico anterior. Outra configuração e execuções opt-in sem sessão mantêm o contrato de transição; isso não é um fallback recomendado ao PRO.

## Interpretar a fotografia

- Ler juntos `Status`, `TipoFalha`, `RespostaAplicacaoReconhecida`, `ContratoResposta`, `CodigoRespostaAplicacao` e `Limitacao`. Reconhecimento pode coexistir com estado inconclusivo; resposta válida/rejeição normal não equivale a autorização.
- `CStat` é exclusivo das famílias reconhecidas por `UsaCStat`; não aplicar seu mapa a GNRE, CIOT, eSocial/Reinf ou NFS-e. Não converter código de aplicação em HTTP nem cStat. Primeiro cStat documental prevalece sobre protocolos internos; SOAP Fault não confirma negócio.
- `EntradasTransporte` não é o total de requests internos de uma operação composta. `IntegracaoRemota` identifica utilitário separado do serviço fiscal; não atribuir sua falha à SEFAZ.
- Sem contrato/código conhecido, manter limitação/inconclusivo. Sem estratégia de status, verificação manual é não aplicável, mas a observação real continua possível. Estruturas locais, enums e classes vazias não provam serviço remoto.
- PDFs: somente evidência limitada de envelope recebido; manter bytes e posição, não reter stream/corpo nem validar assinatura/conteúdo fiscal por cabeçalho/EOF. GeradorCIOTShared continua opaco quanto a endpoint/HTTP/requests internos.
- Bloquear status por consumo indevido classificado, inclusive HTTP429, mesmo quando a fila descarta a observação. Não limpar cache por empresa/pedido para contornar limite. O registro interno atende tipos com status e ambientes válidos; persistência entre reinícios é do PRO.
- Preservar proxy/certificado/autenticação dos fluxos reais e os testes de timeout HttpClient. Cancelamento puro sem evidência de timeout permanece ambíguo nos runtimes antigos. Não inventar causa pelo texto da exceção.

## Onde procurar

Prefixo da biblioteca: `source/.NET Standard/Unimake.Business.DFe`. Ler somente a trilha relevante:

| Problema | Fontes/testes principais |
|---|---|
| Sessão, duplicidade, fila, preparação | `Utility/Disponibilidade/CanalObservacoesDisponibilidade.cs`; `Utility/Rede/CanalObservacoesDisponibilidadeTest.cs` e `MigracaoDiagnosticoSessaoTest.cs` no projeto de testes |
| Formato/código de negócio | `ReconhecedorRespostaDisponibilidade.cs` e reconhecedores NFSe/GNRE/CIOT na mesma pasta; `CoberturaDisponibilidadeTest` e `InterpretacaoContratosDisponibilidadeTest` |
| NFS-e municipal/nacional | `CatalogoRespostasNFSe.cs`, `CodigosRespostaNFSeNacional.cs`; [INTERPRETACAO.md](../../docsplan/diagnostico/INTERPRETACAO.md) e [CONTRATOS-NFSE.csv](../../docsplan/diagnostico/CONTRATOS-NFSE.csv) |
| HTTP/TLS/timeout, sanitização/agrupamento | `TelemetriaDisponibilidade.cs` (também contém classificador/agregador); `DiagnosticoDisponibilidadeTest`, `ProxyConfiguracaoTest`, `UMessenger/Transport/ApiTimeoutTest` |
| Status, cache, bloqueio | `DiagnosticoDisponibilidadeDFe.cs`; `MigracaoDiagnosticoSessaoTest` e status via executor SOAP em `CoberturaDisponibilidadeTest` |
| Autenticação eFrete e Result original | `Servicos/CIOT/Provedores/EFrete`; `CIOT/Servicos/EFreteFluxoExecucaoTest` e testes PDF em `InterpretacaoContratosDisponibilidadeTest` |
| Outros acessos | `Lookup/CpfCnpjComBrLookup.cs`, `Security/Token.cs`, `Security/UMessengerTokenCache.cs`, `Servicos/UMessenger/ServicoBase.cs`; confirmar os chamadores e a configuração efetiva |
| Inventário completo | [COBERTURA.md](../../docsplan/diagnostico/COBERTURA.md), [INVENTARIO.md](../../docsplan/diagnostico/INVENTARIO.md), [CONFIGURACOES.csv](../../docsplan/diagnostico/CONFIGURACOES.csv); não carregar todas as linhas por padrão |
| Migração do PRO | [MIGRACAO-SESSOES.md](../../docsplan/diagnostico/MIGRACAO-SESSOES.md), [COMANDO-PRO.md](../../docsplan/diagnostico/COMANDO-PRO.md); roteiro não autoriza editar outro repositório fora do escopo da tarefa atual |
| Validação anterior | [VALIDACAO.md](../../docsplan/diagnostico/VALIDACAO.md); contagens/versões são registros históricos, não garantia da árvore atual |

## Manter catálogos e validar

- Regras completas e comandos: [skill de disponibilidade](../../.codex/skills/manutencao-disponibilidade-dfe/SKILL.md) e sua referência. Para CIOT, aplicar também a skill manutencao-ciot quando normalização, autenticação ou fluxo fiscal forem afetados.
- Catálogo municipal: alterar/regenerar pelo script `docsplan/diagnostico/Gerar-CatalogoRespostasNFSe.ps1`, mantendo CSV e evidências dos XSDs. Não editar apenas o arquivo gerado nem interpretar o catálogo estrutural como mapa completo de códigos.
- IDs nacionais: `Gerar-CodigosNFSeNacional.py` exige o XLSX oficial auditado por hash. Nova fonte exige revisar significado/política antes de atualizar hash/gerado; não inferir faixas de códigos.
- Regressões determinísticas via serviço/consumidor/executor substituído: positivo, rejeição normal, indisponibilidade explícita, erro desconhecido/malformado, contrato/família errados, HTTP sem confirmação, transporte/autenticação. Conferir retorno/exceção intactos, uma tentativa sem requests extras e ausência de dados sensíveis. Não usar rede externa/A1 real/A3.
- Ao afetar cache/relógio, usar a coleção `MemoriaDiagnosticoDisponibilidade` e restaurar estado; preservar testes de desempenho. Rodar filtros focados do runner xUnit v3, build netstandard2.0/C#7.3 e INTEROP quando aplicável. Não usar VSTest legado nem suíte externa como substituta.
- Integração PRO exige pacote offline oficial novo e imutável, versão/ProductVersion restaurados e base fiscal compatível. DryRun não instala pacote; não comparar pacote anterior como se fosse o HEAD. Não usar ProjectReference ou feed externo. Respeitar consumidor somente leitura quando essa for a autorização da tarefa.
