# Contrato recomendado após a retirada do diagnóstico do UniNFe legado

Decisão do usuário: somente o PRO continuará usando diagnóstico de disponibilidade. O usuário removerá a rotina do UniNFe legado. Esta tarefa altera somente a DLL; o PRO foi consultado, sem edição, restore ou atualização de pacote.

## API recomendada

| Finalidade | API | Responsabilidade |
|---|---|---|
| Observação de uma tentativa real | `CanalObservacoesDisponibilidade.IniciarOperacao(config, operacaoId, contextoId)` | Consumidor abre sessão antes da preparação/construção que possa autenticar. Habilita `config.ColetarTelemetriaDisponibilidade` e chama `DefinirConfiguracao` antes do primeiro transporte se começou com configuração provisória |
| Retorno obtido | `sessao.Concluir(retornoXml, httpStatusCode)` | Novo overload de dois parâmetros. A DLL interpreta o protocolo; não precisa de booleano de reconhecimento vindo do PRO |
| Falha da tentativa | `sessao.Falhar(ex, httpStatusCode)` | Não modifica nem substitui a exceção da operação. Consumidor preserva a propagação original |
| Drenagem | `canal.TentarLer(out observacao)` e `ObservacoesDescartadas` | Consumidor persiste/agrega/apresenta somente a fotografia sanitizada e contabiliza perdas |
| Capacidade sem rede | `new CapacidadesDiagnosticoDisponibilidade(tipo)` ou `PossuiConsultaOficialStatus(tipo)` | Habilitar ações sem reproduzir listas fixas de documentos. Capacidade não garante endpoint para cada configuração nem significa disponibilidade atual |
| Verificação manual de status | `new DiagnosticoDisponibilidadeDFe(config, opcoes).VerificarStatusServico()` | Infraestrutura/status oficial com os mesmos limites/cache/proxy/certificado. Não lê, agrega ou limpa o histórico passivo antigo. Sem estratégia oficial retorna NãoAplicável sem rede |

A verificação manual continua com cache/histórico limitado das **próprias sondas**, necessários ao intervalo mínimo, correlação e consumo indevido. Isso não é o histórico passivo de operações do consumidor. Uma resposta de status vale somente para esse serviço; não encerra automaticamente um incidente de autorização, evento, download ou pagamento.

## Compatibilidade de transição

`Executar()`, `ObterDiagnosticoPassivo()`, `ConsultarStatusServico()`, as respectivas fachadas COM, `HistoricoLegado` e `Concluir(xml, http, bool)` estão marcados como obsoletos, sem remoção de assinatura nem renumeração de enums. Permanecem funcionais para que o pacote atualmente instalado continue migrável. Não são caminhos recomendados para código novo. A retirada definitiva poderá ocorrer depois de o PRO estar migrado e validado; não foi antecipada nesta tarefa.

Quando existe sessão ativa **para a mesma instância de Configuracao**, a base comum deixa de alimentar também o histórico anterior. Isso elimina duplicidade de coleta, classificação e armazenamento nessa execução. Outra configuração no mesmo fluxo não tem seu comportamento de transição suprimido. Sem sessão, a coleta antiga opt-in continua temporariamente compatível; não deve ser usada pelo PRO como fallback se a sessão falhar ou estiver ausente.

O canal registra o bloqueio preventivo de consumo indevido em memória diretamente, sem depender do histórico anterior ou de sucesso na entrega da fila. CStat 656/678 e HTTP429 classificados pela política central continuam distinguíveis. Com bloqueio vigente, `VerificarStatusServico` retorna a supressão **antes de preparar certificado e executar infraestrutura/status**. O bloqueio expira normalmente; não limpar memória para forçar uma nova consulta. A proteção persistente entre reinícios continua no PRO, a partir da classificação de ConsumoIndevido da DLL, inclusive HTTP429, sem duplicar uma lista local de códigos fiscais. O registro interno de supressão aceita somente documentos com status e os dois ambientes válidos, mantendo o número de chaves limitado; outros serviços continuam produzindo a observação de consumo indevido, mas não recebem um status fictício nem uma chave de bloqueio inútil.

## Fotografia que o PRO deve aproveitar

Além dos campos já consumidos (documento, serviço, contexto, HTTP, cStat, estado e falha), o DTO atual ainda precisa transportar os campos abaixo para preservar o diagnóstico implementado na DLL:

- `ContratoResposta`, `CodigoRespostaAplicacao`, `RespostaAplicacaoReconhecida`;
- `EntradasTransporte`, `IntegracaoRemota`, `Limitacao`.

`RespostaAplicacaoReconhecida=true` pode coexistir com estado inconclusivo: formato identificado não equivale a causa interpretada. Código de aplicação deve ser lido junto do contrato; não converter códigos GNRE, CIOT, eSocial ou Reinf em cStat. `EntradasTransporte` conta entradas nos executores instrumentados, não todos os requests internos. `IntegracaoRemota` separa utilitário externo de serviço fiscal. Contratos desconhecidos e observações antigas sem esses metadados não recebem sucesso inventado.

Hoje, `ObservacaoDFe.cs` no PRO passa `_respostaReconhecida` à DLL e monta um DTO sem esses seis campos. `ExecutorVerificacaoDFe` em `VerificacaoDisponibilidadeService.cs` usa uma lista fixa de cinco tipos, chama `ConsultarStatusServico()` e depois exclui telemetria do resultado. Migrar para capacidades da DLL e `VerificarStatusServico()` elimina a necessidade dessa exclusão como forma de corrigir o resultado manual. Confirmar novamente esses achados no código que estiver efetivamente no PRO antes de editar.

## Pacote e validação do consumidor

O último pacote consultado no PRO foi `20261108.1207.21`, ProductVersion `20261108.1207.21+e046f702936f83b5c77efaca1068e9300c9dbb94`. O HEAD da DLL era `f4576e293`, com alterações fiscais independentes entre a base e o HEAD (por exemplo regime IBS/CBS do CT-e, TXT/IPI e schema nNF zerado). Portanto não atualizar o PRO diretamente para um pacote do HEAD sob o pretexto de migrar diagnóstico.

Os DryRuns deste trabalho comprovam compilação/empacotamento das fontes deste checkout; não disponibilizam pacote para instalação e não comprovam equivalência fiscal com o pacote do PRO. Para consumir, preparar uma base isolada fiscalmente compatível e portar somente os patches necessários de diagnóstico/transporte/sanitização, incluindo testes. Confirmar quais documentos/serviços existem nessa base; não inserir mudanças fiscais independentes apenas para satisfazer referências do código novo. Gerar versão offline nova e imutável exclusivamente pelo fluxo oficial. Não usar ProjectReference nem publicar em feed externo. A autorização para uma atualização fiscal ampla permanece separada.

Provas offline na DLL: `MigracaoDiagnosticoSessaoTest` percorre o serviço público NFe.StatusServico com SOAP substituído, confere retorno/contagem/histórico, verifica isolamento manual e cache, supressão em fila cheia, ausência de rede quando não há status e expiração do bloqueio. As demais classes mantêm regressões de contratos, sanitização, timeout e transporte. Comandos e resultados finais ficam em VALIDACAO.md. Não foram executados testes do PRO com o pacote anterior como se validassem a API nova.
