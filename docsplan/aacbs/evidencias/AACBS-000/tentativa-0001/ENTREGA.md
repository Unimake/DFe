# Entrega AACBS-000 — Planejamento inicial

## Identificação e base

| Campo | Registro |
| --- | --- |
| Etapa / tentativa | AACBS-000 / tentativa-0001 |
| Data | 12/11/2026, referência America/Sao_Paulo |
| Base Git | Branch `main`, HEAD `fd974501498dd57eea95246469b5c6b2de3d7d80` |
| Liberação | Pedido do DEV para estudar a integração, realizar entrevista grill-me e criar o plano em docsplan, com agentes, testes e gates humanos. |
| Escopo efetivo | Investigação e planejamento documental. Nenhuma implementação ou chamada de negócio. |
| Status autoritativo | Consultar [PDCA.md](../../../PDCA.md). |

A árvore estava limpa na primeira inspeção. Durante o trabalho surgiu uma alteração concorrente em `source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj`: AssemblyVersion/FileVersion de `2026.11.08.1207` para `2026.11.12.1433`. Nenhum agente desta equipe alterou esse arquivo; a atualização foi preservada e não compõe a entrega AACBS.

## Entrega e critérios

| Critério | Evidência | Resultado |
| --- | --- | --- |
| Objetivo, escopo e sequência implementável | [PLANO.md](../../../PLANO.md), com AACBS-000 a AACBS-005 e critérios por entrega. | Atendido para revisão. |
| Decisões do DEV separadas de propostas | [DECISOES.md](../../../DECISOES.md): consumidor controla espera/retomada e primeiro escopo é somente CBS. Sigla AACBS permanece proposta. | Atendido. |
| Inventário e requisitos reais | [FONTES-E-REQUISITOS.md](../../../FONTES-E-REQUISITOS.md) e [MATRIZ-APIS.md](../../../MATRIZ-APIS.md): seis endpoints de negócio, autenticação e download externo; lacunas identificadas. | Atendido como levantamento inicial; qualificação detalhada pertence à 001. |
| Ligação com o CIOT e infraestrutura central | [ARQUITETURA-E-TESTES.md](../../../ARQUITETURA-E-TESTES.md). | Atendido. |
| Estados e aprovação exclusivamente humana | [PDCA.md](../../../PDCA.md). | Definidos; nenhuma aprovação presumida. |
| Agentes, paralelismo e economia | [AGENTES.md](../../../AGENTES.md): papéis, modelos disponíveis, quatro slots, dono por arquivo e executor único. | Atendido. |
| Evidência proporcional e retomada | [EVIDENCE-TEMPLATE.md](../../../EVIDENCE-TEMPLATE.md) e este dossiê. | Atendido. |
| Ausência de execução prematura | Somente arquivos de documentação em `docsplan/aacbs` produzidos pela equipe. | Nenhuma etapa 001+ iniciada. |

## Agentes e propriedade dos arquivos

Os três agentes usaram modelo e esforço herdados da sessão, sem override. Não houve agente executor de testes: não há implementação nesta etapa.

| Agente / papel efetivo | Responsabilidade |
| --- | --- |
| Raiz / orquestrador | Entrevista, integração, README.md, PLANO.md, DECISOES.md, este dossiê e verificação final. |
| `mapear_ciot` | Inspeção CIOT e dependências; ARQUITETURA-E-TESTES.md; revisão cruzada do plano. |
| `investigar_api_rfb` | Pesquisa documental oficial; FONTES-E-REQUISITOS.md e MATRIZ-APIS.md. |
| `planejar_orquestracao` | Processo e ferramentas; AGENTES.md, PDCA.md e EVIDENCE-TEMPLATE.md; revisão cruzada. |

A investigação e a entrevista foram somente leitura. A redação dos arquivos começou após as duas decisões estruturais do DEV e o encerramento anunciado da entrevista inicial. A raiz integrou a correção de redação do AGENTES.md e registrou a entrega no PDCA.

## Verificação executada

- Inspeção com `git status --short`, `git branch --show-current`, `git rev-parse HEAD` e `git diff` para identificar a base e distinguir alterações concorrentes.
- Leitura das instruções, serviços CIOT, conversores, configurações, modelos XML, validação central, projeto, exemplos de testes e fluxo oficial de pacote. Os comandos de build/runner publicados no plano foram conferidos documentalmente.
- Consulta às páginas oficiais indicadas no inventário e ao manual de plataforma. O manual técnico Receita Integra vigente ainda não foi qualificado; a limitação está registrada e condiciona o código de autenticação.
- Revisão cruzada do conteúdo, escopo, gates, contratos previstos e estratégia de testes.
- Verificação estática por script PowerShell inline com `Get-ChildItem`, leitura dos Markdown e resolução de destinos: **10 arquivos, 27 links locais, zero links quebrados e zero linhas com whitespace final**, exit code 0. O script incluiu os arquivos novos ainda não rastreados pelo Git.
- `git diff --check`: exit code 0. `git status --short` confirmou somente os documentos novos desta entrega e a alteração concorrente de versão no `.csproj` já descrita. O diff-check do Git não foi usado sozinho para afirmar a validade dos documentos não rastreados.

Builds, testes unitários, INTEROP, empacotamento e chamadas reais: **não executados**. Não são prova necessária da criação documental desta etapa; suas execuções e critérios estão previstos nas etapas técnicas.

## Revisão técnica e correção

`mapear_ciot` revisou PLANO.md, MATRIZ-APIS.md, FONTES-E-REQUISITOS.md, AGENTES.md e PDCA.md, sem achados materiais no escopo documental.

`planejar_orquestracao` identificou uma contradição na proibição genérica de dados reais “em arquivos”. A raiz corrigiu AGENTES.md para distinguir código/massas/logs/evidências da configuração local protegida e da gravação operacional dos XMLs pelo consumidor. Isso preserva a entrega de XML real solicitada pelo DEV. A correção foi conferida na versão final. Nenhuma revisão de agente constitui aprovação humana.

## Limitações e retomada

As lacunas da fonte oficial incluem autenticação detalhada, conjunto incompleto de ambientes restritos e divergências de campos/cardinalidades nos exemplos. Há método e responsável para resolvê-las na AACBS-001; não foram preenchidas por suposição. Credenciais, empresa e dados reais ainda precisam ser providenciados antes dos testes reais.

O próximo trabalho, somente após aprovação desta entrega, é a AACBS-001: qualificar fontes e especificar contratos campo a campo. A AACBS-001 permanece documental; as classes e XSDs começam na AACBS-002 após seu próprio gate.

Para retirar exclusivamente esta entrega, considerar somente os novos documentos sob `docsplan/aacbs`. Preservar a alteração concorrente do `.csproj` e qualquer trabalho posterior. Não executar rollback por rotina, nem remover evidências de tentativas entregues.

## Gate humano

- Entrega para revisão: 12/11/2026.
- Decisão do DEV: **AGUARDANDO**.
- Aprovação de AACBS-000: **não recebida**.
- Sucessora: **não iniciada**.

A aprovação explícita desta etapa libera a AACBS-001. A aprovação e o início serão registrados separadamente no PDCA, sem solicitar uma segunda confirmação ritual.
