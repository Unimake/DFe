# AACBS-001 — Qualificação das fontes e especificação dos contratos

## Identificação e autorização

| Campo | Registro |
| --- | --- |
| Etapa / tentativa | AACBS-001 / tentativa-0001 |
| Data | 24/11/2026, America/Sao_Paulo |
| Base Git | `main`, HEAD `451b90b04a8effe061e655b3eebba2cb96f96fbc`; alterações documentais da AACBS-000 e versão do `.csproj` já pendentes foram preservadas. |
| Liberação | DEV: "Já configurei as chaves. Confira a configuração local. Aprovo a AACBS-000 e libero a execução da AACBS-001." Aprovação e início estão em linhas distintas do PDCA. |
| Escopo | Fontes públicas, especificação, catálogo XML e matrizes por campo. Nenhum código da biblioteca, serviço ou XSD de produção criado. |
| Estado autoritativo | [PDCA.md](../../../PDCA.md). |

## Configuração local

Executado `powershell.exe -NoProfile -ExecutionPolicy Bypass -File "C:/projetos/github/Unimake.DFe/docsplan/aacbs/scripts/Configurar-AcessoLocal.ps1" -Verificar`: exit code 0, configuração e credencial cifrada verificadas, sem valores em saída. O host emitiu o aviso conhecido de inicialização do provider FileSystem, mas o preparador concluiu a leitura e sua mensagem de sucesso. Nenhum token foi solicitado ou credencial exportada em texto aberto. O ambiente informado pelo DEV é Produção Restrita; isso não comprova sua autorização ou uma rota remota.

## Entrega e critérios

| Critério AACBS-001 | Evidência | Resultado |
| --- | --- | --- |
| Fontes atuais qualificadas e preservadas | [Baseline](../../../fontes/2026-11-24/README.md) e [MANIFESTO-FONTES.csv](../../../fontes/2026-11-24/MANIFESTO-FONTES.csv): 23 capturas públicas com URL, bytes e SHA-256. | Atendido. PDF complementar qualifica o mesmo emissor OAuth; não importa regras fiscais DeRE. |
| Inventário completo | [MATRIZ-APIS.md](../../../MATRIZ-APIS.md) e [catálogo XML](../../../contratos/CATALOGO-XML.csv). | Dez serviços, 24 raízes distintas; tipos comuns propostos separadamente. |
| Campos, tipos, formatos, cardinalidade, XML/API e testes | [CONSULTAS-CAMPOS.csv](../../../contratos/CONSULTAS-CAMPOS.csv), [DARF-CAMPOS.csv](../../../contratos/DARF-CAMPOS.csv), [ACESSO-CAMPOS.csv](../../../contratos/ACESSO-CAMPOS.csv). | 269 registros únicos, 12 colunas preenchidas; caminhos dos seis schemas de retorno cobertos. |
| Contrato público implementável | [ESPECIFICACAO.md](../../../contratos/ESPECIFICACAO.md), [CONSULTAS.md](../../../contratos/CONSULTAS.md), [DARF.md](../../../contratos/DARF.md), [ACESSO-E-FLUXO.md](../../../contratos/ACESSO-E-FLUXO.md). | Namespace Unimake, XML 1.00, classes/roots/schemas, Result concreto, presença/zeros/datas/COM e erros definidos para aprovação. |
| OAuth e configuração | Fonte F21, RFCs e contrato de acesso: Basic normativo, form-urlencoded, grant fixo, cache/expiração/cotas, membros reais ClientID/ClientSecret. | Qualificação documental concluída; sem autenticação real. |
| Divergências com proposta explícita | Especificação comum e lacunas das fontes. | Datas DARF em escolha exclusiva, união date/date-time no RAD de pagamentos, CNPJ14 DARF e retorno global do lote. Propostas aguardam revisão do DEV. |
| Requisitos reais e restrições | Configuração local verificada; PRR, cenário DARF, limites/PDF e cotas documentados. | Parte local atendida; a falta de rota PRR bloqueia apenas o futuro consumo nesse ambiente, sem impedir XML/XSD offline na 002. |

## Verificações documentais executadas

O executor econômico validou os dados por PowerShell nativo com `ErrorActionPreference=Stop`, exit code 0:

- 23 hashes e tamanhos do manifesto conferem com os arquivos capturados.
- Matrizes: consultas 120 linhas, DARF 52 e acesso 97; total 269 IDs únicos e nenhum campo obrigatório da tabela vazio.
- Catálogo: 24 raízes únicas, dez serviços e igualdade entre raízes das matrizes e do catálogo.
- Percurso dos schemas por `properties`, arrays, referências `$ref` e ramificações: nenhum caminho oficial de propriedade sem linha correspondente na matriz. A contagem abaixo considera caminhos únicos; as matrizes também discriminam ramos e itens de coleção.

| Schema de retorno | Caminhos únicos | Faltantes |
| --- | ---: | ---: |
| Débitos | 21 | 0 |
| Créditos | 29 | 0 |
| Pagamentos | 21 | 0 |
| Recolhimentos | 19 | 0 |
| DARF RAD | 11 | 0 |
| DARF PCONT | 13 | 0 |

A primeira tentativa da checagem estática usou o alias WindowsApps `python.exe`, que não iniciou. O código de processo reportado não comprovou execução, e nenhum resultado foi aceito daquela tentativa. A checagem foi refeita integralmente com PowerShell nativo e produziu o resultado acima. Nenhum runtime foi instalado.

A conferência final de navegação documental percorreu 18 arquivos Markdown e 98 links locais, todos válidos, sem espaços finais nas linhas. `git diff --check` concluiu com exit code 0. `git check-ignore` confirmou os dois arquivos locais de acesso ignorados. Alterações paralelas fora de `docsplan/aacbs` — incluindo arquivos NFCe/NFe e projeto — foram observadas no checkout e preservadas, sem revisão ou edição por esta entrega.

Os documentos especificam grupos de casos A01–A08, ARQ-01–ARQ-12, D01–D11 e COM-01–COM-09. São critérios para os futuros testes; nenhum caso fiscal ou unitário da DLL foi executado nesta etapa. Não houve build, INTEROP, pacote, token, sonda, consulta ou emissão DARF. Os únicos requests externos foram para documentação pública.

## Agentes e revisão independente

| Papel / agente | Modelo e esforço | Arquivos sob responsabilidade |
| --- | --- | --- |
| Orquestrador | Modelo da sessão, herdado | Baseline, contrato comum, catálogo, integração dos resumos, PDCA e dossiê. |
| `aacbs001_consultas` | gpt-6.2-sol, medium | CONSULTAS.md e CONSULTAS-CAMPOS.csv. |
| `aacbs001_darf` | gpt-6.2-sol, medium | DARF.md e DARF-CAMPOS.csv. |
| `aacbs001_acesso` | gpt-6.2-astra, high | ACESSO-E-FLUXO.md e ACESSO-CAMPOS.csv. |
| `aacbs001_revisao` | gpt-6.2-astra, high, contexto independente | Revisão somente leitura dos contratos, baseline e pontos de compatibilidade. |
| `aacbs001_verificacao` | gpt-6-luna, low | Execução mecânica da checagem documental, sem edição. |

A revisão independente apontou seis problemas corrigidos antes da entrega: chave de cache exigindo empresa não disponível no contrato de autenticação; nomes de propriedades do envelope divergindo do catálogo; erroLocal admitido indevidamente dentro dos arquivos; estado/HTTP anterior retido ao reutilizar serviço; responsabilidade pelo descarte/tempo do link; e codificação dos componentes de Basic. As correções e seus testes planejados foram conferidos pelo revisor, sem pendência material nesses ajustes. O parecer não aprova a etapa em nome do DEV.

## Restrições e aceite solicitado

- **PRR:** os comunicados comprovam disponibilidade do ambiente, mas as fontes técnicas consultadas não publicam a URI REST CBS atual aplicável. Não inventar rota nem usar produção como fallback. Obter confirmação oficial antes do real da 003.
- **Compatibilidade documental:** aprovar explicitamente as variantes de data preservadas e as regras de validação propostas. Elas não tornam HTML e JSON Schema simultaneamente conformes.
- **RAD lote:** implementar array de entrada e objeto global como publicados, sem resultado por DF-e fabricado. Teste real inicial unitário; lote real depende de esclarecimento de atomicidade/agregação e cenário específico.
- **PDF/idempotência/limites não publicados:** link preservado, sem download PDF ou pagamento; sem retry de POST ou limite oficial inventado. Resposta vazia de consulta não prova a família sem seu contexto persistido pelo consumidor.

Essas restrições não exigem adivinhar intenção para produzir os modelos/XML/XSD offline especificados. Sua aprovação pode liberar a AACBS-002 com os limites acima; não constitui execução real da 003 ou emissão na 004.

## Retomada, reversão e gate

A retomada ocorre pelo PDCA e pela especificação comum. Mudança de fonte exige comparar com a baseline/hash e revisar o impacto no contrato. Os bytes capturados têm `.gitattributes` local para evitar conversão de EOL e falso conflito de integridade.

Reversão deve considerar apenas os arquivos novos desta etapa e seus hunks de integração. Preservar a aprovação e evidências da AACBS-000, alterações externas do `.csproj` e todo acesso privado do DEV. Não remover chaves, tíquetes ou cache por rotina.

Entrega ao DEV em 24/11/2026. Aprovação posterior da AACBS-001: **APROVADA** em 24/11/2026. DEV: "Aprovo a AACBS-001 e libero a execução da AACBS-002. Pode seguir com as propostas documentadas para as datas e o retorno global do DARF RAD." Aprovação e início da AACBS-002 registrados separadamente no PDCA. AACBS-003 e sucessoras não iniciadas.
