# AACBS — Apuração Assistida da CBS

Plano de integração da DLL Unimake.DFe com as APIs de apuração assistida da Receita Federal. Elaborado em 12/11/2026 e revisado em 24/11/2026, a partir da documentação pública e do código existente do CIOT.

**AACBS é a sigla deste domínio no plano aprovado.** Pastas e namespaces de código usam `AACBS`; esta pasta de planejamento usa `aacbs`.

O desenvolvedor fornecerá XML e receberá XML. A DLL cuidará de autenticação, parâmetros HTTP, JSON, download e conversão. A aplicação consumidora guardará os tíquetes e controlará quando consultar a situação ou baixar o resultado. O primeiro escopo é CBS; IBS depende de documentação própria e de nova autorização de escopo.

O catálogo atual prevê **dez serviços**: oito endpoints de negócio, incluindo geração síncrona de DARF RAD/PCONT, mais autenticação e download externo. A revisão também incorpora seis JSON Schemas oficiais e mudanças de campos/ambientes. As contradições remanescentes estão no documento de fontes; schema JSON não substitui os XSDs da DLL.

O DEV configurou as chaves e o modo local de verificação passou sem exibir valores. As instruções de proteção e retomada estão em [ACESSO-LOCAL.md](ACESSO-LOCAL.md). Esse preparo não faz chamadas à Receita nem comprova autorização remota. A AACBS-000 foi aprovada explicitamente e a execução da AACBS-001 foi liberada; o PDCA é a fonte do estado atual.

A entrega técnica da AACBS-001 começa em [contratos/ESPECIFICACAO.md](contratos/ESPECIFICACAO.md), com catálogo de raízes, matrizes por campo e propostas para os conflitos das fontes. A baseline oficial e seus hashes estão em [fontes/2026-11-24](fontes/2026-11-24/README.md).

## Entrada e retomada

1. Ler [PDCA.md](PDCA.md), a única fonte de status e aprovações.
2. Ler [PLANO.md](PLANO.md) e [DECISOES.md](DECISOES.md).
3. Consultar [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md), [MATRIZ-APIS.md](MATRIZ-APIS.md) e [ARQUITETURA-E-TESTES.md](ARQUITETURA-E-TESTES.md).
4. Seguir [AGENTES.md](AGENTES.md) para distribuir o trabalho da etapa autorizada.
5. Ao entregar uma etapa, registrar seu dossiê conforme [EVIDENCE-TEMPLATE.md](EVIDENCE-TEMPLATE.md), atualizar o PDCA e aguardar o DEV.

Uma aprovação explícita do DEV da etapa N libera a etapa N+1, conforme sua instrução. Aprovação e início são registros separados; não é necessária uma segunda confirmação para iniciar a sucessora já liberada. A entrega de um agente nunca equivale a aprovação humana.

## Entregas abrangentes

| Etapa | Resultado |
| --- | --- |
| AACBS-000 | Plano, decisões estruturais, inventário inicial, agentes e requisitos para testes reais. |
| AACBS-001 | Fontes qualificadas e especificação completa dos contratos API/XML/XSD, ainda sem implementação. |
| AACBS-002 | Classes XML, schemas de envio e retorno, validação central e testes de contratos. |
| AACBS-003 | Autenticação e infraestrutura API, com o fluxo de débitos validado offline e depois no ambiente real liberado. |
| AACBS-004 | Demais consultas e dois DARFs, com cobertura controlada integral e testes reais incrementais dos cenários habilitados. |
| AACBS-005 | Consolidação das evidências reais, compatibilidade, empacotamento oficial, documentação e revisão final. |

AACBS-000, AACBS-001 e AACBS-002 foram aprovadas. A AACBS-002 entrega 24 raízes XML, 25 XSDs, validação central/de conteúdo e testes offline. As propostas de datas e retorno global RAD foram aceitas pelo DEV. A AACBS-003 foi entregue para revisão humana com autenticação e ciclo real de débitos em Produção Beta concluídos, incluindo a compatibilidade NI 8/14 aprovada. O retorno real veio vazio; cenários com itens foram testados offline. Ver o PDCA e o [dossiê da etapa](evidencias/AACBS-003/tentativa-0001/ENTREGA.md). Este trabalho não publica pacote nem altera UniNFe ou PRO.

## Evidências

A entrega da AACBS-002 está em [evidencias/AACBS-002/tentativa-0001/ENTREGA.md](evidencias/AACBS-002/tentativa-0001/ENTREGA.md), com o checkpoint de código, os 292 testes aprovados e os logs de build. A [entrega aprovada da AACBS-001](evidencias/AACBS-001/tentativa-0001/ENTREGA.md), a [revisão aprovada da AACBS-000](evidencias/AACBS-000/tentativa-0002/ENTREGA.md) e o [dossiê inicial](evidencias/AACBS-000/tentativa-0001/ENTREGA.md) permanecem como histórico. Logs adicionais só entram quando ajudam a comprovar o resultado; segredos e dados fiscais identificáveis não pertencem às evidências.

O retrabalho aprovado dos quatro achados do ticket ID #189855 está na [tentativa-0002](evidencias/AACBS-002/tentativa-0002/ENTREGA.md), com testes Debug e INTEROP aprovados. A [pesquisa dos ambientes na AACBS-003](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) registra a distinção entre Piloto/PRR e Beta e a ausência de uma rota PRR confirmada nas fontes atuais.

Os quatro achados da revisão da AACBS-003 foram corrigidos na [tentativa-0002](evidencias/AACBS-003/tentativa-0002/ENTREGA.md): 341 testes Debug, 344 INTEROP, 132 regressões e roteiro offline aprovados, com conferência nativa de COM. A etapa aguarda aprovação humana. Esta rodada não fez novas chamadas reais.

A [tentativa-0003](evidencias/AACBS-003/tentativa-0003/ENTREGA.md) corrige o diagnóstico da autenticação auxiliar, com três regressões novas e 344 testes Debug/347 INTEROP aprovados. É a entrega corrente para revisão humana da AACBS-003; a AACBS-004 continua pendente.
