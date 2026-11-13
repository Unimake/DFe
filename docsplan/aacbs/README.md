# AACBS — Apuração Assistida da CBS

Plano de integração da DLL Unimake.DFe com as APIs de apuração assistida da Receita Federal. Elaborado em 12/11/2026, a partir da documentação pública e do código existente do CIOT.

**AACBS é a sigla proposta para este domínio.** Pastas e namespaces de código usarão `AACBS`; esta pasta de planejamento usa `aacbs`. A revisão da etapa inicial pode ajustar o nome antes de nascer a API pública.

O desenvolvedor fornecerá XML e receberá XML. A DLL cuidará de autenticação, parâmetros HTTP, JSON, download e conversão. A aplicação consumidora guardará os tíquetes e controlará quando consultar a situação ou baixar o resultado. O primeiro escopo é CBS; IBS depende de documentação própria e de nova autorização de escopo.

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
| AACBS-003 | Autenticação e infraestrutura API, com o fluxo completo de débitos validado offline. |
| AACBS-004 | Créditos, pagamentos e recolhimentos completos e cobertura integral dos serviços. |
| AACBS-005 | Testes reais, compatibilidade, empacotamento oficial, documentação de uso e revisão final. |

Esta criação do plano corresponde somente à AACBS-000. Nenhuma etapa de implementação está autorizada antes dos respectivos gates. O planejamento não publica pacote nem altera o UniNFe ou o PRO.

## Evidências

O dossiê inicial está em [evidencias/AACBS-000/tentativa-0001/ENTREGA.md](evidencias/AACBS-000/tentativa-0001/ENTREGA.md). Tentativas posteriores preservam as anteriores. Logs adicionais só entram quando ajudam a comprovar o resultado; segredos e dados fiscais identificáveis não pertencem às evidências.
