# AACBS — Apuração Assistida da CBS

Plano de integração da DLL Unimake.DFe com as APIs de apuração assistida da Receita Federal. Elaborado em 12/11/2026 e revisado em 24/11/2026, a partir da documentação pública e do código existente do CIOT.

**AACBS é a sigla deste domínio no plano aprovado.** Pastas e namespaces de código usam `AACBS`; esta pasta de planejamento usa `aacbs`.

O desenvolvedor fornecerá XML e receberá XML. A DLL cuidará de autenticação, parâmetros HTTP, JSON, download e conversão. A aplicação consumidora guardará os tíquetes e controlará quando consultar a situação ou baixar o resultado. O primeiro escopo é CBS; IBS depende de documentação própria e de nova autorização de escopo.

O catálogo atual prevê **dez serviços**: oito endpoints de negócio, incluindo geração síncrona de DARF RAD/PCONT, mais autenticação e download externo. A revisão também incorpora seis JSON Schemas oficiais e mudanças de campos/ambientes. As contradições remanescentes estão no documento de fontes; schema JSON não substitui os XSDs da DLL.

O DEV configurou as chaves e o modo local de verificação passou sem exibir valores. As instruções de proteção e retomada estão em [ACESSO-LOCAL.md](ACESSO-LOCAL.md). Esse preparo não faz chamadas à Receita nem comprova autorização remota. A AACBS-000 foi aprovada explicitamente e a execução da AACBS-001 foi liberada; o PDCA é a fonte do estado atual.

A entrega técnica da AACBS-001 começa em [contratos/ESPECIFICACAO.md](contratos/ESPECIFICACAO.md), com catálogo de raízes, matrizes por campo e propostas para os conflitos das fontes. A baseline oficial e seus hashes estão em [fontes/2026-11-24](fontes/2026-11-24/README.md).

## Ponto atual de encerramento

A [consolidação técnica da AACBS-005](ENCERRAMENTO-TECNICO.md) reúne o entregue, os checkpoints e todas as pendências reais. O [material para a Receita](encaminhamento-receita/README.md) reúne os 404 de RAD/pagamentos/recolhimentos e anexos sanitizados mínimos. O DEV informou que enviou a solicitação após a versão resumida para o formulário; aguardamos a resposta, com protocolo ainda não informado. A versão completa preparada está cifrada localmente; o Codex não enviou mensagem. O [PDCA](PDCA.md) mantém BLOCKED na parte real e o gate de aceite do DEV; PRR está adiada e nenhum replay ou nova emissão integra este fechamento.
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

AACBS-000 a AACBS-004 foram aprovadas pelo DEV. A AACBS-005 foi entregue para revisão humana: 499 testes Debug, 509 INTEROP, compatibilidade NI de créditos aprovada em 25/11/2026 e original com 27 créditos validado sem nova chamada remota, COM nativo e pacote oficial em DryRun. A prova real de pagamentos/recolhimentos e DARFs permanece limitada conforme a matriz. O [guia de uso](uso/README.md) reúne os dez serviços e 24 XMLs sintéticos. O [dossiê corrente](evidencias/AACBS-005/tentativa-0001/ENTREGA.md) separa resultados comprovados e limitações reais. O PDCA mantém o estado e todas as transições.

## Evidências e histórico

- AACBS-000: [plano inicial](evidencias/AACBS-000/tentativa-0001/ENTREGA.md) e [revisão das fontes e acesso](evidencias/AACBS-000/tentativa-0002/ENTREGA.md).
- AACBS-001: [contratos qualificados](evidencias/AACBS-001/tentativa-0001/ENTREGA.md).
- AACBS-002: [serialização e XSDs](evidencias/AACBS-002/tentativa-0001/ENTREGA.md) e [correções do ticket ID #189855](evidencias/AACBS-002/tentativa-0002/ENTREGA.md).
- AACBS-003: [ciclo real de débitos](evidencias/AACBS-003/tentativa-0001/ENTREGA.md), [correções e compatibilidade COM](evidencias/AACBS-003/tentativa-0002/ENTREGA.md) e [diagnóstico OAuth](evidencias/AACBS-003/tentativa-0003/ENTREGA.md). O retorno real de débitos veio vazio; grupos com itens têm cobertura sintética.
- AACBS-004: [demais serviços e testes reais incrementais](evidencias/AACBS-004/tentativa-0001/ENTREGA.md) e [correções de roteiro e webhooks](evidencias/AACBS-004/tentativa-0002/ENTREGA.md). A aprovação posterior permite a consolidação na 005; não transforma as limitações de 404 e DARF em testes reais aprovados.

A [pesquisa dos ambientes](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) registra a ausência de rota PRR oficialmente qualificada. Produção Beta foi explicitamente autorizada pelo DEV. Antes de qualquer emissão real RAD/PCONT, o cenário e os parâmetros continuam sujeitos à confirmação específica. Nenhum pagamento será executado. Este escopo não publica pacote nem altera UniNFe ou PRO.

Em 25/11/2026, o DEV determinou preservar a [pesquisa aprofundada de Produção Restrita](evidencias/PESQUISA-PRR-2026-11-25/RESULTADO.md) e retomá-la depois, mantendo a continuidade em **Produção Beta**. O dossiê reúne [fontes e canais oficiais](evidencias/PESQUISA-PRR-2026-11-25/ORIGENS-PUBLICAS.md), [verificação local sanitizada](evidencias/PESQUISA-PRR-2026-11-25/VERIFICACAO-LOCAL.json), [roteiro de confirmação sem DARF](evidencias/PESQUISA-PRR-2026-11-25/ACESSO-SEM-DARF.md) e [solicitação técnica pronta, não enviada](evidencias/PESQUISA-PRR-2026-11-25/SOLICITACAO-TECNICA.md). Não há URL PRR atual qualificada nem acesso PRR comprovado; esses pontos não impedem continuar o escopo Beta já autorizado.

Logs só entram quando comprovam o resultado. Credenciais, tokens, tíquetes, links temporários e dados fiscais reais ficam protegidos sob `.local/aacbs`, ignorados pelo Git; não pertencem ao dossiê.

A [retomada AACBS-005, tentativa-0002](evidencias/AACBS-005/tentativa-0002/ENTREGA.md) foi autorizada para investigar os 404 e preparar cenários DARF. O CNPJ atual não tem saldo PCONT, conforme informado pelo DEV; a cobertura complementar offline foi entregue com cinco casos aprovados em Debug e INTEROP, enquanto uma massa real elegível fica pendente. Emissão continua dependendo de confirmação específica. O PDCA identifica o estado corrente; a entrega da tentativa-0001 permanece como histórico validado.
A [RAD-01, tentativa-0003](evidencias/AACBS-005/tentativa-0003/ENTREGA.md), foi expressamente autorizada pelo DEV e executada uma vez. Recebeu HTTP 404 sem retorno fiscal validado; o journal protege o resultado indeterminado e bloqueia repetição. A parte real da AACBS-005 está bloqueada para reconciliação/esclarecimento operacional. A proposta anterior não permanece simplesmente aguardando confirmação; o histórico de preparo e testes foi preservado. Nenhum pagamento foi executado.