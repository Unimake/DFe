# Ledger

| QuestionId | Tema | Pergunta/decisão | Resposta atual | Fonte | Estado | Owner | Deadline | Impacto |
|---|---|---|---|---|---|---|---|---|
| Q-DEST-001 | Destino | Onde gerar o plano da DLL? | C:\projetos\github\Unimake.DFe | pedido 2026-09-10 | ANSWERED | DEV | ABI-000 | material |
| Q-SCOPE-001 | Escopo | Serviços sem endpoint publicado entram como transporte? | Não. Os modelos XML de consulta/eventos permanecem; transporte fica limitado a Status e Autorização publicados em homologação. | MOCs, catálogo oficial e DEC-001 | ANSWERED | DEV | ABI-001 | material alto |
| Q-PFX-001 | Governança | Confirmar prefixo ABI | Confirmado `ABI`, sem renumeração dos IDs candidatos `ABI-000` a `ABI-006`. | autorização da ABI-001 e DEC-003 | ANSWERED | DEV | ABI-001 | baixo |
| Q-API-001 | Compatibilidade | Todas as classes públicas NFeABI seguem padrão INTEROP? | Sim quando houver equivalente público nos DFes vizinhos; tipos internos ou exclusivos de schema não ganham superfície COM artificial. | AGENTS, padrões públicos e RISK-004 | ANSWERED | DEV | ABI-001 | material |
| Q-SCOPE-002 | Escopo | Produtos consumidores entram no plano NF-e ABI? | Não. O plano fica restrito à DLL Unimake.DFe; integrações externas possuem planejamento próprio. | retrabalho explícito do DEV e DEC-005 | ANSWERED | DEV | ABI-001 | material |
| Q-TEST-001 | Qualidade | Teste fiscal online integra o gate determinístico? | Não. É opt-in, exige autorização explícita, certificado/credenciamento e ambiente preparado; nunca substitui os testes offline. | LIM-003, AGENTS e plano ABI-005 | ANSWERED | DEV | ABI-001 | material |
