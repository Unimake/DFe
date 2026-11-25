# Resultado da pesquisa aprofundada PRR — 25/11/2026

Pedido do DEV: localizar as URLs atuais de Produção Restrita e estabelecer como confirmar acesso com a credencial existente sem emitir DARF. Pesquisa pública em paralelo e conferência local somente em memória. A etapa AACBS-005 continua entregue para revisão humana; esta pesquisa não alterou seu aceite nem habilitou rotas.

## Resultado

Não foi localizada uma referência primária atual que publique as quatro URLs PRR de débitos, créditos, situação e obtenção do link. Isso é lacuna de qualificação técnica, não prova de inexistência do ambiente: a [Receita confirmou](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/novembro/receita-federal-libera-acesso-as-novas-apis-de-apuracao-da-cbs/) a disponibilidade de débitos/créditos v2 nos dois ambientes desde 28/10/2026. As páginas técnicas vigentes consultadas publicam somente Produção.

As pistas históricas de terceiros usam /apuracao-cbs-prr/v2, mas retratam payload anterior ao fluxo separado de download implantado em outubro. Não se atribui atualidade à rota nem se deduz uma URL PRR de download por analogia. A existência do ambiente não autoriza copiar a rota Beta para homologação. A página e os manuais da Calculadora/DeRE tratam outras APIs e não qualificam a apuração assistida CBS.

## Avanço verificável

O agente de pesquisa navegou no portal oficial público e encontrou caminhos concretos:

- [Gerar Credencial de Acesso para API](https://piloto-cbs.tributos.gov.br/servico/credencial-api-beta), em [Controle de Acesso](https://piloto-cbs.tributos.gov.br/lista-grupo?categoria=158). O acesso ao serviço direciona para autenticação gov.br. Não houve login, consentimento, criação ou revogação de credencial.
- [Minhas Autorizações de Acesso](https://piloto-cbs.tributos.gov.br/servico/autorizacoes): entrada oficial para conferir representação/autorização, sujeita a autenticação. A pesquisa não confirmou a situação particular da empresa no portal.
- [Fale Conosco do Piloto](https://piloto-cbs.tributos.gov.br/servico/atendimento/home): canal concreto oficial, que exige autenticação. Nenhuma mensagem foi submetida. Foi preparado [texto objetivo](SOLICITACAO-TECNICA.md) para confirmar URLs, versões e habilitação sem compartilhar segredo ou rotacionar chave.

A conferência dos arquivos públicos de Receita Integra encontrou somente o manual Prod v1.4, sem contrato OpenAPI ou catálogo de rotas CBS nesse diretório. A [origem e os limites das fontes](ORIGENS-PUBLICAS.md) registram o inventário da investigação.

## Credencial e confirmação funcional

A [verificação local](VERIFICACAO-LOCAL.json) confirmou arquivo DPAPI legível, configuração válida, ambiente Beta preservado e DARF desabilitado. O único token do cache estava expirado e não forneceu claims JWT inspecionáveis. Nenhuma chamada HTTP foi necessária e o journal foi preservado sob mutex. Não houve motivo demonstrado para revogar a credencial que já funcionou na v2 em Beta.

O [roteiro de confirmação sem DARF](ACESSO-SEM-DARF.md) descreve as provas e seus limites: /token comprova autenticação, não acesso à operação; tíquete PRR válido permite GET sem nova abertura; sem tíquete, uma consulta válida e confirmada por 201 é necessária para testar cada família. O orçamento proposto é uma abertura de débitos, depois uma de créditos, respeitando cache/journal/cotas, sem reenvio automático. A execução depende de qualificar as rotas e ajustar/testar explicitamente o suporte PRR, ainda bloqueado na DLL/harness. Nenhum diagnóstico com rota especulativa, tíquete fictício ou HTTP genérico é apresentado como prova de acesso PRR.

Não foram executados /token, introspecção não documentada, consulta fiscal, emissão DARF, pagamento, alteração de configuração/chave, build, commit ou publicação durante esta pesquisa. O verificador temporário está somente em .local/aacbs/pesquisa-prr, ignorado pelo Git; os documentos públicos não contêm credencial, token ou dados fiscais.

Responsáveis: orquestrador, verificação local e roteiro; pesquisa_prr_oficial (gpt-6.2-sol/high), fontes oficiais/acervo e navegação pública. Não houve agentes concorrentes no uso da credencial: somente o orquestrador a leu, sem transporte.

## Decisão do DEV e ponto de retomada

Em 25/11/2026, o DEV determinou: "Anota tudo isso que vc já pesquisou no plano para não perdermos. Vamos seguir da forma que já estamos fazendo e depois retomamos isso." A investigação/habilitação de PRR fica **adiada por decisão do DEV**; o trabalho continua em **Produção Beta**, conforme autorizações e limites anteriores. Não existe tarefa de pesquisar periodicamente ou testar PRR em segundo plano.

Na retomada solicitada pelo DEV, o orquestrador deverá:

1. Ler este resultado, [fontes e origens](ORIGENS-PUBLICAS.md), [roteiro](ACESSO-SEM-DARF.md) e [solicitação técnica](SOLICITACAO-TECNICA.md). As pistas históricas não qualificam rotas atuais.
2. Verificar se houve publicação oficial de URLs/contrato PRR ou resposta do canal técnico. O atendimento autenticado foi localizado, mas não houve mensagem enviada ou resposta recebida. Preservar essa distinção.
3. Revalidar configuração, credencial e cache local sem expor valores. A verificação de token expirado é um retrato desta data; não presumir o mesmo estado na retomada nem revogar chaves com base nele.
4. Com as rotas qualificadas, preparar o delta de configuração/seleção de ambiente e testes offline de isolamento, respeitando o gate de execução aplicável. Depois confirmar acesso por consultas dentro do orçamento documentado, sem DARF, sem repetir POST ambíguo e sem trocar para Beta automaticamente.

Responsável pela retomada técnica: orquestrador da AACBS, quando o DEV solicitar. Autorizações/contato institucional que dependam do acesso gov.br permanecem com o representante da empresa; o texto pronto não autoriza o Codex a enviar mensagem. Adiar PRR não aprova a AACBS-005, não encerra as lacunas reais de Beta e não dispensa confirmação específica de emissão de DARF.
