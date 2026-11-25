# AACBS-005 — Retomada das pendências reais

Estado DELIVERED_FOR_REVIEW em 25/11/2026, após a retomada autorizada pelo DEV: "Pode continuar na AACBS-005. Investigue os HTTP 404 de pagamentos e recolhimentos e prepare os cenários reais de DARF RAD e PCONT. Antes de emitir qualquer DARF, confirme comigo os parâmetros."

Escopo: Produção Beta; PRR continua adiada. Investigar os 404 preservando as tentativas ambíguas anteriores, qualificar fontes e preparar pedidos concretos de DARF com dados locais disponíveis. Nenhuma emissão RAD/PCONT, pagamento ou envio de cenário será executado antes da confirmação específica. Não rotacionar credenciais nem reenviar automaticamente POSTs ambíguos.

Responsáveis e propriedade: orquestrador coordena dossiê/PDCA, lê configuração e é o único executor potencial de rede fiscal; aacbs005_404_fontes (gpt-6.2-sol/high) faz auditoria pública/estática somente leitura e pode escrever INVESTIGACAO-404.md neste dossiê; aacbs005_darf_preparo (gpt-6.2-astra/high) lê somente artefatos fiscais já protegidos e prepara cenários privados, sem rede/credencial/journal e sem alterar código público. Revisor independente será acionado para resultados críticos; nenhum build paralelo.

Orçamento inicial da retomada: pesquisa pública e inspeção de evidências existentes sem chamadas fiscais. Antes de qualquer prova de rede adicional, definir sua necessidade, URL/método/operação e orçamento neste documento. Não tratar HTTP genérico como prova de inexistência ou falta de permissão.

## Direção complementar do DEV

Ao solicitar PA/forma de pagamento para preparar PCONT, o DEV informou que o CNPJ configurado não tem saldo e pediu avançar nos testes possíveis, deixando a massa real para obter depois com outra empresa. PCONT não recebe período ou valor inventado. Um novo contribuinte exigirá acesso/representação apropriados, sem reaproveitar tíquetes de outra empresa.

aacbs005_darf_cobertura (gpt-6.2-sol/high) foi delegado para auditar lacunas e, somente se concretas, criar AACBSDarfContinuidadeTest.cs e COBERTURA-COMPLEMENTAR.md. Não altera produção e não executa builds/rede. A revisão de cobertura complementa a bateria anterior; não transforma transporte controlado em prova de emissão real.

## Resultado da investigação e preparo

Os oito contratos HTML/JSON Schema de pagamentos/recolhimentos e RAD/PCONT foram reconsultados e mantêm os hashes aprovados: [REVALIDACAO-FONTES.csv](REVALIDACAO-FONTES.csv). Não houve alteração de rota ou payload documentado que justificasse corrigir a DLL. A [investigação 404](INVESTIGACAO-404.md) confirmou o alinhamento das requisições e os limites das evidências antigas. Elas não preservam corpo/cabeçalhos remotos que permitam distinguir gateway e backend. As notícias oficiais de RAD apresentam linguagem divergente e não confirmam individualmente a ativação operacional das duas consultas no Beta.

A causa dos 404 e a existência de solicitações remotas permanecem indeterminadas. Os horários e a pergunta necessária ao suporte estão no [texto preparado](SOLICITACAO-404.md), não enviado. As correlações dos logs são locais e não podem ser apresentadas como request IDs da Receita. Não foram repetidos POSTs ambíguos, alteradas fases do journal ou introduzidos GET/HEAD/OPTIONS de baixo poder diagnóstico apenas para obter outro status.

O [preparo de DARF](PREPARO-DARF.md) gerou o candidato **RAD-01**, com uma NF-e real do arquivo de créditos e opção PAGAMENTO_TOTAL, sem valor parcial. A escolha passou por filtros de chave única, modelo 55, dígito verificador e raiz do emitente distinta da matriz, mas não comprova débito ainda não extinto nem elegibilidade RAD. O XML exato está cifrado e foi validado por XSD/releitura; o visualizador local confere seu hash e permite ao DEV revisar CNPJ/chave/opção. Nenhum valor do crédito foi convertido em valor de DARF. Uma eventual autorização precisa identificar o arquivo exato, confirmar elegibilidade e aceitar uma emissão total; não foi recebida.

PCONT permanece sem cenário real: o retorno de débitos contém zero períodos e o DEV confirmou ausência de saldo na empresa disponível. Não foi fabricado período, saldo ou XML de emissão. A [matriz de retomada](MATRIZ-RETOMADA.md) registra o preparo necessário para outro contribuinte e o isolamento de acesso, empresa, ambiente, tíquete e cota.

## Cobertura e verificações executadas

Uma classe nova de testes, sem alteração de produção, cobre cinco lacunas concretas documentadas em [COBERTURA-COMPLEMENTAR.md](COBERTURA-COMPLEMENTAR.md): cache OAuth compartilhado por RAD/PCONT com Configuracao reutilizada e renovação só na próxima chamada explícita após 401; leitura de DARF interrompida depois dos headers em RAD e PCONT, com HTTP preservado, corpo descartado e resultado anterior limpo; 404 JSON/HTML incompatíveis na abertura de pagamentos/recolhimentos, sem tíquete ou código fiscal inventado, vazamento de corpo ou novo POST. O sucesso de transporte 200 com falha de leitura continua inconclusivo para emissão remota.

| Verificação | Resultado | Evidência |
|---|---|---|
| Build Debug dos testes, --no-restore --no-incremental --disable-build-servers | Exit 0; 4 avisos preexistentes, zero erros | [build-debug.log](build-debug.log) |
| Runner xUnit v3 Debug, filtro da classe nova | 5 casos; zero falhas, erros, ignorados ou não executados | [testes-debug.log](testes-debug.log) |
| Mesmo build em INTEROP_Release | Exit 0; 15 avisos preexistentes, zero erros | [build-interop.log](build-interop.log) |
| Runner xUnit v3 INTEROP_Release, filtro da classe nova | 5 casos; zero falhas, erros, ignorados ou não executados | [testes-interop.log](testes-interop.log) |

Comandos de runner: `dotnet source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.DFe.Test.dll -class Unimake.DFe.Test.AACBS.Servicos.AACBSDarfContinuidadeTest`, e o mesmo caminho em INTEROP_Release. Nenhuma suíte fiscal integral ou nova execução dos 499/509 casos históricos. Não somar contagens de execuções para apresentar toda a suíte como revalidada. O PostBuild preexistente registra Build.bat com SolutionDir indefinido no build isolado e ignora o exit code por configuração existente; esta tentativa comprova compilação/runner, não novo registro/instalação COM.

O executor único aacbs005_executor (gpt-6-luna/low) verificou os **143 hashes** antes/depois, sem divergência. A versão externa do projeto principal passou a 2026.11.25.0805 antes deste checkpoint; foi preservada e não pertence ao delta AACBS. Os hashes das DLLs, do cenário cifrado e do contexto estão em [VERIFICACAO-FINAL.json](VERIFICACAO-FINAL.json). Não houve novo empacotamento ou validação de consumidores: o código de produção e o fluxo de pacote não mudaram nesta tentativa; a prova anterior permanece histórica no respectivo checkpoint.

## Revisão e entrega

aacbs005_revisao (gpt-6.2-astra/high) revisou fontes, hipóteses do 404, preparo privado/visualizador, hashes e novas asserções: [REVISAO.md](REVISAO.md). Sem achados impeditivos remanescentes no escopo. Os autores não executaram builds/rede fiscal; o preparo leu somente fontes protegidas existentes e gravou o candidato em pasta ignorada, sem tocar no journal. Não houve token, API fiscal, DARF, pagamento, pacote, commit ou publicação nesta retomada.

Esta tentativa está **DELIVERED_FOR_REVIEW** com o avanço possível concluído e as condições reais explícitas. A aprovação cabe ao DEV. Pagamentos/recolhimentos dependem de esclarecimento/rastreio operacional; RAD aguarda confirmação específica do candidato; PCONT aguarda empresa/período com saldo, conforme decisão do DEV de obter massa posteriormente. Nenhuma dessas condições foi convertida em prova real aprovada. PRR permanece adiada. Correção posterior abre nova tentativa; reversão solicitada deverá retirar somente o novo teste/documentação desta rodada e preservar os artefatos privados e o trabalho externo. Nenhuma reversão foi executada.

Conferência final: 7 Markdown, 46 links locais sem destinos ausentes, zero ocorrências da matriz real conhecida nos novos documentos/teste e nenhum arquivo privado rastreado. O checkpoint identifica o escopo AACBS testado, não toda a árvore: a consolidação final também detectou mudanças externas em NFeABI/SVRS.xml e NFeABI/Validacao/QRCodeTest.cs, além da versão do projeto e do exemplo eFrete já externos. Foram preservadas, não examinadas como alterações desta entrega e não integram seus resultados de revisão/testes. Não houve novo delta em produção AACBS que exigisse repetir os checks aprovados.
