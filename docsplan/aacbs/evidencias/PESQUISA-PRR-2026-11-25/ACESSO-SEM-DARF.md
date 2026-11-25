# Confirmação de acesso PRR sem emissão de DARF

Pesquisa complementar autorizada pelo DEV em 25/11/2026, após os commits da AACBS-005. Não altera status/aprovações do PDCA nem libera emissão de DARF. O pedido é localizar URLs atuais e determinar como confirmar as chaves existentes sem emissão.

## Verificação local realizada

[Resultado sanitizado](VERIFICACAO-LOCAL.json): credencial DPAPI legível e metadados válidos; ambiente mantido em Produção Beta e DARF desabilitado. Sob o mesmo mutex do harness, um token existente foi examinado somente em memória. Está expirado e é opaco/não JWT; não permite inferir permissões por decodificação. Nenhum token emitido, API fiscal chamada, abertura repetida, journal alterado ou chave rotacionada. Não se atribui expiração do token à invalidade da credencial.

## O que cada prova demonstra

| Prova | O que demonstra | O que não demonstra |
|---|---|---|
| Importar a credencial DPAPI | O usuário/máquina atual consegue usar o arquivo protegido | Chave ainda válida remotamente ou autorização PRR |
| POST no emissor oficial /token com sucesso | Credencial aceita pelo emissor naquele momento | Permissão para todos os serviços ou roteamento PRR |
| Ler claims de token, se JWT | Conteúdo declarado pelo emissor, após validação adequada | Token opaco não pode ser tratado como JWT; scope isolado não garante operação PRR |
| GET de situação/link com tíquete PRR válido do mesmo contribuinte/credencial | Permissão e funcionamento da operação GET nesse contexto, conforme contrato reconhecido | Abertura de débitos/créditos autorizada, ou cobertura de outras operações |
| Uma abertura PRR válida, 201 e tíquete confirmado | Acesso de abertura naquela família e contexto | Ciclo completo antes da situação, link e arquivo validados |
| GET/HEAD/OPTIONS sem autenticação, rota especulativa, ou 401/404 genérico do gateway | Somente a resposta HTTP daquela tentativa | URL oficial, ambiente correto ou permissão de negócio |

## Roteiro mínimo após qualificar as rotas

1. Confirmar em contrato oficial atual as URLs de abertura de débitos/créditos, situação e obtenção do link PRR, o emissor OAuth e suas autorizações. Registrar URL de origem, data e versão. Uma rota encontrada somente em espelho antigo continua sendo pista; não criar a rota de download por analogia.
2. Configurar PRR em contexto próprio, sem fallback para Beta e sem reutilizar tíquetes de Beta. Preservar as contagens atuais e separar o fluxo de negócio por ambiente/família/contribuinte. O limite de emissão de tokens é compartilhado pela identidade do emissor/credencial; trocar ambiente não zera esse limite.
3. Preferir token válido no cache do contexto aplicável. Na conferência local atual não havia nenhum. Se necessário, emitir no máximo um token novo pelo emissor oficial usando a credencial protegida, sob mutex e com tentativa registrada antes do POST; não imprimir token/segredo nem colocar valores em argumentos. Não chamar /token por serviço. Respeitar máximo oficial de dois tokens por hora e tratar rejeição/429 sem repetição automática.
4. Se já houver tíquete PRR conhecido e válido, consultá-lo antes de abrir qualquer solicitação. Não existe tíquete PRR qualificado no histórico deste trabalho; os ciclos conhecidos de débitos/créditos foram Beta. Não usar identificador fictício ou tíquete de outro ambiente para alegar sucesso.
5. Sem tíquete PRR existente, a confirmação funcional exige uma abertura real de consulta no ambiente. Definir orçamento inicial de uma abertura de débitos e, após validá-la, uma de créditos, sem DARF; consultar somente a matriz configurada. Antes de enviar, registrar intenção, família, ambiente, origem da URL e orçamento no journal protegido. Corpo sem webhook: urlRetorno vazio.
6. Aceitar abertura somente com HTTP 201, contrato compatível e tíquete não vazio; persistir antes do acompanhamento. Seguir situação com intervalos/limites do roteiro, obter link por chamada separada e baixar somente a urlAssinada devolvida, sem Bearer. Comparar retorno e XML/XSD/Result. Na execução mínima, um GET de situação inicialmente; se pendente, retomar o tíquete pelo roteiro sem reabrir.
7. Parar em 401, 403, 404 incompatível, 429, timeout de POST ou contrato inesperado. Tratar abertura sem confirmação como ambígua e não repetir. Resposta 403 em URL ainda não qualificada não distingue URL incorreta de permissão ausente; não recomendar rotação ou ampliação de poderes com base nisso.
8. Publicar apenas status HTTP, fase, contagens e resultado de validação sanitizados; manter XMLs, tíquetes, links, tokens e journal sob DPAPI na pasta local ignorada. Não chamar RAD/PCONT, criar pagamentos, acionar PIX ou baixar PDF.

## Ajustes necessários no suporte atual

O serviço AACBS e o harness bloqueiam PRR intencionalmente enquanto a rota não está qualificada. Configuracao customizada não substitui as rotas embutidas. Habilitação futura exige atualizar configuração nacional, seleção de ambiente e testes offline de isolamento/URLs antes do real; não alterar somente o JSON local nem remover a proteção para descobrir uma rota. A pesquisa não fez essas mudanças.

O [manual Receita Integra 1.4](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf) publica /token, Client Credentials, validade de uma hora e dois tokens/hora; a [notícia de credenciais](https://www.gov.br/receitafederal/pt-br/assuntos/noticias/2026/agosto/credenciais-de-acesso-as-api-da-reforma-tributaria-serao-atualizadas) permite a mesma credencial entre serviços, respeitadas autorizações. As [orientações do Piloto](https://www.gov.br/receitafederal/pt-br/acesso-a-informacao/acoes-e-programas/reforma-tributaria-do-consumo/piloto) distinguem procuração para o Piloto e poder para gerar credenciais. Esses documentos não tornam uma rota PRR inferida oficialmente publicada.
