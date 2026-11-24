# Decisões e premissas AACBS

Registro inicial de 12/11/2026, revisado em 24/11/2026. A aprovação das etapas fica no [PDCA.md](PDCA.md); esta lista não a substitui.

## Decisões explícitas do DEV

| ID | Decisão | Consequência |
| --- | --- | --- |
| D01 | O DEV usa XML; a DLL transforma o que a API necessita e entrega os retornos em XML. | Contratos públicos tipados e XSDs para envios e retornos, incluindo respostas intermediárias e finais. |
| D02 | Deve existir um serviço por endpoint. | Cobrir o catálogo qualificado: atualmente oito operações de negócio, autenticação auxiliar e download externo, conforme a matriz. |
| D03 | A aplicação controla espera e retomada; guarda tíquetes e decide quando consultar ou baixar. | Serviços individuais, sem polling ou persistência de fluxo dentro da DLL. Resposta escolhida na entrevista inicial. |
| D04 | O primeiro plano implementará CBS deste conjunto oficial; IBS fica como expansão dependente de documentação própria. | Não criar classes, endpoints ou contratos IBS por analogia. Resposta escolhida na entrevista inicial. |
| D05 | Somente o DEV aprova cada etapa; aprovar uma libera a execução da próxima. | Entrega técnica para revisão não inicia a sucessora. A aprovação permite iniciar sem nova confirmação ritual. |
| D06 | Etapas abrangentes, testes durante a produção e evidências suficientes, sem burocracia excessiva. | Seis etapas no total, um dossiê por tentativa, matriz de cobertura e testes por responsabilidade. |
| D07 | Agentes podem trabalhar em paralelo, com modelos adequados para economizar sem perder qualidade. | Orquestrador, implementador, autor de testes e executor; revisão independente usa um slot liberado. |
| D08 | O CIOT é a referência, sem a organização de provedores para este domínio. | Aproveitar contrato observável e transporte; regras CBS permanecem próprias. |
| D09 | Devem existir testes reais de consumo, com requisitos documentados para o DEV providenciar acesso. | Prova real separada da offline; na revisão atual, distribuída nas etapas 003/004 e consolidada na 005. |
| D10 | Em 24/11, DEV solicitou revisar o plano diante das atualizações do site e informou que já tem as chaves para testes. | Revisar a AACBS-000 em nova tentativa e preparar acesso local, sem inventar aprovação das etapas. |
| D11 | DEV quer que a equipe teste diretamente sem depender de participação manual contínua. | Preparação única das chaves e autonomia de execução/correção dentro da etapa, ambiente, cenários e limites liberados; preservar os gates humanos existentes. |
| D12 | DEV: "Aprovo a AACBS-000 e libero a execução da AACBS-001." | Aprovação da tentativa-0002 e início documental da AACBS-001 registrados separadamente no PDCA. |
| D13 | DEV informou ter preparado as chaves para Produção Restrita e solicitou conferência. | Verificação local passou sem exibir valores; não se confunde com token, permissão ou funcionamento remoto. |
| D14 | DEV: "Aprovo a AACBS-002 e libero a execução da AACBS-003. Pode seguir conforme o plano." | Etapa 002 aprovada após correções e nova revisão; etapa 003 liberada, sem autorização para 004. |
| D15 | DEV não encontrou endereço PRR e solicitou pesquisa dos ambientes. | Pesquisa oficial da AACBS-003 confirma ambientes distintos e credenciais compartilháveis; não qualificou rota PRR nem autoriza mudar silenciosamente para Beta. |
| D16 | DEV autorizou explicitamente os testes em Produção Beta após a pesquisa dos ambientes. | Atualizar somente metadados locais, preservar credencial DPAPI e retomar autenticação/débitos da AACBS-003; manter limites locais e DARF desabilitado. Aprovações entre etapas continuam vigentes. |
| D17 | DEV aprovou compatibilidade 8/14 somente no ni do retorno de débitos, após constatar divergência real Beta. | Preservar ni integral no XML; para 8 comparar com a base enviada, para 14 comparar seus primeiros 8. Outra raiz/tamanho/tipo permanece inválido; envio e demais famílias continuam com 8. |

## Propostas técnicas desta entrega

Estas decisões integram o plano e os contratos aprovados nas etapas 000/001/002, com o aditivo NI 8/14 expressamente aprovado durante a AACBS-003. Essa etapa foi entregue para revisão humana; restrições de fontes e cenários reais permanecem explícitas.

- **AACBS — Apuração Assistida da CBS** como sigla, `docsplan/aacbs` para planejamento e `AACBS` para diretórios/namespaces do domínio.
- Namespace XML Unimake e versão do XML independentes da versão HTTP; valores definitivos na AACBS-001.
- Autenticação preparada pela DLL, com credenciais na configuração apropriada; serviço auxiliar também especificado. O uso habitual não exige que o DEV conheça JSON ou gerencie tokens.
- Fluxo inicial sem webhook hospedado: `urlRetorno` vazia e consultas explícitas de situação. Hospedar um servidor de webhook exige outro escopo; a existência desse recurso opcional não impede a integração CBS completa.
- XSDs próprios e mappers explícitos para preservar semântica, identificadores, valores, datas, grupos repetidos e erros. Não aplicar códigos ANTT ou cStat às respostas CBS.
- Testes reais incrementais depois da prova offline de cada fluxo nas etapas 003/004; pacote e consolidação na 005, sem repetir solicitações já comprovadas por rotina. Nenhum outro repositório é editado automaticamente.
- Dez serviços no catálogo atual, incluindo dois DARFs síncronos. Geração real de DARF exige cenário habilitado separadamente; não há pagamento de guia/PIX neste plano.
- Preparador local Windows guarda o ClientSecret cifrado por DPAPI em `.local/aacbs/credenciais.local.clixml`; metadados em `configuracao.local.json`. Somente suporte de testes lê esses arquivos. A DLL recebe `Configuracao` em memória do consumidor.

Os contratos da AACBS-001 foram aprovados pelo DEV em 24/11/2026, incluindo expressamente as propostas para datas e retorno global do DARF RAD, conforme o PDCA. A [ESPECIFICACAO.md](contratos/ESPECIFICACAO.md) define namespace Unimake, XML 1.00, catálogo de 24 raízes e dez serviços, campos/tipos/cardinalidades, `Result` concreto para download e erro local sem código remoto falso. A aprovação preserva as restrições PRR e as divergências documentadas das fontes.

## Fatos verificados que limitam as escolhas

As fontes e os links detalhados estão em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md).

- As respostas de abertura, situação, link e arquivo final são contratos distintos. Aceitar uma solicitação não significa obter a apuração final.
- A fonte indicada pelo DEV documenta CBS. Não há contrato IBS nesse inventário.
- As páginas atuais descrevem APIs v2. O manual de plataforma de maio/2026 ajuda a localizar geração de credenciais, mas contém rotas e limites de negócio antigos.
- Em 13/11 foram publicados dois DARFs síncronos e revisão 1.3 das quatro consultas, com seis JSON Schemas de retorno. Mudaram nomes, tipos e cardinalidades; divergências antigas corrigidas permanecem somente no histórico de 12/11.
- A página de download deixou de publicar PRR sem alterar sua versão declarada. As fontes atuais de negócio não permitem preencher automaticamente endpoints de produção restrita.
- O manual primário Receita Integra 1.4, de 17/07/2026, informa validade de uma hora, limite de dois tokens/hora e cache obrigatório. Isso exige reutilização entre execuções dos testes, além do isolamento de identidade/ambiente.
- O portal permite geração de ClientId/ClientSecret. Exigir certificado A1/mTLS na chamada CBS ainda não é uma conclusão sustentada pela documentação examinada.
- O CIOT fornece serviços públicos pequenos e um fluxo reutilizável; particularidades de provedor, códigos e namespace ANTT não pertencem ao novo domínio.
- A base XML já obtém namespace de `XmlRoot`; uma mudança global nessa classe não é pré-requisito presumido.
- O transporte/parser comum materializa JSON e possui comportamentos genéricos que exigem auditoria; sua conversão automática para uma raiz `temp` não é, sozinha, o contrato XML público requerido.
- Builds compartilham saídas de compilação e documentação. Haverá um único executor por checkout.

## Hipóteses e método de validação

| Hipótese/proposta | Como validar | Limite |
| --- | --- | --- |
| O transporte existente atende às operações CBS com mudanças locais. | Executar a trilha completa com executor controlado e avaliar parsing de respostas e download. | AACBS-003; alteração comum exige regressões proporcionais. |
| A autenticação pode ser integrada sem certificado obrigatório. | Obter o manual Receita Integra vigente e qualificar o contrato, depois testar acesso real. | Não codificar requisito de certificado por suposição na AACBS-001. |
| A empresa fornecida terá dados suficientes para testar as famílias. | Verificar acesso com a configuração preparada; distinguir retorno vazio válido de massa real com itens e qualificar cenários DARF. | Antes dos reais de AACBS-003/004. |
| O consumo de memória atual é suficiente para o volume representativo. | Teste sintético de volume com medição e, quando possível, contagens reais sanitizadas. | AACBS-004/005; não prometer escala ilimitada. |

## Divergências e bloqueios conhecidos

O POST OAuth foi qualificado documentalmente pelo manual oficial do mesmo emissor (DeRE 1.0.2), sem chamada real. A especificação resolve as formas de XML de maneira explícita para revisão: variantes de data preservadas, CNPJ DARF com 14 dígitos e retorno RAD global sem inferir cada item. URLs PRR, semântica completa do lote real, limites/idempotência e contrato do PDF seguem como restrições documentadas nas fontes e na especificação comum. Não escolher silenciosamente a interpretação mais conveniente para fazer um teste passar.

O acesso local e a obtenção de token não comprovam funcionamento fiscal. Critério de conclusão será a matriz de cenários demonstrados e limitações aprovadas, sem prometer ausência absoluta de bugs ou controlar a disponibilidade da Receita.

Se uma fonte mudar, registrar o impacto nos contratos já aprovados e levar ao DEV uma alteração concreta. Questões de implementação reversíveis podem ser resolvidas pelo responsável técnico dentro do contrato aprovado; não reabrir decisões do DEV sem evidência nova.
