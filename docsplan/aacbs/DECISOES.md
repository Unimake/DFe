# Decisões e premissas AACBS

Registro de 12/11/2026. A aprovação das etapas fica no [PDCA.md](PDCA.md); esta lista não a substitui.

## Decisões explícitas do DEV

| ID | Decisão | Consequência |
| --- | --- | --- |
| D01 | O DEV usa XML; a DLL transforma o que a API necessita e entrega os retornos em XML. | Contratos públicos tipados e XSDs para envios e retornos, incluindo respostas intermediárias e finais. |
| D02 | Deve existir um serviço por endpoint. | Cobrir as seis operações de negócio, autenticação auxiliar e download externo, conforme a matriz. |
| D03 | A aplicação controla espera e retomada; guarda tíquetes e decide quando consultar ou baixar. | Serviços individuais, sem polling ou persistência de fluxo dentro da DLL. Resposta escolhida na entrevista inicial. |
| D04 | O primeiro plano implementará CBS deste conjunto oficial; IBS fica como expansão dependente de documentação própria. | Não criar classes, endpoints ou contratos IBS por analogia. Resposta escolhida na entrevista inicial. |
| D05 | Somente o DEV aprova cada etapa; aprovar uma libera a execução da próxima. | Entrega técnica para revisão não inicia a sucessora. A aprovação permite iniciar sem nova confirmação ritual. |
| D06 | Etapas abrangentes, testes durante a produção e evidências suficientes, sem burocracia excessiva. | Seis etapas no total, um dossiê por tentativa, matriz de cobertura e testes por responsabilidade. |
| D07 | Agentes podem trabalhar em paralelo, com modelos adequados para economizar sem perder qualidade. | Orquestrador, implementador, autor de testes e executor; revisão independente usa um slot liberado. |
| D08 | O CIOT é a referência, sem a organização de provedores para este domínio. | Aproveitar contrato observável e transporte; regras CBS permanecem próprias. |
| D09 | Devem existir testes reais de consumo, com requisitos documentados para o DEV providenciar acesso. | Validação real em etapa própria, separada da prova offline e sem credenciais versionadas. |

## Propostas técnicas desta entrega

Estas propostas integram o plano para revisão, sem alegar aprovação humana já recebida.

- **AACBS — Apuração Assistida da CBS** como sigla, `docsplan/aacbs` para planejamento e `AACBS` para diretórios/namespaces do domínio.
- Namespace XML Unimake e versão do XML independentes da versão HTTP; valores definitivos na AACBS-001.
- Autenticação preparada pela DLL, com credenciais na configuração apropriada; serviço auxiliar também especificado. O uso habitual não exige que o DEV conheça JSON ou gerencie tokens.
- Fluxo inicial sem webhook hospedado: `urlRetorno` vazia e consultas explícitas de situação. Hospedar um servidor de webhook exige outro escopo; a existência desse recurso opcional não impede a integração CBS completa.
- XSDs próprios e mappers explícitos para preservar semântica, identificadores, valores, datas, grupos repetidos e erros. Não aplicar códigos ANTT ou cStat às respostas CBS.
- Testes reais, pacote e documentação final depois da cobertura offline de todas as famílias. Nenhum outro repositório é editado por consequência automática deste plano.

## Fatos verificados que limitam as escolhas

As fontes e os links detalhados estão em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md).

- As respostas de abertura, situação, link e arquivo final são contratos distintos. Aceitar uma solicitação não significa obter a apuração final.
- A fonte indicada pelo DEV documenta CBS. Não há contrato IBS nesse inventário.
- As páginas atuais descrevem APIs v2. O manual de plataforma de maio/2026 ajuda a localizar geração de credenciais, mas contém rotas e limites de negócio antigos.
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
| A empresa fornecida terá dados suficientes para testar as famílias. | DEV verifica acesso e disponibilidade; distinguir retorno vazio válido de massa real com itens. | Antes da AACBS-005. |
| O consumo de memória atual é suficiente para o volume representativo. | Teste sintético de volume com medição e, quando possível, contagens reais sanitizadas. | AACBS-004/005; não prometer escala ilimitada. |

## Divergências e bloqueios conhecidos

O detalhe fica nos documentos de fontes e matriz, com resolução exigida na AACBS-001: manual OAuth não qualificado; URLs de produção restrita incompletas; códigos e cardinalidades inconsistentes nos exemplos; identificação do consumidor; reuso/renovação de link de download. Não escolher silenciosamente a interpretação mais conveniente para fazer um teste passar.

Se uma fonte mudar, registrar o impacto nos contratos já aprovados e levar ao DEV uma alteração concreta. Questões de implementação reversíveis podem ser resolvidas pelo responsável técnico dentro do contrato aprovado; não reabrir decisões do DEV sem evidência nova.
