# Entrega AACBS-000 — Revisão das fontes e preparo do acesso local

## Base, autorização e escopo

| Campo | Registro |
| --- | --- |
| Etapa / tentativa | AACBS-000 / tentativa-0002 |
| Data | 24/11/2026, America/Sao_Paulo |
| Base Git | Branch `main`, HEAD `451b90b04a8effe061e655b3eebba2cb96f96fbc` |
| Autorização efetiva | DEV pediu revisar o plano diante das atualizações oficiais e orientar onde colocar as chaves já obtidas para testes diretos. Não houve aprovação de AACBS-000 nem liberação de 001+. |
| Escopo entregue | Revisão documental, regra de ignore e preparador local de credenciais; nenhum código da biblioteca ou serviço fiscal implementado. |
| Estado autoritativo | [PDCA.md](../../../PDCA.md). |

A primeira inspeção já encontrou alteração pendente em `source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj`. Ela foi preservada e não pertence a esta revisão. O [dossiê de 12/11](../tentativa-0001/ENTREGA.md) não foi alterado; suas conclusões são históricas, não prova do catálogo vigente.

## Mudanças demonstradas

| Mudança em relação à tentativa anterior | Documento que a incorpora |
| --- | --- |
| Oito endpoints de negócio e dois auxiliares; novos DARFs síncronos RAD/PCONT de 13/11 | [MATRIZ-APIS.md](../../../MATRIZ-APIS.md) e [PLANO.md](../../../PLANO.md). |
| Quatro consultas na revisão 1.3; seis JSON Schemas Draft 2020-12; nomes, tipos, cardinalidades e identificação corrigidos | [FONTES-E-REQUISITOS.md](../../../FONTES-E-REQUISITOS.md) e matriz. |
| URL PRR do download e instruções de renovação/reuso removidas sem mudança da versão declarada da página | Fontes; nenhuma URL PRR inferida e nenhum fallback para produção. |
| Novas divergências HTML/schema: data do DARF, data/date-time de pagamentos RAD, cardinalidade do retorno RAD e exemplo CNPJ | Lacunas L09–L13; qualificação exigida antes dos modelos/XSDs. |
| Manual primário Receita Integra 1.4, 17/07: POST token, validade de uma hora, dois tokens/hora e cache obrigatório | Fontes F20, arquitetura, plano e agentes. Forma completa de body/Basic permanece a qualificar. |
| Testes reais incrementais na 003/004; consolidação na 005; autonomia dentro da etapa e cenário liberados | Plano, decisões, PDCA e agentes. Não confundir posse das chaves com aprovação humana. |
| Runner atualizado para net10.0 e riscos concretos de trace/exceções incluídos nos critérios de teste | [ARQUITETURA-E-TESTES.md](../../../ARQUITETURA-E-TESTES.md). |
| Preparo único das credenciais com ClientSecret cifrado por DPAPI, arquivo de metadados e ignore de toda a pasta operacional | [ACESSO-LOCAL.md](../../../ACESSO-LOCAL.md), preparador PowerShell e `.gitignore`. |

As páginas e schemas foram consultados diretamente em fontes oficiais. A tentativa de ler o repositório público de arquivos da Receita pelo shell falhou por resolução de host; foi localizada a fonte primária alternativa do manual no CGIBS, sem depender de cópia de terceiros. Não presumir que a disponibilidade de documentação comprova conectividade ao endpoint de negócio.

## Arquivos e agentes

- Raiz: integração, README, PLANO, DECISOES, ACESSO-LOCAL, script de preparo, `.gitignore` e dossiê desta tentativa; fechamento do PDCA.
- `investigar_api_rfb`: pesquisa oficial, comparação e atualização de FONTES-E-REQUISITOS e MATRIZ-APIS.
- `mapear_ciot`: conferência da infraestrutura atual, atualização de ARQUITETURA-E-TESTES e revisão do preparador local.
- `planejar_orquestracao`: revisão de gates/autonomia, atualização de AGENTES/PDCA e revisão cruzada final.

Os agentes usaram o modelo/esforço herdados da sessão, sem override. Os perfis disponíveis para as próximas etapas foram reconferidos e documentados; nenhum executor de teste fiscal foi iniciado. A raiz integrou correções posteriores de caminhos privados/cache nos documentos.

## Verificações efetivamente executadas

| Verificação | Resultado e limite |
| --- | --- |
| Inspeção Git e leitura dos documentos/projeto de testes | Base identificada, alteração externa preservada; target `net10.0` confirmado. |
| Parser AST PowerShell do preparador | Zero erros de sintaxe. O modo interativo com chaves reais ainda não foi executado. |
| Round-trip sintético de PSCredential pela serialização PowerShell | Assinatura DPAPI presente, segredo sentinela ausente do texto serializado e leitura reconstituída correta. Dados sintéticos em memória; nenhuma chave real usada. |
| Bloco real de `-Verificar` extraído da AST, com arquivos temporários sintéticos | Nove cenários aprovados no PowerShell 7 e os mesmos nove no Windows PowerShell: cotas válidas/fora da faixa/string/fracionária/null e guarda de produção. Credencial DPAPI sintética; zero requests. Arquivos temporários removidos. A primeira extração AST do roteiro de verificação não localizou o bloco; o extrator foi corrigido e os cenários passaram. |
| `git check-ignore -v -- .local/aacbs/credenciais.local.json` | Regra `/.local/aacbs/` efetiva. Caminho JSON inicial foi substituído pelo segredo DPAPI e metadados separados; verificação dos destinos finais na conferência de entrega. |
| Tentativa de restringir ACL do diretório local | Comandos retornaram sucesso, mas a releitura não comprovou restrição. A solução não alega ACL aplicada: o ClientSecret será protegido por DPAPI, e ClientId/metadados continuam locais e não cifrados. |
| `git diff --check` | Sem erros; avisos de conversão LF/CRLF não são falhas de conteúdo. |
| Links locais e whitespace, incluindo arquivos novos | 12 Markdown, 45 links locais, zero links quebrados e zero linhas com whitespace final; exit code 0. |
| Destinos privados finais e histórico | Ignore confirmado para `credenciais.local.clixml`, `configuracao.local.json` e `execucoes/estado.json`; nenhum arquivo privado rastreado. Diff do dossiê 0001 vazio. |

Nenhuma chave real foi lida ou armazenada pela equipe. Não houve autenticação, consumo de API de negócio, geração de DARF, teste fiscal, build, INTEROP ou empacotamento. O script entregue não executa nenhuma dessas ações.

## Revisão cruzada e correções

`mapear_ciot` identificou que `ConvertFrom-Json` pode retornar `Int64`; o verificador inicialmente exigia apenas `Int32` e rejeitaria sua própria configuração no PowerShell 7. A raiz passou a aceitar inteiros Int32/Int64 na faixa prevista, mantendo a rejeição de strings e frações. Os nove cenários acima verificaram o bloco corrigido nas duas versões de PowerShell.

`planejar_orquestracao` encontrou duas contradições documentais: uma linha ainda pedia confirmação do DEV antes de cada teste real, e outra atribuía correções de código ao executor econômico. A raiz corrigiu para confirmação inicial do contexto e correções pelo implementador/autor dos testes, com reexecução pelo executor em checkpoint estável. As demais recomendações de fontes e proteção foram integradas. Nenhum parecer de agente equivale à aprovação humana.

## Limitações e retomada

O DEV executará uma vez o preparador no mesmo usuário/computador Windows, informando CNPJ, ambiente e ClientId/ClientSecret em prompt. Os caminhos serão `.local/aacbs/credenciais.local.clixml` e `.local/aacbs/configuracao.local.json`, ignorados pelo Git. O segredo será recuperável nesse contexto sem pedir sua digitação a cada teste.

Configuração e leitura local não comprovam autorização remota. Falta qualificar o envio completo do token, endpoints/ambiente e os conflitos do contrato na AACBS-001. O leitor/harness dos testes reais é entrega futura da AACBS-003. Os casos reais de DARF dependem de habilitação e dados específicos; nenhuma ação bancária está incluída.

Credencial inválida, cota esgotada, resposta ambígua ou falha externa não serão contornadas repetindo POST ou declarando sucesso. O critério de término é a matriz de contratos/cenários comprovados e limitações aprovadas. Não há promessa de ausência absoluta de bugs.

Rollback desta tentativa deve considerar apenas seu diff e os novos documentos/script/regra de ignore. Não apagar arquivos privados com chaves ou tíquetes eventualmente preenchidos depois pelo DEV, nem retirar sua proteção por rotina; preservar o dossiê inicial e qualquer alteração externa.

## Gate humano

- Revisão da AACBS-000 entregue novamente ao DEV; consultar o PDCA para o estado.
- Aprovação humana desta tentativa: **recebida em 24/11/2026** — DEV: "Aprovo a AACBS-000 e libero a execução da AACBS-001."
- Registro posterior à entrega: aprovação e início da AACBS-001 constam como transições distintas no PDCA; AACBS-002 a AACBS-005 não iniciadas.

O pedido de revisão autorizou este retrabalho e o preparo de acesso. A aprovação da AACBS-000 continuará sendo necessária para iniciar a etapa seguinte, como definido pelo DEV.
