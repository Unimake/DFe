# Regras técnicas do diagnóstico de disponibilidade

Contrato/APIs, fontes e classes de teste: [instrução específica](../../../../.agents/instructions/disponibilidade-dfe.instructions.md). Esta referência detalha semântica e proteções; as matrizes de cobertura vinculadas na instrução devem ser confirmadas no código atual.

## Invariantes

- A telemetria é opt-in, passiva, somente em memória e limitada.
- A operação remota real é a evidência primária, fiscal ou de outra integração; não executar GET/POST genérico antes do serviço de negócio para testar acesso.
- Nunca enviar XML sintético como sonda de disponibilidade.
- Uma falha interna da telemetria nunca pode interromper ou atrasar perceptivelmente autorização, consulta, evento ou inutilização.
- `StatusServico` explícito usa cache em memória, intervalo mínimo e bloqueio de consumo indevido.
- O cache vive somente durante o processo e não cria arquivos.
- Sessões são o caminho recomendado. Sua coleta não depende da allowlist do histórico anterior nem da existência de status oficial. Preparação local não deve ser apresentada como transporte observado.
- `VerificarStatusServico` separa sondas manuais do histórico passivo. Não confundir seu cache/histórico limitado de sondas com persistência de operações do PRO.
- A sessão acompanha uma tentativa lógica, incluindo autenticações/downloads quando existirem. `EntradasTransporte` não revela requests ocultos, redirecionamentos ou todo tráfego de componentes externos.

## Contratos que não usam cStat

- Selecionar a família/serviço a partir da configuração efetiva e conferir raiz, namespace, estrutura e código. Não interpretar um `cStat` incidental dentro de outro contrato.
- eSocial/Reinf têm status próprios (`cdResposta`/`cdRetorno`) e hierarquias de lote/evento; preservar a precedência principal e as regras específicas de cada família.
- GNRE tem códigos de negócio que podem coincidir com HTTP (por exemplo 401/503). Conferir recepção, resultado de lote e configuração de UF separadamente; erros antigos de guias não representam a situação atual do portal/UF.
- NFS-e municipal depende de padrão, serviço, namespace e marcador. NFS-e nacional tem XML e envelopes JSON normalizados com contratos distintos para DPS, eventos e ADN. Não exigir cStat, aceitar qualquer `temp`/Codigo, inferir faixas ou tratar todo erro desconhecido como rejeição normal.
- CIOT é interpretado após normalização do provedor. Codigo/Sucesso/Temp devem ser coerentes; erro ou código desconhecido não vira operacional apenas por HTTP200. GeradorCIOTShared continua opaco quanto a HTTP interno.
- PDF recebido oferece somente evidência de envelope: tipo/serviço coerentes, cabeçalho/término e Base64 quando aplicável. Não consumir/alterar/guardar stream, invalidar Base64 válido com quebras de linha nem confirmar entrega por Sucesso sem arquivo. Não inferir validade integral ou assinatura.
- Uma resposta reconhecida pode estar degradada, indicar certificado/autenticação ou permanecer inconclusiva. Ler `RespostaAplicacaoReconhecida` junto de `Status`, `TipoFalha`, `ContratoResposta`, `CodigoRespostaAplicacao` e `Limitacao`.

## Classificação nas famílias que usam cStat

Aplicar somente às famílias aceitas por `ReconhecedorRespostaDisponibilidade.UsaCStat`. Rejeição normal comprova processamento do serviço, não autorização. HTTP de falha e SOAP Fault continuam sendo avaliados; não promover falha HTTP objetiva a sucesso por um código interno.

| cStat | Estado da sonda | Interpretação |
|---:|---|---|
| 108 | `Indisponivel` | Indisponibilidade declarada pela autoridade fiscal |
| 109 | `Indisponivel` | Indisponibilidade declarada pela autoridade fiscal |
| 999 | `Indisponivel` | Erro não catalogado tratado como provável instabilidade fiscal |
| 656 | `Degradado` | Consumo indevido; bloquear novas consultas diagnósticas pelo período configurado |
| 678 | `Degradado` | Consumo indevido; bloquear novas consultas diagnósticas pelo período configurado |
| Outro maior que zero | `Operacional` | A aplicação fiscal recebeu e processou a mensagem |
| Zero ou ausente com HTTP 2xx | `Inconclusivo` | Sem outro contrato/marcador reconhecido, HTTP isolado não comprova processamento |

Ao adicionar um código especial, atualizar uma única política central e testar seus efeitos no classificador, agregador, cache e consulta explícita.

## Precedência de cStat

Nas famílias cStat, usar o primeiro em ordem documental. Não escolher o maior, o último ou o código de autorização interno para esconder o retorno principal.

Retornos de autorização e consulta podem conter:

```xml
<retCTe>
  <cStat>104</cStat>
  <protCTe>
    <infProt>
      <cStat>100</cStat>
    </infProt>
  </protCTe>
</retCTe>
```

Nesse caso, usar `104`. O primeiro código descreve o serviço/lote; o segundo descreve o documento.

Quando não houver código no nível principal, usar o primeiro interno:

| Retorno | Caminho aplicável |
|---|---|
| Inutilização NFe/NFCe | `retInutNFe/infInut/cStat` |
| Cadastro NFe/NFCe/CTe | `retConsCad/infCons/cStat` |
| Evento CTe | `retEventoCTe/infEvento/cStat` |
| Evento MDFe | `retEventoMDFe/infEvento/cStat` |

Nas famílias cStat, o status principal prevalece sobre protocolos internos do documento, como `protNFe`, `protCTe`, `protMDFe` e `protNF3e`. Conferir novamente o XSD e a seleção de família quando surgir versão, documento ou provedor novo; presença de enum não basta para habilitar essa interpretação.

## Serviços essenciais

- Autorização e `StatusServico` são essenciais.
- Protocolo, recibo, inutilização, evento, cadastro, distribuição, consulta de não encerrados, consulta de chaves e download são secundários, salvo nova decisão de domínio explícita.
- Indisponibilidade total exige evidência dos serviços essenciais observados.
- Falha exclusivamente secundária resulta em indisponibilidade parcial ou inconclusiva, conforme as demais evidências.

## Evidências de transporte

- DNS, TLS, proxy, certificado e configuração não devem ser atribuídos automaticamente ao órgão remoto. Conexão recusada tem categoria própria e requer correlação; não inventar causa por mensagem localizada.
- HTTP401/403 indicam autenticação/autorização de acesso; 407 é proxy e 429 consumo indevido. Reinf495/496 tem interpretação de certificado própria do contrato; não a generalizar para outras integrações.
- SOAP/WebException e HttpClient podem encapsular causas de formas diferentes. Manter timeout REST com TimeoutException interna; cancelamento puro sem prova de timeout continua ambíguo em runtimes antigos. Não analisar texto localizado para inventar a categoria.
- Timeout isolado permanece inconclusivo sem infraestrutura local comprovadamente saudável; com DNS/TCP/TLS saudáveis, fica degradado e com origem indeterminada.
- Dois timeouts fiscais recentes podem indicar indisponibilidade quando a infraestrutura estiver saudável ou quando DNS funcionar e TCP/serviço expirarem repetidamente no mesmo endpoint, sem DNS, TLS, proxy, certificado ou configuração local inválida.
- Conexão recusada deve ser identificada pela `SocketException`, sem analisar a mensagem localizada; uma ocorrência fica degradada e duas falhas recentes de timeout/recusa no mesmo endpoint podem confirmar indisponibilidade.
- HTTP 503 declara que o endpoint está temporariamente indisponível e confirma imediatamente a indisponibilidade do serviço observado. Os demais HTTP 5xx indicam resposta do host e devem ser correlacionados pelo mesmo serviço e endpoint.
- SOAP fault, schema local, autenticação inválida e HTTP 4xx não provam indisponibilidade da SEFAZ.
- O cache de `StatusServico` conserva no máximo duas execuções anteriores compatíveis para permitir essa correlação, sem reduzir o intervalo mínimo nem criar chamadas extras.

## Consumo indevido e transição do histórico

- As sessões registram supressão por consumo indevido classificado mesmo se a fila estiver cheia; o bloqueio não depende do histórico anterior. A API manual nova verifica o bloqueio antes de preparar certificado/infraestrutura/status.
- O registro interno aceita tipos com estratégia oficial e ambientes válidos, evitando chaves ilimitadas. Integrações sem status ainda entregam observações/classificação; não ganham sondas fictícias. Preservar expiração sem limpeza forçada e os limites nacionalizados já implementados.
- Sessão ativa da mesma instância de Configuracao evita armazenamento/classificação duplicados na API histórica. Não suprimir outra configuração no mesmo fluxo.
- APIs antigas, incluindo o booleano de reconhecimento do consumidor, permanecem obsoletas somente para transição. Não remover membros públicos/COM silenciosamente nem utilizar o histórico antigo para medir cobertura.

## Segurança e desempenho

- A fotografia contém somente escalares sanitizados: identidade/contexto, destino, protocolo/etapa/duração, HTTP, cStat ou código/contrato, reconhecimento, classificação, entradas de transporte, integração e limitação. Nenhum XML, corpo remoto, referência a configuração, credencial ou exceção.
- Endpoint público contém somente esquema/host/porta e Excecao usa texto controlado; não reintroduzir caminho sensível ou mensagem remota por uma regex permissiva. Cache/agrupamento usam identidade derivada interna do endpoint completo para distinguir serviços no mesmo host.
- Não guardar `xMotivo` bruto quando puder conter identificadores ou dados fiscais; usar mensagem controlada ou sanitizada.
- Evitar carregar certificado apenas para formar chave de telemetria.
- Evitar locks bloqueantes no caminho fiscal; se o histórico estiver ocupado, descartar a amostra.
- Reutilizar `Stopwatch.GetTimestamp()` no caminho crítico e evitar alocações ou desserialização desnecessárias.
- Separar cache por todas as entradas que mudam o resultado, incluindo timeout e identidade irreversível de proxy/certificado quando aplicável.

## Cobertura mínima de testes

- `107`, `108`, `109`, `999`, `656`, `678` e outro código positivo.
- Primeiro `cStat` prevalecendo sobre protocolo interno.
- `cStat` em `infInut`, `infCons` e `infEvento`.
- DNS, conexão, timeout, TLS, proxy, HTTP 4xx/5xx e certificado/configuração.
- Lentidão, repetição, cache, bloqueio nacional/estadual e expiração.
- Agregação operacional, degradada, parcial, indisponível, inconclusiva e não aplicável.
- Sanitização de URL, credenciais, tokens, caminhos e identificadores fiscais.
- Ausência de transporte extra e tolerância a falhas internas da telemetria.
- Contratos por serviço/família: positivo, rejeição normal, indisponibilidade explícita, desconhecido/malformado, namespace/serviço errados, autenticação e HTTP200 sem confirmação. Exercitar serviço/consumidor/executor real com transporte substituído, preservando retorno/Result/exceção.
- Sessão versus histórico duplicado, verificação manual sem histórico passivo, cache/intervalo, fila cheia com consumo indevido, supressão antes de certificado/rede, expiração e limites de chaves. Restaurar relógio/cache na coleção compartilhada de testes.
- No PDF, preservar posição e bytes e reconhecer truncamento/HTML/ausência do arquivo. Não usar certificado real, token A3 nem rede externa nos testes determinísticos.
