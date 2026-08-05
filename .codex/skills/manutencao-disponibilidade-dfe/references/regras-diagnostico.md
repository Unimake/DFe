# Regras técnicas do diagnóstico de disponibilidade

## Invariantes

- A telemetria é opt-in, passiva, somente em memória e limitada.
- A operação fiscal real é a prova primária; não executar GET/POST genérico antes de SOAP ou REST fiscal.
- Nunca enviar XML sintético como sonda de disponibilidade.
- Uma falha interna da telemetria nunca pode interromper ou atrasar perceptivelmente autorização, consulta, evento ou inutilização.
- `StatusServico` explícito usa cache em memória, intervalo mínimo e bloqueio de consumo indevido.
- O cache vive somente durante o processo e não cria arquivos.

## Classificação fiscal atual

| cStat | Estado da sonda | Interpretação |
|---:|---|---|
| 108 | `Indisponivel` | Indisponibilidade declarada pela autoridade fiscal |
| 109 | `Indisponivel` | Indisponibilidade declarada pela autoridade fiscal |
| 999 | `Indisponivel` | Erro não catalogado tratado como provável instabilidade fiscal |
| 656 | `Degradado` | Consumo indevido; bloquear novas consultas diagnósticas pelo período configurado |
| 678 | `Degradado` | Consumo indevido; bloquear novas consultas diagnósticas pelo período configurado |
| Outro maior que zero | `Operacional` | A aplicação fiscal recebeu e processou a mensagem |
| Zero ou ausente com HTTP 2xx | `Inconclusivo` | Endpoint respondeu, mas não houve status fiscal utilizável |

Ao adicionar um código especial, atualizar uma única política central e testar seus efeitos no classificador, agregador, cache e consulta explícita.

## Precedência de cStat

Usar o primeiro `cStat` em ordem documental.

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

NFe/NFCe, CTe e MDFe usam status principal antes de `protNFe`, `protCTe` ou `protMDFe`. Conferir novamente o XSD quando surgir versão, documento ou provedor novo.

## Serviços essenciais

- Autorização e `StatusServico` são essenciais.
- Protocolo, recibo, inutilização, evento, cadastro, distribuição, consulta de não encerrados, consulta de chaves e download são secundários, salvo nova decisão de domínio explícita.
- Indisponibilidade total exige evidência dos serviços essenciais observados.
- Falha exclusivamente secundária resulta em indisponibilidade parcial ou inconclusiva, conforme as demais evidências.

## Evidências de transporte

- DNS, conexão recusada, TLS, proxy, certificado e configuração indicam ambiente local.
- Timeout isolado permanece inconclusivo sem infraestrutura local comprovadamente saudável; com DNS/TCP/TLS saudáveis, fica degradado e com origem indeterminada.
- Dois timeouts recentes podem indicar indisponibilidade do endpoint somente com infraestrutura saudável.
- HTTP 5xx indica resposta do host e deve ser correlacionado pelo mesmo serviço e endpoint.
- SOAP fault, schema local, autenticação inválida e HTTP 4xx não provam indisponibilidade da SEFAZ.
- O cache de `StatusServico` conserva no máximo duas execuções anteriores compatíveis para permitir essa correlação, sem reduzir o intervalo mínimo nem criar chamadas extras.

## Segurança e desempenho

- Guardar somente endpoint sanitizado, protocolo, duração, HTTP, primeiro `cStat`, classificação e mensagem resumida.
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
