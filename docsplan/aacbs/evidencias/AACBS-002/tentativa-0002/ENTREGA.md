# AACBS-002 — Correções da revisão do ticket ID #189855

Data: 24/11/2026. Estado: DELIVERED_FOR_REVIEW. Liberação: DEV solicitou analisar os achados e corrigir os coerentes. A tentativa-0001 permanece preservada. AACBS-003 não iniciada.

## Correções confirmadas

| Achado | Correção | Prova |
| --- | --- | --- |
| Validação COM removia espaços | Carga AACBS preserva whitespace antes da validação; erros de formação são sanitizados nesse domínio. | String e arquivo: tíquete composto por espaço aceito, callback composto por espaço recusado. |
| Round-trip modificava CR/CRLF | Escrita com entitização no transporte do DOM para leitura/validação e em GerarXMLString; XMLBase e caminhos dos outros domínios preservados. | Tíquete e mensagem, DOM normal/estrito, string e saída COM preservados. |
| xs:dateTime arredondava frações no limite | InstanteComFuso usa representação lexical; calendário, fuso e projeção continuam no AACBSValidator. | Frações 99999994 e 99999995 no último segundo de 9999 aceitas; calendário/hora impossíveis recusados. |
| maxLength contava UTF-16 | Guarda XSD de 200 unidades e regra efetiva de 100 pontos de código no validator para chave/chaveDFE. | Cinco famílias: 100 ASCII, combinação ASCII/suplementar e 100 suplementares aceitos; 101 pontos recusados. |

A validação completa combina XSD e regras de conteúdo. O limite público das chaves permanece 100; não foram relaxados calendário, fuso, obrigatoriedade ou condições RAD/PCONT. O teste existente de calendário inválido foi transferido da checagem isolada de XSD para o pipeline completo. As matrizes e a descrição dos tipos foram atualizadas para explicar essa divisão.

## Verificação efetiva

SDK/runtime e caminhos seguem o roteiro da tentativa-0001. Builds com DOTNET_CLI_TELEMETRY_OPTOUT=1, sem restore, rede ou credenciais. Runner xUnit v3 executado a partir do diretório bin correspondente.

| Comando/cenário | Resultado | Evidência |
| --- | --- | --- |
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore | Exit 0, 4 avisos, 0 erros | [build-debug.log](build-debug.log) |
| dotnet Unimake.DFe.Test.dll -trait DFe=AACBS — Debug/net10.0 | 239 testes, 0 falhas/erros/ignorados | [testes-debug.log](testes-debug.log) |
| Cinco classes de regressão da tentativa-0001, argumentos -class | 66 testes, 0 falhas/erros/ignorados | [regressao.log](regressao.log) |
| dotnet build source/Unimake.DFe.Test/Unimake.DFe.Test.csproj --no-restore -c INTEROP_Release | Exit 0, 15 avisos, 0 erros | [build-interop.log](build-interop.log) |
| dotnet Unimake.DFe.Test.dll -trait DFe=AACBS — INTEROP_Release/net10.0 | 241 testes, 0 falhas/erros/ignorados | [testes-interop.log](testes-interop.log) |

As classes de regressão foram ValidarSchemaTest, ValidarConfigTest, NormalizacaoXMLTest, NFe.Serializacao.DesserializacaoSeguraTest e DCe.Serializacao.SerializacaoDesserializacaoTest, com namespaces completos do projeto. Os avisos permanecem registrados nos logs; nenhum erro de compilação. O [checkpoint final](CHECKPOINT-FINAL.csv) confirma 91/91 hashes inalterados entre início e término da execução.

## Responsabilidade e escopo

Agentes corrigir_textos e corrigir_limites (modelo herdado) produziram código em arquivos distintos; testar_correcoes (modelo herdado) escreveu testes independentemente. O executor aacbs002_executor usou gpt-6-luna/low, com um único build/runner por vez. O orquestrador inspecionou a integração dos helpers e as alterações de domínio.

Alterações externas em NFeABI, ServicoBase e versão do projeto foram observadas e preservadas. Nenhum commit, publicação, mudança de consumidor, chamada de negócio ou leitura das credenciais foi realizado. As fontes oficiais arquivadas não foram alteradas. Logs finais sanitizados estão permitidos no .gitignore; a pasta privada continua ignorada.

## Gate e retomada

Entrega para revisão humana em 24/11/2026. Aprovação: recebida do DEV em 24/11/2026: "Aprovo a AACBS-002 e libero a execução da AACBS-003. Pode seguir conforme o plano." Retomar pelo PDCA e pelo checkpoint desta tentativa. Reversão, se solicitada, deve retirar somente os hunks deste retrabalho, preservando o ticket anterior e os trabalhos externos. Nenhuma reversão foi executada. A AACBS-003 foi liberada e iniciada em registro separado no PDCA.
