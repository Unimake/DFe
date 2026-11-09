# Validação e autorrevisão — 09/11/2026

## Migração para sessões — estado atual

Decisão do usuário: a rotina será removida do UniNFe legado; o PRO será o consumidor do diagnóstico. Implementação e instruções: [MIGRACAO-SESSOES.md](MIGRACAO-SESSOES.md) e [COMANDO-PRO.md](COMANDO-PRO.md). O PRO continua somente consultado nesta tarefa.

**416 testes offline aprovados**, zero erros/falhas/ignorados/não executados, dez classes. Runner xUnit v3/net8, duração final 1,813 s. Inclui os 401 casos anteriores e **15 regressões de migração**. Nenhuma regra fiscal, de certificado ou teste foi desabilitada.

Build final dos testes: 71 avisos, zero erros (30,32 s). Além dos avisos preexistentes, CS0618 registra usos intencionais das APIs obsoletas nos testes de compatibilidade que foram preservados. As assinaturas antigas continuam funcionais; o contrato recomendado não depende delas. Build principal Debug aprovado, 1 aviso NU1900 em execução incremental, zero erros (1,30 s). A validação INTEROP e o DryRun finais estão registrados no complemento ao fim deste arquivo.

Comando do runner nesta etapa (acrescenta a classe de migração e mantém as anteriores):

```powershell
dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" `
  -class Unimake.DFe.Test.Utility.Rede.MigracaoDiagnosticoSessaoTest `
  -class Unimake.DFe.Test.Utility.Rede.InterpretacaoContratosDisponibilidadeTest `
  -class Unimake.DFe.Test.Utility.Rede.CoberturaDisponibilidadeTest `
  -class Unimake.DFe.Test.Utility.Rede.CanalObservacoesDisponibilidadeTest `
  -class Unimake.DFe.Test.Utility.Rede.DiagnosticoDisponibilidadeTest `
  -class Unimake.DFe.Test.CIOT.Servicos.EFreteFluxoExecucaoTest `
  -class Unimake.DFe.Test.Utility.Rede.ProxyConfiguracaoTest `
  -class Unimake.DFe.Test.NFCe.Utilitarios.CpfCnpjComBrLookupTest `
  -class Unimake.DFe.Test.Utility.Xml.SoapResponseParserTest `
  -class Unimake.DFe.Test.UMessenger.Transport.ApiTimeoutTest `
  -trait- "Utility=DisponibilidadeIntegracao" -noLogo
```

Autorrevisão da migração:

- Sessão da mesma configuração suprime coleta paralela no histórico anterior. Serviço público NFe.StatusServico atravessa executor SOAP substituído e mantém Result/XML/exceções e uma única chamada/observação. Outra configuração e chamadas sem sessão continuam compatíveis durante a transição.
- VerificarStatusServico reutiliza o mesmo núcleo fiscal/infraestrutura/cache, sem ler ou apagar histórico passivo anterior. A regressão mantém uma evidência passiva 108 e recebe status manual 107 operacional sem misturar as fontes; a segunda chamada reutiliza cache.
- O bloqueio de consumo indevido não pode desaparecer com a retirada do histórico: sessões o registram diretamente, inclusive com fila cheia. 656, 678 e HTTP429 suprimem status antes de certificado/infraestrutura, sem request extra; o bloqueio expira sem limpeza forçada.
- O registro interno de supressão não deve crescer com famílias amplas ou enums inválidos: só aceita tipos com status oficial e ambientes produção/homologação (máximo de 20 combinações na lista atual). Os demais continuam entregando a classificação de consumo indevido, sem inventar status.
- APIs e membros antigos recebem Obsolete sem remover assinaturas. O novo overload de Concluir delega a interpretação à DLL; o overload com booleano permanece compatível. A fachada COM mantém os membros anteriores e recebe o novo método ao fim da declaração.
- Testes que compartilham cache/relógio foram colocados na mesma coleção. Não foi necessária alteração fiscal, schema, endpoint ou dependência.
- O pacote do PRO foi reconferido: 20261108.1207.21, ProductVersion 20261108.1207.21+e046f702936f83b5c77efaca1068e9300c9dbb94. O roteiro exige base fiscal compatível para empacotar os patches de diagnóstico; não promete equivalência ao HEAD. Não houve atualização nem testes do PRO com pacote anterior apresentados como validação desta API.

Limites: APIs obsoletas permanecem temporariamente até a migração do PRO; não houve retirada binária antecipada, execução .NET Framework 4.7.2, rede/certificado real/A3 ou pacote permanente. O consumidor ainda deve implementar a migração de DTO/persistência/agregação/UI descrita no comando. Os registros das etapas anteriores abaixo são históricos; os 416 casos são o total atual.

### Complemento final dos builds e do pacote da migração

`INTEROP_Release` aprovado: 12 avisos preexistentes, zero erros, 25,40 s. DryRun oficial final aprovado (exit code 0): versão temporária `20261109.1436.28`, AssemblyVersion `2026.11.09.1436`. Validou assets somente `lib/netstandard2.0`, dependências e hash de GeradorCIOTShared. Nenhum pacote foi enviado ou persistido; csproj restaurado byte a byte (SHA256 `EC4AD764F5BB1C5935BE1B374D84DB0989A1B987ADC4DDA25A10301BBBF5D22F`). Os hashes centrais foram regenerados e conferidos, sem divergências. `git diff --check` sem erro de whitespace; apenas avisos usuais de LF/CRLF. Nenhum commit/push.

## Registro da revisão do item 3 — anterior à migração de API

**401 testes offline aprovados**, zero erros, falhas, ignorados ou não executados. São nove classes, incluindo **101 casos novos** em `InterpretacaoContratosDisponibilidadeTest`. Runner xUnit v3/net8, duração final 1,247 s. Os 300 casos anteriores foram mantidos na execução, com expectativas corrigidas onde antes se aceitava código CIOT desconhecido ou cStat fora da família de contrato.

Build principal Debug: 5 avisos, zero erros, 5,21 s. Build `INTEROP_Release`: 12 avisos, zero erros, 25,06 s. Build final dos testes: 25 avisos, zero erros, 25,28 s; reconstruções anteriores reportaram 29 devido aos avisos do projeto principal recompilado. Permanecem NU1900 do feed privado, CS0618 e avisos xUnit preexistentes; nenhuma validação ou regra de certificado foi enfraquecida.

Os comandos de build, runner e DryRun continuam os registrados abaixo. Acrescentar ao runner o filtro:

```powershell
-class Unimake.DFe.Test.Utility.Rede.InterpretacaoContratosDisponibilidadeTest
```

O DryRun oficial foi repetido após a última alteração de interpretação. **Aprovado**, exit code 0, pacote temporário `20261109.1412.10`, AssemblyVersion `2026.11.09.1412`. Validados conteúdo, dependências, assets somente netstandard2.0 e hash de GeradorCIOTShared. Sem pacote persistente, publicação, atualização do PRO ou versão rastreada alterada. Hash do csproj antes/depois: `EC4AD764F5BB1C5935BE1B374D84DB0989A1B987ADC4DDA25A10301BBBF5D22F`.

Matriz semântica e fontes: [INTERPRETACAO.md](INTERPRETACAO.md). Catálogo municipal: 439 combinações distintas, 516 declarações XSD rastreadas em [CONTRATOS-NFSE.csv](CONTRATOS-NFSE.csv). Catálogo nacional: 441 IDs, extração reproduzida pelo script com hash obrigatório do XLSX oficial. Inventário continua com 151 classes e 3.191 declarações/configurações. Nenhuma mudança em schema, modelo fiscal, endpoint ou regra de autorização foi feita nesta revisão.

### Autorrevisão adicional

| Bloco | Defeito prevenido/corrigido | Prova |
|---|---|---|
| Contratos | cStat incidental não pode promover NFS-e/GNRE/CIOT a operacional | Famílias com parser próprio não herdam a política cStat; testes do canal e contratos |
| Municipal | Raiz genérica, namespace de outro padrão, número de lote isolado e booleano malformado produziam evidência insuficiente | Gate gerado dos XSDs, serviço/tag configurada, marcadores concretos; casos positivos e negativos atravessam ConsumirAPI |
| Nacional | Rejeição conhecida e certificado exigem categorias distintas; código conhecido não pode ocultar outro desconhecido | Catálogo oficial auditado, códigos de certificado separados, listas mistas e envelopes sem tipoAmbiente cobertos |
| GNRE | 401/503 de negócio eram confundíveis com HTTP; erro antigo de guia não descreve situação atual; enum histórico de ConsultaLoteRecepcao difere do nome da classe | Política por raiz/grupo/serviço, códigos de acesso/processamento e regressão do enum histórico, sem alterar o fluxo fiscal |
| CIOT | Qualquer código alfanumérico não prova processamento normal; Sucesso=false/erro prevalecem sobre código 110 | Casos de código desconhecido, autenticação, erro explícito e valores contraditórios |
| PDF | HTTP200, Content-Type ou Sucesso isolado não comprovam entrega; código 110 sem arquivo também não | Verificação limitada de envelope; truncamento/HTML/cabeçalho inválido/Base64 malformado e ausência de arquivo ficam inconclusivos |
| PDF/compatibilidade | Base64 com quebras de linha é válido; conferir o PDF não pode alterar stream, XML ou Result | Varredura sem alocação proporcional ao arquivo; bordas decodificadas em buffers pequenos. Stream preserva posição/bytes. Executar público eFrete preserva Result antes/depois do diagnóstico, inclusive bytes após normalização CRLF/LF já feita pelo parser XML existente |

Os testes de contrato conferem uma requisição/uma entrada e uma observação por execução simples, ausência de corpo/token/dados sintéticos sensíveis na fotografia e igualdade do retorno original antes/depois da conclusão. Os testes existentes de transporte, exceção original, saturação e desempenho fiscal foram incluídos novamente.

Limites remanescentes são explícitos em INTERPRETACAO.md: semântica ainda não comprovada de códigos municipais/adicionais e contratos diferentes do catálogo, outros binários e validação integral de PDF. Não houve teste com rede, certificado real ou A3, nem execução em .NET Framework 4.7.2. O PRO permaneceu somente leitura; não se executou seu build/teste com pacote antigo como se validasse este código.

As seções seguintes preservam o registro da etapa anterior para rastreabilidade; os números e o DryRun finais desta revisão são os desta seção.

## Estado anterior (antes da revisão do item 3)

- Biblioteca: `netstandard2.0`, C# 7.3; builds Debug e `INTEROP_Release` aprovados.
- Testes: runner xUnit v3, .NET 8, oito classes offline, **300 casos aprovados, zero erros/falhas/ignorados**. Duração reportada pelo runner: 1,226 s. O filtro excluiu os testes reais de disponibilidade (`DisponibilidadeIntegracao`). Não foi executada a suíte integral dependente de serviços/certificados reais.
- Certificados dos testes de status/CIOT: autossinados e temporários, criados no processo; sem A1 real, A3, PIN, conta ou certificado de contribuinte.
- `git diff --check`: sem erros. Avisos de conversão LF/CRLF do Git não são falhas de whitespace.
- Nenhuma alteração em `Xml`, schemas ou `Servicos/Config`. Nenhum commit/push.
- PRO somente consultado, inclusive `project.assets.json`, `ProductVersion`, documentação e código de coleta/análise. Sem build, restore, edição, pacote novo ou migração no PRO.

## Comandos executados

Os comandos abaixo são relativos à raiz `C:\projetos\github\Unimake.DFe`:

```powershell
$env:DOTNET_CLI_TELEMETRY_OPTOUT = '1'
$env:DOTNET_CLI_USE_MSBUILD_SERVER = '0'

dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore -v quiet /nr:false
dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" -c INTEROP_Release --no-restore -v quiet /nr:false
dotnet build "source/Unimake.DFe.Test/Unimake.DFe.Test.csproj" --no-restore -v quiet /nr:false

dotnet "source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll" `
  -class Unimake.DFe.Test.Utility.Rede.CoberturaDisponibilidadeTest `
  -class Unimake.DFe.Test.Utility.Rede.CanalObservacoesDisponibilidadeTest `
  -class Unimake.DFe.Test.Utility.Rede.DiagnosticoDisponibilidadeTest `
  -class Unimake.DFe.Test.CIOT.Servicos.EFreteFluxoExecucaoTest `
  -class Unimake.DFe.Test.Utility.Rede.ProxyConfiguracaoTest `
  -class Unimake.DFe.Test.NFCe.Utilitarios.CpfCnpjComBrLookupTest `
  -class Unimake.DFe.Test.Utility.Xml.SoapResponseParserTest `
  -class Unimake.DFe.Test.UMessenger.Transport.ApiTimeoutTest `
  -trait- "Utility=DisponibilidadeIntegracao" -noLogo

./docsplan/diagnostico/Atualizar-Inventario.ps1
git diff --check

./source/Packaging/Pack-DFe.ps1 -Mode Offline -DryRun `
  -StartDate '08/11/2026 00:00:00' -EndDate '09/11/2026 00:00:00'
```

Resultado do runner:

```text
Total: 300, Errors: 0, Failed: 0, Skipped: 0, Not Run: 0, Time: 1,226s
```

O build principal final reportou 5 avisos e zero erros; o INTEROP final, 12 avisos e zero erros. Há avisos CS0618 em `EnviNFe` e NU1900 por consulta de vulnerabilidades ao feed privado Azure. O build dos testes reportou 29 avisos e zero erros, incluindo obsolescências e analisadores xUnit de testes existentes. Nenhuma regra de certificado, validação fiscal ou teste foi desabilitada para obter aprovação.

O primeiro comando dotnet no sandbox encontrou bloqueio de acesso ao registro da telemetria do CLI; a variável `DOTNET_CLI_TELEMETRY_OPTOUT=1` permitiu executar os builds. A primeira tentativa de DryRun não resolveu `api.nuget.org` no sandbox. A execução autorizada fora dessa restrição consultou somente metadados NuGet/GitHub e completou a validação oficial. Não houve publicação.

## Empacotamento

O primeiro DryRun validou o pacote temporário `20261109.1218.47`, conferindo conteúdo, framework e hash de GeradorCIOTShared pelo fluxo oficial. Não persistiu pacote no feed nem versão rastreada. A validação foi repetida no estado final; o resultado final está registrado ao fim deste arquivo.

Não se gerou pacote permanente nem se atualizou o consumidor para rodar testes do UniNFe. A instrução específica desta tarefa mantém o PRO somente para consulta, prevalecendo sobre a rotina geral de atualizar pacote/rodar consumidor. Rodar testes do PRO com seu pacote anterior não provaria esta versão da DLL. A validação de consumo fica pendente de uma atualização de pacote separadamente autorizada e fiscalmente compatível.

## Autorrevisão por bloco

| Bloco | Defeitos encontrados e corrigidos | Prova |
|---|---|---|
| Inventário | Enum SNCM tinha configuração, mas classe de serviço é vazia; CT-e simplificado e BP-e variantes não são tipos independentes; DARE/uMessenger/CIOT produção desviam da base comum | Rastreamento das implementações Executar, dos consumidores e das saídas HTTP; tabela nominal com limitações explícitas |
| Respostas | cdResposta/cdRetorno não eram interpretados; raiz SOAP configurada difere da raiz municipal de negócio; Status 0 é válido em PIX/eBoleto; códigos CIOT não são cStat; listas CIOT precisam do primeiro Item; mensagens uMessenger precisam ser examinadas em conjunto | Consumidores reais com executores substituídos; modelos CIOT públicos e massas sintéticas de contratos |
| Transporte/preparação | Hook SOAP/REST acontecia antes da verificação de certificado/mapeamento; TLS HttpClient podia aparecer como protocolo; erro de autenticação era HTTP genérico; helper AGILI pode substituir a exceção original | Regressões de certificado antes de rede, identidade da exceção original, AuthenticationException interna, HTTP 401/403/407/429, preservação escalar da falha auxiliar |
| Status manual | Cinco tipos com serviço oficial implementado não tinham estratégia no diagnóstico; cStat ausente podia virar Degradado; HTTP503 com código de sucesso não deve virar operacional | Consulta pública com infraestrutura e SOAP controlados, request oficial e cache; HTTP503, código 0 e resposta HTML |
| Segurança | Histórico expunha path de URL e texto externo apenas filtrado por regex; remoção do path precisava manter endpoints distintos | Origem pública + identidade SHA-256 interna; cache e agrupamento usam identidade; mensagens controladas. Regressão com mesmo host/dois caminhos e corpo privado |
| Semântica | SOAP Fault com código interno não é resposta de negócio; Reinf 495/496 é recusa de certificado específica, não regra HTTP universal | Regressões de Fault, prioridade HTTP, certificado Reinf versus PIX e HTTP404 com contrato de lote não encontrado |
| Concorrência/desempenho | Testes de status novos compartilhavam relógio/cache com a classe histórica; varredura genérica de Fault percorria lotes sem necessidade | Coleção de teste comum para o estado estático; verificação de Fault limitada à estrutura SOAP. Testes existentes de contenção, saturação e caminho fiscal permanecem verdes |

A prova de preservação do negócio inclui retorno CIOT normalizado até Result, exceção de transporte propagada por identidade, uma chamada de executor por tentativa simples, duas entradas para login+declaração, ausência de repetição quando login falha, nenhuma observação duplicada e nenhuma referência a XML/configuração/exceção na fotografia. Falhas de diagnóstico continuam isoladas pelo canal; os testes de saturação, descarte e exceção sem mensagem permanecem aprovados.

## Riscos residuais e limites reais

- Não há execução de todos os municípios/provedores/versões e endpoints. Cobertura de fronteira e interpretação de cada contrato são dimensões distintas; o inventário as separa.
- NFSe municipal/nacional ainda possui formatos/códigos sem política completa; GNRE/CIOT não têm mapa universal de causas. A observação de transporte existe e o contrato não reconhecido fica inconclusivo.
- Não há introspecção do HTTP interno de GeradorCIOTShared. Seu teste substitui o componente; não comprova a infraestrutura real dele.
- Não se executou o HTTP real dos helpers AGILI/uMessenger nem autenticação A3. AGILI não tem chamador localizado na DLL. A regressão nova comprova a preservação da categoria do hook, sem afirmar integração real do provedor.
- Houve execução de testes em .NET 8 e compilação COM/netstandard2.0. Não houve execução em .NET Framework 4.7.2. Cancelamento sem TimeoutException em runtimes antigos permanece desconhecido por ser ambíguo.
- O histórico legado continua restrito e não substitui a sessão ampla. A capacidade pública explicita essa diferença; ampliar a chave estadual antiga para contexto municipal/autenticação não foi simulado.
- `Endpoint` histórico agora contém somente esquema/host/porta e `Excecao` usa mensagem controlada; consumidores que dependiam do texto/path devem usar identidade própria de serviço/contexto, sem persistir dados sensíveis.
- Nenhuma consulta de status confirma automaticamente recuperação de autorização, evento, pagamento ou download. Não foi criado tráfego adicional no caminho crítico nem enviada massa fiscal para testar disponibilidade.

## DryRun anterior (antes da revisão do item 3)

**Aprovado**, exit code 0, pacote temporário **20261109.1232.11**, AssemblyVersion `2026.11.09.1232`. O fluxo oficial validou conteúdo, versão, dependências, somente `lib/netstandard2.0` e hash de `GeradorCIOTShared.dll`. Reportou: “DryRun concluído: nenhum pacote foi enviado e nenhum arquivo rastreado foi alterado.” O projeto foi conferido sem diff após a restauração da versão original. O pacote temporário não é uma versão disponibilizada para consumo pelo PRO.

Os hashes das fontes centrais efetivamente entregues estão em [DISPONIBILIDADE.sha256](DISPONIBILIDADE.sha256). O inventário foi regenerado após as alterações finais, preservando 151 classes e 3.191 registros de configuração.
