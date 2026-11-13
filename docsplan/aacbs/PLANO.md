# Plano de execução AACBS

## Objetivo e limite da entrega

Integrar todas as operações publicadas no conjunto de apuração assistida CBS à Unimake.DFe, oferecendo contratos XML documentados e tipados, XSDs de envio e retorno e um serviço por endpoint. A API HTTP permanece um detalhe tratado pela DLL. Além dos seis endpoints de negócio, o plano cobre autenticação e o download externo do resultado.

O sucesso observável é executar o ciclo de cada família — solicitação, situação, obtenção do link e download — a partir de XML, obter XML final validável e `Result` utilizável em .NET e INTEROP, preservando os dados e erros do contrato remoto. Cada chamada continua sendo uma operação explícita. A aplicação guarda o tíquete e decide a próxima chamada.

Não fazem parte deste primeiro escopo: API IBS sem documentação própria, calculadora tributária, conclusão de apuração mencionada apenas em material antigo sem endpoint vigente neste conjunto, hospedagem de webhook, agendamento, banco de dados, reconciliação contábil, telas, adaptação do UniNFe/PRO e publicação no nuget.org. A configuração de `urlRetorno` pode ser suportada; o fluxo completo inicial usará consulta de situação. Não é necessário que o consumidor implemente JSON para usar esse fluxo.

## Base verificada e desenho

O inventário de fontes e lacunas está em [FONTES-E-REQUISITOS.md](FONTES-E-REQUISITOS.md). A correspondência inicial entre endpoints, serviços e contratos está em [MATRIZ-APIS.md](MATRIZ-APIS.md). O mapa de código e a estratégia de verificação estão em [ARQUITETURA-E-TESTES.md](ARQUITETURA-E-TESTES.md).

```mermaid
flowchart LR
    A[Aplicação: XML e Configuracao] --> B[Serviço AACBS e validação XSD]
    B --> C[Mapper: path, headers e JSON]
    C --> D[Transporte compartilhado]
    D --> E[API RFB ou download autorizado]
    E --> F[Mapper do contrato de resposta]
    F --> G[XML validado, Result e gravação]
    G --> H[Aplicação guarda tíquete e decide retomada]
```

- Criar uma base fiscal `Servicos/AACBS/ServicoBase.cs`, configuração nacional e mappers do domínio. Reutilizar o transporte atual; não reproduzir provedores ou normalizações ANTT/eFrete.
- Usar classes em `Xml/AACBS`, XSDs em `Xml/Schemas/AACBS`, configuração em `Servicos/Config/AACBS` e validações de conteúdo em `Validator/AACBS`, quando necessárias.
- Adicionar valores explícitos de `TipoDFe` e `Servico` sem mudar os números existentes. Registrar recursos no projeto e integrar a detecção de XML/versão e o catálogo de validação existentes.
- O namespace XML será um contrato Unimake, a definir na AACBS-001. Os XSDs produzidos por nós não serão apresentados como schemas oficiais da Receita. Versão XML e versão HTTP são identificadores independentes.
- Expor o padrão `Envio`, `Result`, inicialização por objeto/XML e métodos INTEROP. A autenticação será preparada pela DLL; o DEV não precisará manipular JSON ou renovar tokens manualmente. O serviço auxiliar de autenticação também será especificado, sem exigir seu uso direto no fluxo habitual.
- O download terá contexto explícito da família solicitada e resultado tipado para os quatro layouts. A especificação definirá raízes e eventual envelope, com um mapeamento rastreável de cada campo remoto; não haverá um campo genérico contendo o JSON inteiro como substituto do modelo XML.
- A gravação dos XMLs seguirá o contrato de métodos da biblioteca. A aplicação escolhe quando e onde gravar; a observação passiva não fará essa persistência.

## Execução e gates

Status, aprovação e histórico ficam exclusivamente no [PDCA.md](PDCA.md). O orquestrador só distribui tarefas da etapa liberada. A aprovação do DEV libera a sucessora; ao terminar uma entrega, a equipe para em `DELIVERED_FOR_REVIEW`.

### AACBS-000 — Planejamento inicial

**Entrega:** este plano, decisões da entrevista, inventário inicial de endpoints, referências do CIOT, requisitos de ambiente, lacunas conhecidas, papéis dos agentes e modelo de evidências.

**Aceitação:** arquivos navegáveis; distinção entre fatos, decisões e propostas; etapas executáveis e abrangentes; nenhum código funcional alterado; nenhuma chamada de negócio realizada. O DEV revisa a sigla, o escopo e a sequência antes de liberar a AACBS-001.

### AACBS-001 — Qualificação das fontes e especificação dos contratos

**Entrada:** AACBS-000 aprovada. Esta etapa altera apenas a documentação do plano e suas fontes técnicas.

**Trabalho:**

1. Obter a documentação técnica vigente de autenticação Receita Integra indicada pela RFB. Confirmar método, URI, headers, body, escopos, resposta, expiração, renovação e requisitos de acesso por ambiente. Distinguir acesso humano ao portal de autenticação do transporte.
2. Atualizar o inventário dos seis endpoints de negócio, autenticação e download externo. Registrar versão/data e SHA-256 dos arquivos técnicos efetivamente obtidos; para HTML, conservar a referência e uma captura textual datada com hash quando viável. Uma alteração de fonte exige avaliar o diff do contrato, sem importar automaticamente novas regras.
3. Produzir uma matriz por campo: origem documental; path/header/body/response; nome JSON; nome e caminho XML; tipo C#; tipo/restrição XSD; cardinalidade; ausente/vazio/zero/null; formato; regra condicional; serviço e teste correspondente. Cobrir respostas imediatas, situação, link, autenticação, erros e os quatro arquivos finais.
4. Resolver e registrar as divergências conhecidas entre tabela, narrativa e exemplo. Esclarecimento oficial ou resposta real legitimamente obtida pode fornecer evidência; uma resposta isolada não resolve sozinha todas as regras de cardinalidade. Quando não houver resposta, propor ao DEV uma representação conservadora e explicitar seu limite antes da aprovação.
5. Definir nomes públicos, raízes, namespace Unimake, versão inicial, schemas comuns/por operação e resultado de download por família. Definir tratamento de campos/códigos novos sem perda silenciosa, política de erro HTTP/aplicação e comportamento de retorno incompatível.
6. Especificar a configuração de credenciais, isolamento de token por identidade/ambiente, proteção do link temporário e preservação de códigos HTTP. Segredos ficam fora das massas, documentos e logs; XML operacional que contenha link/token recebe tratamento de dado sensível e não é incluído em evidências.
7. Confirmar o caminho de testes reais: empresa matriz, autorização de representação, credenciais habilitadas, ambientes oficialmente disponíveis, tipos de dados existentes e orçamento de chamadas. Registrar com o DEV o ambiente das futuras consultas, sem inferir produção restrita para endpoints não publicados.

**Aceitação:** todas as unidades da matriz possuem contrato e critério de teste; lacunas críticas da implementação seguinte resolvidas ou submetidas ao DEV com restrição explícita; credenciais podem ainda estar sendo providenciadas, mas seu mecanismo não pode ser inventado. Esta etapa não implementa classes, serviços ou XSDs de produção.

### AACBS-002 — Contratos XML, schemas e validação

**Entrada:** especificação AACBS-001 aprovada.

**Trabalho:** produzir classes de envio e retorno, grupos compartilhados e famílias finais; atributos XML explícitos; números monetários em `decimal`; datas que preservem precisão e offset; identificadores textuais; coleções e membros COM. Criar todos os XSDs definidos na matriz, incluindo erros, e embutir seus recursos. Integrar `TipoDFe`, os valores de `Servico`, `ValidarConfig.xml`, a identificação central do XML e a resolução de versão. Alterar normalização/XMLBase somente se uma necessidade comprovada exigir.

Adicionar testes de geração/leitura/round-trip, ordem, formatos, cardinalidade, campos opcionais, zeros obrigatórios, listas vazias e múltiplas, namespace/versão, schema positivo/negativo e localização de recursos. Cobrir os exemplos oficiais após tratar suas inconsistências; usar dados sintéticos. Os testes devem comprovar contrato e comportamento observável, não reproduzir a implementação.

**Aceitação:** cada contrato da matriz tem exemplo válido e verificação negativa pertinente; XMLs de retorno são validados com o mesmo rigor dos envios; validação central resolve domínio/serviço/versão; builds normal e INTEROP passam com a API pública documentada. Não há tráfego real nesta etapa.

### AACBS-003 — Autenticação, transporte e fluxo completo de débitos

**Entrada:** AACBS-002 aprovada e contrato OAuth qualificado.

**Trabalho:** implementar a base do domínio, configuração nacional, serviço auxiliar de autenticação, mappers e integração com o transporte compartilhado. Implementar `ConsultarDebitos`, `ConsultarSituacaoSolicitacao`, `ConsultarLinkDownload` e `BaixarArquivoRetorno` para o resultado de débitos. Validar a entrada antes de transportar e o XML público de saída após mapear o retorno. Disponibilizar `Result` e gravação no padrão da DLL.

Provar o ciclo completo com executor controlado: autenticação, abertura, processamento, conclusão, link e arquivo final, em chamadas explícitas. Exercitar token válido/expirado, erro de autenticação, isolamento entre empresas/credenciais/ambientes, reutilização de `Configuracao`, proxy direto/manual/automático e ausência de credenciais no download externo. Preservar a infraestrutura passiva existente; não criar sonda nem confundir situação da solicitação com status oficial do serviço.

Não repetir abertura após timeout ou erro sem comando explícito do consumidor. A perda da resposta de um POST não prova que a solicitação deixou de ser criada. Não adicionar espera de horas, polling automático, arquivo de estado ou contagem persistente de cotas dentro da DLL.

**Aceitação:** fluxo XML → HTTP controlado → XML/XSD/`Result` comprovado para débitos; request count e conteúdo HTTP esperados; erros e exceções preservados/sanitizados conforme o contrato aprovado; nenhum request extra para disponibilidade; builds e testes focados verdes. Se houver alteração compartilhada, executar também regressões determinísticas diretamente afetadas.

### AACBS-004 — Demais famílias e cobertura integral

**Entrada:** AACBS-003 aprovada.

**Trabalho:** implementar os serviços de créditos, pagamentos e recolhimentos, seus mappers e seleção explícita do layout final no download. Completar os fluxos controlados das três famílias, sem confundir campos semelhantes de contratos distintos.

Consolidar a matriz endpoint/contrato/teste. Cobrir todas as situações publicadas, erros HTTP e de aplicação, resposta não JSON/incompatível, campo desconhecido conforme a política aprovada, lista vazia, dados repetidos, números extremos válidos, precisão temporal, URL expirada, download interrompido e contrato de outra família. Os cenários de limite e falha usam transporte controlado, sem gastar cotas ou forçar erros reais.

**Aceitação:** os seis endpoints de negócio, autenticação e download externo possuem implementações e testes completos; nenhuma família depende de JSON manipulado pelo DEV; a matriz não contém operação sem cobertura. A revisão independente verifica aderência às fontes e aos XMLs/XSDs, além do resultado dos testes.

### AACBS-005 — Validação real, compatibilidade e entrega

**Entrada:** AACBS-004 aprovada; empresa, ambiente e credenciais disponíveis para os testes reais descritos na AACBS-001.

**Trabalho:**

1. Rodar os ciclos reais das quatro famílias no ambiente qualificado, respeitando cotas e tempos de processamento. O acompanhamento ocorre na tarefa em execução, com chamadas espaçadas e sem automação recorrente criada para vigiar o teste. Guardar apenas evidência sanitizada: serviço, ambiente, horário, correlação não sensível, HTTP, situação, contagens e resultado da validação XML.
2. Comparar de forma controlada a resposta efetiva e o XML gerado, preservando todos os campos conhecidos. Resposta vazia válida comprova o fluxo, mas não a materialização real dos grupos ausentes; registrar essa cobertura residual e usar as massas sintéticas para comprová-los. Se faltar dado essencial ou houver divergência, corrigir e repetir apenas o necessário.
3. Executar os testes offline afetados, build normal e INTEROP, smoke tests da API pública/coleções e leitura/gravação XML. Fornecer exemplos de uso por XML mostrando abertura, retomada com tíquete, situação, link, download e tratamento de erro.
4. Validar o fluxo oficial de empacotamento em `source/Packaging/Pack-DFe.ps1 -Mode Offline -DryRun`, com os parâmetros de intervalo exigidos pelo script. Conferir assets exclusivamente em `lib/netstandard2.0`, os quatro arquivos previstos e hash de `GeradorCIOTShared.dll`. Não criar fluxo paralelo ou publicar no nuget.org.
5. Se um teste de consumidor externo for acrescentado ao escopo pelo DEV, gerar primeiro pacote offline novo e imutável pelo fluxo oficial, restaurar a versão exata no consumidor autorizado e verificar versão/ProductVersion. A aprovação desta integração da DLL não autoriza editar automaticamente UniNFe/PRO.
6. Fazer revisão final independente dos contratos, configurações, recursos, testes e documentação. Consolidar limitações, resultados reais e instruções de retomada.

**Aceitação:** evidências offline e reais separadas; nenhum requisito real declarado testado sem execução; todos os contratos possuem cobertura; compatibilidade e pacote validados; limitações materiais submetidas ao DEV. Credenciais ou ambiente indisponíveis deixam a parte real bloqueada, sem reclassificá-la como sucesso. A equipe entrega para a revisão final do DEV.

## Riscos, prevenção e recuperação

| Risco concreto | Prevenção e detecção | Recuperação |
| --- | --- | --- |
| Documentação contraditória ou alterada | Qualificar fontes e rastrear os campos na AACBS-001; revisar mudanças antes de endurecer XSD. | Reabrir o contrato afetado e seus testes, com nova tentativa e revisão do DEV. |
| Perda de precisão ou de campos na conversão | Mapper explícito, `decimal`, datas/offset preservados e XML completo comparado em testes. | Corrigir mapper/modelo/schema em conjunto; manter massa sintética que reproduz o defeito. |
| Solicitação duplicada e gasto de cota | Chamadas explícitas e teste de número de requests; ausência de retry automático de abertura. | Preservar o tíquete conhecido e reportar resultado ambíguo ao consumidor quando não houver resposta. |
| Token ou endpoint reutilizado para outra identidade | Contexto de cache e testes de troca de ambiente, credencial e empresa. | Invalidar apenas o contexto afetado e autenticar novamente, sem repetir negócio às cegas. |
| Bearer/certificado vazando para storage | Preparação separada do request de download e testes de headers/certificado. | Interromper o download e corrigir a preparação antes de novo teste. |
| Link temporário ou payload em logs/exceções | Testes de sanitização, uso de massas sintéticas e dossiê sem corpos reais. | Remover a evidência imprópria e tratar o segredo comprometido fora do repositório. |
| Arquivo grande excede recursos | Medir um volume sintético representativo; identificar que o transporte atual materializa o corpo. | Registrar limite medido; adotar streaming apenas se necessário e com regressões do caminho compartilhado. |
| Agentes sobrescrevem arquivos ou testam versão diferente | Dono por arquivo, executor único e identificação do incremento testado. | Integrar novamente e repetir somente verificações afetadas pelo diff. |

Rollback de uma etapa significa desfazer exclusivamente suas alterações identificadas e voltar ao último contrato aprovado. Não usar reset destrutivo, remover trabalho do usuário, revogar fonte de pacote do usuário nem reaproveitar versão de pacote offline. Resultados de teste e tentativas anteriores permanecem como histórico.

## Questões adiáveis com responsável

| Questão | Responsável e momento limite |
| --- | --- |
| Sigla e nomenclatura pública final | DEV revisa AACBS-000; congelar antes da aprovação AACBS-001. |
| Detalhes OAuth, divergências de campos, política de extensões e envelope final | Orquestrador/implementador propõem na AACBS-001; DEV aprova antes do código. |
| Credenciais, empresa, ambiente e massa fiscal disponível | DEV providencia em paralelo; registrar na AACBS-001 e disponibilizar antes da AACBS-005. |
| Necessidade de otimização para volume | Medir na AACBS-004; resolver antes da entrega final se o volume representativo falhar. |
| Webhook hospedado, IBS, adaptação de consumidores ou publicação | Novo escopo explícito do DEV; não impede os fluxos CBS por consulta de situação. |
