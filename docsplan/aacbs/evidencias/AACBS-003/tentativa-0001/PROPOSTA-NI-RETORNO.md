# Divergência real em ni do arquivo de débitos

## Constatação e fontes

O ciclo Beta concluiu a solicitação e retornou arquivo JSON válido por HTTP 200. O campo `ni` é uma string de **14 dígitos ASCII**, igual ao CNPJ completo da matriz configurada; seus oito primeiros dígitos coincidem com a base enviada no pedido. O tíquete também corresponde à solicitação. Nenhum identificador real é reproduzido aqui.

Na reconsulta de 24/11/2026, tanto o [HTML de débitos](https://docs.receitafederal.gov.br/apuracao-cbs/debito.html) quanto o [JSON Schema oficial](https://docs.receitafederal.gov.br/apuracao-cbs/schemas/debito-retorno.schema.json) continuam limitando `ni` a oito dígitos. O contrato XML aprovado na AACBS-001 seguiu essa regra. A rejeição atual em `$.ni` é coerente com aquele contrato; o retorno observado exige decidir uma compatibilidade explícita.

O diagnóstico utilizou somente um GET adicional de link e um GET adicional de arquivo, mantendo o tíquete original, sem reabrir a consulta nem autenticar novamente. O JSON real permanece cifrado em `.local/aacbs`; a evidência versionada limita-se a metadados e verificações booleanas.

## Proposta para aprovação do DEV

1. Aceitar **8 ou 14 dígitos ASCII exclusivamente no ni de retArquivoDebitos**, preservando integralmente o texto recebido no XML. Não cortar o CNPJ completo para fazê-lo caber no contrato anterior.
2. Manter `cnpj` de envio com oito dígitos. Quando o retorno trouxer oito, conferir igualdade exata com a base enviada; quando trouxer 14, conferir igualdade de seus oito primeiros dígitos com essa base. Rejeitar outra raiz de CNPJ, outros tamanhos e tipos.
3. Restringir a compatibilidade ao arquivo de débitos comprovado nesta etapa. Não alterar os envios, niConsumidor, outras famílias, valores ou datas por analogia.
4. Atualizar o XSD próprio, o mapper, a especificação e os testes sintéticos; conservar a fonte oficial arquivada sem alterações. Registrar que a aceitação de 14 é compatibilidade baseada em evidência real Beta, além do schema oficial atual.

As propriedades C# já são string; não haverá troca de tipo público, normalização, perda de zeros ou mudança de namespace. Não há pacote publicado desta integração; a proposta preserva a versão XML 1.00 em preparação.

## Verificação e retomada propostas

Testar round-trip e validação de oito e 14 dígitos, preservação de zeros, rejeição de outra raiz/tamanho/tipo, envio ainda limitado a oito e isolamento das outras famílias. Revalidar o JSON real cifrado sem exibir dados. Depois, reconciliar explicitamente o estado de download do tíquete existente e repetir somente link/download com o código corrigido; não repetir POST ou autenticação enquanto o token for válido.

Estado: **APROVADA pelo DEV em 24/11/2026**, resposta: "Aprovar compatibilidade 8/14 no retorno de débitos (Recomendado)". Implementação, testes e retomada real concluídos na AACBS-003; ver o dossiê da etapa. Nenhuma ampliação ocorreu antes da aprovação.

## Impacto estrutural e consumidores

Base anterior: o XSD versionado em Git e o checkpoint da AACBS-002 usam `Identificacao8` no elemento preexistente `/retArquivoDebitos/ni`. A alteração é de domínio/tamanho desse elemento; não há inserção/remoção de tag, movimentação ou mudança de ordem/cardinalidade/namespace/versão.

Raiz dos schemas: `source/.NET Standard/Unimake.Business.DFe/Xml/Schemas/AACBS`. Fechamento: `retBaixarArquivoRetorno_v1.00.xsd` inclui `retArquivoDebitos_v1.00.xsd`, que inclui `tiposComuns_v1.00.xsd`; `RetArquivoDebitosTipo` é usado pela raiz independente e pelo ramo de débitos do envelope. O tipo comum `Identificacao8` não será alterado. A união 8/14 será local ao elemento de débitos.

Pasta de classes: `source/.NET Standard/Unimake.Business.DFe/Xml/AACBS`. Correspondência demonstrada por `[XmlRoot("retArquivoDebitos", Namespace="http://www.unimake.com.br/dfe/aacbs")]`, `RetArquivoDebitosTipo`, propriedade `RetArquivoDebitos.Ni`/`[XmlElement("ni", Order=1)]` e `RetBaixarArquivoRetorno.ArquivoDebitos`. O tipo C# continua `string`; a única mudança na classe é a documentação do domínio aceito. INTEROP/COM e demais propriedades permanecem compatíveis.

| Consumidor | Dependência e ação |
|---|---|
| `Servicos/AACBS/RespostaAACBS.Arquivo` | Conferência exata anterior rejeitava NI14 da mesma empresa; ampliar apenas essa comparação com validação prévia de tamanho/dígitos e preservação integral |
| `RetBaixarArquivoRetorno.ArquivoDebitos` | Consome o tipo de arquivo no envelope; compilar/validar ambos os fechamentos, sem alterar a propriedade ou o choice |
| Validação central e AACBSValidator | Recursos e raízes permanecem os mesmos; a regra de tamanho está no XSD, sem duplicação global no validator |
| `Xml/Validar/ValidarConfig.xml` e `.csproj` | Nome/versão/localização do schema e recursos não mudam; nenhum ajuste necessário |
| Créditos, pagamentos, recolhimentos e pedidos | Seus elementos continuam usando Identificacao8; provar isolamento em testes, sem ampliar por nome homônimo |
| Testes e fixtures | Acrescentar positivos 8/14 e negativos de correlação/tamanho/tipo, round-trip e isolamento; apenas dados sintéticos |
| Exemplos e consumidores no checkout | Pesquisa por tipo/propriedade/raiz não identificou consumidor externo ao domínio e aos seus testes que imponha tamanho 8; não há integração AACBS existente no UniNFe a alterar nesta etapa |

O risco observável para futuros consumidores é receber 14 onde a documentação anterior indicava oito. Esse comportamento é agora explicitamente aprovado e documentado; não se deve usar comprimento fixo de oito para interpretar `ni` retornado em débitos. Nenhum enum ou campo de data foi alterado. O conjunto de XSD e os testes serão verificados no estado final, conforme o dossiê.
