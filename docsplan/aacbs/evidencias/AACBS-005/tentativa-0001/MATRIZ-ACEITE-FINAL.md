# Matriz de aceite final — AACBS-005

Esta matriz cobre as dez unidades do catálogo. As 24 raízes de envio, retorno e arquivo e seus XSDs estão no [catálogo](../../../contratos/CATALOGO-XML.csv); o [guia](../../../uso/README.md) liga cada serviço aos exemplos XML. Um teste controlado substitui somente o transporte, mantendo o fluxo público Executar, Result, XML e validação. Não comprova habilitação remota.

| Unidade | Contrato / prova controlada | Prova real e limite |
|---|---|---|
| Autenticar | autenticar/retAutenticar; AACBSAutenticacaoTest, AACBSFalhasExecucaoTest: OAuth, cache, isolamento e falhas | HTTP 200 comprovado na 003; token real preservado somente no cache protegido. Evidência reutilizada. |
| ConsultarDebitos | consultarDebitos/retConsultarDebitos; AACBSFluxoDebitosTest, AACBSArquivoDebitosTest | Ciclo Beta finalizado na 003; apuração vazia. Grupos com itens cobertos sinteticamente, sem nova abertura na 005. |
| ConsultarCreditos | consultarCreditos/retConsultarCreditos; AACBSFluxoConsultasTest, AACBSArquivosConsultasTest | Abertura 201 e conclusão/link/download 200 na 004. A 005 confere o original cifrado sem alterar NI14 após aprovação específica; resultado final em VERIFICACAO-REAL-CREDITOS.json. |
| ConsultarPagamentos | consultarPagamentos/retConsultarPagamentos; AACBSFluxoConsultasTest, AACBSArquivosConsultasTest | HTTP 404 e corpo incompatível na única abertura; sem tíquete confirmado. Causa indeterminada; parte real bloqueada. Não houve replay. |
| ConsultarRecolhimentos | consultarRecolhimentos/retConsultarRecolhimentos; AACBSFluxoConsultasTest, AACBSArquivosConsultasTest | Mesma limitação de 404, sem tíquete confirmado; parte real bloqueada. Não houve replay. |
| ConsultarSituacaoSolicitacao | consultarSituacaoSolicitacao/retConsultarSituacaoSolicitacao; AACBSContratosHttpTest, fluxos de cada família: todas as situações e erros | Executada para os tíquetes conhecidos de débitos e créditos, ambos concluídos. Nenhuma situação real de pagamentos/recolhimentos confirmada. |
| ConsultarLinkDownload | consultarLinkDownload/retConsultarLinkDownload; contexto, URL, expiração, sanitização e erros nos testes de contrato e segurança | Executada para débitos e créditos concluídos. Sem link das famílias com abertura ambígua. |
| BaixarArquivoRetorno | baixarArquivoRetorno/retBaixarArquivoRetorno e retArquivoDebitos/Creditos/Pagamentos/Recolhimentos; testes de parsing/fluxo/segurança: quatro famílias, fidelidade, vazio, listas, falhas e contratos cruzados | Débitos vazios na 003; original real de créditos conferido na 005. Pagamentos/recolhimentos permanecem sem prova real do arquivo. |
| GerarDarfAdquirente | gerarDarfAdquirente/retGerarDarfAdquirente; AACBSDarfTest: array ordenado, total/parcial, retorno global e erros | Não emitido. Faltam chaves elegíveis e opção/valor de cenário habilitado, depois confirmação específica do DEV antes do POST. |
| GerarDarfContribuinte | gerarDarfContribuinte/retGerarDarfContribuinte; AACBSDarfTest: objeto, PA, total/parcial, principal/multa/juros e erros | Não emitido. Falta PA e opção/valor de cenário habilitado, depois confirmação específica do DEV antes do POST. |

## Compatibilidade, exemplos e qualidade

AACBSExemplosUsoTest valida os 24 XMLs publicados, leitura, geração, gravação/releitura, precisão temporal e decimal, XML público e coleções INTEROP. AACBSLimitesContratoTest comprova XSDs isolados, envelope e resolução central, NI8/14 de débitos/créditos e envio8; os demais retornos mantêm NI8. AACBSTextosPreservadosTest protege CR/LF e caracteres significativos. Os testes de segurança/HTTP mantêm proxy explícito, ausência de token/certificado no download, sem requests extras, sem repetição automática de POST e sem vazamento em exceções/trace.

A medição de mil créditos da [004](../../AACBS-004/tentativa-0001/ENTREGA.md) permanece aplicável ao cenário sintético medido. A ampliação de comprimento do NI não muda a materialização do corpo em memória; não há alegação de escala ilimitada.

## Lacunas para decisão do DEV

O [diagnóstico dos 404](../../AACBS-004/tentativa-0001/PESQUISA-404.md) não permite afirmar inexistência, indisponibilidade ou falta de habilitação da API. A retomada exige qualificação da rota/habilitação com a Receita ou orientação explícita para nova tentativa controlada, preservando o estado ambíguo. Não reiniciar cota nem repetir automaticamente.

Antes de RAD/PCONT, receber os parâmetros do cenário em arquivo local protegido, preparar o XML concreto e submetê-lo à confirmação específica do DEV. Não copiar chaves de DF-e ou valores reais para o dossiê; o consumidor guardará o XML operacional protegido. Nenhum pagamento, PIX ou download de PDF está no escopo.

A revisão final da 005 submete estas limitações materiais ao DEV. Aprovação das etapas anteriores não converte critérios reais pendentes em sucesso. Publicação, adaptação de consumidores e IBS continuam fora deste escopo.
