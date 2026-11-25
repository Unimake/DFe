# Cobertura AACBS-004

Estado de execução e aprovações exclusivamente no PDCA. Esta matriz relaciona o contrato aos testes e às provas da entrega, sem substituir evidência real por massa sintética.

| Unidade | Prova controlada | Prova real |
|---|---|---|
| Autenticar | AACBSAutenticacaoTest, cache/isolamento e falha auxiliar | Histórico AACBS-003; reutilização do cache nesta etapa conforme validade |
| ConsultarDebitos | AACBSFluxoDebitosTest e regressões preservados | Ciclo Beta concluído na AACBS-003, apuração vazia |
| ConsultarCreditos | AACBSFluxoConsultasTest, AACBSArquivosConsultasTest | POST 201, situação concluída/link/download 200; NI 14 divergente do contrato 8, decisão DEV pendente |
| ConsultarPagamentos | AACBSFluxoConsultasTest, AACBSArquivosConsultasTest; RAD/PCONT distintos | Uma tentativa: HTTP 404 e corpo incompatível; sem tíquete confirmado ou replay. Critério real pendente |
| ConsultarRecolhimentos | AACBSFluxoConsultasTest, AACBSArquivosConsultasTest | Uma tentativa: HTTP 404 e corpo incompatível; sem tíquete confirmado ou replay. Critério real pendente |
| ConsultarSituacaoSolicitacao | Reutilização em cada ciclo e situações/erros dos testes da AACBS-003 | Mesmo tíquete da abertura de cada família; intervalo local 120 s |
| ConsultarLinkDownload | URL/expiração, contexto e ausência de download implícito | Mesmo tíquete concluído de cada família |
| BaixarArquivoRetorno | Família explícita; XML/XSD/Result, precisão, listas, contratos cruzados, falha de leitura e sem Bearer | Guardar XML cifrado e conferir estrutura/contagens, sem versionar payload |
| GerarDarfAdquirente | AACBSDarfTest: array, lote ordenado e retorno global, total/parcial e erros; nenhum pagamento | Pendente de confirmar cenário/parâmetros com o DEV antes do POST |
| GerarDarfContribuinte | AACBSDarfTest: objeto, período, total/parcial e erros, multa/juros; nenhum pagamento | Pendente de confirmar cenário/parâmetros com o DEV antes do POST |

Volume sintético: AACBSArquivosConsultasTest.MilCreditosMantemContagemEContratoNoRoundTrip, com mil créditos e medição externa documentada separadamente. Isso demonstra o cenário medido, não escala ilimitada.

O fluxo local preserva o journal anterior, separa a cota por API e compartilha o cache de autenticação. SemAguardar retorna antes da janela de consulta e permite acompanhar famílias independentemente. Não há emissão de DARF no roteiro de consultas, nem modo de download/pagamento de PDF/PIX.

Compatibilidade NI: 8/14 somente no retorno de débitos conforme decisão anterior do DEV. Os outros três contratos continuam exigindo oito dígitos. Divergência real exige registro e decisão concreta, sem flexibilização silenciosa.
