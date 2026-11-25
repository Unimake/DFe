# Compatibilidade do NI no arquivo de créditos

Decisão **AGUARDANDO DEV**, 24/11/2026. A aprovação anterior de NI 8/14 aplica-se somente a débitos e não foi estendida silenciosamente.

O ciclo Beta de créditos teve uma abertura HTTP 201, situação concluída HTTP 200, link HTTP 200 e download HTTP 200. A DLL rejeitou o arquivo em $.ni antes de disponibilizar XML operacional. O estado foi preservado como DownloadIndisponivel.

Um diagnóstico explícito do mesmo tíquete fez somente um GET adicional de link e um de arquivo, usando token existente, sem abertura, autenticação ou consulta de situação adicionais. O [registro sanitizado](DIAGNOSTICO-CREDITOS.json) comprova: ni textual com 14 dígitos ASCII, igual à matriz configurada e à raiz do pedido, tíquete correspondente e dois grupos de apuração. O JSON original de 8.969 bytes permanece cifrado localmente. A evidência contém somente seu hash cifrado; o script diagnóstico temporário foi removido para impedir replay acidental. O journal permaneceu inalterado.

Proposta submetida ao DEV:

- Aceitar oito ou quatorze dígitos somente no ni de retArquivoCreditos; manter os envios com oito.
- Preservar integralmente o identificador recebido, sem truncamento, e exigir correspondência dos primeiros oito dígitos com o pedido.
- Manter pagamentos e recolhimentos com seus contratos atuais; nenhuma compatibilidade de outra família será presumida.
- Atualizar apenas o XSD específico, o mapper de créditos e as regressões relacionadas, preservando tipo público string, namespace, versão e ordem XML.
- Validar o JSON original cifrado, sem substituir campos, depois da correção aprovada; não repetir abertura. Resultado real válido com itens será documentado por contagens e booleans estruturais, sem payload identificável versionado.

Até a decisão explícita, o contrato e os testes continuam exigindo oito dígitos para créditos. Nenhum XML normalizado artificialmente conta como entrega operacional ou prova de compatibilidade com o original.
