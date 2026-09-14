# Claims de segurança

| Claim | Ameaça | Mitigação | Teste negativo | Limitação |
|---|---|---|---|---|
| XML assinado usa infraestrutura existente | adulteração do documento | XMLDSig/certificado e validação central | assinatura ausente/inválida rejeitada | não prova habilitação fiscal |
| Secrets não aparecem em evidence | exposição de certificado/senha/XML fiscal | sanitização e fixtures sintéticas | varredura por padrões sensíveis | revisão humana continua necessária |
| Produção não usa homologação | envio ao ambiente errado | ausência explícita de URL de produção | configuração de produção falha fechada | depende de atualização futura oficial |
