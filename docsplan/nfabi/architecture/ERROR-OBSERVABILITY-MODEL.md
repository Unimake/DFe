# Erros e observabilidade

Classificar separadamente validação XSD/conteúdo, assinatura/certificado, configuração/endpoint, DNS/TLS/proxy/timeout e rejeição `cStat`. Logs e retornos ERP seguem helpers existentes, com correlação por arquivo/AttemptId sem XML fiscal, senha ou certificado. Nenhum retry automático de autorização é adicionado sem idempotência comprovada.
