# Atributos de qualidade

| Atributo | Meta | Evidência |
|---|---|---|
| Compatibilidade | zero modernização de framework/API fora do incremento | build e revisão de API/diff |
| Fidelidade XML | ordem, namespace, ocorrências, precisão e assinatura conforme XSD/MOC | round-trip, XPath e schema |
| Segurança | certificado/proxy existentes; zero secret/log fiscal em evidence | testes negativos e revisão |
| Resiliência | distinguir falha fiscal de transporte; sem retry de autorização fora do padrão | testes determinísticos |
| Manutenibilidade | boundary próprio NF-e ABI e padrão vizinho reconhecível | revisão arquitetural |
