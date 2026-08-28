---
name: manutencao-ciot
description: Implementar, corrigir ou revisar XML, XSD, serviços, transporte, autenticação, retornos, INTEROP, testes ou integração UniNFe do CIOT via ANTT e eFrete, preservando o contrato público comum e a compatibilidade do fluxo ANTT.
---

# Manutenção do CIOT

Use esta skill para qualquer manutenção do domínio `CIOT` que possa atingir a ANTT, a eFrete ou a seleção entre provedores.

## Antes de alterar

1. Identifique se a mudança é exclusiva da ANTT, exclusiva da eFrete ou compartilhada pelas classes XML públicas.
2. Se tocar eFrete, seleção de provedor, schemas compartilhados, transporte ou normalização de retorno, leia [references/efrete.md](references/efrete.md) por completo.
3. Se tocar o UniNFe, roteamento de arquivos ou configuração da empresa, leia também [references/uninfe.md](references/uninfe.md) por completo.
4. Para alterar payload, obrigatoriedade, endpoint ou versão eFrete, confira o manual e a lista REST vigentes. O contrato implementado atualmente foi baseado no Manual de Integração eFrete 8.1 e na planilha de URLs REST; não deduza o formato apenas a partir das classes XML.

## Invariantes de compatibilidade

- Considere a ANTT como baseline de compatibilidade. Sem `<ProvedorCIOT>`, o fluxo deve continuar ANTT, com os mesmos endpoints, JSON, schemas, validações e retornos.
- Mantenha as mesmas classes XML públicas para ANTT e eFrete. Campos exclusivos da eFrete devem ser opcionais na serialização comum e obrigatórios apenas na validação condicional do provedor.
- `<ProvedorCIOT>` é metadado de roteamento dos XMLs de envio: deve ser o primeiro elemento, aceita `ANTT` ou `EFrete`, prevalece sobre `Configuracao.ProvedorCIOT` e nunca deve entrar no JSON nem nos XMLs de retorno.
- Separe particularidades em `Servicos/CIOT/Provedores`. A `ServicoBase` do CIOT apenas orquestra seleção, configuração, validação, transporte e normalização; não acumule regras específicas de uma instituição.
- Carregue endpoints pelo XML embutido do provedor. Não codifique URLs na classe de serviço.
- Normalize respostas eFrete para o contrato público CIOT antes que DLL, INTEROP ou UniNFe as consumam. Consumidores não devem precisar de ramificações por provedor para reconhecer sucesso ou ler mensagens.
- Preserve C# 7.3, `netstandard2.0`, documentação XML pública e compatibilidade INTEROP. Coleções públicas novas precisam seguir o padrão COM já existente.

## Camadas que devem evoluir juntas

Ao adicionar ou alterar um campo ou serviço, revise conforme aplicável:

- classes em `Xml/CIOT`;
- XSDs ANTT e/ou o conjunto específico em `Xml/Schemas/CIOT/EFrete`;
- recursos embutidos no projeto;
- configuração em `Servicos/Config/CIOT`;
- provedor, mapper, authenticator, validator e normalizador de retorno;
- serviço público, enum `Servico` e contratos INTEROP;
- recursos e testes em `Unimake.DFe.Test/CIOT`;
- tarefas, configuração e roteamento do UniNFe, quando o serviço for exposto por ele.

Não afrouxe o XSD ANTT para acomodar uma regra exclusiva da eFrete. Valide eFrete com seu schema composto e mantenha a validação manual para regras condicionais que XSD não representa bem.

## Verificação mínima

- Cubra serialização, desserialização, round-trip, ordem, opcionais e INTEROP quando o XML público mudar.
- Para mappers, compare o JSON estruturalmente; confirme especialmente arrays, ausência de nulos e exclusão de `<ProvedorCIOT>`.
- Para transporte e autenticação, execute `Executar()` com transporte controlado, cobrindo GET com corpo, POST, token, login, certificado, sucesso e erro até `Result`.
- Para retornos, valide propriedades tipadas e o XML final completo, incluindo ordem e grupos repetidos. Não considere suficiente testar somente o mapper isolado.
- Mantenha pelo menos uma regressão explícita da ANTT ao alterar código compartilhado.
- Compile a biblioteca principal com `--no-restore`, execute apenas as classes CIOT afetadas pelo runner xUnit v3 e rode o filtro CIOT correspondente do `UniNFe.Test` em `Debug`.
- Execute `INTEROP_Release` quando mudar API pública, enum, coleção, nullable, dependência ou transporte usado pela compilação COM.

Testes reais na homologação eFrete são opcionais, exigem autorização explícita e credenciais fornecidas fora do repositório. Nunca grave ou reproduza segredos em código, massas, logs ou respostas.
