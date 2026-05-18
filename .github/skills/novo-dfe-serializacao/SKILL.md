---
name: novo-dfe-serializacao
description: Implementa classes de serialização/desserialização para um novo documento fiscal eletrônico na DLL Unimake.DFe, usando como entrada apenas o nome da subpasta do documento.
---

# Novo DF-e — Serialização/Desserialização

## Objetivo

Implementar classes C# de serialização/desserialização para um novo documento fiscal eletrônico na DLL `Unimake.DFe`, seguindo exatamente o padrão já existente no projeto.

A entrada obrigatória é somente o nome da subpasta do novo documento.

Exemplo de entrada:

```text
DCe
```

Com essa entrada, implementar em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\DCe
```

E criar testes em:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\DCe
```

---

## Entrada obrigatória

O usuário deve informar apenas o nome da subpasta/documento.

Exemplos:

```text
DCe
```

```text
NFCom
```

```text
MDFe
```

Se o nome da subpasta não for informado, solicite essa informação antes de alterar arquivos.

---

## Fonte de verdade obrigatória

Antes de implementar, analise e replique o padrão das pastas:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\NFCom
```

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\NFe
```

O novo documento deve seguir o mesmo padrão estrutural, técnico, documental e arquitetural.

Não invente padrões novos.

---

## Local de implementação

Considerando `{Documento}` como o nome informado pelo usuário, criar ou ajustar arquivos em:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\{Documento}
```

Criar testes em:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\{Documento}
```

Projeto de testes:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\Unimake.DFe.Test.csproj
```

---

## Regras obrigatórias de implementação

### Estrutura

As classes devem seguir exatamente o padrão existente em `NFCom` e `NFe`, incluindo:

- Namespace
- Nome de classes
- Nome de propriedades
- Organização em arquivos
- Hierarquia de classes
- Padrão de construtores, quando houver
- Padrão de listas, inicializações e propriedades auxiliares
- Padrão de comentários XML

A estrutura deve refletir fielmente os schemas fornecidos para o novo documento.

---

### Atributos XML

Usar corretamente os atributos já utilizados no projeto:

- `[XmlRoot]`
- `[XmlElement]`
- `[XmlAttribute]`
- `[XmlIgnore]`
- Outros atributos XML já usados nas classes de referência, quando aplicável

Os nomes das propriedades e elementos devem respeitar o XML, inclusive case, semântica e hierarquia.

---

### INTEROP

Implementar compatibilidade com INTEROP exatamente como nas classes de referência.

Analise como isso é feito em `NFCom` e `NFe` e replique o mesmo padrão.

Não crie abordagem nova para INTEROP.

---

### Documentação

Todas as classes e propriedades públicas devem conter `summary`.

O estilo, linguagem e formatação dos `summary` devem seguir o padrão já existente nas classes de referência.

---

### Reuso obrigatório

Antes de criar uma nova classe, verifique se já existe tipo equivalente no projeto.

Reutilize estruturas comuns sempre que possível.

Evite duplicação desnecessária.

Não recrie classes já existentes.

---

### Assinatura digital

Quando o schema exigir assinatura digital, reutilize obrigatoriamente a classe existente:

```text
C:\projetos\github\Unimake.DFe\source\.NET Standard\Unimake.Business.DFe\Xml\Signature.cs
```

Não recrie estrutura de assinatura.

---

## Testes unitários

Criar testes seguindo exatamente o padrão de:

```text
C:\projetos\github\Unimake.DFe\source\Unimake.DFe.Test\NFCom\SerializacaoDesserializacaoTest.cs
```

Os testes devem validar, no mínimo:

- Serialização para XML válido
- Desserialização para objeto correto
- Round-trip: serializar e desserializar
- Preservação dos dados
- Estrutura XML conforme esperado
- Uso correto dos namespaces XML
- Uso correto dos atributos e elementos obrigatórios

Executar somente os testes criados para `{Documento}`.

Todos devem passar.

---

## Restrições

Não alterar arquivos fora das pastas necessárias, exceto quando indispensável para compilar ou reutilizar estrutura existente.

Não introduzir novo padrão arquitetural.

Não duplicar tipos já existentes.

Não recriar `Signature`.

Não alterar padrões globais do projeto.

Não executar toda a suíte de testes se for possível executar apenas os testes criados.

---

## Checklist antes de finalizar

Antes de concluir, valide obrigatoriamente:

- [ ] Classes seguem o padrão de `NFCom` e `NFe`
- [ ] Nenhum padrão novo foi introduzido
- [ ] Estrutura reflete fielmente os schemas
- [ ] Atributos XML foram aplicados corretamente
- [ ] INTEROP segue o padrão existente
- [ ] Todos os `summary` foram criados
- [ ] Tipos existentes foram reutilizados quando possível
- [ ] Não há duplicação evitável
- [ ] `Signature.cs` foi reutilizada quando aplicável
- [ ] Testes foram criados no padrão correto
- [ ] Somente os testes criados foram executados
- [ ] Todos os testes criados passaram
- [ ] Código compila sem warnings relevantes

---

## Saída esperada

Ao finalizar, responda somente com um relatório objetivo contendo:

```text
Documento implementado:
- {Documento}

Arquivos criados/modificados:
- ...

Estruturas implementadas:
- ...

Reaproveitamento de código:
- ...

Testes executados:
- ...

Resultado:
- ...
```

Não explique passo a passo.

Não inclua introduções.

Não inclua justificativas longas.

Não descreva o processo de execução.