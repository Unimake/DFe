# Unimake.DFe

> 📦 Biblioteca open source para emissão de documentos fiscais eletrônicos (NFe, NFCe, MDFe, CTe, NFSe, GNRE, EFDReinf, etc.) — com suporte multiplataforma e multi linguagem.

[![NuGet](https://img.shields.io/nuget/v/Unimake.DFe.svg?logo=nuget)](https://www.nuget.org/packages/Unimake.DFe/)
[![Documentação](https://img.shields.io/badge/docs-Wiki-blue)](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](#)
[![Discord](https://img.shields.io/discord/936980012257652788?label=Discord&logo=discord&logoColor=white&color=5865F2)](https://discord.gg/JhCEhbfK)

---

## 📚 Sumário

- [Sobre](#sobre)
- [Documentação](#documentação)
- [Como Usar](#como-usar)
  - [C#](#c)
  - [WinDev](#windev)
  - [xHarbour / Harbour](#xharbour--harbour)
  - [Visual FoxPro (VFP)](#visual-foxpro-vfp)
  - [VB6 / VBS / VB.NET](#vb6--vbs--vbnet)
  - [PHP](#php)
- [Breaking Changes](#breaking-changes)
- [Veja Também](#veja-também)

---

## 📦 Sobre

**Unimake.DFe** é uma DLL desenvolvida para facilitar a integração de softwares ERP com os sistemas de documentos fiscais eletrônicos da SEFAZ. Suporta múltiplos tipos de documentos:

- NFe / NFCe
- MDFe / CTe
- NFSe / GNRE
- EFDReinf
- Entre outros

---

## 📄 Documentação

- 📖 **Manual Geral**: [wiki.unimake.com.br](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe)
- 📚 **API de Classes**: [Documentação técnica](https://www.unimake.com.br/uninfe/docdll/api/index.html)

---

## 🚀 Como Usar

### C#
- [🔗 Exemplos no GitHub](https://github.com/Unimake/DFe/tree/main/Exemplos/CSharp)
- [🎥 Vídeos de treinamento](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe/VideosCsharp)

### WinDev
- [🔗 Exemplos](https://github.com/Unimake/DFe/tree/main/Exemplos/Windev)
- [🎥 Vídeos](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe/VideosWindev)

### xHarbour / Harbour
- [🔗 Exemplos](https://github.com/Unimake/DFe/tree/main/Exemplos/xharbour)
- [🎥 Vídeos](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe/VideosxHarbour)

### Visual FoxPro (VFP)
- [🔗 Exemplos](https://github.com/Unimake/DFe/tree/main/Exemplos/VFP)
- [🎥 Vídeos](https://wiki.unimake.com.br/index.php/Manuais:Unimake.DFe/VideosVisualFoxPro)

### VB6 / VBS / VB.NET
- [🔗 VB6](https://github.com/Unimake/DFe/tree/main/Exemplos/VB6)
- [🔗 VBS](https://github.com/Unimake/DFe/tree/main/Exemplos/VBS)
- [🔗 VB.NET](https://github.com/Unimake/DFe/tree/main/Exemplos/VBNET)

### PHP
- [🔗 Exemplos](https://github.com/Unimake/DFe/tree/main/Exemplos/PHP)

---

## ⚠️ Breaking Changes

> Antes de atualizar o pacote via NuGet, consulte as mudanças que podem afetar o funcionamento do seu projeto:

📄 [Arquivo de mudanças](https://github.com/Unimake/DFe/blob/main/Breaking-Changes.md)  
📦 [NuGet: Unimake.DFe](https://www.nuget.org/packages/Unimake.DFe)

---

## 🖨️ Veja Também

Pacote para impressão de DANFE, DACTE, DAMDFE, entre outros:

📘 [Unimake.Unidanfe – Manual](https://wiki.unimake.com.br/index.php/Manuais:Unimake.Unidanfe)

---

## 🏢 Sobre a Unimake

https://www.unimake.com.br

<!-- DEVPLANNER:START -->
## Planejamento DevPlanner - NF-e ABI

### Leitura obrigatória do DEV

Antes de qualquer execução, leia `AGENTS.md`, `docsplan/nfabi/CODEX-START-HERE.md`, `docsplan/nfabi/planning/PROJECT-BRIEF.md`, `docsplan/nfabi/plans/PDCA.md`, o plano e o manifesto da etapa autorizada. A pasta documental externa `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi` deve ser relida no Plan e no Check de toda etapa `002+`; mudança de hash interrompe a execução e exige análise de impacto.

### Como o plano funciona

`ABI-000` entrega esta base; sua aprovação permite somente a revisão `ABI-001`. Produto começa em `ABI-002` apenas depois da aprovação da `001`. Somente o DEV marca `APPROVED`, e cada etapa para em `DELIVERED_FOR_REVIEW`.

O plano NF-e ABI é exclusivo da DLL Unimake.DFe e dos testes pertencentes a este repositório. Produtos consumidores externos possuem planejamento próprio e ficam integralmente fora deste fluxo.

### Como executar uma etapa

Use linguagem explícita: `Execute somente ABI-NNN`. Para aprovar, use `Aprovo ABI-NNN`. Para reprovar, use `Reprovei ABI-NNN: motivo`. Para retomar, use `Retome somente ABI-NNN`.

### O que revisar antes de aprovar

Confira incremento demonstrável, diff restrito, build/testes, arquivos ERP quando aplicável, hashes da documentação oficial, rollback, limitações e dossiê imutável em `docsplan/nfabi/plans/evidence/`.

### Reprovação, bloqueio e retomada

Reprovação cria `REWORK` com motivo e novo AttemptId. Falta de decisão, certificado, endpoint publicado ou documento coerente resulta em `BLOCKED`, preservando checkpoint. Aprovação e início da sucessora são transições separadas.

### Fontes de verdade

- Estado: `docsplan/nfabi/plans/PDCA.md`.
- Decisões: `docsplan/nfabi/plans/DECISION-REGISTER.md`.
- Riscos: `docsplan/nfabi/planning/RISK-REGISTER.md`.
- Modelos: `docsplan/nfabi/plans/MODEL-CATALOG.md`.
- Evidências: `docsplan/nfabi/plans/evidence/`.

### Validação do plano

Execute `pwsh -NoProfile -File .agents/skills/plan-linter/scripts/test-plan.ps1 -RepositoryRoot .`. Resultado diferente de zero impede entrega ou aprovação.
<!-- DEVPLANNER:END -->
