---
name: atualizar-layout-txt-nfe
description: Recria e confere o Markdown do layout TXT de NF-e/NFC-e a partir do NFeTxtLayoutCatalog da DLL Unimake.DFe. Use quando o usuário chamar esta skill ou pedir para atualizar a referência dos segmentos TXT.
---

# Atualizar layout TXT de NF-e/NFC-e

Ao ser chamada, execute a atualização completa sem solicitar caminho, versão ou formato ao usuário.

- A fonte única é `source/.NET Standard/Unimake.Business.DFe/Xml/NFe/Txt/NFeTxtLayoutCatalog.cs` neste repositório. Não use o catálogo legado do UniNFe, o XSD nem massas de teste para compor o layout.
- O destino é `LAYOUT-TXT-NFE-NFCE.md` na raiz deste repositório. Preserve o formato atual: grupos alfabéticos, linhas `BLOCO|tag|tag|...|` em blocos `text` e identificação `# CHAVE` imediatamente antes das variantes. O `§` é um marcador interno e não entra no Markdown.
- Execute `scripts/Update-LayoutTxtNFe.ps1` desta skill com PowerShell para recriar o arquivo. Em seguida execute o mesmo script com `-Check`; este modo apenas compara o arquivo com o catálogo e falha se houver divergência.
- Inspecione o diff do Markdown, informe quantos layouts foram documentados e entregue o link do arquivo. Se a saída for idêntica, diga que o arquivo já estava atualizado.
- Se o catálogo contiver uma entrada que o script não reconheça, pare e informe a limitação; não entregue um layout parcial. Ajuste o script somente quando for possível representar a nova entrada fielmente.
- Não altere o catálogo, o conversor, o UniNFe nem testes por causa desta tarefa. Não execute a suíte unitária para uma atualização exclusivamente documental.

O script resolve a raiz do repositório a partir de seu próprio caminho; não requer argumentos na chamada normal.
