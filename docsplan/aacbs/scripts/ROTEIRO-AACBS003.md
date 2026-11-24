# Roteiro local da AACBS-003

Execute no Windows com PowerShell 7.6 ou superior para usar a saída `net10.0` dos testes. O preparador existente cria o `PSCredential` protegido por DPAPI. Execute o roteiro com o mesmo usuário e computador que prepararam esse acesso. O roteiro usa conexão direta. [Acesso local](../ACESSO-LOCAL.md)

## Verificação offline

Os switches de modo explicitamente informados precisam ter valor verdadeiro. `-Autenticar:$false` e `-Debitos:$false` são rejeitados antes de acessar a configuração local, inclusive quando passados por splatting. Sem um modo informado, permanece a verificação local padrão.

Execute a partir da raiz do repositório. O primeiro comando usa somente dados sintéticos, valida persistência DPAPI, cotas, retomada e ordem das operações simuladas. Acrescente `-Dll` para executar os cinco serviços com transporte controlado e respostas JSON sintéticas, no mesmo contexto de carga usado pelo roteiro real. Use a saída completa do build dos testes com `Unimake.DFe.Test.dll` e as dependências adjacentes. O modo `-Verificar` importa e valida a configuração e a credencial locais. Ambos os modos executam sem tráfego HTTP.

```powershell
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Offline
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Offline -Dll 'source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll'
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Verificar
```

## Execução real autorizada

Produção Restrita fica bloqueada antes de qualquer request até a qualificação das rotas oficiais AACBS e do emissor aplicável. A seleção explícita de `ProducaoBeta` e `permitirProducao=true` são condições para o acesso de produção. Use apenas a credencial e a matriz habilitadas para o ambiente autorizado. A prova offline precede a execução real. [Contratos de acesso e fluxo](../contratos/ACESSO-E-FLUXO.md)

Execute cada comando em um novo processo do PowerShell. `-Autenticar` usa o cache local cifrado e emite um token somente quando necessário. `-Debitos` abre a primeira solicitação da empresa ou retoma seu estado persistido. Depois de uma confirmação, o roteiro executa as consultas de situação, de link e de download como chamadas independentes à DLL.

```powershell
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Autenticar -Dll 'source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll'
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS003.ps1 -Debitos -Dll 'source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll'
```

- O checkout principal hospeda `.local/aacbs`, inclusive quando o roteiro roda em um worktree ligado a ele. Entre clones independentes, indique sempre o mesmo checkout que contém a credencial e o estado usando `-RepositorioLocal`. O bloqueio por emissor e ClientID permite um executor por conta Windows. O cache separa ambiente e revisão do segredo. O orçamento de duas emissões em uma janela móvel de 60 minutos inclui falhas e abrange a troca de segredo e de empresa representada.
- A janela móvel de 24 horas permite uma abertura de débitos por credencial, incluindo tentativas interrompidas. `-NovaSolicitacao` exige que a solicitação anterior tenha situação terminal conhecida e que a janela local permita outra abertura. Um POST ambíguo sem tíquete exige reconciliação externa. Preserve o journal para manter essas garantias entre execuções.
- O acompanhamento começa 120 segundos após a abertura e mantém pelo menos 120 segundos entre consultas. A contagem persistida respeita o menor limite configurado até 120 consultas e 240 minutos. Ao esgotar o acompanhamento, o roteiro preserva o tíquete. O limite de 48 horas para o arquivo usa o instante da abertura como referência conservadora quando não há aviso de pronto por webhook.
- A consulta de link concluída segue para uma tentativa de download. A URL assinada permanece em memória e é descartada após a tentativa ou expiração. Interrupção com link ou download incerto exige reconciliação antes de uma nova tentativa. Com `-GuardarXmlFinal`, o roteiro cifra exatamente o XML de retorno validado pela DLL em `.local/aacbs/execucoes`, preservando as referências XML que representam `CR` e `CRLF`.
- As observações registram somente horário, ambiente, operação, estado, HTTP recebido e uma correlação local opaca. Mantenha os arquivos operacionais na pasta ignorada pelo Git. Use somente as observações sanitizadas nas evidências da etapa.
