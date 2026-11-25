# Roteiro de consultas da AACBS-004

Usa a configuração privada já preparada para Produção Beta e o cache/journal cifrado da AACBS-003. Executar em PowerShell 7.6 ou superior, com a saída completa do build da etapa. O desenvolvedor continua fornecendo XML à DLL; este roteiro atua somente como consumidor local de testes.

Quando RepositorioLocal não é informado, o roteiro resolve o checkout principal pela lista de worktrees do Git, como na AACBS-003. Assim, executar a entrada em um worktree vinculado mantém o acesso e o estado operacional no mesmo local padrão. RepositorioLocal explícito continua selecionando o repositório indicado.

Prova offline, sem importar configuração real nem consumir APIs:

```powershell
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS004.ps1 -Offline -Dll 'source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll'
```

Consulta real de uma família autorizada:

```powershell
pwsh -NoProfile -File docsplan/aacbs/scripts/Executar-AACBS004.ps1 -Consultar -Familia Creditos -Dll 'source/Unimake.DFe.Test/bin/Debug/net10.0/Unimake.Business.DFe.dll' -GuardarXmlFinal -SemAguardar
```

Famílias permitidas: Creditos, Pagamentos e Recolhimentos. A primeira chamada abre no máximo uma solicitação e guarda o tíquete antes de prosseguir. Com SemAguardar, se a janela de 120 segundos ainda não chegou, retorna JanelaAguardando. Repetir a mesma chamada depois da janela retoma o tíquete existente; não abre outra solicitação. Sem SemAguardar, o consumidor aguarda dentro dos limites de consultas e duração já configurados. A DLL não agenda nem aguarda.

As três famílias compartilham cache/cota de autenticação e mutex por credencial com a AACBS-003. Abre-se no máximo uma solicitação por API na janela móvel de 24 horas, inclusive se mudar a empresa representada. O journal conserva as execuções antigas de débitos e separa cada família/empresa/ambiente. NovaSolicitacao é uma decisão explícita e respeita os limites; abertura ambígua continua sem replay. Resultados finalizados não geram requests adicionais. Esses limites são do executor, não cotas oficiais inferidas.

GuardarXmlFinal grava somente o XML validado, cifrado por DPAPI, sob .local/aacbs/execucoes. Nenhum token, XML real ou link assinado integra a evidência versionada. Os modos Consultar e Offline explicitamente falsos são recusados antes de importar o módulo. O modo padrão é Offline.

O roteiro não possui modo de geração de DARF. Os serviços RAD e PCONT são validados com transporte controlado. A emissão real exige confirmação do DEV para o cenário e os parâmetros concretos, antes de qualquer request de geração. Não há pagamento, download de PDF ou tentativa de emissão para descobrir um caso válido.
