# Acesso local e testes reais AACBS

Revisão de 24/11/2026. Após preparar as chaves, o DEV aprovou a AACBS-000 e liberou a AACBS-001. O modo `-Verificar` passou no computador atual, sem exibir valores ou solicitar token. O ambiente informado pelo DEV é Produção Restrita. A autorização remota e os fluxos fiscais ainda não foram testados.

## Preparar uma vez no computador de desenvolvimento

Executar no PowerShell do Windows, usando o mesmo usuário que executa o Codex:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "C:\projetos\github\Unimake.DFe\docsplan\aacbs\scripts\Configurar-AcessoLocal.ps1"
```

O script pedirá o CNPJ completo da matriz, o ambiente da credencial e uma credencial: preencher **Usuário com ClientId** e **Senha com ClientSecret**. Não enviar esses valores ao chat nem colocá-los em argumentos de comando. O script não pede certificado e não faz chamadas HTTP.

Serão criados:

| Arquivo local | Conteúdo |
| --- | --- |
| `.local/aacbs/credenciais.local.clixml` | PSCredential: ClientId no identificador e ClientSecret cifrado pelo DPAPI do Windows. |
| `.local/aacbs/configuracao.local.json` | CNPJ, ambiente e limites do roteiro; não contém as chaves. |

A pasta inteira está ignorada em `.gitignore`, inclusive futuros tíquetes, cache e estado de execução. O segredo cifrado só pode ser recuperado pelo mesmo usuário no mesmo computador. Isso protege o ClientSecret em repouso sem depender de uma restrição de ACL não comprovada. ClientId e metadados não são cifrados; não compartilhar a pasta nem copiar seu conteúdo para logs/evidências. [Proteção documentada pela Microsoft](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-clixml?view=powershell-7.5)

Para conferir a preparação, sem exibir valores e sem acessar a Receita:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "C:\projetos\github\Unimake.DFe\docsplan\aacbs\scripts\Configurar-AcessoLocal.ps1" -Verificar
```

O preparador e o roteiro verificam os mesmos campos, tipos, limites e credencial DPAPI. O resultado confirma a leitura local. A autorização remota depende do ambiente e do serviço.

Para substituir o par, execute o preparador com `-AtualizarCredenciais`. Ele valida os dois arquivos temporários, retira o JSON anterior, substitui a credencial e publica o JSON novo por último. Se a atualização for interrompida após retirar o JSON, o acesso ficará bloqueado. Execute o preparador novamente com `-AtualizarCredenciais` e informe o par pretendido. As configurações existentes continuam legíveis nos caminhos originais.

Consumidores locais devem obter os dois valores juntos com `Read-LocalAccess`. No Windows, o leitor mantém o JSON aberto até terminar a importação da credencial, impedindo a troca do par durante essa leitura.

## Ambiente e autorização de operações

Escolher `ProducaoRestrita` se a credencial/cenário for de testes nesse ambiente, ou `ProducaoBeta` se for esse o acesso obtido. O preparador habilita produção apenas quando o DEV escolhe explicitamente `ProducaoBeta`. No conteúdo público conferido em 24/11, as rotas de negócio de produção restrita não estão publicadas; isso não prova que o ambiente foi encerrado. A equipe deve qualificar a URL oficial antes de qualquer request. Não trocar prefixos, testar URLs candidatas ou usar produção como fallback.

A configuração começa com testes reais habilitados, orçamento conservador de uma abertura por API por dia e geração de DARF desabilitada. O limite local não representa saldo oficial; a FAQ publica até quatro aberturas/dia/credencial para as quatro consultas. Para acompanhar solicitações, o roteiro propõe 120 segundos entre consultas, no máximo 120 consultas e 240 minutos; são limites locais, não uma frequência garantida pela Receita. [FAQ oficial](https://docs.receitafederal.gov.br/apuracao-cbs/faq.html)

O roteiro de consultas exige `permitirGeracaoDarf=false` e `cenariosDarf=[]`. A autorização pontual da RAD-01 foi consumida em uma tentativa registrada como indeterminada, com repetição bloqueada. PCONT aguarda cenário elegível e autorização específica do DEV. [Condições de retomada](ENCERRAMENTO-TECNICO.md#condições-de-retomada-e-responsáveis)

## Autonomia do executor e limites da entrega atual

Depois da configuração e da liberação da etapa técnica, o executor pode autenticar, executar os cenários previstos, aguardar de forma limitada e verificar XML/XSD/`Result`, sem solicitar participação do DEV em cada chamada. O orquestrador encaminha correções ao implementador/autor dos testes; o executor repete os checks afetados sobre o novo checkpoint estável, sem editar código ou testes. As aprovações humanas entre etapas continuam vigentes. O fornecimento de chaves não aprova automaticamente AACBS-000 nem libera a implementação.

O preparador apenas cria/verifica os arquivos locais. O leitor e o roteiro da AACBS-003 estão descritos em [ROTEIRO-AACBS003.md](scripts/ROTEIRO-AACBS003.md). A [pesquisa oficial de ambientes](evidencias/AACBS-003/tentativa-0001/PESQUISA-AMBIENTES.md) mantém a Produção Restrita bloqueada por falta de rota confirmada; não houve mudança automática da configuração. Não há promessa de ausência absoluta de bugs.

O roteiro deve:

- Importar a credencial cifrada dentro do processo e preencher a configuração em memória; não imprimir o objeto, exportar segredo em texto aberto ou passá-lo nos argumentos do runner. A DLL de produção continua recebendo `Configuracao` do consumidor e não procura arquivos particulares no disco.
- Reutilizar token válido em cache cifrado entre execuções e coordenar um único executor por identidade. O manual Receita Integra 1.4 limita emissão a dois tokens/hora e informa validade de uma hora. Reiniciar o teste não autoriza solicitar outro token nem repetir um POST. [Manual oficial](https://cgibs.gov.br/upload/arquivos/202609/05164748-08-manual-de-integracao-tecnica-receita-integra-v-1-4.pdf)
- Manter em `.local/aacbs/execucoes` o estado necessário à retomada: abertura iniciada, resultado confirmado/ambíguo, tíquete, família, ambiente e contadores. Não registrar chaves; tokens e URLs temporárias exigem proteção cifrada ou recuperação pela operação oficial quando permitida.
- Retomar um tíquete conhecido depois de reinício, sem reabrir a consulta. Se houve timeout antes de obter o tíquete, manter resultado ambíguo e interromper o reenvio automático. Essa situação pode exigir ação externa; não presumir que o POST falhou.
- Interromper por configuração inválida, cota/HTTP429, contrato incompatível, falta de permissão, limite de espera ou efeito não autorizado; registrar apenas diagnóstico sanitizado e a condição de retomada.
- Usar sentinelas em testes offline para provar ausência de chaves, corpo fiscal e URL temporária em trace/exceções **antes** de introduzir credenciais reais.

O aceite é uma matriz de contratos e cenários com resultados comprovados, incluindo limitações reais. Credencial inválida/revogada, permissão ausente, fonte contraditória ou indisponibilidade externa podem exigir intervenção do DEV ou da Receita; não serão contornadas por repetição ou por declaração de sucesso.
