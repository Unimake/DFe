# Comando para executar no Codex do projeto PRO

Adapte o diagnóstico de disponibilidade do UniNFe PRO para o contrato atual da DLL Unimake.DFe. O usuário decidiu remover completamente essa rotina do UniNFe legado; somente o PRO continuará usando o recurso. Não altere o repositório do legado.

Repositório desta tarefa: `C:\projetos\UniNFe.Desktop`.
Fonte e evidências da DLL: `C:\projetos\github\Unimake.DFe`.

Leia primeiro o AGENTS.md e as skills aplicáveis do PRO. Leia também estes arquivos da DLL:

1. `docsplan\diagnostico\MIGRACAO-SESSOES.md` — contrato recomendado, alterações desta migração e compatibilidade;
2. `docsplan\diagnostico\COBERTURA.md`, `INTERPRETACAO.md`, `INVENTARIO.md` e `VALIDACAO.md` — cobertura, fontes, limitações e testes;
3. o código atual de `Utility\Disponibilidade`, especialmente `CanalObservacoesDisponibilidade`, `CapacidadesDiagnosticoDisponibilidade` e `DiagnosticoDisponibilidadeDFe`, sob `source\.NET Standard\Unimake.Business.DFe`.

Confirme tudo no código e nos testes atuais. Os documentos anteriores do PRO são contexto histórico, não prova de cobertura concluída.

O que a DLL já implementou:

- Canal de sessões amplo, com interpretação dos retornos na DLL, falhas de transporte/autenticação/preparação e limitações explícitas. Não exige cStat genérico em NFS-e/eSocial/Reinf/GNRE/CIOT.
- Ampliação de contratos de NFS-e municipal/nacional, política GNRE e CIOT e verificação limitada do envelope de PDFs. Não confundir isso com autorização fiscal, validação integral do PDF ou interpretação universal de códigos.
- Capacidades para observação passiva e status oficial. Ausência de status não impede observar a operação real.
- `sessao.Concluir(retornoXml, httpStatusCode)` de dois parâmetros como API recomendada. O overload que recebe booleano de reconhecimento ficou obsoleto, mantido para transição.
- `DiagnosticoDisponibilidadeDFe.VerificarStatusServico()` para verificação manual sem incorporar o histórico passivo anterior. Mantém cache, correlação de sondas, certificado, proxy e bloqueio de consumo indevido. Sem status oficial não executa rede.
- Sessões da mesma configuração não alimentam também o histórico antigo. Consumo indevido continua bloqueando status mesmo se a fila de observações estiver cheia; a nova API suprime sondas antes de preparar certificado quando o contexto está bloqueado.
- APIs de histórico anteriores e `HistoricoLegado` ficaram obsoletas, preservando assinaturas e enums para transição. Não use essas APIs como fonte do Dashboard nem como fallback silencioso.

Implemente no PRO:

1. Rastreie os caminhos reais de observação em `source\UniNFe.Services\Disponibilidade`, nas fábricas de Configuracao, nos processadores XML e nos serviços concretos. Abra uma sessão por tentativa lógica, envolvendo construção/preparação que possa autenticar e execução. Vincule a configuração efetiva antes do primeiro transporte. Preserve a operação fiscal e a exceção original. Não duplique observações em wrappers aninhados e não interprete um serviço composto como um único request HTTP.
2. Em `ObservacaoDFe`, use o overload de Concluir com dois parâmetros. Remova a interpretação de respostas criada exclusivamente para disponibilidade no consumidor e o envio de `_respostaReconhecida`; mantenha os parsers e validações necessários ao processamento fiscal normal. A DLL é a fonte da semântica. Contratos desconhecidos ficam inconclusivos.
3. Propague, persista, consulte e apresente de forma coerente os campos `ContratoResposta`, `CodigoRespostaAplicacao`, `RespostaAplicacaoReconhecida`, `EntradasTransporte`, `IntegracaoRemota` e `Limitacao`, além dos campos atuais. Faça as migrações necessárias sem fabricar metadados para registros antigos. Não transforme todo código em cStat. Separe utilitários externos de serviços fiscais. Revise operacaoId/contextoId para garantir idempotência e isolamento de empresa/acesso/configuração sem expor credenciais.
4. Substitua listas fixas de documentos e decisões duplicadas por capacidades da DLL onde for aplicável. Não confunda capacidade com disponibilidade atual. Revise pedidos, API, worker, DTOs e Dashboard para que todos os serviços observados possam ser representados, inclusive sem sonda manual. Não crie destino/endpoint/serviço fictício a partir de um enum.
5. Na verificação manual, substitua `ConsultarStatusServico()` por `VerificarStatusServico()`. O resultado novo não contém histórico passivo antigo; retire adaptações que só compensavam essa mistura. Preserve a execução durável, exclusividade, cache/limites, revisão de acesso e os bloqueios persistentes do PRO. Para os novos registros, use TipoFalhaDisponibilidade.ConsumoIndevido da DLL, não uma nova lista local de cStat 656/678: HTTP429 também pode produzir essa classificação. Confira o alcance da supressão de status por documento/ambiente, inclusive entre UFs, sem tratar isso como autorização para repetir operações de negócio. Não execute sondas dentro da operação de emissão nem repita negócio para verificar disponibilidade. Não limpe memória/cache da DLL a cada empresa, pedido, atualização do Dashboard ou incidente.
6. Na agregação temporal e apresentação, use os estados/falhas/limitações da DLL, preservando as regras de escopo e evidência do PRO. Resposta de negócio reconhecida não é documento autorizado; resposta de status não prova recuperação de autorização, evento ou download. Falha de certificado/autenticação/proxy não deve ser atribuída automaticamente ao órgão remoto. Reconhecimento de formato pode coexistir com resultado inconclusivo.
7. Preserve os dados sensíveis: nenhuma persistência de XML, corpo remoto, exceção bruta, token, credencial, certificado/PIN ou URL sensível. Os novos campos devem vir da fotografia sanitizada da DLL, sem enriquecer com mensagens remotas brutas.

Pacote — condição necessária antes de validar a integração:

- Confira a versão exata restaurada, ProductVersion e conteúdo do pacote. Na última consulta eram `20261108.1207.21` e ProductVersion `20261108.1207.21+e046f702936f83b5c77efaca1068e9300c9dbb94`. Os DryRuns na DLL não publicaram nem persistiram uma versão instalável.
- Esse pacote é baseado em uma base fiscal anterior ao HEAD da DLL, com patches de diagnóstico. Não troque diretamente por um pacote do HEAD nem inclua alterações fiscais independentes nesta migração.
- Prepare uma base isolada fiscalmente compatível e aplique somente os patches necessários de diagnóstico/transporte/sanitização e testes. Se necessário, use checkout/worktree isolado da DLL, preservando a árvore de trabalho existente. Confira também as dependências dos patches já commitados de sessões, associação de configuração, timeout REST e eventos CT-e/MDF-e. Não copie schemas/modelos fiscais indiscriminadamente nem adicione novos documentos fiscais à base apenas para fazer compilar. Registre qualquer capacidade que dependa de documento ausente nessa base.
- Gere um pacote offline novo e imutável exclusivamente pelo fluxo oficial `source\packDFeOffline.bat` / `source\Packaging\Pack-DFe.ps1`, conforme AGENTS.md da DLL. Valide assets somente netstandard2.0 e o hash de GeradorCIOTShared. Nunca substitua versão existente, publique em feed externo ou use ProjectReference.
- Atualize o PRO para essa versão offline exata e confira o asset efetivamente restaurado. Não faça commit da referência offline enquanto a versão não existir no nuget.org. Se houver uma dependência fiscal inseparável, apresente o ponto concreto antes de executá-la e continue o trabalho independente; não trate uma atualização fiscal ampla como autorização implícita desta tarefa.

Crie e execute testes determinísticos, sem rede externa, certificado real ou A3, para coleta única e retorno/exceção preservados, todos os novos campos e registros antigos, códigos sem cStat, respostas desconhecidas/inconclusivas, autenticação, escopo de contexto, capacidade sem sonda, separação manual/passivo, bloqueio de consumo indevido, persistência/idempotência e não reexecução de negócio. Execute os builds e testes proporcionais do PRO com o pacote novo efetivamente restaurado, conforme suas instruções. Não use a suíte externa como substituta nem enfraqueça testes para obter aprovação.

Trabalhe autonomamente em blocos, com autorrevisão e correção dos defeitos encontrados. Entregue alterações implementadas, versão/base fiscal/ProductVersion usados, comandos/resultados de testes/builds, migrações de dados, limitações reais e pendências concretas. Não encerre somente com análise. Não faça commit nem push sem solicitação.
