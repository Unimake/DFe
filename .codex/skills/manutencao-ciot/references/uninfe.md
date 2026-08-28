# Integração CIOT no UniNFe

Leia este arquivo quando a manutenção atingir configuração, tarefas ou roteamento de arquivos do UniNFe em `C:\projetos\github\UniNFe`.

## Configuração e autenticação

- A empresa guarda `EFreteIntegrador`, `EFreteToken`, `EFreteUsuario` e `EFreteSenha` no `UniNfeConfig.xml` de forma criptografada.
- A UI mostra o grupo eFrete somente para `TipoAplicativo.CIOT` e `TipoAplicativo.Todos`; token e senha são mascarados.
- Configurações antigas sem essas tags carregam strings vazias.
- Validação da UI: integrador sozinho permite certificado; token exige integrador; usuário e senha devem ser informados juntos e exigem integrador; token pode coexistir com credenciais e tem prioridade.
- `TaskCIOTBase.CriarConfiguracao()` copia credenciais, ambiente, certificado e proxy para a `Configuracao` da DLL.
- Não copie de volta para a empresa o token obtido por login.
- O UniNFe não escolhe o provedor na configuração da empresa. A fonte autoritativa continua sendo `<ProvedorCIOT>` no XML operacional.

## Transparência dos retornos

- As tarefas CIOT existentes reconhecem autorização/sucesso pelo contrato ANTT, inclusive `Codigo == "110"`.
- Mantenha essa regra genérica. A DLL deve normalizar um sucesso eFrete para `Codigo=110`, mensagem e grupo de mensagens compatíveis.
- Não adicione ramificações eFrete nas tarefas para interpretar JSON, ausência de código ou identificadores com `/XXXX`.
- O CIOT retornado pela DLL deve ter 12 caracteres válidos para nome de arquivo e para cancelamento/encerramento.

## Arquivos e serviços exclusivos

- Cadastros eFrete usam `CIOTCadastro` e o sufixo `-cadciot.xml`.
- Retorno de cadastro usa `-ret-cadciot.xml`; erro usa `-ret-cadciot.err`.
- As raízes `GravarProprietario`, `GravarVeiculo` e `GravarMotorista` têm tasks próprias e reutilizam `TaskCIOTBase.CriarConfiguracao()`.
- Não direcione esses serviços para ANTT; a DLL deve recusá-los antes do transporte quando o provedor não for eFrete.

## Build BETA e testes

- Em `Debug`/BETA, confirme que os projetos usam `ProjectReference` para o checkout local da Unimake.DFe, não uma DLL NuGet antiga.
- Teste reconhecimento da raiz, task, extensão, propagação das credenciais e geração de retorno/erro.
- Depois de testes CIOT da DLL, compile `C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj` em `Debug` e execute o filtro CIOT correspondente.
- Quando o problema ocorrer somente no executável BETA, confira a DLL efetivamente copiada para `source\uninfe\bin`, mas não trate data do EXE como prova da versão da DLL.
