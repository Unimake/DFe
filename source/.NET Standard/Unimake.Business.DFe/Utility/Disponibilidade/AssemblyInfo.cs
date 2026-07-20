// Este arquivo libera os tipos internos do diagnóstico para o projeto de testes.
// Isso permite testar regras de cache, classificação e sanitização sem expor
// essas peças internas para quem utiliza a DLL em produção.
using System.Runtime.CompilerServices;

// Permite que o assembly de testes acesse classes e métodos internos do motor
// de disponibilidade. Essa autorização existe somente para testes e não cria
// uma nova API pública para os desenvolvedores consumidores da DLL.
[assembly: InternalsVisibleTo("Unimake.DFe.Test")]
