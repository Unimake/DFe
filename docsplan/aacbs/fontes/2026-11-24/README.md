# Baseline oficial da AACBS-001

Arquivos públicos obtidos em 24/11/2026, sem autenticação ou consumo de APIs de negócio. O [MANIFESTO-FONTES.csv](MANIFESTO-FONTES.csv) registra URL de origem, tamanho, SHA-256 e instante UTC de captura de cada arquivo.

O conjunto contém 11 páginas HTML do catálogo CBS, seis JSON Schemas de retorno e dois PDFs oficiais: Receita Integra 1.4 e manual DeRE 1.0.2. O último é fonte complementar exclusivamente para o POST do mesmo Authorization Server Receita Integra; não transporta regras fiscais, assinatura ou payload DeRE para CBS. Dois comunicados oficiais da RFB e os textos das RFCs 6749/6750 complementam habilitação, produção restrita e protocolo OAuth, totalizando 23 fontes no manifesto.

Os arquivos são a baseline documental, não schemas XML da DLL. As referências e versões estão em [FONTES-E-REQUISITOS.md](../../FONTES-E-REQUISITOS.md); as propostas de contrato em [ESPECIFICACAO.md](../../contratos/ESPECIFICACAO.md).

Para revalidar integridade local, ler cada linha do manifesto e comparar `Get-FileHash -Algorithm SHA256` e tamanho com o arquivo indicado. Antes de uma etapa dependente, revisar a fonte pública novamente; se mudou, comparar conteúdo e registrar impacto. Um rótulo de versão igual não comprova igualdade dos arquivos. Não substituir a baseline aprovada silenciosamente para fazer um teste passar.

O download pelo shell exigiu acesso de rede fora do sandbox; as solicitações se limitaram aos documentos públicos listados, sem credenciais ou endpoints de negócio. A configuração privada `.local/aacbs` não integra este diretório, o manifesto ou o pacote da biblioteca.
