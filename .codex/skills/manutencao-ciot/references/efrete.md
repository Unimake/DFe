# Contrato eFrete e compatibilidade ANTT

Leia este arquivo ao alterar qualquer parte da eFrete ou uma infraestrutura compartilhada pelo CIOT.

## Mapa da implementação

- Classes públicas comuns: `source/.NET Standard/Unimake.Business.DFe/Xml/CIOT`.
- Serviços públicos e base: `source/.NET Standard/Unimake.Business.DFe/Servicos/CIOT`.
- Implementação isolada: `Servicos/CIOT/Provedores/EFrete`.
- Endpoints embutidos: `Servicos/Config/CIOT/EF.xml`.
- Schemas específicos: `Xml/Schemas/CIOT/EFrete`, resolvidos por `EFreteSchemaResolver`.
- Testes principais: `Unimake.DFe.Test/CIOT/Servicos/EFreteIntegracaoTest.cs`, `EFreteFluxoExecucaoTest.cs` e suítes de schema/serialização do CIOT.

## Seleção do provedor

- A tag `<ProvedorCIOT>` existe somente nos nove XMLs comuns de envio e nos três cadastros eFrete.
- Nos XMLs comuns ela é opcional; ausente significa obrigatoriamente ANTT, ainda que a configuração reutilizada contenha eFrete.
- Nos serviços exclusivos eFrete, informe `EFrete`; ausência ou `ANTT` deve falhar antes do transporte.
- A seleção precisa ocorrer antes de `Inicializar(...)`, porque define configuração, endpoint, autenticação, schema e payload.
- Ao reutilizar uma instância/configuração, limpe o provedor interno e invalide a configuração carregada quando houver troca.
- A tag não pertence a nenhum retorno nem a nenhum JSON transmitido.

## Serviços

Equivalentes eFrete implementados:

- declaração → `AdicionarOperacaoTransporteV2`;
- consulta por `MatrizCNPJ` e `IdOperacaoCliente`;
- cancelamento;
- encerramento;
- consulta de situação e consulta de frota, ambas sobre `ConsultaSituacaoTransportador`;
- cadastros explícitos `GravarProprietario`, `GravarVeiculo` e `GravarMotorista`.

Não suportados na API eFrete 8.1:

- `GerarIdOperacaoTransporte` — a emissão começa na declaração;
- retificação;
- consulta de exceção.

Recuse os não suportados antes de carregar endpoint ou enviar requisição. Não simule equivalência.

Nos cadastros, a ordem operacional recomendada é proprietário, veículo, motorista e declaração. A DLL não deve fazer cadastros ocultos durante a declaração.

Contratos atuais dos cadastros:

- motorista: endpoint `motoristas/gravar`, JSON versão 2; CPF, CNH, data de nascimento, nome, endereço e celular são obrigatórios;
- proprietário: endpoint `proprietarios/gravarV2`, JSON versão 4; CPF/CNPJ é transportado no campo histórico `CNPJ`, com RNTRC, razão social e endereço obrigatórios; telefones são opcionais;
- veículo: endpoint `veiculos/gravar`, JSON versão 1; somente veículo, chassi, número de eixos, placa, RNTRC e Renavam são obrigatórios; anos, capacidades, tara, cor, marca, modelo, município, carroceria e rodado permanecem opcionais.

Nos retornos dos cadastros, faça mapeamento explícito dos objetos quando enums, nulos ou formatos da API não coincidirem com a serialização XML. Não transforme opcionais ausentes em tags com zero, enum inválido ou objeto vazio.

## Autenticação e transporte

- `EFreteIntegrador` é sempre necessário para usar eFrete.
- Prioridade: token informado → login por usuário/senha/integrador → certificado digital.
- Token informado não dispara login. Token obtido no login pode ser reaproveitado apenas na `Configuracao` da execução; não o persista automaticamente.
- Token ou credenciais devem desativar certificado e seleção automática de certificado do Windows. Certificado só é enviado quando essa modalidade for efetivamente escolhida.
- Login e consultas eFrete usam GET com JSON no corpo. Esse caso requer `WinHttpHandler` no Windows.
- Restrinja `UsaWinHttpHandler` ao provedor eFrete e ao GET com corpo. Demais APIs continuam com `HttpClientHandler`.
- Preserve proxy, timeout e credenciais padrão. Nunca registre integrador, usuário, senha ou token.

## XML, XSD e validação

- As classes XML são comuns, mas a eFrete usa seu schema composto `CIOT.EFrete.ciotEFrete_v1.00.xsd` e também `EFreteValidator`.
- Não use somente validação manual quando o XML eFrete puder ser validado pelo schema.
- Não altere estruturas de retorno nos XSDs de envio.
- Campos opcionais sem valor não entram no JSON. Em INTEROP, value types opcionais usam sentinela/`ShouldSerialize`, não nullable incompatível com COM.
- RNTRC com oito dígitos é aceito no XML quando previsto e recebe zero à esquerda somente no mapper eFrete.

Regras que já causaram falhas reais:

- `IdOperacaoCliente` é obrigatório e é a chave de idempotência; não envie `IdOperacaoTransporte` na declaração eFrete.
- `NotasFiscais` deve ser um array JSON direto. Não serialize o wrapper XML `NotaFiscal` como objeto.
- Mesmo uma única nota fiscal permanece array.
- Em nota com `TipoDeCalculo=SemQuebra`, não envie `ValorDoFretePorUnidadeDeMercadoria`.
- `TomadorServico` é um papel diferente do responsável pelo pagamento. Não envie nele `RNTRC` nem `ResponsavelPeloPagamento`.
- O responsável deve ser indicado em `Contratante`, `Destinatario`, `Subcontratante` ou `Consignatario`; ao menos um deles deve ser `true`.
- `Contratante` sempre informa explicitamente `ResponsavelPeloPagamento`. Subcontratante e consignatário, quando presentes, também informam explicitamente o indicador.
- Preserve diferenças entre lotação, fracionado e TAC agregado; não envie grupos proibidos no TAC agregado.
- Dados bancários e PIX são mutuamente exclusivos em cada pagamento.

## Identificador e dígito verificador

- A API eFrete trabalha externamente com `CodigoIdentificacaoOperacao` de 12 caracteres.
- A própria eFrete usa internamente o CIOT de 16 caracteres, com dígito verificador, ao falar com a ANTT.
- Se homologação retornar algo como `123456789012/XXXX`, exponha e transmita somente `123456789012` nos serviços eFrete.
- Cancelamento e encerramento recebem os 12 caracteres; não exija consulta apenas para obter o dígito verificador.
- Essa normalização é exclusiva do mapper eFrete. Não corte identificadores ANTT.

## Normalização obrigatória dos retornos

O contrato público de retorno é o CIOT atual, não o JSON da eFrete.

- Declaração eFrete bem-sucedida expõe o código real de 12 caracteres em `IdOperacaoTransporte`.
- Declaração, cancelamento e encerramento bem-sucedidos expõem `Codigo=110`, pois consumidores existentes e o UniNFe reconhecem assim o sucesso CIOT.
- Na declaração autorizada, mantenha simultaneamente:
  - `<Mensagem>` no nível raiz;
  - `<Mensagens><Mensagem><Codigo>110</Codigo><Descricao>...</Descricao></Mensagem></Mensagens>`.
- Se a eFrete fornecer mensagem de sucesso, preserve-a. Se não fornecer, use `Dados inseridos com sucesso!` tanto em `Mensagem` quanto em `Descricao`.
- Teste o `RetornoWSString` final. Testar apenas `EFreteMapper.NormalizarRetorno` não garante que a base e o normalizador tipado preservem ordem, mensagem e grupo.
- Erros mantêm código e mensagem da eFrete e preenchem `Temp`. Não convertê-los em sucesso.
- Não invente datas, protocolo ou aviso quando a resposta não os fornecer.

Essa normalização deve permanecer na DLL. Não crie condições eFrete no UniNFe ou em cada consumidor para compensar diferenças do provedor.

## Testes de regressão

Além dos testes de serialização e schema, atravesse o fluxo público `Executar()` com transporte controlado e verifique:

- endpoint e método;
- GET com corpo e POST;
- ausência/presença de certificado em cada modalidade de autenticação;
- token obtido no login e reaproveitado;
- JSON, especialmente arrays e ausência de campos vazios;
- sucesso e erro convertidos até `Result` e `RetornoWSString`;
- identificador de 12 caracteres, `Codigo=110`, `Mensagem` e `Descricao`;
- pelo menos um cenário ANTT comprovando que endpoint, método, payload e retorno continuam inalterados.
