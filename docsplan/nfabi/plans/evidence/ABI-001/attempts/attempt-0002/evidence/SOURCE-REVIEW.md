# Revisão da fonte normativa - ABI-001

- Coleta: 2026-09-14.
- Fonte normativa: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi`.
- Origem dos schemas: `C:\Users\Wandrey\OneDrive\Downloads\NFeAbi\PL_NFeABI_1.00`.
- Método: leitura integral dos dois MOCs, parsing XML integral dos vinte XSDs, SHA-256 individual e snapshot agregado reproduzido pela regra da ABI-000.

## Integridade

| Conjunto | Quantidade | Snapshot ABI-000 | Snapshot ABI-001 | Resultado |
|---|---:|---|---|---|
| Fonte normativa completa | 22 arquivos | `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` | `0383F95D81140925C4EF91046C2D723CA9D94F477E138069F7202D79D9CCCAD8` | PASS |
| Origem dos schemas | 20 XSDs | `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` | `BA44B39981C4DF3860A6BE9AA7C0739904D664377EABE963AB52AC8C2E74AE40` | PASS |

Os 22 hashes individuais também coincidem com `docsplan/nfabi/plans/evidence/ABI-000/evidence/SOURCE-SNAPSHOT.md`.

## MOCs lidos

| Documento | Linhas | Bytes | SHA-256 | Cobertura |
|---|---:|---:|---|---|
| `MOC_NFe_ABI_Anexo_I_Leiaute_e_RV_v1.00a.md` | 1638 | 162553 | `FE9868E5E5280EFE518C37AC70AABB833B197B1DE2BEA11C7B90322FE65B3E94` | leiaute, eventos, preenchimento, validações gerais e regras de negócio |
| `MOC_NFe_ABI_Visao_Geral_v1.00a.md` | 1635 | 91906 | `8DEF3134279864662CDE7D8A45CFF4665E9534AE8A890909410817893149104D` | arquitetura, XML, TLS/SOAP, assinatura, serviços, retornos e consulta pública |

## XSDs lidos

Todos os vinte XSDs foram carregados integralmente por parser XML sem erro. Foram confirmados:

- namespace de negócio `http://www.portalfiscal.inf.br/nfeabi` e import separado de XMLDSig;
- raízes `NFeABI`, `nfeabiProc`, `retNFeABI`, `consSitNFeABI`, `retConsSitNFeABI`, `consStatServNFeABI`, `retConsStatServNFeABI`, `evento`, `retEventoNFeABI`, `procEventoNFeABI`, `evCancNFeABI`, `evPagParcelaNFeABI` e `evApropCredIndNFeABI`;
- includes/imports resolvidos somente para arquivos presentes no pacote;
- versão 1.00, modelo 77, escolhas CNPJ/CPF, XMLDSig e processamento síncrono coerentes com os MOCs.

## Conclusões para o plano

- Os MOCs descrevem Autorização, Consulta de Protocolo, Status de Serviço e Recepção de Evento como contratos síncronos.
- A documentação dos contratos XML não comprova, por si só, publicação de endpoint. O catálogo oficial aprovado registra somente Status e Autorização em homologação.
- Consulta e eventos devem ser modelados a partir dos XSDs, mas não recebem transporte/configuração remota até publicação oficial.
- Produção permanece sem endpoint; nenhuma URL de homologação pode ser reutilizada.
- Teste fiscal online exige autorização, certificado/credenciamento e ambiente preparado e não substitui validações offline determinísticas.

## Publicação oficial reconferida

Em 2026-09-14, o Portal de Serviços continuava listando somente `NFeABIStatusServico` 1.00 e `NFeABIAutorizacao` 1.00, ambos em homologação. O Portal de Documentos continuava publicando os dois MOCs v1.00a e o pacote de schemas 1.00. Não havia endpoint de produção, Consulta de Protocolo ou Recepção de Evento publicado nas páginas consultadas.

- Serviços: https://dfe-portal.svrs.rs.gov.br/NFABI/Servicos
- Documentos: https://dfe-portal.svrs.rs.gov.br/NFABI/Documentos
