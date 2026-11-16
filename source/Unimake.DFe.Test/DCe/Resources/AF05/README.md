# Schemas oficiais de caracterização DC-e — AF-05 / ID #189439

Cópias imutáveis, exclusivas dos testes, obtidas do pacote oficial `PL_DCe_v1.00a_NT2024.001v1.00.zip` em 14/11/2026. URL, hash do ZIP e hashes por entrada estão em `proveniencia.json`. Conteúdo e avisos dos arquivos XSD foram preservados byte a byte; o .gitattributes local impede normalização de quebras de linha pelo Git. Não há download durante build/testes, nem substituição dos recursos distribuídos pela DLL.

`Emissao` conserva o diretório `xsd/pl100/dce`; `Consulta` conserva `xsd/pl100/consulta`. Arquivos com o mesmo nome possuem contratos diferentes. O tipo TDCe de consulta torna país/nome do país opcionais; o de emissão os exige e inclui infSolicDCe. A disponibilização desse grupo pelo modelo e a regra do tipo de emitente não são alteradas por estes testes.

Os testes distinguem o contrato oficial, o comportamento permissivo atual e a geração de distribuição da própria biblioteca. A raiz auxiliar usada para examinar o tipo TDCe de consulta só liga um elemento ao tipo original em memória; não transforma esse documento em pedido de consulta. As consultas reais de situação são verificadas separadamente pelo seu schema próprio.

Fixtures fiscais são sintéticas, geradas em memória pelos modelos públicos e assinadas pela DLL com certificado temporário. O grupo de solicitação ainda não modelado é representado somente na fixture de referência, antes da assinatura, sem serviço remoto. Nenhum XML/certificado de cliente ou histórico operacional foi copiado.

Uma correção futura deve rever explicitamente os testes chamados CaracterizacaoAtual. Eles documentam lacunas conhecidas, sem declará-las regra fiscal desejada. Não impor o XSD de emissão à leitura de consultas ou distribuições históricas por mera troca global do arquivo compartilhado.