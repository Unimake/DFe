# ABI-005 attempt-0003 - Configuração estadual

- Modelo copiado byte a byte: `source/.NET Standard/Unimake.Business.DFe/Servicos/Config/NFGas/AC.xml`.
- SHA-256 do modelo e de cada arquivo estadual NFeABI: `756B6744B329648FC4BED2861815A099A8D2FA1BD6043A2597EB5F9F5D854FB1`.
- UFs: `AC, AL, AM, AP, BA, CE, DF, ES, GO, MA, MG, MS, MT, PA, PB, PE, PI, PR, RJ, RN, RO, RR, RS, SC, SE, SP, TO`.
- Inventário: 27 esperadas, 27 encontradas, nenhuma ausente e nenhuma extra.
- Conteúdo: XML válido com `<Heranca>SVRS.xml</Heranca>`.
- Projeto: 27 entradas `None Remove` e 27 entradas `EmbeddedResource`.
- Execução: as 27 UFs resolveram `NFeABIStatusServico` por `Configuracoes.Load(GetType().Name)` e herdaram o endpoint de homologação oficial.

Resultado: PASS.
