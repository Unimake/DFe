# Threat model NF-e ABI

Ativos: XML fiscal, chave de acesso, certificado, senha/PIN, endpoints e arquivos ERP. Boundaries: ERP/pasta monitorada, processo UniNFe, DLL, proxy/rede e autorizador. Ameaças: XML adulterado, assinatura inválida, endpoint errado, vazamento em log/evidence, replay/retry de autorização e confusão homologação/produção. Mitigações: schema/assinatura existentes, endpoint configurado por ambiente, fail-closed para produção, fixtures sintéticas, sanitização e sem repetição de operação fiscal como diagnóstico.
