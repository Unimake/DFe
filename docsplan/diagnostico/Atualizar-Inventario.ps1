[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$lib = Join-Path $repo 'source/.NET Standard/Unimake.Business.DFe'
$servicos = Join-Path $lib 'Servicos'
$config = Join-Path $servicos 'Config'
$linhas = [Collections.Generic.List[string]]::new()
$linhas.Add('# Inventário nominal dos caminhos remotos')
$linhas.Add('')
$linhas.Add('Gerado por Atualizar-Inventario.ps1. Esta tabela complementa COBERTURA.md e INTERPRETACAO.md: cada linha refere-se a uma classe concreta, mas a prova dinâmica compartilhada é da fronteira indicada, não da execução de cada município ou de cada serviço público. Os caminhos foram conferidos nas implementações de Executar e nos consumidores; herança isolada não constitui prova de teste.')
$linhas.Add('')
$linhas.Add('| Família | Serviço concreto / fonte | Caminho de execução | Transporte | Coleta | Retorno, falhas, sondas e limites | Teste offline da fronteira |')
$linhas.Add('|---|---|---|---|---|---|---|')

foreach ($dir in Get-ChildItem $servicos -Directory | Sort-Object Name) {
    if ($dir.Name -in @('Config', 'Enums', 'Interop', 'SNCM')) { continue }
    foreach ($file in Get-ChildItem $dir.FullName -File -Filter '*.cs' | Sort-Object Name) {
        $s = [IO.File]::ReadAllText($file.FullName)
        $m = [regex]::Match($s, 'public\s+class\s+(\w+)\s*:\s*([^\r\n{]+)')
        if (!$m.Success -or $m.Groups[1].Value -eq 'ServicoBase') { continue }
        $nome = $m.Groups[1].Value
        $linha = ($s.Substring(0, $m.Index) -split "`n").Count
        $arquivo = '../../source/.NET%20Standard/Unimake.Business.DFe/Servicos/' + $dir.Name + '/' + $file.Name + '#L' + $linha
        $caminho = $dir.Name + '.ServicoBase → Servicos.ServicoBase.Executar → ConsumirWS/ConsumirAPI'
        $transporte = 'SOAP'
        $teste = 'CanalObservacoesDisponibilidadeTest; DiagnosticoDisponibilidadeTest (fronteira)'
        $nota = 'Política F; Status oficial conforme família; sem regressão individual de cada classe'
        if ($dir.Name -eq 'NFCe') { $caminho = 'NFe.ServicoBase → Servicos.ServicoBase.Executar → ConsumirWS' }
        if ($dir.Name -eq 'GNRE') { $caminho = 'GNRE.ServicoBase → NFe.ServicoBase → base comum → ConsumirWS'; $nota = 'Política G; sem status manual; conferir contrato/versionamento na configuração' }
        if ($dir.Name -eq 'NFSe') { $transporte = 'SOAP/REST pela configuração efetiva'; $nota = 'Política M; padrão/município/versão em CONFIGURACOES.csv; reconhecimento parcial explícito' }
        if ($dir.Name -eq 'EFDReinf') { $transporte = 'REST'; $nota = 'Política R; sem sonda de negócio; cdResposta/cdRetorno' }
        if ($dir.Name -eq 'ESocial') { $nota = 'Política E; sem sonda de negócio; cdResposta' }
        if ($dir.Name -eq 'DARE') { $transporte = 'REST'; $caminho = 'DARE.ServicoBase.Executar → ConsumirAPI (contorna base comum)'; $nota = 'Política D; sem status manual' }
        if ($dir.Name -in @('EBoleto', 'PIX')) { $transporte = 'REST'; $caminho = $dir.Name + '.ServicoBase.Executar → ConfigureAuth → UMessengerTokenCache → base comum → ConsumirAPI'; $nota = 'Política U; autenticação auxiliar/cache; sem sonda de pagamento' }
        if ($dir.Name -eq 'UMessenger') { $transporte = 'REST'; $caminho = 'PublishUMessenger.Executar (loop) → ConfigureAuth → ServicoBase.Executar → ConsumirAPI; ExecutarRaw → ApiTransportExecutorFactory'; $nota = 'Política U; uma observação lógica, várias entradas; sem sonda de publicação' }
        if ($dir.Name -eq 'CIOT') {
            $transporte = 'REST; componente externo em GerarId produção ANTT'
            $caminho = 'CIOT.ServicoBase.Executar → Provedor.PrepararExecucao (login eFrete) → base comum → ConsumirAPI → normalização do provedor'
            if ($nome -eq 'GerarIdOperacaoTransporte') { $caminho = 'Executar: produção ANTT → GerarCIOTProducao/GeradorCIOTShared; demais → base CIOT/API' }
            $nota = 'Política C; eFrete não suporta GerarId/retificação/exceção; componente opaco preservado; PDF e códigos conforme INTERPRETACAO.md'
            $teste = 'EFreteFluxoExecucaoTest (fluxos públicos); CanalObservacoesDisponibilidadeTest (componente); CoberturaDisponibilidadeTest'
        }
        if ($dir.Name -in @('ESocial', 'EFDReinf', 'GNRE', 'DARE', 'PIX', 'EBoleto', 'UMessenger', 'NFSe')) { $teste = 'CoberturaDisponibilidadeTest (consumidor/contrato); sem execução individual de todos os serviços' }
        if ($dir.Name -in @('GNRE', 'NFSe', 'CIOT')) { $teste += '; InterpretacaoContratosDisponibilidadeTest (contratos, falhas e downloads; ver INTERPRETACAO.md)' }
        if ($dir.Name -eq 'NFeABI') { $nota = 'F; inventariado sem executar etapa ABI; produção sem endpoint; sem sonda manual registrada' }
        $linhas.Add('| ' + $dir.Name + ' | [' + $nome + '](' + $arquivo + ') | ' + $caminho + ' | ' + $transporte + ' | Sessão opt-in, zero tráfego adicional | ' + $nota + ' | ' + $teste + ' |')
    }
}
[IO.File]::WriteAllLines((Join-Path $PSScriptRoot 'INVENTARIO.md'), $linhas, [Text.UTF8Encoding]::new($false))

# Declarações efetivas de serviço/versão por arquivo. Não exportar URLs, ações ou payloads.
# A herança fica explícita para revisão junto do Load real; não fingir resolver overrides só pelo nome.
$rows = foreach ($dir in Get-ChildItem $config -Directory | Sort-Object Name) {
    foreach ($file in Get-ChildItem $dir.FullName -File -Filter '*.xml' | Sort-Object Name) {
        [xml]$xml = [IO.File]::ReadAllText($file.FullName)
        $heranca = ($xml.SelectNodes('/Configuracoes/Heranca') | ForEach-Object InnerText) -join ';'
        $nodes = @($xml.SelectNodes('/Configuracoes/Servicos/*[@versao]'))
        if ($nodes.Count -eq 0) {
            [pscustomobject]@{ Familia = $dir.Name; Arquivo = $file.Name; Servico = '(herança/configuração sem declaração própria)'; Versao = ''; Heranca = $heranca; TransporteDeclarado = ''; Coleta = 'Conforme destino do Load, ver COBERTURA.md'; Prova = 'Estática; não execução de endpoint' }
        }
        foreach ($node in $nodes) {
            $rest = $null -ne $node.SelectSingleNode('MetodoAPI|RequestURIProducao|RequestURIHomologacao')
            $soap = $null -ne $node.SelectSingleNode('WebSoapString|WebEnderecoProducao|WebEnderecoHomologacao')
            [pscustomobject]@{ Familia = $dir.Name; Arquivo = $file.Name; Servico = $node.LocalName; Versao = $node.GetAttribute('versao'); Heranca = $heranca; TransporteDeclarado = $(if ($rest) { 'REST/override' } elseif ($soap) { 'SOAP/override' } else { 'Herdado' }); Coleta = 'Sessão em ConsumirAPI/ConsumirBase'; Prova = 'Estática; conferir herança em Configuracao.Load; ver limites/testes em COBERTURA.md' }
        }
    }
}
$rows | Export-Csv (Join-Path $PSScriptRoot 'CONFIGURACOES.csv') -NoTypeInformation -Encoding utf8
Write-Output ('Classes concretas: ' + ($linhas.Count - 6) + '; declarações/configurações: ' + @($rows).Count)
