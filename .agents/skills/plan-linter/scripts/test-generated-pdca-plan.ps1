[CmdletBinding()]
param([string]$RepositoryRoot = (Get-Location).Path)

$ErrorActionPreference = "Stop"
$root = (Resolve-Path -LiteralPath $RepositoryRoot).Path
$errors = [System.Collections.Generic.List[string]]::new()
$states = @("PLANNED", "IN_PROGRESS", "BLOCKED", "DELIVERED_FOR_REVIEW", "REWORK", "APPROVED")
$profiles = @("ECONOMY", "BALANCED", "DEEP", "INDEPENDENT_REVIEW", "SPECIALIST")
$effortRanks = @{ none=0; minimal=1; low=2; medium=3; high=4; xhigh=5; max=6; ultra=7 }

function Add-Duplicates([string[]]$Values, [string]$Source) {
    $Values | Group-Object | Where-Object Count -gt 1 | ForEach-Object { $errors.Add("${Source}: ID duplicado: $($_.Name)") }
}
function Assert-Sequence([string[]]$Expected, [string[]]$Actual, [string]$Source) {
    if (($Expected -join "|") -ne ($Actual -join "|")) { $errors.Add("${Source}: ordem/etapas divergem do PDCA.md") }
}
function Require-File([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { $errors.Add("arquivo obrigatório ausente: $Path") }
}

Push-Location $root
try {
    foreach ($path in @(
        "README.md", "AGENTS.md", "docs/CODEX-START-HERE.md",
        ".agents/instructions/pdca-execution.instructions.md", ".agents/instructions/model-routing.instructions.md",
        ".agents/skills/plan-linter/SKILL.md", ".agents/skills/plan-linter/agents/openai.yaml",
        ".agents/skills/plan-linter/scripts/test-plan.ps1", ".agents/skills/plan-linter/scripts/test-generated-pdca-plan.ps1",
        "docs/planning/PROJECT-BRIEF.md", "docs/planning/QUESTION-LEDGER.md", "docs/planning/RISK-REGISTER.md",
        "docs/architecture/PROJECT-VISION.md", "docs/architecture/DECISIONS-LOCKED.md", "docs/architecture/DATA-ARCHITECTURE.md",
        "docs/architecture/INTEGRATION-CATALOG.md", "docs/architecture/QUALITY-ATTRIBUTES.md", "docs/architecture/LIMITS-CATALOG.md",
        "docs/architecture/SECURITY-CLAIMS.md", "docs/architecture/ERROR-OBSERVABILITY-MODEL.md", "docs/architecture/VERSIONING-BUILD.md",
        "docs/design/UI-DESIGN-SYSTEM.md", "docs/security/THREAT-MODEL.md",
        "docs/plans/INDEX.md", "docs/plans/STATUS.md", "docs/plans/PDCA.md", "docs/plans/READINESS.md",
        "docs/plans/DECISION-REGISTER.md", "docs/plans/MODEL-CATALOG.md", "docs/plans/STAGE-ID-MAP.md",
        "docs/plans/ENVIRONMENT-MATRIX.md", "docs/plans/TRACEABILITY.md", "docs/plans/VALIDATION-MANIFEST.md",
        "docs/plans/VALIDATION-REGISTRY.md", "docs/plans/EXECUTION-GUARDRAILS.md", "docs/plans/EVIDENCE-TEMPLATE.md",
        "docs/plans/evidence/README.md", "docs/testing/TEST-LAB.md"
    )) { Require-File $path }

    if (-not (Test-Path -LiteralPath "docs/plans" -PathType Container)) {
        throw (($errors | Sort-Object -Unique | ForEach-Object { "ERROR: $_" }) -join "`n")
    }

    $brief = if (Test-Path -LiteralPath "docs/planning/PROJECT-BRIEF.md") { Get-Content -Raw -LiteralPath "docs/planning/PROJECT-BRIEF.md" } else { "" }
    if ($brief -notmatch '(?m)^- Pasta de destino resolvida:\s+\S') { $errors.Add("PROJECT-BRIEF.md: pasta de destino resolvida ausente") }
    if ($brief -notmatch '(?m)^- ProjectMode:\s+(GREENFIELD|EVOLUTION)\s*$') { $errors.Add("PROJECT-BRIEF.md: ProjectMode deve ser GREENFIELD ou EVOLUTION") }
    $prefixMatch = [regex]::Match($brief, '(?m)^- StagePrefix:\s+([A-Z]{3})\s*$')
    if (-not $prefixMatch.Success) {
        if (@(Get-ChildItem -LiteralPath "docs/plans" -File -Filter "PARTE-*.md" -ErrorAction SilentlyContinue).Count -gt 0) {
            $errors.Add("plano legado PARTE-XX detectado: migração autorizada é necessária")
        }
        $errors.Add("PROJECT-BRIEF.md: StagePrefix deve ter exatamente três letras ASCII maiúsculas"); $prefix = "ZZZ"
    } else { $prefix = $prefixMatch.Groups[1].Value }

    if (Test-Path -LiteralPath "README.md") {
        $projectReadme = Get-Content -Raw -LiteralPath "README.md"
        if (([regex]::Matches($projectReadme, '<!-- DEVPLANNER:START -->')).Count-ne 1 -or ([regex]::Matches($projectReadme, '<!-- DEVPLANNER:END -->')).Count-ne 1) { $errors.Add("README.md: seção gerenciada do DevPlanner ausente ou duplicada") }
        foreach ($requiredSection in @("Leitura obrigatória do DEV", "Como o plano funciona", "Como executar uma etapa", "O que revisar antes de aprovar", "Reprovação, bloqueio e retomada", "Fontes de verdade", "Validação do plano")) {
            if ($projectReadme -notmatch [regex]::Escape($requiredSection)) { $errors.Add("README.md: seção operacional ausente: $requiredSection") }
        }
        foreach ($requiredReference in @("docs/plans/PDCA.md", "docs/plans/DECISION-REGISTER.md", "docs/planning/RISK-REGISTER.md", "docs/plans/MODEL-CATALOG.md", "docs/plans/evidence/")) {
            if ($projectReadme -notmatch [regex]::Escape($requiredReference)) { $errors.Add("README.md: fonte de verdade ausente: $requiredReference") }
        }
        foreach ($command in @("Execute somente", "Aprovo", "Reprovei", "Retome somente")) {
            if ($projectReadme -notmatch [regex]::Escape($command)) { $errors.Add("README.md: comando do DEV ausente: $command") }
        }
    }

    if (Test-Path -LiteralPath "docs/planning/QUESTION-LEDGER.md") {
        $ledger = Get-Content -Raw -LiteralPath "docs/planning/QUESTION-LEDGER.md"
        if ($ledger -notmatch '(?m)^\| Q-DEST-001 \|.*\| ANSWERED \|') { $errors.Add("QUESTION-LEDGER.md: Q-DEST-001 respondida ausente") }
    }
    if(Test-Path -LiteralPath ".agents/instructions/pdca-execution.instructions.md"){
        $pdcaInstruction=Get-Content -Raw ".agents/instructions/pdca-execution.instructions.md"
        foreach($term in @("DELIVERED_FOR_REVIEW","APPROVED","Somente o DEV","pare antes da próxima etapa")){if($pdcaInstruction-notmatch[regex]::Escape($term)){$errors.Add("pdca-execution.instructions.md: contrato ausente: $term")}}
    }
    if(Test-Path -LiteralPath ".agents/instructions/model-routing.instructions.md"){
        $modelInstruction=Get-Content -Raw ".agents/instructions/model-routing.instructions.md"
        foreach($term in @("ECONOMY","BALANCED","DEEP","INDEPENDENT_REVIEW","fallback")){if($modelInstruction-notmatch[regex]::Escape($term)){$errors.Add("model-routing.instructions.md: contrato ausente: $term")}}
    }
    if(Test-Path -LiteralPath ".agents/skills/plan-linter/SKILL.md"){
        $planLinterSkill=Get-Content -Raw ".agents/skills/plan-linter/SKILL.md"
        if($planLinterSkill-notmatch'(?m)^name: plan-linter\s*$' -or $planLinterSkill-notmatch'test-plan\.ps1'){$errors.Add("plan-linter/SKILL.md: interface/runner inválido")}
    }
    if((Test-Path -LiteralPath ".agents/skills/plan-linter/agents/openai.yaml") -and (Get-Content -Raw ".agents/skills/plan-linter/agents/openai.yaml")-notmatch'\$plan-linter'){$errors.Add("plan-linter/agents/openai.yaml: prompt deve invocar `$plan-linter")}
    if (Test-Path -LiteralPath "docs/plans/STATUS.md") {
        $status = Get-Content -Raw -LiteralPath "docs/plans/STATUS.md"
        if ($status -notmatch '\]\(PDCA\.md\)' -or $status -match '(?m)^\|\s*[A-Z]{3}-\d{3}\s*\|') {
            $errors.Add("STATUS.md: deve apenas apontar para PDCA.md, sem tabela de estados")
        }
    }

    $plans = @(Get-ChildItem -LiteralPath "docs/plans" -File | Where-Object { $_.BaseName -match "^${prefix}-[0-9]{3}-" })
    $stageById = @{}
    foreach ($plan in $plans) {
        if ($plan.BaseName -notmatch "^(${prefix}-([0-9]{3}))-") { continue }
        $id=$Matches[1]; $number=[int]$Matches[2]; $text=Get-Content -Raw -LiteralPath $plan.FullName
        if ($stageById.ContainsKey($id)) { $errors.Add("docs/plans: ID duplicado: $id") }
        $stageById[$id]=[pscustomobject]@{Id=$id;Number=$number;File=$plan;Text=$text}
        foreach ($section in @("Plan","Do","Check","Act","Definition of Done")) {
            if ($text -notmatch [regex]::Escape($section)) { $errors.Add("$($plan.Name): seção obrigatória ausente: $section") }
        }
        if ($text -match '\{\{[^}]+\}\}|(?m)^\s*(?:[-*]\s*)?(?:TODO|TBD|FIXME)(?:\s*[:=-]|\s*$)') { $errors.Add("$($plan.Name): placeholder não resolvido") }
    }
    $stageIds=@($stageById.Values|Sort-Object Number|ForEach-Object Id)
    $stageNumbers=@($stageById.Values|Sort-Object Number|ForEach-Object Number)
    $expectedNumbers=if($stageNumbers.Count-gt 0){@(0..($stageNumbers.Count-1))}else{@()}
    if(($stageNumbers-join'|')-ne($expectedNumbers-join'|')){$errors.Add("docs/plans: etapas devem formar sequência contínua de 000 em diante")}
    foreach ($id in @("${prefix}-000","${prefix}-001","${prefix}-002")) { if ($id -notin $stageIds) { $errors.Add("etapa obrigatória ausente: $id") } }
    $idsFrozen=$brief-match'(?m)^- StageIdsFrozen:\s+true\s*$'
    $frozenMatch=[regex]::Match($brief,'(?m)^- FrozenStageIds:\s+(.+?)\s*$')
    if(-not$frozenMatch.Success){$errors.Add("PROJECT-BRIEF.md: FrozenStageIds ausente")}
    elseif($idsFrozen){
        $frozenIds=@($frozenMatch.Groups[1].Value.Split(',')|ForEach-Object{$_.Trim()}|Where-Object{$_})
        $executionIds=@($stageIds|Where-Object{$_-notin@("${prefix}-000","${prefix}-001")})
        if(($frozenIds-join'|')-ne($executionIds-join'|')){$errors.Add("PROJECT-BRIEF.md: FrozenStageIds diverge da sequência 002+")}
    }elseif($frozenMatch.Groups[1].Value.Trim()-ne'N/A'){$errors.Add("PROJECT-BRIEF.md: FrozenStageIds deve ser N/A antes do congelamento")}
    if ($stageById["${prefix}-000"] -and $stageById["${prefix}-000"].Text -notmatch 'PLANNING_BASELINE') { $errors.Add("${prefix}-000: tipo PLANNING_BASELINE ausente") }
    if ($stageById["${prefix}-001"] -and $stageById["${prefix}-001"].Text -notmatch 'PLAN_REVIEW') { $errors.Add("${prefix}-001: tipo PLAN_REVIEW ausente") }

    $pdcaRows=@(); $pdca=""; $deliveredAttempts=@{}
    if (Test-Path -LiteralPath "docs/plans/PDCA.md") {
        $pdca=Get-Content -Raw -LiteralPath "docs/plans/PDCA.md"
        $statePattern=$states -join '|'
        $pdcaPattern = '(?m)^\| `?' + [regex]::Escape($prefix) + '-([0-9]{3})`? \|.*\| (' + $statePattern + ') \|.*\|\r?$'
        foreach ($match in [regex]::Matches($pdca, $pdcaPattern)) {
            $pdcaRows += [pscustomobject]@{Id="${prefix}-$($match.Groups[1].Value)";State=$match.Groups[2].Value}
        }
        $pdcaIds=@($pdcaRows|ForEach-Object Id); Add-Duplicates $pdcaIds "PDCA.md"; Assert-Sequence $stageIds $pdcaIds "PDCA.md"
        $s000=($pdcaRows|Where-Object Id -eq "${prefix}-000").State; $s001=($pdcaRows|Where-Object Id -eq "${prefix}-001").State
        if($s000-notin@("DELIVERED_FOR_REVIEW","APPROVED")){$errors.Add("PDCA.md: plano gerado exige 000 entregue para revisão ou aprovada")}
        if ($s001 -in @("IN_PROGRESS","BLOCKED","DELIVERED_FOR_REVIEW","REWORK","APPROVED") -and $s000 -ne "APPROVED") { $errors.Add("PDCA.md: 001 avançou sem 000 APPROVED") }
        if (@($pdcaRows|Where-Object {$_.Id -notin @("${prefix}-000","${prefix}-001") -and $_.State -ne "PLANNED"}).Count -gt 0 -and $s001 -ne "APPROVED") { $errors.Add("PDCA.md: produto iniciado antes de 001 APPROVED") }
        if ($s001 -eq "APPROVED" -and ($brief -notmatch '(?m)^- PlanningStatus:\s+READY_FOR_EXECUTION\s*$' -or $brief -notmatch '(?m)^- StageIdsFrozen:\s+true\s*$')) {
            $errors.Add("PROJECT-BRIEF.md: 001 APPROVED exige READY_FOR_EXECUTION e StageIdsFrozen true")
        }
        foreach ($row in $pdcaRows|Where-Object State -eq "APPROVED") {
            $escaped=[regex]::Escape($row.Id)
            $approvalPattern = '(?mi)^\|[^|]+\| `?' + $escaped + '`? \|[^|]+\| APPROVED \|[^|]*(DEV|PROPRIET.RIO|USU.RIO)[^|]*\|'
            if ($pdca -notmatch $approvalPattern) { $errors.Add("PDCA.md: $($row.Id) APPROVED sem autoridade humana") }
        }
        $allowedTransitions=@{
            "PLANNED"=@("IN_PROGRESS");"IN_PROGRESS"=@("BLOCKED","DELIVERED_FOR_REVIEW");"BLOCKED"=@("IN_PROGRESS");
            "DELIVERED_FOR_REVIEW"=@("APPROVED","REWORK");"REWORK"=@("IN_PROGRESS");"APPROVED"=@()
        }
        foreach($line in Get-Content "docs/plans/PDCA.md"){
            $cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim().Trim('`')})
            if($cols.Count-ne 6 -or $cols[1]-notmatch("^"+[regex]::Escape($prefix)+"-[0-9]{3}$")){continue}
            $stage=$cols[1];$from=$cols[2];$to=$cols[3];$reason=$cols[4];$attempt=$cols[5]
            if($from-eq'—' -and $to-eq'PLANNED'){}elseif($from-notin$states -or $to-notin$allowedTransitions[$from]){$errors.Add("PDCA.md: transição inválida $from -> $to em $stage")}
            if([string]::IsNullOrWhiteSpace($reason) -or $reason-eq'—' -or [string]::IsNullOrWhiteSpace($attempt) -or $attempt-eq'—'){$errors.Add("PDCA.md: transição sem motivo/AttemptId em $stage")}
            if($to-eq'REWORK' -and $reason.Length-lt 8){$errors.Add("PDCA.md: reprovação sem motivo explícito em $stage")}
            if($to-eq'DELIVERED_FOR_REVIEW'){
                if(-not$deliveredAttempts.ContainsKey($stage)){$deliveredAttempts[$stage]=[System.Collections.Generic.List[string]]::new()}
                if($attempt-notin$deliveredAttempts[$stage]){$deliveredAttempts[$stage].Add($attempt)}
            }
        }
    }

    if(Test-Path "docs/plans/STAGE-ID-MAP.md"){
        $map=Get-Content -Raw "docs/plans/STAGE-ID-MAP.md"
        $renumberingMatch=[regex]::Match($map,'(?m)^- RenumberingCount:\s+([0-9]+)\s*$')
        if(-not$renumberingMatch.Success -or [int]$renumberingMatch.Groups[1].Value-notin@(0,1)){$errors.Add("STAGE-ID-MAP.md: RenumberingCount deve ser 0 ou 1")}
        $mapRows=@([regex]::Matches($map,"(?m)^\| (${prefix}-[0-9]{3}) \| (${prefix}-[0-9]{3}) \|[^|]+\|[^|]+\| APPLIED \|\r?$")|ForEach-Object{[pscustomobject]@{Old=$_.Groups[1].Value;Final=$_.Groups[2].Value}})
        Add-Duplicates @($mapRows|ForEach-Object Old) "STAGE-ID-MAP.md old"
        Add-Duplicates @($mapRows|ForEach-Object Final) "STAGE-ID-MAP.md final"
        foreach($row in $mapRows){if($row.Final-notin$stageIds){$errors.Add("STAGE-ID-MAP.md: ID final inexistente: $($row.Final)")}}
        if($renumberingMatch.Success -and (($mapRows.Count-gt 0) -ne ([int]$renumberingMatch.Groups[1].Value-eq 1))){$errors.Add("STAGE-ID-MAP.md: RenumberingCount diverge do mapa APPLIED")}
        if($brief-match'(?m)^- StageIdsFrozen:\s+true\s*$' -and $mapRows.Count-eq 0 -and $map-notmatch'(?mi)\bN/A\b'){$errors.Add("STAGE-ID-MAP.md: congelamento exige mapa APPLIED ou N/A justificado")}
    }

    if (Test-Path -LiteralPath "docs/plans/INDEX.md") {
        $ids=@([regex]::Matches((Get-Content -Raw "docs/plans/INDEX.md"),"\((${prefix}-[0-9]{3})-[^)]+\.md\)")|ForEach-Object {$_.Groups[1].Value}); Add-Duplicates $ids "INDEX.md"; Assert-Sequence $stageIds $ids "INDEX.md"
    }
    if (Test-Path -LiteralPath "docs/plans/VALIDATION-REGISTRY.md") {
        $validationText=Get-Content -Raw "docs/plans/VALIDATION-REGISTRY.md"
        $ids=@([regex]::Matches($validationText,"(?m)^\| (${prefix}-[0-9]{3}) \|")|ForEach-Object {$_.Groups[1].Value}); Add-Duplicates $ids "VALIDATION-REGISTRY.md"; Assert-Sequence $stageIds $ids "VALIDATION-REGISTRY.md"
    }

    $knownEnvironments=@();if(Test-Path "docs/plans/ENVIRONMENT-MATRIX.md"){$knownEnvironments=@([regex]::Matches((Get-Content -Raw "docs/plans/ENVIRONMENT-MATRIX.md"),'(?m)^\| (ENV-[A-Z0-9-]+) \|')|ForEach-Object{$_.Groups[1].Value});Add-Duplicates $knownEnvironments "ENVIRONMENT-MATRIX.md"}
    if(Test-Path "docs/plans/VALIDATION-REGISTRY.md"){
        foreach($line in Get-Content "docs/plans/VALIDATION-REGISTRY.md"){
            if($line-notmatch("^\| "+[regex]::Escape($prefix)+"-[0-9]{3} \|")){continue};$cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim()})
            if($cols.Count-lt 4 -or [string]::IsNullOrWhiteSpace($cols[1]) -or $cols[2]-notin$knownEnvironments -or [string]::IsNullOrWhiteSpace($cols[3])){$errors.Add("VALIDATION-REGISTRY.md: runner/ambiente/resultado inválido em $($cols[0])")}
        }
    }
    $knownDecisions=@();foreach($decisionFile in @("docs/plans/DECISION-REGISTER.md","docs/architecture/DECISIONS-LOCKED.md")){if(Test-Path $decisionFile){$knownDecisions+=@([regex]::Matches((Get-Content -Raw $decisionFile),'(?m)^\| (DEC-[0-9]{3}) \|')|ForEach-Object{$_.Groups[1].Value})}};$knownDecisions=@($knownDecisions|Sort-Object -Unique)
    $knownRisks=@();if(Test-Path "docs/planning/RISK-REGISTER.md"){$knownRisks=@([regex]::Matches((Get-Content -Raw "docs/planning/RISK-REGISTER.md"),'(?m)^\| (RISK-[0-9]{3}) \|')|ForEach-Object{$_.Groups[1].Value});Add-Duplicates $knownRisks "RISK-REGISTER.md"}
    $knownRequirements=@();$requirementsByStage=@{}
    if(Test-Path "docs/plans/TRACEABILITY.md"){
        foreach($line in Get-Content "docs/plans/TRACEABILITY.md"){
            if($line-notmatch'^\| (REQ-[0-9]{3}) \|'){continue};$cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim()});$req=$cols[0]
            $knownRequirements+=$req
            if($cols.Count-lt 7 -or $cols[3]-notin$stageIds -or [string]::IsNullOrWhiteSpace($cols[4]) -or $cols[5]-notmatch[regex]::Escape("docs/plans/evidence/$($cols[3])")){$errors.Add("TRACEABILITY.md: etapa/validação/evidência inválida em $req");continue}
            if(-not$requirementsByStage.ContainsKey($cols[3])){$requirementsByStage[$cols[3]]=[System.Collections.Generic.List[string]]::new()};$requirementsByStage[$cols[3]].Add($req)
        }
        Add-Duplicates $knownRequirements "TRACEABILITY.md"
    }

    $catalogProfiles=@{}
    if (Test-Path -LiteralPath "docs/plans/MODEL-CATALOG.md") {
        foreach($line in Get-Content "docs/plans/MODEL-CATALOG.md"){
            if($line-notmatch'^\| (ECONOMY|BALANCED|DEEP|INDEPENDENT_REVIEW|SPECIALIST) \|'){continue}
            $cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim()})
            if($cols.Count-ne 10){$errors.Add("MODEL-CATALOG.md: linha deve ter 10 colunas: $($cols[0])");continue}
            $rank=0;$fallbackRank=0;$rankValid=[int]::TryParse($cols[3],[ref]$rank);$fallbackRankValid=[int]::TryParse($cols[7],[ref]$fallbackRank)
            $catalogProfiles[$cols[0]]=[pscustomobject]@{Model=$cols[1];Effort=$cols[2];Rank=$rank;RankValid=$rankValid;VerifiedAt=$cols[5];Fallback=$cols[6];FallbackRank=$fallbackRank;FallbackRankValid=$fallbackRankValid;State=$cols[9]}
        }
        foreach($profile in $profiles){if(-not $catalogProfiles.ContainsKey($profile)){$errors.Add("MODEL-CATALOG.md: perfil ausente: $profile")}}
        foreach($profile in @("ECONOMY","BALANCED","DEEP","INDEPENDENT_REVIEW")){
            $e=$catalogProfiles[$profile]
            if($e -and ($e.State-ne"VERIFIED" -or $e.Model-match'^(N/A|—)$' -or $e.Fallback-match'^(N/A|—)$' -or [string]::IsNullOrWhiteSpace($e.VerifiedAt))){$errors.Add("MODEL-CATALOG.md: $profile deve ter modelo/fallback/disponibilidade VERIFIED")}
            if($e -and (-not$e.RankValid -or -not$e.FallbackRankValid -or $e.Rank-lt 1 -or $e.FallbackRank-lt$e.Rank)){$errors.Add("MODEL-CATALOG.md: $profile possui rank/fallback com downgrade")}
            if($e -and -not$effortRanks.ContainsKey([string]$e.Effort)){$errors.Add("MODEL-CATALOG.md: $profile possui esforço inválido")}
        }
    }

    $manifestIds=@()
    foreach($id in $stageIds){
        $mp="docs/plans/manifests/$id.json"; Require-File $mp
        $skill=$id.ToLowerInvariant()+"-orchestrator"; Require-File ".agents/skills/$skill/SKILL.md"; Require-File ".agents/skills/$skill/agents/openai.yaml"
        if(Test-Path ".agents/skills/$skill/SKILL.md"){$stageSkill=Get-Content -Raw ".agents/skills/$skill/SKILL.md";if($stageSkill-notmatch[regex]::Escape($id) -or $stageSkill-notmatch'Somente o DEV' -or $stageSkill-notmatch'(?i)não inicie a sucessora'){$errors.Add("${skill}/SKILL.md: escopo/gate/parada inválido")}}
        if(Test-Path ".agents/skills/$skill/agents/openai.yaml"){$stageAgent=Get-Content -Raw ".agents/skills/$skill/agents/openai.yaml";if($stageAgent-notmatch[regex]::Escape("`$$skill") -or $stageAgent-notmatch[regex]::Escape($id)){$errors.Add("${skill}/agents/openai.yaml: prompt não invoca o orquestrador da etapa")}}
        if(-not(Test-Path $mp)){continue}; try{$m=Get-Content -Raw $mp|ConvertFrom-Json}catch{$errors.Add("${mp}: JSON inválido");continue}
        $manifestIds += [string]$m.stage_id
        $expectedTypes=if($id-eq"${prefix}-000"){@("PLANNING_BASELINE")}elseif($id-eq"${prefix}-001"){@("PLAN_REVIEW")}else{@("EXECUTION","REPLANNING")}
        if($m.schema_version-ne 1 -or $m.stage_id-ne$id -or $m.stage_type-notin$expectedTypes){$errors.Add("${mp}: identidade/tipo inválido")}
        if($m.status-notin$states -or $m.human_gate-ne$true -or $m.critical_gate-notin@($true,$false)){$errors.Add("${mp}: status/human_gate/critical_gate inválido")}
        $dependencies=@($m.depends_on)
        if($id-eq"${prefix}-000" -and $dependencies.Count-ne 0){$errors.Add("${mp}: 000 não pode ter predecessora")}
        $stageNumber=[int]$id.Substring($id.Length-3)
        if($stageNumber-gt 0){
            $previous="${prefix}-$('{0:D3}' -f ($stageNumber-1))"
            if($m.stage_type-ne"REPLANNING" -and $previous-notin$dependencies){$errors.Add("${mp}: deve depender da predecessora imediata $previous")}
            if($m.stage_type-eq"REPLANNING" -and $dependencies.Count-eq 0){$errors.Add("${mp}: REPLANNING exige ao menos uma dependência aprovada")}
            foreach($dependency in $dependencies){
                if($dependency-notin$stageIds){$errors.Add("${mp}: dependência inexistente: $dependency");continue}
                if([int]$dependency.Substring($dependency.Length-3)-ge$stageNumber){
                    $dependencyManifestPath="docs/plans/manifests/$dependency.json";$dependencyType=""
                    if(Test-Path $dependencyManifestPath){try{$dependencyType=(Get-Content -Raw $dependencyManifestPath|ConvertFrom-Json).stage_type}catch{}}
                    if($dependencyType-ne"REPLANNING"){$errors.Add("${mp}: dependência futura só pode apontar para REPLANNING: $dependency")}
                }
            }
        }
        if([string]::IsNullOrWhiteSpace([string]$m.plan_revision) -or [string]::IsNullOrWhiteSpace([string]$m.rollback) -or @($m.allowed_subtrees).Count-eq 0 -or @($m.environments).Count-eq 0){$errors.Add("${mp}: revisão/rollback/subtrees/ambientes incompletos")}
        foreach($environment in @($m.environments)){if($environment-notin$knownEnvironments){$errors.Add("${mp}: ambiente inexistente: $environment")}}
        foreach($decision in @($m.decisions)){if($decision-notin$knownDecisions){$errors.Add("${mp}: decisão inexistente: $decision")}}
        foreach($risk in @($m.risks)){if($risk-notin$knownRisks){$errors.Add("${mp}: risco inexistente: $risk")}}
        foreach($requirement in @($m.requirements)){if($requirement-notin$knownRequirements){$errors.Add("${mp}: requisito inexistente: $requirement")}}
        if($requirementsByStage.ContainsKey($id)){foreach($expectedRequirement in @($requirementsByStage[$id])){if($expectedRequirement-notin@($m.requirements)){$errors.Add("${mp}: requisito da rastreabilidade ausente: $expectedRequirement")}}}
        if($m.stage_type-eq"EXECUTION" -and @($m.requirements).Count-eq 0){$errors.Add("${mp}: etapa de execução sem requisito relacionado")}
        if(-not(Test-Path $m.normative_plan)){$errors.Add("${mp}: normative_plan inexistente")}; if($m.dossier-ne"docs/plans/evidence/$id"){$errors.Add("${mp}: dossier incorreto")}
        $parts=@($m.parts); $expectedIds=1..4|ForEach-Object{"${id}-P$($_.ToString('00'))"}
        if(($expectedIds-join'|')-ne(@($parts|ForEach-Object part_id)-join'|')){$errors.Add("${mp}: partes devem ser P01..P04")}
        if((@($parts|ForEach-Object phase)-join'|')-ne'PLAN|DO|CHECK|ACT'){$errors.Add("${mp}: fases devem ser PLAN|DO|CHECK|ACT")}
        foreach($p in $parts){
            if($p.model_profile-notin$profiles -or -not$catalogProfiles.ContainsKey([string]$p.model_profile) -or $catalogProfiles[[string]$p.model_profile].State-ne"VERIFIED"){$errors.Add("${mp}: perfil não resolvido em $($p.part_id)")}
            foreach($field in @("role","validation","stop_condition")){if([string]::IsNullOrWhiteSpace([string]$p.$field)){$errors.Add("${mp}: $field ausente em $($p.part_id)")}}
            if(@($p.inputs).Count-eq 0 -or @($p.outputs).Count-eq 0){$errors.Add("${mp}: inputs/outputs ausentes em $($p.part_id)")}
        }
        if($id-in@("${prefix}-000","${prefix}-001")){
            if($m.critical_gate-ne$true){$errors.Add("${mp}: 000/001 devem ser critical_gate")}
            if((($parts|Where-Object phase -eq "PLAN").model_profile)-ne"DEEP" -or (($parts|Where-Object phase -eq "DO").model_profile)-ne"DEEP"){$errors.Add("${mp}: Plan/Do de 000/001 exigem DEEP")}
            if((($parts|Where-Object phase -eq "CHECK").model_profile)-ne"INDEPENDENT_REVIEW"){$errors.Add("${mp}: 000/001 exigem INDEPENDENT_REVIEW")}
            foreach($subtree in @($m.allowed_subtrees)){if($subtree-notmatch'^(docs|\.agents)(/|$)'){$errors.Add("${mp}: planejamento permite produto: $subtree")}}
        }
        if($m.stage_type-eq"REPLANNING"){
            if(-not$idsFrozen -or $s001-ne"APPROVED" -or $stageNumber-lt 3){$errors.Add("${mp}: REPLANNING só é permitida após 001 APPROVED, com IDs congelados e ID 003+")}
            if((@($m.requirements).Count+@($m.decisions).Count+@($m.risks).Count)-eq 0){$errors.Add("${mp}: REPLANNING sem requisito, decisão ou risco relacionado")}
            if($m.critical_gate-ne$true){$errors.Add("${mp}: REPLANNING deve ser critical_gate")}
            if((($parts|Where-Object phase -eq "PLAN").model_profile)-ne"DEEP" -or (($parts|Where-Object phase -eq "DO").model_profile)-ne"DEEP"){$errors.Add("${mp}: REPLANNING exige DEEP em Plan/Do")}
            foreach($subtree in @($m.allowed_subtrees)){if($subtree-notmatch'^(docs|\.agents)(/|$)'){$errors.Add("${mp}: REPLANNING permite produto: $subtree")}}
        }
        if($m.critical_gate-eq$true -and (($parts|Where-Object phase -eq "CHECK").model_profile)-ne"INDEPENDENT_REVIEW"){$errors.Add("${mp}: critical_gate exige INDEPENDENT_REVIEW")}
        $rowState=($pdcaRows|Where-Object Id -eq $id).State;if($rowState -and $m.status-ne$rowState){$errors.Add("${mp}: status diverge do PDCA")}
        if(($rowState -and $rowState-ne"PLANNED") -or $m.stage_type-eq"REPLANNING"){
            foreach($dependency in $dependencies){
                $dependencyState=($pdcaRows|Where-Object Id -eq $dependency).State
                if($dependencyState-ne"APPROVED"){$errors.Add("${mp}: dependência $dependency não está APPROVED")}
            }
        }
        if($rowState-in@("IN_PROGRESS","BLOCKED","REWORK")){Require-File "docs/plans/evidence/$id/EVIDENCE.md"}
        if($rowState-in@("DELIVERED_FOR_REVIEW","APPROVED")){foreach($f in @("DELIVERY.md","REVIEW.md","TESTS.md","MANIFEST.json","EVIDENCE.md")){Require-File "docs/plans/evidence/$id/$f"}}
        if($rowState-in@("DELIVERED_FOR_REVIEW","APPROVED") -and (Test-Path "docs/plans/evidence/$id/MANIFEST.json")){
            try{$deliveryManifest=Get-Content -Raw "docs/plans/evidence/$id/MANIFEST.json"|ConvertFrom-Json
                if($deliveryManifest.schema_version-ne 1 -or $deliveryManifest.stage_id-ne$id -or $deliveryManifest.state-ne"DELIVERED_FOR_REVIEW" -or $deliveryManifest.next_stage_started-ne$false){$errors.Add("dossiê ${id}: identidade/estado/next_stage_started inválido")}
                foreach($field in @("attempt_id","base_state","started_at","finished_at")){if([string]::IsNullOrWhiteSpace([string]$deliveryManifest.$field)){$errors.Add("dossiê ${id}: $field ausente")}}
                $expectedArchive="docs/plans/evidence/$id/attempts/$($deliveryManifest.attempt_id)"
                if($deliveryManifest.attempt_archive-ne$expectedArchive){$errors.Add("dossiê ${id}: attempt_archive incorreto")}
                if(-not(Test-Path -LiteralPath "docs/plans/evidence/$id/evidence" -PathType Container)){$errors.Add("dossiê ${id}: diretório evidence ausente")}
                foreach($archiveFile in @("DELIVERY.md","REVIEW.md","TESTS.md","MANIFEST.json","EVIDENCE.md")){
                    $topFile="docs/plans/evidence/$id/$archiveFile";$snapshotFile="$expectedArchive/$archiveFile";Require-File $snapshotFile
                    if((Test-Path -LiteralPath $topFile -PathType Leaf) -and (Test-Path -LiteralPath $snapshotFile -PathType Leaf) -and (Get-FileHash -LiteralPath $topFile -Algorithm SHA256).Hash-ne(Get-FileHash -LiteralPath $snapshotFile -Algorithm SHA256).Hash){$errors.Add("dossiê ${id}: snapshot da tentativa diverge em $archiveFile")}
                }
                foreach($deliveredAttempt in @($deliveredAttempts[$id])){
                    foreach($historicalFile in @("DELIVERY.md","REVIEW.md","TESTS.md","MANIFEST.json","EVIDENCE.md")){Require-File "docs/plans/evidence/$id/attempts/$deliveredAttempt/$historicalFile"}
                }
                $deliveredFiles=@($deliveryManifest.files)
                if($deliveredFiles.Count-eq 0){$errors.Add("dossiê ${id}: files vazio")}
                foreach($deliveredFile in $deliveredFiles){
                    if(-not(Test-Path -LiteralPath $deliveredFile -PathType Leaf)){$errors.Add("dossiê ${id}: arquivo declarado inexistente: $deliveredFile");continue}
                    $hash=[string]$deliveryManifest.hashes_sha256.$deliveredFile
                    if($hash-notmatch'^[a-fA-F0-9]{64}$'){$errors.Add("dossiê ${id}: hash SHA-256 ausente/inválido: $deliveredFile");continue}
                    if((Get-FileHash -LiteralPath $deliveredFile -Algorithm SHA256).Hash-ne$hash){$errors.Add("dossiê ${id}: hash diverge: $deliveredFile")}
                }
                $commands=@($deliveryManifest.commands)
                if($commands.Count-eq 0){$errors.Add("dossiê ${id}: commands vazio")}
                foreach($command in $commands){if([string]::IsNullOrWhiteSpace([string]$command.command) -or [string]::IsNullOrWhiteSpace([string]$command.result) -or [string]::IsNullOrWhiteSpace([string]$command.evidence)){$errors.Add("dossiê ${id}: comando sem resultado/evidência")}}
                if($deliveryManifest.tests.status-notin@("PASS","PASS_WITH_JUSTIFIED_NA")){$errors.Add("dossiê ${id}: testes sem status de entrega válido")}
                $modelUses=@($deliveryManifest.models_used)
                if((@($modelUses|ForEach-Object part_id)-join'|')-ne($expectedIds-join'|')){$errors.Add("dossiê ${id}: models_used deve registrar todas as partes em ordem")}
                foreach($modelUse in $modelUses){
                    if([string]::IsNullOrWhiteSpace([string]$modelUse.part_id) -or [string]::IsNullOrWhiteSpace([string]$modelUse.role) -or [string]::IsNullOrWhiteSpace([string]$modelUse.profile) -or [string]::IsNullOrWhiteSpace([string]$modelUse.model) -or [string]::IsNullOrWhiteSpace([string]$modelUse.effort) -or [string]::IsNullOrWhiteSpace([string]$modelUse.context_id)){$errors.Add("dossiê ${id}: models_used incompleto")}
                    $declaredPart=$parts|Where-Object part_id -eq $modelUse.part_id|Select-Object -First 1
                    if($declaredPart -and $modelUse.profile-ne$declaredPart.model_profile){$errors.Add("dossiê ${id}: perfil efetivo diverge do manifesto em $($modelUse.part_id)")}
                    if($declaredPart -and $modelUse.role-ne$declaredPart.role){$errors.Add("dossiê ${id}: papel efetivo diverge do manifesto em $($modelUse.part_id)")}
                    $catalogEntry=$catalogProfiles[[string]$modelUse.profile]
                    if($catalogEntry -and $modelUse.model-notin@($catalogEntry.Model,$catalogEntry.Fallback)){$errors.Add("dossiê ${id}: modelo efetivo fora do catálogo em $($modelUse.part_id)")}
                    if($catalogEntry -and (-not$effortRanks.ContainsKey([string]$modelUse.effort) -or $effortRanks[[string]$modelUse.effort]-lt$effortRanks[[string]$catalogEntry.Effort])){$errors.Add("dossiê ${id}: esforço efetivo abaixo do catálogo em $($modelUse.part_id)")}
                }
                $doContext=($modelUses|Where-Object part_id -eq "${id}-P02").context_id;$checkContext=($modelUses|Where-Object part_id -eq "${id}-P03").context_id
                if($m.critical_gate-eq$true -and $doContext-eq$checkContext){$errors.Add("dossiê ${id}: revisão crítica não usou contexto independente")}
            }catch{$errors.Add("dossiê ${id}: MANIFEST.json inválido")}
        }
    }
    Assert-Sequence $stageIds $manifestIds "manifests"

    $s001=($pdcaRows|Where-Object Id -eq "${prefix}-001").State
    if($s001-in@("DELIVERED_FOR_REVIEW","APPROVED") -and (Test-Path "docs/plans/DECISION-REGISTER.md")){
        $d=Get-Content -Raw "docs/plans/DECISION-REGISTER.md"
        if($d-match'(?mi)^\| DEC-[0-9]{3} \|[^\r\n]*\| (OPEN|PROPOSED) \| (DEV|OWNER|PROPRIET.RIO|USU.RIO) \|'){$errors.Add("DECISION-REGISTER.md: 001 entregue com decisão do DEV aberta")}
    }
    if($s001-in@("DELIVERED_FOR_REVIEW","APPROVED") -and (Test-Path "docs/planning/QUESTION-LEDGER.md")){
        foreach($line in Get-Content "docs/planning/QUESTION-LEDGER.md"){
            if($line-notmatch'^\| Q-[^|]+ \|'){continue};$cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim()})
            if($cols.Count-ge 9 -and $cols[5]-in@("OPEN","PROPOSED") -and $cols[6]-match'^(DEV|OWNER|PROPRIET.RIO|USU.RIO)$' -and $cols[8]-match'(?i)alto|material|cr.tico'){$errors.Add("QUESTION-LEDGER.md: 001 entregue com questão material do DEV aberta: $($cols[0])")}
        }
    }
    if($s001-in@("DELIVERED_FOR_REVIEW","APPROVED") -and (Test-Path "docs/planning/RISK-REGISTER.md")){
        foreach($line in Get-Content "docs/planning/RISK-REGISTER.md"){
            if($line-notmatch'^\| RISK-[0-9]{3} \|'){continue};$cols=@($line.Trim('|').Split('|')|ForEach-Object{$_.Trim()})
            if($cols.Count-lt 11){$errors.Add("RISK-REGISTER.md: linha incompleta: $($cols[0])");continue}
            if($cols[10]-in@("PRESENT","MITIGATION_DEFINED","DECISION_PENDING") -and ([string]::IsNullOrWhiteSpace($cols[5]) -or [string]::IsNullOrWhiteSpace($cols[6]) -or [string]::IsNullOrWhiteSpace($cols[7]) -or [string]::IsNullOrWhiteSpace($cols[8]) -or [string]::IsNullOrWhiteSpace($cols[9]))){$errors.Add("RISK-REGISTER.md: risco aberto sem indicador/mitigação/contingência/owner/deadline: $($cols[0])")}
        }
    }

    $markdown=@(Get-ChildItem -Recurse -File -Filter "*.md" -Path ".agents","docs" -ErrorAction SilentlyContinue); if(Test-Path "AGENTS.md"){$markdown+=Get-Item "AGENTS.md"}
    foreach($file in $markdown){
        $text=Get-Content -Raw $file.FullName
        if($file.FullName-notmatch'[\\/]assets[\\/]templates[\\/]' -and $text-match'\{\{[^}]+\}\}|(?m)^\s*(?:[-*]\s*)?(?:TODO|TBD|FIXME)(?:\s*[:=-]|\s*$)'){$errors.Add("$($file.FullName): placeholder não resolvido")}
        foreach($link in [regex]::Matches($text,'\[[^\]]*\]\(([^)]+)\)')){$target=($link.Groups[1].Value.Trim()-split'#')[0];if([string]::IsNullOrWhiteSpace($target)-or$target-match'^(https?://|mailto:|#|/)'){continue};if(-not(Test-Path (Join-Path $file.DirectoryName $target))){$errors.Add("$($file.FullName): link local inexistente: $target")}}
    }
    if($errors.Count-gt 0){throw (($errors|Sort-Object -Unique|ForEach-Object{"ERROR: $_"}) -join "`n")}
    Write-Host "Generated PDCA plan validation passed: $($stageIds.Count) stages, prefix $prefix, $($manifestIds.Count) manifests."
} finally { Pop-Location }
