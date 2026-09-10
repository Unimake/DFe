# Versionamento e build

Preservar `netstandard2.0` e C# 7.3. Schema NFeABI usa pasta/versionamento 1.00 e recursos embutidos. Não alterar valores existentes de enums. Build: `dotnet build "source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj" --no-restore`. Testes xUnit v3: compilar e executar a DLL net8.0 pela classe focada. Release/NuGet não faz parte deste plano.
