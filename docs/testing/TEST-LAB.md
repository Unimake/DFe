# Laboratório

ENV-DLL: Windows win-x64, pwsh 7.6.5, SDK .NET 10.0.401, checkout local, fixtures sintéticas. Build DLL sem restore; compilar testes e executar `dotnet source/Unimake.DFe.Test/bin/Debug/net8.0/Unimake.DFe.Test.dll -class "Unimake.DFe.Test.NFeABI.Serializacao.SerializacaoDesserializacaoTest"`. ENV-INTEGRATED também executa `dotnet test C:\projetos\github\UniNFe\source\UniNFe.Test\UniNFe.Test.csproj -c Debug --no-restore` com filtro focado quando disponível. Certificado/rede são opt-in e não entram no gate determinístico.
