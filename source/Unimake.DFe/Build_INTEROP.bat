SET dllPath=%1%
SET dllPathSecurity=%2%Compilacao\INTEROP_Release\net472\Unimake.Security.Platform.dll
ECHO %dllPath%
ECHO %dllPathSecurity%

C:\Windows\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe %dllPath% /codebase /tlb
C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %dllPath% /codebase /tlb

C:\Windows\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe %dllPathSecurity% /codebase /tlb
C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %dllPathSecurity% /codebase /tlb
EXIT 0