SET source=%1%
echo Registering: 32bits %source%
C:\Windows\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe %source% /codebase /tlb

echo Registering: 64bits %source%
C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %source% /codebase /tlb

