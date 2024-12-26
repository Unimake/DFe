rem SET source=%1%
rem echo Registering: 32bits %source%
rem C:\Windows\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe %source% /codebase /tlb

rem echo Registering: 64bits %source%
rem C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %source% /codebase /tlb

rem copy "C:\projetos\github\Unimake.DFe\Exemplos\VB6\System.Security.Cryptography.Xml.dll" %3