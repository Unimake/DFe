@ECHO OFF
SETLOCAL
CHCP 65001 >NUL

POWERSHELL.EXE -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%~dp0Packaging\Pack-DFe.ps1" -Mode Offline
SET EXIT_CODE=%ERRORLEVEL%

IF NOT "%EXIT_CODE%"=="0" (
    ECHO.
    PAUSE
)

EXIT /B %EXIT_CODE%
