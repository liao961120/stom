@echo off
REM Adjust the path to your Rscript executable and the script file
set RscriptPath="C:\Users\tomliao\AppData\Local\Programs\R\R-4.2.3\bin\Rscript.exe"
set ScriptPath="C:\Users\tomliao\AppData\Local\Programs\R\R-4.2.3\library\stom\cli\pdf2svg.R"

if "%1"=="" (
    echo Usage: pdf2svg input.pdf [-o output.svg]
    exit /b 1
)

%RscriptPath% %ScriptPath% %*
