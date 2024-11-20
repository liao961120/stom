@echo off
REM Adjust the path to your Rscript executable and the script file
set RscriptPath="C:\Users\tomliao\AppData\Local\Programs\R\R-4.2.3\bin\Rscript.exe"
set ScriptPath="C:\Users\tomliao\AppData\Local\Programs\R\R-4.2.3\library\stom\cli\svg2png.R"

if "%1"=="" (
    echo Usage: svg2png input.svg [-o output.png]
    exit /b 1
)

%RscriptPath% %ScriptPath% %*
