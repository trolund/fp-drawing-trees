@echo off
CD Runner
dotnet run Runner.dll > ..\output\output.ps
CD ..
CD output
ps2pdf output.ps output.pdf