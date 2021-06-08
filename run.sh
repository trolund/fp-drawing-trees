cd Runner
dotnet run Runner.dll > ../output.ps
ps2pdf ../output.ps ../output.pdf >> ../log.txt
