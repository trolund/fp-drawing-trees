dotnet build 
cd Runner
dotnet run Runner.dll > ../output.ps
gs \
   -sDEVICE=pdfwrite \
   -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/printer \
   -dNOPAUSE \
   -dQUIET \
   -dBATCH \
   -sOutputFile=output.pdf \
    input.ps