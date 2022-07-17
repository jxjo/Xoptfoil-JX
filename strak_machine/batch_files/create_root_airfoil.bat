@echo off 
echo Creating root airfoil JX-GS-14.dat..
cd .\airfoil_library\F3F\JX\JX-GS
..\..\..\..\bin\xfoil_worker.exe -w blend 24 -a JX-GS-15.dat -a2 JX-GS-10.dat -o JX-GS-14
cd ..\..\..\..
md .\build
copy .\airfoil_library\F3F\JX\JX-GS\JX-GS-14.dat .\build\JX-GS-14.dat

echo Success !
pause 
