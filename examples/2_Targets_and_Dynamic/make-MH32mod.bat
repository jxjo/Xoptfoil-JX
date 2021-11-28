@echo off
set Airfoil=MH32mod

rem Is xoptfoil-jx in the default directory?  
set LocalPath=..\..\windows\bin\
if not exist %LocalPath%xoptfoil-jx.exe set LocalPath=

%LocalPath%xoptfoil-jx -i %Airfoil%.inp -o %Airfoil%

pause


