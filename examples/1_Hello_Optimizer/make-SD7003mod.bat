@echo off
set Airfoil=SD7003mod

rem Is xoptfoil-jx in the default directory?  
if exist ..\..\windows\bin\xoptfoil-jx.exe (
  ..\..\windows\bin\xoptfoil-jx -i %Airfoil%.inp -o %Airfoil%   
) else (
rem no - use normal windows search path
  xoptfoil-jx -i %Airfoil%.inp -o %Airfoil%   
)

pause


