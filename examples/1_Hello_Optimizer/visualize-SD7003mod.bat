@echo off
set Airfoil=SD7003mod

rem ... is xoptfoil_visualizer-jx in the default directory?  
set LocalPath=..\..\windows\bin\
if not exist %LocalPath%xoptfoil-jx.exe set LocalPath=

%LocalPath%xoptfoil_visualizer-jx -c %Airfoil% -o 3