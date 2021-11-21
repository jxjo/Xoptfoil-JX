@echo off
set Airfoil=SD7003mod

rem Is there a Visualizer exe in the default directory?  
if exist ..\..\windows\bin\xoptfoil_visualizer-jx.exe (
  ..\..\windows\bin\xoptfoil_visualizer-jx -c %Airfoil% -o 3
) else (
rem Use normal python search path
  xoptfoil_visualizer-jx.py -c %Airfoil% -o 3
)