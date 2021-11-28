@echo off
set Airfoil=F3F-Example

rem Is xoptfoil-jx in the default directory?  
set LocalPath=..\..\windows\bin\
if not exist %LocalPath%xoptfoil-jx.exe set LocalPath=

%LocalPath%xoptfoil-jx -i %Airfoil%.inp -o %Airfoil%

rem Generate polars for Xflr5 of the new airfoil

start %LocalPath%xfoil_worker -w polar -i Polars-T1-n7.inp -a %Airfoil%.dat -r 300000 
start %LocalPath%xfoil_worker -w polar -i Polars-T1-n7.inp -a %Airfoil%.dat -r 800000 
start %LocalPath%xfoil_worker -w polar -i Polars-T1-n7.inp -a %Airfoil%.dat -r 600000 
pause


