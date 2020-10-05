@echo off
rem
rem Xfoil_worker will smooth ("-w smooth") the airfoil ("-a HD45.dat"). 
rem A new airfoil with a default name will be generated ("HD45-smoothed.dat")
rem     
rem Option "-v" will generate the file "HD45-smoothed_design_coordinates.dat" to visualize the result in the visualizer.
rem 
xfoil_worker -w smooth -i iSmooth.txt -a HD45.dat -v
pause
