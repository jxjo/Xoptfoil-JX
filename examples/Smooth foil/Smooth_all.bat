@echo off
echo This little xfoil_worker job will smooth all airfoils in the current directory
pause
dir *.dat /B > temp.txt
for /f "delims=#" %%f in (temp.txt) do xfoil_worker -w smooth -a "%%f"
del temp.txt