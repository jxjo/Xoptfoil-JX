rem neccessary zip-tool: 7-zip -> download from the internet, add to PATH-variable

rem create new directory
md Strakmachine_pure

rem create sub-folders
md .\Strakmachine_pure\doc
md .\Strakmachine_pure\XFLR5
md .\Strakmachine_pure\bin
md .\Strakmachine_pure\scripts
md .\Strakmachine_pure\build
md .\Strakmachine_pure\ressources

rem copy all script-files to strakmachine pure
rem use copy to create exe-files
copy ..\src\python\planform_creator.py .\Strakmachine_pure\scripts\
copy ..\src\python\strak_machineV2.py .\Strakmachine_pure\scripts\
copy ..\src\python\strak_machine_gui.py .\Strakmachine_pure\scripts\
copy ..\src\python\xoptfoil_visualizer-jx.py .\Strakmachine_pure\scripts\
copy ..\src\python\best_airfoil.py .\Strakmachine_pure\scripts\
copy ..\src\python\change_airfoilname.py .\Strakmachine_pure\scripts\
copy ..\src\python\show_status.py .\Strakmachine_pure\scripts\
copy ..\src\python\FLZ_Vortex_export.py .\Strakmachine_pure\scripts\

rem copy xoptfoil and xfoil-worker to bin-folder
copy .\bin\*.exe .\Strakmachine_pure\bin\

rem copy short instruction
copy ..\doc\Strak_Machine_Short_instruction.pdf .\Strakmachine_pure\

rem copy full reference
copy ..\doc\Strak_Machine_Reference.pdf .\Strakmachine_pure\doc

rem copy all ressource-files
xcopy .\ressources\*.* .\Strakmachine_pure\ressources\ /Y /E /H /C /I

rem copy airfoil-library
xcopy .\airfoil_library\*.* .\Strakmachine_pure\airfoil_library\ /Y /E /H /C /I

rem delete all batch files
del .\Strakmachine_pure\*.bat

rem copy batch files
xcopy .\batch_files\*.* .\Strakmachine_pure\ /Y /E /H /C /I

rem create zip-archive
rem 7z a .\Strakmachine_pure\ Strakmachine_pure_1_1.zip

pause
