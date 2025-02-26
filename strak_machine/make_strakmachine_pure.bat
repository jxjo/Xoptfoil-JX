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
copy ..\src\python\xoptfoil_visualizer-jx.py .\Strakmachine_pure\scripts\
copy ..\src\python\best_airfoil.py .\Strakmachine_pure\scripts\
copy ..\src\python\change_airfoilname.py .\Strakmachine_pure\scripts\
copy ..\src\python\show_status.py .\Strakmachine_pure\scripts\
copy ..\src\python\FLZ_Vortex_export.py .\Strakmachine_pure\scripts\

rem copy xoptfoil and xfoil-worker to bin-folder
copy ..\build\*.exe .\Strakmachine_pure\bin\

rem copy short instruction
copy ..\doc\Strak_Machine_Short_instruction.pdf .\Strakmachine_pure\

rem copy full reference
copy ..\doc\Strak_Machine_Reference.pdf .\Strakmachine_pure\doc

rem copy all ressource-files
xcopy ..\src\python\ressources\*.* .\Strakmachine_pure\ressources\ /Y /E /H /C /I

rem copy airfoil-library
xcopy .\airfoil_library\*.* .\Strakmachine_pure\airfoil_library\ /Y /E /H /C /I

rem create program calls
del .\Strakmachine_pure\*.bat
echo python .\scripts\strak_machineV2.py >>.\Strakmachine_pure\start_strakmachine.bat
echo pause >>.\Strakmachine_pure\start_strakmachine.bat

echo python scripts\planform_creator.py -p ressources/planformdata_wing >>.\Strakmachine_pure\create_all_planforms.bat
echo copy build\planforms\XFLR5_wing.xml ressources\XFLR5_wing.xml >>.\Strakmachine_pure\create_all_planforms.bat
echo copy build\planforms\FLZ_wing.flz ressources\FLZ_wing.flz >>.\Strakmachine_pure\create_all_planforms.bat
echo python scripts\planform_creator.py -p ressources/planformdata_tail >>.\Strakmachine_pure\create_all_planforms.bat
echo pause >>.\Strakmachine_pure\create_all_planforms.bat

echo python scripts/planform_creator.py -p ressources/planformdata_root >>.\Strakmachine_pure\create_root_airfoil.bat
echo copy build\airfoils\JX-GS-139k.dat airfoil_library\F3F\JX\JX-GS\JX-GS-14.dat >>.\Strakmachine_pure\create_root_airfoil.bat
echo pause >>.\Strakmachine_pure\create_root_airfoil.bat

echo python scripts\planform_creator.py -p ressources/planformdata_tail>>.\Strakmachine_pure\create_tail_planform.bat
echo pause >>.\Strakmachine_pure\create_tail_planform.bat

echo python scripts\planform_creator.py -p ressources/planformdata_wing >>.\Strakmachine_pure\create_wing_planform.bat
echo copy build\planforms\XFLR5_wing.xml ressources\XFLR5_wing.xml >>.\Strakmachine_pure\create_wing_planform.bat
echo copy build\planforms\FLZ_wing.flz ressources\FLZ_wing.flz >>.\Strakmachine_pure\create_wing_planform.bat
echo pause >>.\Strakmachine_pure\create_wing_planform.bat

echo pythonw  .\scripts\show_status.py >>.\Strakmachine_pure\show_status.bat

rem create zip-archive
rem 7z a .\Strakmachine_pure\ Strakmachine_pure_1_1.zip
