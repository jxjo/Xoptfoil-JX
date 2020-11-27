rem use pyinstaller to create exe-files
pyinstaller --onefile ..\src\python\planform_creator.py
pyinstaller --onefile ..\src\python\strak_machineV2.py
pyinstaller --onefile ..\src\python\xoptfoil_visualizer-jx.py
pyinstaller --onefile ..\src\python\start_visu.py
pyinstaller --onefile ..\src\python\best_airfoil.py

rem create new directory
md Strakmachine_instant

rem create sub-folders
md .\Strakmachine_instant\doc
md .\Strakmachine_instant\XFLR5
md .\Strakmachine_instant\bin
md .\Strakmachine_instant\build
md .\Strakmachine_instant\ressources

rem copy all exe-files to strakmachine instant
copy .\dist\*.exe .\Strakmachine_instant\bin\
copy ..\windows\bin .\Strakmachine_instant\bin\

rem copy short instruction
copy ..\doc\Strak_Machine_Short_instruction.pdf .\Strakmachine_instant\

rem copy full reference
copy ..\doc\Strak_Machine_Reference.pdf .\Strakmachine_instant\doc

rem copy all ressource-files to strakmachine instant
xcopy ..\src\python\ressources\*.* .\Strakmachine_instant\ressources\ /Y /E /H /C /I

rem copy XFLR5-Projects from strak-machine pure
xcopy .\Strakmachine_pure\XFLR5\*.* Strakmachine_instant\XFLR5\ /Y /E /H /C /I

rem copy build-results from strakmachine-pure
xcopy .\Strakmachine_pure\build\*.* Strakmachine_instant\build\ /Y /E /H /C /I

rem create zip-archive
rem 7z a .\Strakmachine_instant\ strakmachine_instant_1_1.zip

pause