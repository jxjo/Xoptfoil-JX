rem apply eliptic wing-planform to existing XFLR5-XML-File
start "" python ..\..\..\src\python\planform_creator.py
timeout /T 3

rem create batchfile for strak generation
python ..\..\..\src\python\strak_machine.py -x plane_out -o make_fx_strak -i iFX-strak -it iFX-tip -ft 7 -r 150000

rem execute batchfile, generate strak
make_fx_strak.bat