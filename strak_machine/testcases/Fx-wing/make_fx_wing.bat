rem apply eliptic wing-planform to existing XFLR5-XML-File
start "" python ..\..\..\src\python\planform_creator.py
timeout /T 3

rem create batchfile for strak generation
rem python ..\..\..\src\python\strak_machine.py -i fx_strakdata_pre -o make_fx_strak
python ..\..\..\src\python\strak_machine.py -i fx_strakdata_root -o make_fx_strak

rem execute batchfile, generate strak
make_fx_strak.bat