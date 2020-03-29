rem apply eliptic wing-planform to existing XFLR5-XML-File
python ..\..\..\src\python\planform_creator.py

rem create batchfile for strak generation
python ..\..\..\src\python\strak_machine.py -x plane_out -o make_fx_strak -i iFX-strak -it iFX-tip -r 137000

rem execute batchfile, generate strak
make_fx_strak.bat