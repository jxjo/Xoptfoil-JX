cd build

del progress.txt
echo main-task progress: 0.0 >> progress.txt

start "" "pythonw"  ..\scripts\show_status.py

echo sub-task start: create airfoil JX-GS-080k.dat >> progress.txt
echo sub-task progress: 0.0 >> progress.txt
echo %TIME%   finalizing airfoil: JX-GS-080k >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_080k.txt -r 80000 -a JX-GS-138k.dat -o JX-GS-080k
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a JX-GS-080k.dat -o JX-GS-080k
echo sub-task progress: 100.0 >> progress.txt
echo %TIME%   finished airfoil >> progress.txt
echo %TIME%   calculating polars for airfoil: JX-GS-080k.dat >> progress.txt
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "JX-GS-080k.dat" -w polar -o "JX-GS-080k" -r 1200000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "JX-GS-080k.dat" -w polar -o "JX-GS-080k" -r 80000
python ..\scripts\strak_machine.py -w merge -p1 "JX-GS-080k_polars\T1_Re1.200_M0.00_N7.0.txt"  -p2 "JX-GS-080k_polars\T2_Re0.080_M0.00_N7.0.txt" -m "JX-GS-080k_polars\merged_polar_080k.txt" -c 0.004444
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "JX-GS-080k.dat" -w polar -o "JX-GS-080k" -r 900000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "JX-GS-080k.dat" -w polar -o "JX-GS-080k" -r 60000
python ..\scripts\strak_machine.py -w merge -p1 "JX-GS-080k_polars\T1_Re0.900_M0.00_N7.0.txt"  -p2 "JX-GS-080k_polars\T2_Re0.060_M0.00_N7.0.txt" -m "JX-GS-080k_polars\merged_polar_060k.txt" -c 0.004444
echo %TIME%   finished calculating polars >> progress.txt
copy JX-GS-080k.dat airfoils\JX-GS-080k.dat

echo sub-task end >> progress.txt

echo main-task progress: 100.0 >> progress.txt
cd..
