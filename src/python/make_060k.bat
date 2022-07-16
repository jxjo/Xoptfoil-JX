cd build

del progress.txt
echo main-task progress: 0.0 >> progress.txt

start "" "pythonw"  ..\scripts\show_status.py

echo sub-task start: create airfoil airfoil-060k.dat >> progress.txt
echo sub-task progress: 0.0 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-060k_1_1 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_060k_1.txt -r 60000 -a airfoil-138k.dat -o airfoil-060k_1_1
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-060k_1_1.dat -o airfoil-060k_1_1
echo sub-task progress: 14.8 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-060k_1_2 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_060k_1.txt -r 60000 -a airfoil-138k.dat -o airfoil-060k_1_2
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-060k_1_2.dat -o airfoil-060k_1_2
echo sub-task progress: 29.6 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-060k_1_3 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_060k_1.txt -r 60000 -a airfoil-138k.dat -o airfoil-060k_1_3
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-060k_1_3.dat -o airfoil-060k_1_3
echo sub-task progress: 44.4 >> progress.txt
python ..\scripts\best_airfoil.py -a airfoil-060k_1 -n 3
echo %TIME%   finalizing airfoil: airfoil-060k >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_060k.txt -r 60000 -a airfoil-060k_1.dat -o airfoil-060k
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-060k.dat -o airfoil-060k
echo sub-task progress: 100.0 >> progress.txt
echo %TIME%   finished airfoil >> progress.txt
echo %TIME%   calculating polars for airfoil: airfoil-060k.dat >> progress.txt
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "airfoil-060k.dat" -w polar -o "airfoil-060k" -r 900000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "airfoil-060k.dat" -w polar -o "airfoil-060k" -r 60000
python ..\scripts\strak_machine.py -w merge -p1 "airfoil-060k_polars\T1_Re0.900_M0.00_N7.0.txt"  -p2 "airfoil-060k_polars\T2_Re0.060_M0.00_N7.0.txt" -m "airfoil-060k_polars\merged_polar_060k.txt" -c 0.004444
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "airfoil-060k.dat" -w polar -o "airfoil-060k" -r 600000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "airfoil-060k.dat" -w polar -o "airfoil-060k" -r 40000
python ..\scripts\strak_machine.py -w merge -p1 "airfoil-060k_polars\T1_Re0.600_M0.00_N7.0.txt"  -p2 "airfoil-060k_polars\T2_Re0.040_M0.00_N7.0.txt" -m "airfoil-060k_polars\merged_polar_040k.txt" -c 0.004444
echo %TIME%   finished calculating polars >> progress.txt
copy airfoil-060k.dat airfoils\airfoil-060k.dat

echo sub-task end >> progress.txt

echo main-task progress: 100.0 >> progress.txt
cd..
