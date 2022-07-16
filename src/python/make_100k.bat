cd build

del progress.txt
echo main-task progress: 0.0 >> progress.txt

start "" "pythonw"  ..\scripts\show_status.py

echo sub-task start: create airfoil airfoil-100k.dat >> progress.txt
echo sub-task progress: 0.0 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-100k_1_1 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_100k_1.txt -r 100000 -a airfoil-138k.dat -o airfoil-100k_1_1
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-100k_1_1.dat -o airfoil-100k_1_1
echo sub-task progress: 14.8 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-100k_1_2 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_100k_1.txt -r 100000 -a airfoil-138k.dat -o airfoil-100k_1_2
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-100k_1_2.dat -o airfoil-100k_1_2
echo sub-task progress: 29.6 >> progress.txt
echo %TIME%   creating preliminary-airfoil: airfoil-100k_1_3 >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_100k_1.txt -r 100000 -a airfoil-138k.dat -o airfoil-100k_1_3
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-100k_1_3.dat -o airfoil-100k_1_3
echo sub-task progress: 44.4 >> progress.txt
python ..\scripts\best_airfoil.py -a airfoil-100k_1 -n 3
echo %TIME%   finalizing airfoil: airfoil-100k >> progress.txt
echo y | ..\bin\xoptfoil-jx.exe -i iOpt_100k.txt -r 100000 -a airfoil-100k_1.dat -o airfoil-100k
echo y | ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a airfoil-100k.dat -o airfoil-100k
echo sub-task progress: 100.0 >> progress.txt
echo %TIME%   finished airfoil >> progress.txt
echo %TIME%   calculating polars for airfoil: airfoil-100k.dat >> progress.txt
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "airfoil-100k.dat" -w polar -o "airfoil-100k" -r 1500000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "airfoil-100k.dat" -w polar -o "airfoil-100k" -r 100000
python ..\scripts\strak_machine.py -w merge -p1 "airfoil-100k_polars\T1_Re1.500_M0.00_N7.0.txt"  -p2 "airfoil-100k_polars\T2_Re0.100_M0.00_N7.0.txt" -m "airfoil-100k_polars\merged_polar_100k.txt" -c 0.004444
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "airfoil-100k.dat" -w polar -o "airfoil-100k" -r 900000
echo y | ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "airfoil-100k.dat" -w polar -o "airfoil-100k" -r 60000
python ..\scripts\strak_machine.py -w merge -p1 "airfoil-100k_polars\T1_Re0.900_M0.00_N7.0.txt"  -p2 "airfoil-100k_polars\T2_Re0.060_M0.00_N7.0.txt" -m "airfoil-100k_polars\merged_polar_060k.txt" -c 0.004444
echo %TIME%   finished calculating polars >> progress.txt
copy airfoil-100k.dat airfoils\airfoil-100k.dat

echo sub-task end >> progress.txt

echo main-task progress: 100.0 >> progress.txt
cd..
