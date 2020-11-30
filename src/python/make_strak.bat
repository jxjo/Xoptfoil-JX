cd build

del progress.txt
echo timestamp: %TIME% >> progress.txt
echo main-task progress: 0.0 >> progress.txt

start "" "pythonw"  ..\scripts\show_status.py

echo main-task start: create whole set of strak-airfoils SD-strak-150k, SD-strak-080k>> progress.txt

echo sub-task start: create strak-airfoil SD-strak-150k.dat >> progress.txt
echo creating preliminary airfoil: SD-strak-150k_1_1 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_1.txt -r 150000 -a SD-root-220k.dat -o SD-strak-150k_1_1
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_1_1.dat -o SD-strak-150k_1_1
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-150k_1_2 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_1.txt -r 150000 -a SD-root-220k.dat -o SD-strak-150k_1_2
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_1_2.dat -o SD-strak-150k_1_2
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-150k_1_3 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_1.txt -r 150000 -a SD-root-220k.dat -o SD-strak-150k_1_3
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_1_3.dat -o SD-strak-150k_1_3
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-150k_1_4 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_1.txt -r 150000 -a SD-root-220k.dat -o SD-strak-150k_1_4
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_1_4.dat -o SD-strak-150k_1_4
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
python ..\scripts\best_airfoil.py -a SD-strak-150k_1 -n 4
echo creating preliminary airfoil: SD-strak-150k_2_1 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_2.txt -r 150000 -a SD-strak-150k_1.dat -o SD-strak-150k_2_1
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_2_1.dat -o SD-strak-150k_2_1
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-150k_2_2 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k_2.txt -r 150000 -a SD-strak-150k_1.dat -o SD-strak-150k_2_2
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k_2_2.dat -o SD-strak-150k_2_2
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
python ..\scripts\best_airfoil.py -a SD-strak-150k_2 -n 2
echo finalizing strak-airfoil: SD-strak-150k >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_150k.txt -r 150000 -a SD-strak-150k_2.dat -o SD-strak-150k
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-150k.dat -o SD-strak-150k
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 100.0 >> progress.txt
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "SD-strak-150k.dat" -w polar -o "SD-strak-150k" -r 1500000
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "SD-strak-150k.dat" -w polar -o "SD-strak-150k" -r 150000
python ..\scripts\strak_machineV2.py -w merge -p1 "SD-strak-150k_polars\T1_Re1.500_M0.00_N9.0.txt"  -p2 "SD-strak-150k_polars\T2_Re0.150_M0.00_N9.0.txt" -m "SD-strak-150k_polars\merged_polar_150k.txt" -c 0.010000
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "SD-strak-150k.dat" -w polar -o "SD-strak-150k" -r 800000
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "SD-strak-150k.dat" -w polar -o "SD-strak-150k" -r 80000
python ..\scripts\strak_machineV2.py -w merge -p1 "SD-strak-150k_polars\T1_Re0.800_M0.00_N9.0.txt"  -p2 "SD-strak-150k_polars\T2_Re0.080_M0.00_N9.0.txt" -m "SD-strak-150k_polars\merged_polar_080k.txt" -c 0.010000
copy SD-strak-150k.dat airfoils\SD-strak-150k.dat

echo sub-task end >> progress.txt

echo timestamp: %TIME% >> progress.txt
echo main-task progress: 33.3 >> progress.txt
echo sub-task start: create strak-airfoil SD-strak-080k.dat >> progress.txt
echo creating preliminary airfoil: SD-strak-080k_1_1 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_1.txt -r 80000 -a SD-strak-150k.dat -o SD-strak-080k_1_1
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_1_1.dat -o SD-strak-080k_1_1
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-080k_1_2 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_1.txt -r 80000 -a SD-strak-150k.dat -o SD-strak-080k_1_2
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_1_2.dat -o SD-strak-080k_1_2
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-080k_1_3 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_1.txt -r 80000 -a SD-strak-150k.dat -o SD-strak-080k_1_3
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_1_3.dat -o SD-strak-080k_1_3
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-080k_1_4 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_1.txt -r 80000 -a SD-strak-150k.dat -o SD-strak-080k_1_4
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_1_4.dat -o SD-strak-080k_1_4
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
python ..\scripts\best_airfoil.py -a SD-strak-080k_1 -n 4
echo creating preliminary airfoil: SD-strak-080k_2_1 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_2.txt -r 80000 -a SD-strak-080k_1.dat -o SD-strak-080k_2_1
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_2_1.dat -o SD-strak-080k_2_1
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
echo creating preliminary airfoil: SD-strak-080k_2_2 >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k_2.txt -r 80000 -a SD-strak-080k_1.dat -o SD-strak-080k_2_2
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k_2_2.dat -o SD-strak-080k_2_2
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 10.3 >> progress.txt
python ..\scripts\best_airfoil.py -a SD-strak-080k_2 -n 2
echo finalizing strak-airfoil: SD-strak-080k >> progress.txt
 ..\bin\xoptfoil-jx.exe -i istrak_080k.txt -r 80000 -a SD-strak-080k_2.dat -o SD-strak-080k
 ..\bin\xfoil_worker.exe -w smooth -i ..\ressources\iSmooth.txt -a SD-strak-080k.dat -o SD-strak-080k
echo timestamp: %TIME% >> progress.txt
echo sub-task progress: 100.0 >> progress.txt
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T1.txt" -a "SD-strak-080k.dat" -w polar -o "SD-strak-080k" -r 800000
 ..\bin\xfoil_worker.exe -i "..\ressources\iPolars_T2.txt" -a "SD-strak-080k.dat" -w polar -o "SD-strak-080k" -r 80000
python ..\scripts\strak_machineV2.py -w merge -p1 "SD-strak-080k_polars\T1_Re0.800_M0.00_N9.0.txt"  -p2 "SD-strak-080k_polars\T2_Re0.080_M0.00_N9.0.txt" -m "SD-strak-080k_polars\merged_polar_080k.txt" -c 0.010000
copy SD-strak-080k.dat airfoils\SD-strak-080k.dat

echo sub-task end >> progress.txt

echo timestamp: %TIME% >> progress.txt
echo main-task progress: 33.3 >> progress.txt
cd..
echo main-task end >> progress.txt
pause
