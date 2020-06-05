cd build
md SD-root-220k_polars
copy ..\foil_polars\*.* SD-root-220k_polars\*.*
copy ..\ressources\SD.dat SD.dat
copy ..\ressources\istrak_190k.txt istrak_190k.txt
copy ..\ressources\istrak_160k.txt istrak_160k.txt
copy ..\ressources\istrak_130k.txt istrak_130k.txt
copy ..\ressources\istrak_100k.txt istrak_100k.txt
copy ..\ressources\istrak_070k.txt istrak_070k.txt
change_airfoilname.py -i ..\ressources\SD.dat -o SD-root-220k.dat
copy SD-root-220k.dat airfoils\SD-root-220k.dat
xoptfoil-jx -i istrak_190k.txt -r 190000 -a SD.dat -o SD-strak-190k
copy SD-strak-190k.dat airfoils\SD-strak-190k.dat
xoptfoil-jx -i istrak_160k.txt -r 160000 -a SD.dat -o SD-strak-160k
copy SD-strak-160k.dat airfoils\SD-strak-160k.dat
xoptfoil-jx -i istrak_130k.txt -r 130000 -a SD.dat -o SD-strak-130k
copy SD-strak-130k.dat airfoils\SD-strak-130k.dat
xoptfoil-jx -i istrak_100k.txt -r 100000 -a SD.dat -o SD-strak-100k
copy SD-strak-100k.dat airfoils\SD-strak-100k.dat
xoptfoil-jx -i istrak_070k.txt -r 70500 -a SD.dat -o SD-strak-070k
copy SD-strak-070k.dat airfoils\SD-strak-070k.dat
cd..
