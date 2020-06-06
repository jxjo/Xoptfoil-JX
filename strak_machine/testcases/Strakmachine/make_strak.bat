cd build
md rg15-root-220k_polars
copy ..\foil_polars\*.* rg15-root-220k_polars\*.*
copy ..\ressources\rg15.dat rg15.dat
copy ..\ressources\istrak_190k.txt istrak_190k.txt
copy ..\ressources\istrak_160k.txt istrak_160k.txt
copy ..\ressources\istrak_130k.txt istrak_130k.txt
copy ..\ressources\istrak_100k.txt istrak_100k.txt
copy ..\ressources\istrak_070k.txt istrak_070k.txt
change_airfoilname.py -i ..\ressources\rg15.dat -o rg15-root-220k.dat
copy rg15-root-220k.dat airfoils\rg15-root-220k.dat
xoptfoil-jx -i istrak_190k.txt -r 190000 -a rg15.dat -o rg15-strak-190k
copy rg15-strak-190k.dat airfoils\rg15-strak-190k.dat
xoptfoil-jx -i istrak_160k.txt -r 160000 -a rg15.dat -o rg15-strak-160k
copy rg15-strak-160k.dat airfoils\rg15-strak-160k.dat
xoptfoil-jx -i istrak_130k.txt -r 130000 -a rg15.dat -o rg15-strak-130k
copy rg15-strak-130k.dat airfoils\rg15-strak-130k.dat
xoptfoil-jx -i istrak_100k.txt -r 100000 -a rg15.dat -o rg15-strak-100k
copy rg15-strak-100k.dat airfoils\rg15-strak-100k.dat
xoptfoil-jx -i istrak_070k.txt -r 70500 -a rg15.dat -o rg15-strak-070k
copy rg15-strak-070k.dat airfoils\rg15-strak-070k.dat
cd..
