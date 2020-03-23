xoptfoil-jx -i iFX-strak.txt -r 130000 -a JX-FXrcn-15.dat -o FX-strak-13
del FX-strak-13_optimization_history.dat /s /q 
del FX-strak-13_particles.csv  /s /q 
ren optimization_history.dat FX-strak-13_optimization_history.dat
ren particles.csv FX-strak-13_particles.csv 

xoptfoil-jx -i iFX-strak.txt -r 110000 -a FX-strak-13.dat -o FX-strak-11 
del FX-strak-11_optimization_history.dat /s /q 
del FX-strak-11_particles.csv  /s /q 
ren optimization_history.dat FX-strak-11_optimization_history.dat
ren particles.csv FX-strak-11_particles.csv 

xoptfoil-jx -i iFX-strak.txt -r  90000 -a FX-strak-11.dat -o FX-strak-09 
del FX-strak-09_optimization_history.dat /s /q 
del FX-strak-09_particles.csv  /s /q 
ren optimization_history.dat FX-strak-09_optimization_history.dat
ren particles.csv FX-strak-09_particles.csv 

xoptfoil-jx -i iFX-tip.txt   -r  60000 -a FX-strak-09.dat -o FX-strak-06 
del FX-strak-06_optimization_history.dat /s /q 
del FX-strak-06_particles.csv  /s /q 
ren optimization_history.dat FX-strak-06_optimization_history.dat
ren particles.csv FX-strak-06_particles.csv 