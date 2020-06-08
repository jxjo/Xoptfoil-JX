xoptfoil-jx  -i iJX-FXevo-15.txt -o JX-FXevo-15 
xfoil_worker -i iPolars-T1.txt -a JX-FXevo-15.dat -o JX-FXevo-15 -r 600000 -w polar
xfoil_worker -i iPolars-T2.txt -a JX-FXevo-15.dat -o JX-FXevo-15 -r 150000 -w polar
