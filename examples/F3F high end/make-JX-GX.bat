set Profil=JX-GX-15

xoptfoil-jx  -i i%Profil%.txt -o %Profil% 
xfoil_worker -i iPolars-T1-n7.txt -a %Profil%.dat -o %Profil% -r 600000 -w polar
xfoil_worker -i iPolars-T2-n7.txt -a %Profil%.dat -o %Profil% -r 150000 -w polar
xfoil_worker -i iPolars-T1.txt -a %Profil%.dat -o %Profil% -r 600000 -w polar
xfoil_worker -i iPolars-T2.txt -a %Profil%.dat -o %Profil% -r 150000 -w polar
pause


