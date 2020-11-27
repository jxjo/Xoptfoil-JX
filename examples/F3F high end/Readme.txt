Advanced:

Creation of a high end airfoil for F3F 
see http://www.rc-network.de/forum/showthread.php/769110-Entwicklung-eines-F3F-Profils

This optimization squeezes the last out of the possibiities of Xoptfoil-JX.
The goal was to have low drag close to cd=0.1 and a good dynamic behaviour with flaps.

The airfoil is optimized for a flight envelope along type 2 polar at RE*sqrt(cl) = 150000 
and a good dynamic behaviour along Type 1 polar at Re = 600000.

Most of the operating points are based on xfoil ncrit=7, a few point use ncrit=9 to get an oveall robust airfoil design.

The seed airfoil is a simple hand made spline airfoil which ensures a good curvature quality of the initial airfoil. The "auto_curvature" feature of Xoptfoil-JX v1.60 will keep the curvature quality of the seed airfoil when optimizing the aerodynamic properties.

The drag target values defined at the operating points are the result of some iterations before.
The values are fine tuned to be as close as possible to the physical limits of the desired polar.

This optimization takes some time... Because of some random behaviour of the particle swarm, the optimization may lead to just 99% of the optimum based on the targets.  

--

After the optimization the polars of the new airfoil are generated automatically using the tool Xfoil_worker
(have a look in the batch file make-JX-FXevo.bat) 

