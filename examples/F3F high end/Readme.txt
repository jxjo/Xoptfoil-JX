Advanced:

Creation of a high end airfoil for F3F 
see http://www.rc-network.de/forum/showthread.php/769110-Entwicklung-eines-F3F-Profils

This optimization squeezes the last out of the possibiities of Xoptfoil.
The goal was to have very low drag close to cd=0.05 and a good dynamic behaviour with flaps.

The airfoil is optimized for a flight envelope along type 2 polar at RE*sqrt(cl) = 150000 
with some additional operating points at Re = 600000 to ensure dynamic flight capabilities

The seed airfoil is a simple hand made spline airfoil which ensures a good quality of the initial airfoil.
Serveral measures are taken to achieve a "bump free" perfect surface:

  curv_threshold       = 0.01

A very low value of the threshold helps for an early detection of eventual reversals. 
There is one reversal allowed at the bottom side ("rearload" type of airfoil) 

  highlow_treshold     = 0.02	

Also the threshold for bump detection has a quite low value to avoid any tendency of bumps.

  max_te_curvature     = 0.3

Finally the trailing edge curve control is also set to allow value to ensure a well formed trailing edge 
When tweaking these constraints, the visualizer is very suitable to watch the impact of the changes
(look a curvature plot) 

The drag target values at the operating points are the result of some iterations before.
The values are fine tuned to be as close as possible to the physical limits of the desired polar.

This optimization takes some time...

--

After the optimization the poalrs of the new airfoil are generated automatically using the tool Xfoil_worker
(have a look in the batch file make-JX-FXevo.bat) 

