Advanced:

Creation of a high end airfoil for F3F 
seehttp://www.rc-network.de/forum/showthread.php/769110-Entwicklung-eines-F3F-Profils

This optimization squeezes the last out of the possibiities of Xoptfoil.
The goal was to have very low drag close to cd=0.05 a good dynamic behaviour with flaps.

The airfoil is optimized for a flight envelope along type 2 polar at RE*sqrt(cl) = 150000 
with some additional operating points at Re = 600000 to ensure dynamic flight capabilities

The seed airfoil is a simple hand made spline airfoil out of xflr5.
Smoothing is used during optimization to reduce the danger of bumps on the surface.

Two additional geometric targets close to the trailing edge are defined to achieve minimum thickness
and to avoid xoptfoil artefacts where the upper surface at the trailing edge is lifted.

The drag target values at the operating points are the result of some iterations before.
The values are fine tuned to be as close as possible to the physical limits of the desired polar.

This optimization takes some time...

