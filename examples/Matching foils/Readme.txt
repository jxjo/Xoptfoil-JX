Medium:


Using the "match_foils mode" of Xoptfoil to evaluate the geometric precision and learn about optimization.

match-hicks-henne.bat
---------------------

In this case hicks-henne shape functions are used to match a airfoil which is identical to the seed
airfoil (the particle swarm starts to deform the airfoil although it is not needed. So they have to 
find the way back to zero) 

This optimization goes to the limits of geometric precision of hicks-henne - which is amazing...

Start the visualizer with 'animate-hicks-henne'

match-camb-thick.bat
---------------------

In this case the 'camb-thick shape functions type' is used to match a airfoil which was previously deformed by changing camber and thickness (made in xflr5) 

This optimization shows quite well how fast 'camb-thick' converges to a solution.

Start the visualizer with 'animate-camb-thick'
