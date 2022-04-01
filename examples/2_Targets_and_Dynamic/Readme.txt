
Another simple example to understand the use op optimization targets and the comfort of dynamic weighting. 


Optimization Task: 

The well-known MH32 which works best at mid to higher Reynolds numbers shall be adapted to lower Reynolds number. To do this, operating points are based on the Type 2 polar (fixed lift) at Re*sqrt(cl) = 100,000.
Target cd values are taken to optimize mid cl range. Best glide ratio should be at cl = 0.7

The new airfoil should have a thickness of 8.2% which is achieved by a geometric target value.

Again shape_functions='camb-thick' will be used, where the airfoil is modified only by changing
camber, thickness, their highpoints and leading edge radius. This leads to a very fast convergence
towards the final airfoil. Only a few operting points are needed to achieve good results.


Run the example by: 

-> Double click 'make-MH32mod.bat' to start the optimizer 
-> Double click 'visualize-MH32mod.bat' to start the visualizer for this optimization

... compare the polar of the new MH32mod to the original polar with Xflr5.

Enjoy!





