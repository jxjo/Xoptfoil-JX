Medium: 

The low reynolds airfoil SD7003 is taken to generate the root airfoil of a
mid size fast glider similar to the airfoil MH30.

Here shape_functions='camb-thick' is used, where the airfoil is modified only by changing
camber, thickness, their highpoints and leading edge radius. This leads to a very fast convergence
towards the final airfoil. Only a few operting points are needed.

Operating point 4 uses a little trick: The optimization_type='min-glide-slope'ensures that the best 
glide ratio will be at this point. Point 3 are 5 are needed to calculate the slope of the curve at point 4.

The Reynolds number of the operating points, which is RE*sqrt(cl)=220000, is passed from the command line.

A the end of the optimization a polar ready to import into xflr5 will be generated.

