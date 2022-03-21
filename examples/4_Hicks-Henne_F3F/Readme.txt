
Finally a high end example of using the full power of Hicks-Henne shape functions to generate fine airfoils. 

Hicks_Henne shape functions are extremely powerful when it comes to more sophisticated airfoil optimization or airfoil design. The high number of degrees of freedom in shape manipulation allow highly optimized airfoils. 


Optimization Task: 

A new airfoil for the special flight task of F3F competitions shall be generated. "Polar by design" is used, where all operating points are defined with a target cd value. The optimizer will try to find an airfoil which completly satisfies these targets.
The operating points are part of the Type 1 polar at Re = 600,000. An additional operating point is defined at Re = 300.000 to reduce the typical weakness at lower Reynolds numbers as a result of the laminar separation bubble on top side of the airfoi. 

The dynamic weighting of the operating points during optimizations takes care that all operating points should reach their target in a balanced manner.

Automation: 

After the new airfoil is generated, the little tool program "Xfoil_worker" generates different polars ready to be important to Xflr5.


Run the example by: 

-> Double click 'make-F3F.bat' to start the optimizer 
-> Double click 'visualize-F3F.bat' to start the visualizer for this optimization

... have a look at the optimizer console window to watch the particle swarm when driving the operating points to their target (it takes a while ... popcorn and/or a glas of wine needed)

Enjoy!





