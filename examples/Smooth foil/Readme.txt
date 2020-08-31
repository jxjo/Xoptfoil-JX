This little examples show the usage of the utility program Xfoil_Worker.

Smooth_HD45.bat
---------------

The airfoil HD45 will be repaneled, normalized and smoothed.
With the input file 'iSmooth.txt' some parameters are fine tuned.
You may visualize the results by starting the visualizer with 

	xoptfoil_visualizer-jx.py -c HD45-smoothed -o 1 

Smooth_all.bat
---------------

A loop in the batch job will repanel, noramlize and smooth *all* files in the current directory.
No input file is defined so the default values are used for the operations.
