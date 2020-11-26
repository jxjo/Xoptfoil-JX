# Change Log for Xoptfoil-JX

All notable changes to this project are documented in this file.

## [1.60] - 2020-11-27

This is a consolitation release with a bunch of smaller but nice enhancements and some refactorings inside the programm

### Xoptfoil-JX

- More robust xfoil driver which reduces the situations, xfoil doesn't get convergence at an operating point. Switching on `show_details` the xfoil driver will give some information about iterations and retries.
- Auto-detection of curvature constraints especially when using  'Hicks-Henne' shape functions. The various curvature parameters will be automatically set to achieve best possible quality for the optimized airfoil based on the quality of the seed airfoil. The new parameter `auto_curvature` is active by default.
- Smoothing of the seed airfoil surface is now done automatically if the auto-detection of curvature signals too many 'spikes' on the surface. Again `show_details` will give some more information. The new default value is now `do_smoothing = .true.`
- New screen output during particle swarm optimization to get more details about the success of each particle. This greatly improves an understanding of the overall optimization process based on particle swarm. Use `show_details` to get more information about single operating points especially when using `target_drag`
- Multi-threading is now always active also if `show_details = .true.`  
- Experimental: Retry of a single particle shape design if this design fails due to geometry constraints to improve optimization speed
- A lot of minor things...

### Xfoil_worker

- Added new option `-w set` which allows to modify the geometry parameters like thickness, camber and their positions of maximum  

- Added new option "-w check" to check the surface (curvature) quality of an airfoil and tell about best optimization options for the curvature. This is the same information as `auto_curvature = .true.` when using the optimizer.

- Added new option `-w blend` to blend an airfoil with another airfoil by xx%. 

## [1.52.1] - 2020-06-25

This is just a bugfix and maintenance release

### Xoptfoil-JX

- Fix: When smoothing an exception occured when the number of spikes became 0
- Fix: Typo 'highlow_treshold'.  
**breakin change:** All occurences of `highlow_treshold` in input files have to be changed in `highlow_threshold`

### Xoptfoil_visualizer-JX

- In polar view little markers will show the value changes during optimization

### Xfoil_worker

- added new option "-w flap" to set flap at an airfoil and generate a new airfoil with flap set

## [1.52] - 2020-06-10

### Xoptfoil-JX

- Normalization of the airfoil - the optimized airfoil will be repaneled, moved, scaled and rotated to have the leading edge at 0,0 and the trailing edge at 1,0
  - depending on the repaneling result a new airfoil will have 200 or 201 points (if an additional leading edge point was added)
  - *breaking change* as the new airfoil can differ a little from previous runs
- Revision of smoothing and bump prevention for Hicks Henne shape functions
  - improved smoothing algorithm for the seed airfoil which now also includes leading edge area
  - new constraints max_curv_highlow_top and max_curv_highlow_top to limit the number of "bumps" at top and bottom side (default = 0)
  - moved parameter highlow_threshold from smoothing options to constraints namelist - *breaking change*
  - added examples for smoothing
  - see updated documentation for further information on smoothing and bump prevention
- New shape functions type 'camb-thick-plus' where upper and lower side of the airfoil is treated sperately when modifying the surface. This broadens and improves the usecases for this fast lane optimization type.
- Updated example of a 'high end F3F airfoil' to feature the capabilties of Xoptfoil-JX 
- New utility tool `Xfoil_worker` to execute little jobs - also convinient for automizations
  - generate polars of an airfoil in xfoil format which can be imported directly in xflr5 or flow5
  - normalize, optionally smooth an airfoil. Generates a 7 decimals airfoil file.
  - see documentation of `Xfoil_worker`
- For Windows the programs will be linked 'static' - so no more additional libraries are needed to run `Xoptfoil-JX` or `Xfoil_worker`
- Fixes
  - Different methods were used to calculate thickness and camber. This is now consistent taking the more precise method

### Xoptfoil_visualizer-JX

- Fixes
  - new plot objects were added at each refresh. This slowed down the visualizer more and more during long optimizations
  - removed flicker of the three plot windows

Thanks to Tobias for testing and advices, Alexander for his hint regarding smoothing algorithm - and of course to my super developer mate Matthias.

## [1.51.0] - 2020-05-24

### Xoptfoil-JX

- Revision of match_foils mode. Added ready to run examples to demonstrate the theoretical precision of Hicks-Henne or Camb-Thick based optimizations.
- New geometric constraint 'max_te_curvature' to handle curvature problems at TE when using Hicks-Henne shape functions
- Re-activated multi threading for optimization runs. Multi threading is switched off when 'show_details = .true.' is set to avoid mixed up console output.
- The smoothing option now affects only the seed airfoil at the beginning of an optimization. During optimization smoothing is switched off. The assessment of the surface is still active to detect bumps (High-Lows of curvature) - experimental.
- The default value for the number of panels ('npan') used for xfoil calculations is now set to 201 (previously 160). This avoids discrepancies between the xfoil results during optimization and the properties of the final airfoil (which has 201 panels)
- Fixed: When creating 'initial designs' the xfoil calculation isn't called anymore. This speeds up 'initial designs' heavily.
- Code cleanings and minor bugs

### Xoptfoil_visualizer-JX

- created short documentation (Xoptfoil_visualizer-JX Reference.pdf)
- show points of transition laminar-turbulent in geometric view
- some code revisions - ease setting of additional plot options (see documentation)

## [1.50.1] - 2020-04-24

- fixed a bug for LINUX colored output
- fixed LINUX built to handle environment variables properly (thanks John)
- fixed display of airfoil name when generating polar

## [1.50.0] - 2020-04-22

- Initial release with a compiled version for Windows
- ready to run examples in ./examples
