Change Log for Xoptfoil-JX

## [1.71.4 beta] - 2022-03

This is a minor maintenance release with a few enhancements. 
Have a look in the version 1.70 and 1.71 feature list to get all enhancements of version 1.70. 

Features
- New parameter `airfoil_te_gap` in `optimization_options` allows to set the trailing edge gap of the (seed) airfoil to a certain value in % of chord (=1.0). A trailing edge gap of at least 0.02% will smooth the results of xfoil viscous calculation.

Fixes:
- misbehaviour of the internal curvature routine when the panel length at trailing edge changes quickly which is controlled by the xfoil option `cterat`. Now the curvature and the 3. derivative of the airfoil surface polyline looks much "better" close to trailing edge. The default value of `cterat` is now again 0.15 which is also the typical default value of xfoil.
- curvature artefact at trailing edge when thickness and/or camber was changed.
- improved xfoil outlier detection for polar generation
- auto_curvature adapted to improved curvature calculation at trailing edge

Improvements: 
- added glide ratio values to `show_details` during optimization.
- all seed airfoils in `.\examples\JX Seed` do have now a trailing edge gap of 0.03%
- new seed airfoils for reflexed airfoils and symmetric airfoils
- more infos about results in match foil mode. 

### Xfoil_worker
- New option `-w set te=x` sets the trailing edge gap to the defined vaue in % of chord (=1) 


## [1.71.3] - 2022-01

This is bugfix release.  
Have a look in the version 1.70 feature list to get all enhancements of version 1.70. 

Fixes:
- Setting and optimization of flpas didn't work.
- Less safety threshold for auto curvature checks.
- Worker: Output filename for option -norm.
- Visualizer: Handling of directory path now compatible with LINUX
- Minor docu fixes.

Improvements: 
- Polar generation is done now in two parts starting at alpha = 0 downwards and then at alpha = 0 upwards to improve xfoil convergence.

## [1.71.2 ] - 2022-01

This is bugfix release.  
Have a look in the version 1.70 feature list to get all enhancements of version 1.70. 
  
- Visualizer stopped monitoring when no polar was generated during optimization.
- Xfoil paneling options didn't work. 
- Minor improvements for symmetrical airfoils. 

## [1.71.1 ] - 2021-12

This is bugfix release.  
Have a look in the version 1.70 feature list to get all enhancements of version 1.70. 

  
- Revision of "xfoil non convergence" logic. Avoid message "This should not happen..."
- Source module hierarchy corrected to allow compilation again ...  


## [1.71 ] - 2021-12

This is a minor maintenance release with a few, but fine enhancements. 
Have a look in the version 1.70 feature list to get all enhancements of version 1.70. 

  
- Optimization with target values - The new parameter `allow_improved_target` does not penalize an operating point if the result is better than the defined target. "Better" depends on the `optimization_type` of the operating point: lower drag, higher lift, higher glide ratio. Especially when all operating points are based on targets, `allow_improved_target` may show suprising improvements. 
- Presetting of the seed airfoil to geometry targets or constraints is now optional
- Further tweaks of dynamic weighting to be more robust
- Improvement of Xfoil PANGEN (repanel airfoil) mal beviour at trailing edge leading to smoother trailing edges
- Some minor bugs and enhancements...




## Xoptfoil-JX

## [1.70 ] - 2021-12

This is a major functional release with some enhancements and new features.
Have a look in the following feature list and the updated reference guide. 




## Xoptfoil-JX
  

- Dynamic weighting during optimization - The weighting of all operating points having target values and all geometry targets will be automatically adjusted to ensure that targets will be uniformly reached.  
Dynamic weighting eliminates the tedious task of fine tuning the weighting of each op point to get good results. The new parameter `dynamic_weighting` in namelist `&operating_conditions` is active by default.
Dynamic weightings can be watched with `show_details = .true.`

- Auto curvature control when using 'Hicks-Henne' shape functions. Right before optimization the various curvature parameters and constraints are evaluated based on the curvature quality of the seed airfoil and the initial values in the input file. During optimization these parameters are constrained to achieve best possible quality for the optimized airfoil. The parameter `auto_curvature` in namelist `&curvature` is active by default for Hicks-Henne shape functions.  
(This feature was already introduced in version 1.60 but didn't show the desired functionality)
- Auto retry of a single particle if its shape design fails due to geometry or curvature constraints. With auto retry the particle will have a second chance to create a valid design. Auto retry highly improves optimization speed. The parameter `auto_retry` is active by default for Hicks-Henne shape functions.
- Bubble detect of possible laminar separation bubble at an operating point. The detection is based on Xfoils TAU value (sheer stress getting < 0). Bubble detection does not directly affect the objective function - but bubbles will influence the   aerodynamic results. Bubble information will be displayed with activated `show_details`.
- Preset of thickness and/or camber of the seed airfoil when geometry targets or thickness/camber constraints are defined. This improves the quality of the initial designs for the particle swarm and reduces optimization time.
- New screen output during particle swarm optimization to get more details about the ongoing optimization of each op point. Use `show_details` to activate the details.
- A full Xfoil polar will be generated for each new design (parameter `generate_polar` in namelist `&polar_generation`). This polar is used by the visualizer as additional information in the polar plots during optimzation. This intermediate polar file uses the Xfoil polar format, so it can be imported into Xflr5 or Flow5.
- The temporary files to display the ongoing optimization with the visualizer moved to a subdirectory. This helps to keep working directory tidily.  
- Revision of the `Xoptfoil-JX` documentation including the `visualizer` and `Xfoil_worker`. You will now find not only the subset of parameters which extend the original Xoptfoil, but a complete listing of all parameters supported by Xoptfoil-JX including a bunch of tips for  usage. Please have a read...  
- New or improved example cases
- ... plus a lot of minor things ...

#### Breaking Changes

- All curvature related constraints moved from namelist `&constraints` to the new namelist `&curvature`
- Deprecated curvature constraints: `highlow_threshold` `max_curv_highlow_top` `max_curv_highlow_top` `max_spikes_top` `max_spikes_bot` 
- Deprecated geometry target types: `zTop` and `zBot` including their extra parameter `x_pos`
- Polar generation with Xptfoil-JX will now generate a single intermediate polar for each new design during optimization. This intermediate polar is used as a reference polar in the Viualizer. To create a polar set for the final, optimized airfoil, use the `Xfoil_Worker` (it will work like before). 




## Xoptfoil_visualizer-JX



- General revision of the visualizer with an additional plot for curvature and 3rd derivative of airfoil surface, new window sizes, colors, ...
- If available a real polar of the seed airfoil and the current design will pe plotted in the polar window (Xopfoil-JX option `generate_polar`). The polar is  helpful for an early estimation of optimzation success.  

### Have Fun!

---

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
