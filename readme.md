
<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/jxjo/Xoptfoil">
    <img src="images/logo.png" alt="Logo" width="800" >
  </a>
</p>

# Xoptfoil-JX

This is Xoptfoil-JX, a fork of Xoptfoil - the amazing airfoil optimizer by Daniel Prosser  



## About the project

The project was started to handle some off the difficulties when it comes to optimize an airfoil having more advanced requirements on the quality of the generated airfoil. 

Main changes and improvements made up to now based on the original Xoptfoil

* Aerodynamic target values for an operating point to ease the final tweaking of an optimization - or to reverse engineer an airfoil from an existing polar.
* Geometric target values to replace optimization based on geometric constraints
* Dynamic re-weighting of operating points during the optimization process
* Bump detection and preventation when using Hicks-Heenne shape functions.
* Instead of Hicks Henne shape functions the geometric airfoil parameters thickness, camber and leading edge radius can be used to perform a lightweight and fast optimization
* Support for polar type 1 (fixed speed) and polar type 2 (fixed lift) optimization
* Utility tool `Xfoil_worker` to perform a little jobs like airfoil normalization and smoothing of an airfoil or generation of a complete polar set of the final airfoil
* Some enhancements in the Visualizer


## Documentation 

The options of Xoptfoil-JX and some explanations can be found in the [Xoptfoil-JX Reference](https://github.com/jxjo/Xoptfoil/blob/master/doc/Xoptfoil-JX%20Reference.pdf)

Also have a look into [ChangeLog](https://github.com/jxjo/Xoptfoil/blob/master/doc/ChangeLog.pdf) for the newest enhancements and changes.

## Usage example

The development of a high end F3F airfoil using Xoptfoil-JX is described in the arctivle [Entwicklung eines F3F-Profils](http://www.rc-network.de/forum/showthread.php/769110-Entwicklung-eines-F3F-Profils) - sorry, it's in German.

Various ready to run examples can be found in the ./examples folder including a brief description of the features used.

## Installation

### Windows Runtime

The actual compiled Windows version of Xoptfoil-JX can be found in  [Code-Releases tab](https://github.com/jxjo/Xoptfoil/releases) on this side.
Download the zip file and copy the files either in an existing Xoptfoil directory or into a new directory of your choice. In the latter case add the bin folder of Xoptfoil-JX to your path environment (or just copy the exe files into your project folder)

Xoptfoil newbies will have to install a python environment to use the visualizer Xoptfoil_visualizer-JX

- [Python](https://www.python.org/downloads/) - the recommended minimum environment for Python. In this case two additional libraries have to be installed to Python with the command:
  - pip install matplotlib
  - pip install numpy
- [Anaconda](https://www.anaconda.com/distribution/) - a complete but quite large environment

Tip: There is also a ready-built EXE of the visualizer. Using this there is no need to install Python.

### Linux and Developers

Developers and linux users should download the complete repository. The build script (build_windows.bat and build_linux.sh) will start the compilation.

Windows developers will have to install the [MinGW toolchain](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/) for compilation.

## Meta and Contribution

* Initial fork and first modifications by Jochen Guenzel 
* Camber & Tickness based optimization by Matthias Boese and Jochen Guenzel

Feel free to contact us - and of course we are happy for any contributions and suggestions!

Jochen Guenzel, November 2021
