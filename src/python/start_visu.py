#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      matth
#
# Created:     23.05.2020
# Copyright:   (c) matth 2020
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import os
import sys
from json import load

# paths and separators
bs = "\\"
buildPath = 'build'
scriptPath = 'scripts'
exePath = 'bin'

# fixed filenames
# name of python-interpreter
pythonInterpreterName = "python"

# filename of the visualizer
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"

# filename of progress-file
progressFileName = "progress.txt"

# gets the name of the airfoil that is currently processed by the strak-machine
def getCurrentAirfoilName(filename):
    file_content = None
    airfoilname = ""

    try:
        file = open(filename, 'r')
        file_content = file.readlines()
        file.close()
    except:
        print("Error, File %s could not be opened !" % filename)
        sys.exit(-1)

    for line in file_content:
        # look for name of current airfoil
        if line.find("current airfoil") >= 0:
            splitlines = line.split(": ")
            airfoilname = splitlines[1]

    return airfoilname


def main():
    # get program-call from arguments
    call = sys.argv[0]

    # was it an .exe-call ?
    if call.find('.exe') >= 0:
        # yes, perform all following calls as exe-calls
        scriptsAsExe = True
    else:
        # yes, perform all following calls as python-calls
        scriptsAsExe = False

    # get current airfoilname from progressfile for starting the visualizer
    airfoilname = getCurrentAirfoilName(buildPath + bs + progressFileName)

    # setup tool-calls
    exeCallString =  " .." + bs + exePath + bs
    pythonCallString = pythonInterpreterName + ' ..' + bs + scriptPath + bs

    if (scriptsAsExe):
        xoptfoilVisualizerCall = exeCallString + xoptfoilVisualizerName + '.exe'
    else:
        xoptfoilVisualizerCall = pythonCallString + xoptfoilVisualizerName + '.py'

    # change to build-directory
    os.chdir("build")

    # compose system-call-string
    systemString = ("%s -o 3 -c %s") % (xoptfoilVisualizerCall, airfoilname)

    # now execute system-call
    os.system(systemString)

if __name__ == '__main__':
    main()
