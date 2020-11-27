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
from json import load

# paths and separators
bs = "\\"
buildPath = 'build'
ressourcesPath = 'ressources'
scriptPath = 'scripts'
exePath = 'bin'

# fixed filenames

# name of python-interpreter
pythonInterpreterName = "python"

# filename of the visualizer
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"

# filename of progress-file
progressFileName = "progress.txt"

# filename of strakdata-file
strakDataFileName = ressourcesPath + bs + "strakdata.txt"


scriptsAsExe = False # TODO aus strakdata.txt einlesen!


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
        exit(-1)

    for line in file_content:
        # look for name of current airfoil
        if line.find("current airfoil") >= 0:
            splitlines = line.split(": ")
            airfoilname = splitlines[1]

    return airfoilname


################################################################################
# function that gets a single boolean parameter from dictionary and returns a
#  default value in case of error
def get_booleanParameterFromDict(dict, key, default):
    value = default
    try:
        string = dict[key]
        if (string == 'true'):
            value = True
        else:
            value = False
    except:
        print('parameter \'%s\' not specified, using' \
        ' default-value \'%s\'' % (key, str(value)))
    return value


def main():

    # try to open .json-file containg strakdata
    try:
        strakDataFile = open(strakDataFileName)
    except:
        ErrorMsg('failed to open file %s' % strakDataFileName)
        sys.exit(-1)

    # load dictionary from .json-file
    try:
        strakdata = load(strakDataFile)
        strakDataFile.close()
    except:
        ErrorMsg('failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
        sys.exit(-1)

    # determine if the visualizer shall be started as a python-script
    # or exe-file
    scriptsAsExe = get_booleanParameterFromDict(strakdata,
                             "scriptsAsExe", False)

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
    #print (systemString) #Debug
    #print ("Ready\n") #Debug

    # now execute system-call
    os.system(systemString)

if __name__ == '__main__':
    main()
