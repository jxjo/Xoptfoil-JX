#!/usr/bin/env python

#  This file is part of XOPTFOIL-JX.

#  XOPTFOIL-JX is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  XOPTFOIL-JX is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with XOPTFOIL-JX.  If not, see <http://www.gnu.org/licenses/>.

#  Copyright (C) 2020 Matthias Boese

import xml.etree.ElementTree as ET
import argparse
from sys import version_info
from sys import exit
import json
import os

################################################################################
#
# example-dictionary for creating .json-file
#
################################################################################
strakdata = {
            # folder containing the inputs-files
            "inputFolder": 'ressources',
            # folder containing the output / result-files
            "outputFolder": 'build',
            # name of XFLR5-xml-file
            "XMLfileName": 'plane_template.xml',
            # choice of the planform, 'wing' or 'fin'
            "planformType": 'wing',
            # ReSqrtCl of root airfoil
            "ReSqrtCl": '150000',
            # strategy for strak-airfoil-generation
            "strategy" : 'fromRootAirfoil',
             # name of the xoptfoil-inputfile for strak-airfoil(s)
            "strakInputFileName": 'i-strak.txt',
            # name of the xoptfoil-inputfile for tip-airfoil(s)
            "tipInputFileName": 'i-tip.txt',
            # number of tip-airfoils
            "numberOfTipAirfoils": 0,
            # generate batchfile for running Xoptfoil
            "generateBatchfile" : 'true',
            # name of the batchfile
            "batchfileName" : 'make_strak.bat',
            }

################################################################################
#
# strakData class
#
################################################################################
class strakData:
    def __init__(self):
        self.inputFolder = ''
        self.outputFolder = ''
        self.airfoilFolder = ''
        self.xmlFileName = None
        self.strakInputFileName = 'i-strak.txt'
        self.tipInputFileName = None
        self.numberOfTipAirfoils = 0
        self.ReSqrtCl = 150000
        self.useWingPlanform = True
        self.fromRootAirfoil= True
        self.generateBatch = True
        self.batchfileName = 'make_strak.bat'
        self.wingData = {}


################################################################################
# Input function that checks python version
def my_input(message):

  # Check python version

  python_version = version_info[0]

  # Issue correct input command

  if (python_version == 2):
    return raw_input(message)
  else:
    return input(message)


################################################################################
# function that gets the name of the wing
def get_wingName(wing):
    for name in wing.iter('Name'):
        return name.text

    # name was not found, return default-name
    return 'wing'


################################################################################
# function, that gets the chord-length of a section
def get_chordFromSection(section):
    # create an empty list
    chordList = []

    # iterate through elements
    for chord in section.iter('Chord'):
        # convert text to float
        chordlength = float(chord.text.strip("\r\n\t '"))

        #append chordlength to list
        chordList.append(chordlength)

    return chordList


################################################################################
# function that gets the airfoil-name of a section
def get_airfoilNameFromSection(section):
    # create an empty list
    airfoilNameList = []

    # iterate through elements
    for airfoilName in section.iter('Left_Side_FoilName'):

        #append airfoilName to list
        airfoilNameList.append(airfoilName.text)

    return airfoilNameList


################################################################################
# function that gets the chord-lengths of the wing
def get_wingChords(wing):
    # iterate the elements of the wing
    for section in wing.iter('Sections'):
        return get_chordFromSection(section)


################################################################################
# function that gets the airfoil-names of the wing
def get_airfoilNames(wing):
    # iterate the elements of the wing
    for section in wing.iter('Sections'):
        return get_airfoilNameFromSection(section)


################################################################################
# function that reads plane-data from XFLR5 XML-file
def read_planeDataFile(fileName):

    # init data as an empty list
    data = []

    # parse the file containing XFLR5-plane-data
    tree = ET.parse(fileName)

    #get root of XML-tree
    root = tree.getroot()

    # find wing-data
    for wing in root.iter('wing'):
        # create dictionary containg the wing-data
        wingDict = 	{ 'name': get_wingName(wing),
                      'chordLengths': get_wingChords(wing),
                      'airfoilNames': get_airfoilNames(wing)
                    }

        #append dictionary to data
        data.append(wingDict)

    # debug output
    #print data
    return data


################################################################################
# function that gets the name of an airfoil
def get_FoilName(wing, index):

    # get airfoil-names from wing-dictionary
    airfoilNames = wing.get('airfoilNames')
    foilName = airfoilNames[index]

    return (foilName)



################################################################################
# function that generates commandlines to run Xoptfoil
def generate_commandlines(params):

    #create an empty list
    commandLines = []

    # if strak-airfoil is created from previous airfoil, an example-file looks
    # like THIS
    ##xoptfoil-jx -i iFX-strak.txt -r 130000 -a JX-FXrcn-15.dat -o FX-strak-13
    ##xoptfoil-jx -i iFX-strak.txt -r 110000 -a FX-strak-13.dat -o FX-strak-11
    ##xoptfoil-jx -i iFX-strak.txt -r  90000 -a FX-strak-11.dat -o FX-strak-09
    ##xoptfoil-jx -i iFX-tip.txt   -r  60000 -a FX-strak-09.dat -o FX-strak-06

    # if strak-airfoil is created from root-airfoil, an example looks like THIS
    # xoptfoil-jx -i iSD-strak.txt -r 190000 -a SD-root-22.dat    -o SD-strak-19
    # xoptfoil-jx -i iSD-strak.txt -r 160000 -a SD-root-22.dat    -o SD-strak-16
    # xoptfoil-jx -i iSD-strak.txt -r 130000 -a SD-root-22.dat    -o SD-strak-13
    # xoptfoil-jx -i iSD-strak.txt -r 100000 -a SD-root-22.dat    -o SD-strak-10
    # xoptfoil-jx -i iSD-strak.txt -r  70500 -a SD-root-22.dat    -o SD-strak-07

    # do some initializations / set local variables
    rootFoilName = get_FoilName(params.wingData, 0)
    numChords = len(params.wingData.get('chordLengths'))
    ReSqrtCl = params.ReSqrtCl
    prevChord = 0.0
    ReSqrtCl_old = 0
    idx = 1

    if (params.numberOfTipAirfoils == 0):
        firstTipIdx = numChords
    else:
        if (params.numberOfTipAirfoils < numChords):
            firstTipIdx = numChords - params.numberOfTipAirfoils

    # change current working dir to output folder
    commandline = "cd %s\n" % params.outputFolder
    commandLines.append(commandline)

    #copy rootfoil to output-folder
    commandline = "copy ..\\%s\\%s %s\n" % \
    (params.inputFolder, rootFoilName+'.dat', rootFoilName +'.dat')
    commandLines.append(commandline)

    #copy root-airfoil to airfoil-folder
    commandline = "copy %s %s\\%s\n" % \
    (rootFoilName+'.dat', params.airfoilFolder, rootFoilName +'.dat')
    commandLines.append(commandline)

    for chord in params.wingData.get('chordLengths'):
        # skip the root airfoil
        if (prevChord > 0.0):
            # calculate new ReSqrtCl
            ReSqrtCl = ReSqrtCl_old * (chord / prevChord)

            if params.fromRootAirfoil:
                rootFoilName = get_FoilName(params.wingData, 0)
            else:
                rootFoilName = get_FoilName(params.wingData, idx-1)

            strakFoilName = get_FoilName(params.wingData, idx)

            #set input-file name for Xoptfoil
            iFile =  '../' + params.inputFolder + '/'

            #choose input-file, strak or tip?
            if (idx < firstTipIdx):
                #set inputFileName to strak-input-file
                iFile =  iFile + params.strakInputFileName
            else:
                #set inputFileName to tip-input-file
                iFile = iFile + params.tipInputFileName

            # generate Xoptfoil-commandline
            commandline = "xoptfoil-jx -i %s -r %d -a %s.dat -o %s\n" %\
                        (iFile, ReSqrtCl, rootFoilName, strakFoilName)

            commandLines.append(commandline)

            #copy strak-airfoil to airfoil-folder
            commandline = "copy %s %s\\%s\n" % \
            (strakFoilName +'.dat', params.airfoilFolder, strakFoilName +'.dat')
            commandLines.append(commandline)

            # set index to next airfoil
            idx = idx + 1

        # store actual chordLength and ReSqrtCl for calculations of next
        # strak-airfoil
        prevChord = chord
        ReSqrtCl_old = ReSqrtCl

    # change current working dir back
    commandline = "cd..\n"
    commandLines.append(commandline)

    return commandLines


################################################################################
# function that generates a Xoptfoil-batchfile
def generate_batchfile(batchFileName, commandlines):
    try:
        # create a new file
        outputfile = open(batchFileName, "w+")
    except:
        print ('Error, file %s could not be opened' % batchFileName)
        return

    # write Xoptfoil-commandline to outputfile
    for element in commandlines:
        outputfile.write(element)

    # close the outputfile
    outputfile.close()


################################################################################
# function that gets the name of the strak-machine-data-file
def getInFileName(args):

    if args.input:
        inFileName = args.input
    else:
        # use Default-name
        inFileName = 'strakdata'

    inFileName = inFileName + '.txt'
    print("filename for strak-machine input-data is: %s" % inFileName)
    return inFileName


################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    parser.add_argument("-input", "-i", help="filename of strak-machine input"\
                        "-file (e.g. strak_data)")

    # read arguments from the command line
    args = parser.parse_args()
    return (getInFileName(args))


################################################################################
# function that gets parameters from dictionary
def getParameters(dict):

    params = strakData()

    try:
        params.inputFolder = dict["inputFolder"]
    except:
        print ('inputFolder not specified, assuming no input-folder shall be used.')

    try:
        params.outputFolder = dict["outputFolder"]
    except:
        print ('outputFolder not specified, assuming no output-folder shall be used.')

    try:
        params.batchfileName = dict["batchfileName"]
    except:
        print ('batchfileName not found, setting default-filename \'%s\'.'\
                % params.batchfileName)

    try:
        params.xmlFileName = dict["XMLfileName"]
    except:
        print ('XMLfileName not specified, assuming no xml-file shall be used.')

    try:
        params.strakInputFileName = dict["strakInputFileName"]
    except:
        print ('strakInputFileName not found, setting default-filename \'%s\'.'\
                % params.strakInputFileName)

    try:
        params.tipInputFileName = dict["tipInputFileName"]
    except:
        params.numberOfTipAirfoils = 0
        print ('tipInputFileName not speciified, using no tip-input-file and'\
                'no tip-airfoils.')

    if params.tipInputFileName != None:
        try:
            params.numberOfTipAirfoils = dict["numberOfTipAirfoils"]
        except:
            print ('numberOfTipAirfoils not specified, assuming that no'\
                   'tip-airfoils shall be created.')

    try:
        params.ReSqrtCl = int(dict["ReSqrtCl"])
    except:
        print ('ReSqrtCl not specified, assuming default-value \'%d\'.' % params.ReSqrtCl)

    try:
        planformtype = dict["planformType"]
    except:
        planformtype == 'wing'
        print ('planformType not specified, assuming planformType is \'%s\'.'\
               % planformtype)


    if (planformtype == 'wing'):
        params.useWingPlanform = True
    else:
        params.useWingPlanform = False

    try:
        strategy = dict["strategy"]
    except:
        strategy == 'fromRootAirfoil'
        print ('strategy not specified, assuming strategy is \'%s\'.'\
         % strategy)

    if (strategy == 'fromRootAirfoil'):
        params.fromRootAirfoil = True
    else:
        params.fromRootAirfoil = False

    return params


################################################################################
# Main program
if __name__ == "__main__":

    #get command-line-arguments or user-input
    strakDataFileName = getArguments()

    #debug
    #json.dump(strakdata, open("strakdata.txt",'w'))

    # try to open .json-file
    try:
        strakDataFile = open(strakDataFileName)
    except:
        print('Error, failed to open file %s' % strakDataFileName)
        exit(-1)

    # load dictionary from .json-file
    try:
        strakdata = json.load(strakDataFile)
        strakDataFile.close()
    except:
        print('Error, failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
        exit(-1)

    # get strak-machine-parameters from dictionary
    params = getParameters(strakdata)
    # print strakdata

    # read plane-data from XML-File
    if (params.xmlFileName != None):
        try:
            xmlFileName = params.inputFolder + '/' + params.xmlFileName
            planeData = read_planeDataFile(xmlFileName)
        except:
            print("Error, file \"%s\" could not be opened.") % xmlFileName
            exit(-1)

   # choose wing or fin
    if (params.useWingPlanform == True):
        print("the wing-strak shall be created")
        params.wingData = planeData[0]
    else:
        print("the fin-strak shall be created")
        params.wingData = planeData[1]

    # compose name of the folder, where the airfoils shall be stored
    params.airfoilFolder = 'airfoils'

    # check if output-folder exists. If not, create folder.
    if not os.path.exists(params.outputFolder):
        os.makedirs(params.outputFolder)

    # check if airfoil-folder exists. If not, create folder.
    if not os.path.exists(params.outputFolder + '\\' + params.airfoilFolder):
        os.makedirs(params.outputFolder + '\\' + params.airfoilFolder)

    # generate Xoptfoil command-lines
    commandlines = generate_commandlines(params)

    # debug-output
    #for element in commandlines:
    #    print element

        # generate batchfile
    if (params.generateBatch == True):
        print ('generating batchfile \'%s\'' % params.batchfileName)
        generate_batchfile(params.batchfileName, commandlines)

    print("Ready.")
