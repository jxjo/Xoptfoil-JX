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

strakdata = {
             # name of XFLR5-xml-file
             "XMLfileName": 'plane.xml',
             # choice of the planform, 'wing' or 'fin'
             "planformType": 'wing',
             # ReSqrtCl of root airfoil
             "ReSqrtCl": '150000',
              # name of the xoptfoil-inputfile for strak-airfoil(s)
             "strakInputFileName": 'inputs-strak.txt',
             # name of the xoptfoil-inputfile for tip-airfoil(s)
             "tipInputFileName": 'inputs-tip.txt',
             # number of tip-airfoils
             "numberOfTipAirfoils": 1,
            }

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
# function that generates a Xoptfoil-batchfile
def generate_batchfile(wing, batchFileName, inputFileName, tipInputFileName,
                       numTipArifoils, ReSqrtCl):

    # create a new file
    outputfile = open(batchFileName, "w+")

    # an example-file looks like THIS
    ##xoptfoil-jx -i iFX-strak.txt -r 130000 -a JX-FXrcn-15.dat -o FX-strak-13
    ##xoptfoil-jx -i iFX-strak.txt -r 110000 -a FX-strak-13.dat -o FX-strak-11
    ##xoptfoil-jx -i iFX-strak.txt -r  90000 -a FX-strak-11.dat -o FX-strak-09
    ##xoptfoil-jx -i iFX-tip.txt   -r  60000 -a FX-strak-09.dat -o FX-strak-06

    # do some initializations
    prevChord = 0.0
    ReSqrtCl_old = 0
    idx = 1
    numChords = len(wing.get('chordLengths'))

    if (numTipArifoils == 0):
        firstTipIdx = numChords
    else:
        if (numTipArifoils < numChords):
            firstTipIdx = numChords - numTipArifoils

    for chord in wing.get('chordLengths'):
        # skip the root airfoil
        if (prevChord > 0.0):
            # calculate new ReSqrtCl
            ReSqrtCl = ReSqrtCl_old * (chord / prevChord)
            rootFoilName = get_FoilName(wing, idx-1)
            strakFoilName = get_FoilName(wing, idx)

            #choose input-file, strak or tip?
            if (idx < firstTipIdx):
                #set inputFileName to strak-input-file
                iFile = inputFileName
            else:
                #set inputFileName to tip-input-file
                iFile = tipInputFileName

            # generate Xoptfoil-commandline
            commandLine = "xoptfoil-jx -i %s -r %d -a %s.dat -o %s\n" %\
                         (iFile, ReSqrtCl, rootFoilName, strakFoilName)

            # debug-output
            #print commandLine

            # write Xoptfoil-commandline to outputfile
            outputfile.write(commandLine)
            idx = idx + 1

        # store actual chordLength and ReSqrtCl for calculations of next
        # strak-airfoil
        prevChord = chord
        ReSqrtCl_old = ReSqrtCl

    # close the outputfile
    outputfile.close()
    print('File \'%s\' was successfully written' % batchFileName)


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
# function that gets the name of the output-Batchfile
def getOutFileName(args):

    if args.output:
        outFileName = args.output
    else:
        # use Default-name
        outFileName = 'make_strak'

    outFileName = outFileName + '.bat'
    print("outFileName is: %s" % outFileName)
    return outFileName


################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    parser.add_argument("-input", "-i", help="filename of strak-machine input"\
                        "-file (e.g. strak_data)")

    parser.add_argument("-output", "-o", help="filename of the outputfile"\
                        "(e.g. make_strak)")


    # read arguments from the command line
    args = parser.parse_args()
    return (getInFileName(args), getOutFileName(args))


################################################################################
# function that gets parameters from dictionary
def getParameters(dict):

    xmlFileName = dict["XMLfileName"]
    inputFileName = dict["strakInputFileName"]
    tipInputFileName = dict["tipInputFileName"]
    numberOfTipAirfoils = dict["numberOfTipAirfoils"]
    ReSqrtCl = int(dict["ReSqrtCl"])

    if (dict["planformType"] == 'fin'):
        wingFinSwitch = 1
    else:
        wingFinSwitch = 0

    return (xmlFileName, inputFileName, tipInputFileName, numberOfTipAirfoils,
            ReSqrtCl, wingFinSwitch)


################################################################################
# Main program
if __name__ == "__main__":

    #get command-line-arguments or user-input
    (strakDataFileName, batchFileName) = getArguments()

    #debug
    #json.dump(strakdata, open("strakdata.txt",'w'))

    # read strak-machine-parameters from file
    try:
        strakdata = json.load(open(strakDataFileName))
    except:
        print('Error, failed to open file %s' % strakDataFileName)
        exit(-1)

    # get strak-machine-parameters from dictionary
    (xmlFileName, inputFileName, tipInputFileName, numberOfTipAirfoils,\
    ReSqrtCl, wingFinSwitch) = getParameters(strakdata)

    # read plane-data from XML-File
    try:
        planeData = read_planeDataFile(xmlFileName)
    except:
        print("Error, file \"%s\" could not be opened.") % xmlFileName
        exit(-1)

    if (wingFinSwitch == 0):
        # generate batchfile for wing
        print("generating outputfile for the wing-strak")
        wingData = planeData[0]
    else:
        print("generating outputfile for the fin-strak")
        wingData = planeData[1]

    generate_batchfile(wingData, batchFileName, inputFileName, tipInputFileName,
                       numberOfTipAirfoils, ReSqrtCl)


    print("Ready.")
