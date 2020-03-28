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
# function, that gets the name of the wing
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
# function, that gets the chord-lengths of the wing
def get_wingChords(wing):

    # iterate the elements of the wing
    for section in wing.iter('Sections'):
        return get_chordFromSection(section)


################################################################################
# function that reads plane-data from XFLR5 XML-file
def read_planeDataFile(fileName):

    # init data as an empty list
    data = []

    # open file containing XFLR5-plane-data
    file = open(fileName, "r")

    # parse the file
    tree = ET.parse('plane.xml')

    #get root of XML-tree
    root = tree.getroot()

    # find wing-data
    for wing in root.iter('wing'):
        # create dictionary containg the wing-data
        wingDict = 	{ 'name': get_wingName(wing),
                      'chordLengths': get_wingChords(wing)
                    }

        #append dictionary to data
        data.append(wingDict)

    # debug output
    #print data

    return data


def generate_batchfile(wing, batchFileName, inputFileName, rootFoilName, ReSqrtCl):

    # add file-extension
    batchFileName = batchFileName + '.bat'

    # create a new file
    outputfile = open(batchFileName, "w+")

    # an example-file looks like THIS
    ##xoptfoil-jx -i iFX-strak.txt -r 130000 -a JX-FXrcn-15.dat -o FX-strak-13
    ##xoptfoil-jx -i iFX-strak.txt -r 110000 -a FX-strak-13.dat -o FX-strak-11
    ##xoptfoil-jx -i iFX-strak.txt -r  90000 -a FX-strak-11.dat -o FX-strak-09
    ##xoptfoil-jx -i iFX-tip.txt   -r  60000 -a FX-strak-09.dat -o FX-strak-06

    # initialize the chord length of previous airfoil
    prevChord = 0.0
    ReSqrtCl_old = 0

    for chord in wing.get('chordLengths'):
        # skip the root airfoil
        if (prevChord > 0.0):
            # calculate new ReSqrtCl
            ReSqrtCl = ReSqrtCl_old * (chord / prevChord)

            # compose name of strak-airfoil, Remove ReSqrt-CL
            rootFoilName = rootFoilName.strip('.dat')
            splitString = '%d' % (ReSqrtCl_old / 10000)
            elements = rootFoilName.split(splitString)
            strakFoilName = elements[0] + ('%d' % (ReSqrtCl / 10000))

            # generate Xoptfoil-commandline
            commandLine = "xoptfoil-jx -i %s -r -a %s.dat -o %s\n"\
                         % (inputFileName, rootFoilName, strakFoilName)

            # debug-output
            #print commandLine

            # write Xoptfoil-commandline to outputfile
            outputfile.write(commandLine)

            # set rootFoilName for next strak-airfoil
            rootFoilName = strakFoilName + '.dat'

        # store actual chordLength for calculations of next strak-airfoil
        prevChord = chord
        ReSqrtCl_old = ReSqrtCl

    # close the outputfile
    outputfile.close()
    print('File \'%s\' was successfully written' % batchFileName)


def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("--plane", "-p", help="filename containing plane-data"\
                        "(e.g. plane)")

#TODO implement other arguments
##    parser.add_argument("--root", "-r", help="filename root-airfoil-data"\
##                        "(e.g. rootfoil)")
##
##    parser.add_argument("-output", "-o", help="filename of the outputfile"\
##                        "(e.g. make_strak)")
##
##    parser.add_argument("-input", "-i", help="filename xoptfoil input-data"\
##                        "(e.g. inputs.txt)")
##
##    parser.add_argument("-resqrtcl", "-r", help="reSqrtCl-Value for the root"\
##                        "-airfoil")

  # read arguments from the command line
    args = parser.parse_args()

    if args.plane:
        xmlFileName = args.plane
    else:
        message = "Enter the filename of the plane-data xml-file"\
        "(e.g., plane, which  is the default filename), "\
        "hit <enter> for default:"

        xmlFileName = my_input(message)

        # set default filename in case there was no input
        if (xmlFileName == ""):
            xmlFileName = 'plane'

    xmlFileName = xmlFileName + '.xml'
    print("filename is: %s" % xmlFileName)

    #TODO remove
    batchFileName = 'make-Fx-strak' #TODO
    inputFileName = 'iFX-strak.txt' #TODO
    rootFoilName =  'JX-FXrcn-15'   #TODO
    ReSqrtCl = 150000               #TODO

    return (xmlFileName, batchFileName, inputFileName, rootFoilName, ReSqrtCl)


################################################################################
# Main program
if __name__ == "__main__":

    #get command-line-arguments
    (xmlFileName, batchFileName, inputFileName, rootFoilName, ReSqrtCl)\
     = getArguments()

    try:
        planeData = read_planeDataFile(xmlFileName)
    except:
        print("Error, file %s could not be opened." % fileName)
        exit -1

    # get only data of the wing
    wing = planeData[0]

    # generate batchfile for wing
    generate_batchfile(wing, batchFileName, inputFileName, rootFoilName,\
                       ReSqrtCl)

#TODO: future use, generate strak for wing and fin
##    for wing in planeData:
##        generate_batchfile(wing, batchFileName, inputFileName, rootFoilName,\
##        ReSqrtCl)

    print("Ready.")
