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
                       ReSqrtCl):

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

    for chord in wing.get('chordLengths'):
        # skip the root airfoil
        if (prevChord > 0.0):
            # calculate new ReSqrtCl
            ReSqrtCl = ReSqrtCl_old * (chord / prevChord)
            rootFoilName = get_FoilName(wing, idx-1)
            strakFoilName = get_FoilName(wing, idx)

            #choose input-file, strak or tip?
            if (idx < (numChords-1)):
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
# function that gets the ReSqrtCl-value of the root-airfoil
def getReSqrtCl(args):

    if args.resqrtcl:
        ReSqrtCl = args.resqrtcl
    else:
        message = "Enter ReSqrtCl-value for the root-airfoil"\
        "(e.g. 150000, which  is the default-value), "\
        "hit <enter> for default:"

        ReSqrtCl = my_input(message)

        # set default value in case there was no input
        if (ReSqrtCl == ""):
            ReSqrtCl = '150000'

    print("ReSqrtCl is: %s" % ReSqrtCl)
    return ReSqrtCl


################################################################################
# function that gets the name of the Xoptfoil-input-file
def getInputFileName(args):

    if args.input:
        inputFileName = args.input
    else:
        message = "Enter the filename of the xoptfoil-input-file"\
        "(e.g., inputs, which  is the default filename), "\
        "hit <enter> for default:"

        inputFileName = my_input(message)

        # set default value in case there was no input
        if (inputFileName == ""):
            inputFileName = 'inputs'

    inputFileName = inputFileName + '.txt'
    print("input-filename is: %s" % inputFileName)
    return inputFileName


################################################################################
# function that gets the name of the Xoptfoil-tip-input-file
def getTipInputFileName(args):

    if args.input:
        tipInputFileName = args.input
    else:
        message = "Enter the filename of the xoptfoil-input-file"\
        "for the wing-tip (e.g., inputs_tip, which  is the default filename), "\
        "hit <enter> for default:"

        tipInputFileName = my_input(message)

        # set default value in case there was no input
        if (tipInputFileName == ""):
            tipInputFileName = 'inputs_tip'

    tipInputFileName = tipInputFileName + '.txt'
    print("tip-input-filename is: %s" % tipInputFileName)
    return tipInputFileName


################################################################################
# function that gets the name of the output-Batchfile
def getOutFileName(args):

    if args.output:
        outFileName = args.output
    else:
        message = "Enter the filename of the output-file"\
        "(e.g. make_strak, which  is the default filename), "\
        "hit <enter> for default:"

        outFileName = my_input(message)

        # set default value in case there was no input
        if (outFileName == ""):
            outFileName = 'make_strak'

    outFileName = outFileName + '.bat'
    print("outFileName is: %s" % outFileName)
    return outFileName


################################################################################
# function that gets the name of the input-XML-file
def getXmlFileName(args):

    if args.xml:
        xmlFileName = args.xml
    else:
        message = "Enter the filename of the plane-data xml-file"\
        "(e.g., plane, which  is the default filename), "\
        "hit <enter> for default:"

        xmlFileName = my_input(message)

        # set default value in case there was no input
        if (xmlFileName == ""):
            xmlFileName = 'plane'

    xmlFileName = xmlFileName + '.xml'
    print("xml-filename is: %s" % xmlFileName)
    return xmlFileName


################################################################################
# function that gets arguments either from the commandline or by user-input
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("--xml", "-x", help="filename containing xml-data"\
                        "(e.g. plane)")

    parser.add_argument("-output", "-o", help="filename of the outputfile"\
                        "(e.g. make_strak)")

    parser.add_argument("-fin", "-f", help="generate outputfile for the fin"\
                                           " instead of wing")

    parser.add_argument("-input", "-i", help="filename xoptfoil of input-file"\
                        "(e.g. inputs.txt)")

    parser.add_argument("-input_tip", "-it", help="filename xoptfoil of input-"\
                        "file for the wing-tip (e.g. inputs_tip.txt)")

    parser.add_argument("-resqrtcl", "-r", help="reSqrtCl-value for the root"\
                        "-airfoil")

    # read arguments from the command line
    args = parser.parse_args()

    # get required arguments
    xmlFileName = getXmlFileName(args)
    inputFileName = getInputFileName(args)
    tipInputFileName = getTipInputFileName(args)
    ReSqrtCl = float(getReSqrtCl(args))
    batchFileName = getOutFileName(args)

    if args.fin:
        wingFinSwitch = 1
    else:
        wingFinSwitch = 0

    return (xmlFileName, batchFileName, inputFileName, tipInputFileName,
            ReSqrtCl, wingFinSwitch)


################################################################################
# Main program
if __name__ == "__main__":

    #get command-line-arguments or user-input
    (xmlFileName, batchFileName, inputFileName, tipInputFileName, ReSqrtCl,\
     wingFinSwitch) = getArguments()

    # read plane-data from XML-File
    try:
        planeData = read_planeDataFile(xmlFileName)
    except:
        print("Error, file %s could not be opened." % fileName)
        exit -1

    # generate output-file
    if (wingFinSwitch == 0):
        # generate batchfile for wing
        print("generating outputfile for the wing-strak")
        generate_batchfile(planeData[0], batchFileName, inputFileName,
                           tipInputFileName, ReSqrtCl)
    else:
        # generate batchfile for fin
        print("generating outputfile for the fin-strak")
        generate_batchfile(planeData[1], batchFileName, inputFileName,
                           tipInputFileName, ReSqrtCl)

    print("Ready.")
