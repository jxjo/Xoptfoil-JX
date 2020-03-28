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
        chordList.append(chord.text)

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
        wingDict = 	{ "name": get_wingName(wing),
                      "chordLengths": get_wingChords(wing)
                    }

        #append dictionary to data
        data.append(wingDict)

    print data
    return data

################################################################################
# Main program
if __name__ == "__main__":

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("--input", "-i", help="filename containing plane-data"\
                        "(e.g. plane)")

  # read arguments from the command line
    args = parser.parse_args()

    if args.input:
        fileName = args.input
    else:
        message = "Enter the filename of the plane-data-xml-file"\
        "(e.g., plane, which  is the default filename), "\
        "hit <enter> for default:"

        fileName = my_input(message)

        # set default filename in case there was no input
        if (fileName == ""):
            fileName = 'plane'

    fileName = fileName + '.xml'
    print("filename is: %s" % fileName)

    try:
        planeData = read_planeDataFile(fileName)
    except:
        print("Error, file %s could not be opened." % fileName)
        exit -1

    print("Ready.")
