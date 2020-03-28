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
#global variables
wingTag = 'wing'
sectionsTag = 'Sections'
chordTag = 'Chord'
NameTag = 'Name'


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
# function that reads plane-data from XFLR5 XML-file
def read_planeDataFile(fileName):
    data = []
     # open file containing XFLR5-plane-data
    file = open(fileName, "r")
    tree = ET.parse('plane.xml')
    root = tree.getroot()

    #print root
    #for section in root.iter(sectionsTag):
    #for section in root.findall(sectionsTag):
       # print(section.attrib, section.text)

    for child in root:
        #print(child.tag, child.attrib)
        for subchild in child:
            #print(subchild.tag, subchild.attrib)
            if subchild.tag == wingTag:

                for element in subchild:
                    if element.tag == NameTag:
                        print "Found wing %s" % element.text
                    if element.tag == sectionsTag:
                        #print "Found sections"
                        for section in element:
                            #print "Section start"
                            #print(section.tag, section.attrib)
                            for part in section:
                                if part.tag == chordTag:
                                    print(part.tag, part.text)
                                #print part
                            #print "Section end\n"
    return data

################################################################################
# Main program
if __name__ == "__main__":

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("--input", "-i", help="filename containing plane-data (e.g. plane)")

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
