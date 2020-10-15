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



#  !!!!! WORK IN PROGRESS !!!!


import xml.etree.ElementTree as ET
from copy import deepcopy
import argparse
from sys import version_info
import os
from shutil import copyfile
from matplotlib import pyplot as plt
from matplotlib import rcParams
import numpy as np
from math import log10, floor
import json

################################################################################
# some global variables

# folder containing the inputs-files
inputFolder = 'ressources'

# folder containing the output / result-files
outputFolder ='ressources'

# dictionary, containing all data of the planform
PLanformDict =	{
            # name of XFLR5-template-xml-file
            "templateFileName": 'plane_template.xml',
            # name of the generated XFLR5-xml-file
            "outFileName": "rocketeerMainWing.xml",
            # name of the root-airfoil
            "rootAirfoilName": "AM",
            # Re*sqrt(Cl) of root-airfoil
            "rootReynolds": 137000,
            # name of the planform
            "planformName": 'main wing',
            # Wing or Fin
            "isFin": 'false',
            # spanwidth in m
            "spanwidth": 2.54,
            # overeliptic shaping of the wing
            "overElipticOffset": 0.09,
             # length of the root-chord in m
            "rootchord": 0.223,
            # list of manual given values
            "listValues": [137000, 120000, 100000, 80000, 60000, 40000, 20000],
            # number of airfoils that shall be calculated along the wing
            "numberOfSections": 7,
            # backsweep of the tip of the wing
            "backsweep": 0.031,
            # depth of the aileron / flap in percent of the chord-length
            "hingeDepthPercent": 23.5,
            # dihedral of the of the wing in degree
            "dihedral": 2.5
            }


################################################################################
#
# helper functions
#
################################################################################
#simple linear equation
def linearEquation(x1, x2, y1, y2, x):
    y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
    return y

# function that rounds Re and returns a rounded decimal number
def round_Re(Re):
    floatRe = Re/1000.0
    decRe = round(floatRe, 0)
    return int(decRe)


# transform reynolds-number into a string e.g. Re = 123500 -> string = 124k
def get_ReString(Re):
    return ("%03dk" % round_Re(Re))

################################################################################
#
# wingSection class
#
################################################################################
class wingSection:

    #class init
    def __init__(self):
        self.number = 0

        # geometrical data of the wing planform
        self.y = 0
        self.chord = 0
        self.leadingEdge = 0
        self.trailingEdge = 0
        self.hingeDepth = 0
        self.hingeLine = 0
        self.meanLine = 0
        self.dihedral= 3.00

        # name of the airfoil-file that shall be used for the section
        self.airfoilName = ""

################################################################################
#
# wingGrid class
#
################################################################################
class wingGrid:

    # class init
     def __init__(self):
        self.y = 0
        self.chord = 0
        self.leadingEdge = 0
        self.trailingEdge = 0
        self.hingeDepth = 0
        self.hingeLine = 0
        self.meanLine = 0

################################################################################
#
# Wing class
#
################################################################################
class wing:

  #class init
  def __init__(self):
    self.rootAirfoilName = ""
    self.rootchord = 0.0
    self.spanwidth = 0.0
    self.overElipticOffset = 0.11
    self.halfspanwidth = 0.0
    self.numberOfSections = 0
    self.numberOfGridChords = 0
    self.backsweep = 0.00
    self.hingeDepthPercent = 0.0
    self.hingeInnerPoint = 0
    self.hingeOuterPoint = 0
    self.tipDepth = 0
    self.dihedral = 0.00
    self.sections = []
    self.grid = []
    self.valueList = []

    # Fontsize for planform-plotting
    self.fontsize = 10


  # set basic data of the wing
  def setData(self, dictData):
    self.rootchord = dictData["rootchord"]
    self.spanwidth = dictData["spanwidth"]
    self.overElipticOffset = dictData["overElipticOffset"]
    self.halfspanwidth = (self.spanwidth/2)
    self.numberOfSections = dictData["numberOfSections"]
    self.numberOfGridChords = self.numberOfSections * 256
    self.backsweep = dictData["backsweep"]
    self.hingeDepthPercent = dictData["hingeDepthPercent"]
    self.dihedral = dictData["dihedral"]
    self.rootAirfoilName = dictData["rootAirfoilName"]
    self.rootReynolds = dictData["rootReynolds"]
    self.planformName = dictData["planformName"]
    self.wingFinSwitch = dictData["isFin"]

    try:
        self.valueList = dictData["listValues"]
    except:
        pass

  # find grid-values for a given chord-length
  def findGrid(self, chord):
    for element in self.grid:
        if (element.chord <= chord):
          return element


  # copy grid-values to section
  def copyGridToSection(self, grid, section):
        section.y = grid.y
        section.chord = grid.chord
        section.hingeDepth = grid.hingeDepth
        section.hingeLine = grid.hingeLine
        section.trailingEdge = grid.trailingEdge
        section.leadingEdge = grid.leadingEdge
        section.meanLine = grid.meanLine
        section.dihedral = self.dihedral
        if (section.number == 1):
            suffix = '-root'
        else:
            suffix = '-strak'
        Re = (section.chord / self.rootchord) * self.rootReynolds
        section.airfoilName = (self.rootAirfoilName + "%s-%s.dat") % \
        (suffix ,get_ReString(Re))


  # calculate grid-values of the wing (high-resolution wing planform)
  def calculateGrid(self):
    self.hingeInnerPoint = (1 - (self.hingeDepthPercent/100)) * self.rootchord
    self.tipDepth = self.rootchord * self.overElipticOffset
    self.hingeOuterPoint= 0.5*self.rootchord + (self.tipDepth*(1-self.hingeDepthPercent/100)) + self.backsweep


    # calculate all Grid-chords
    for i in range(1, (self.numberOfGridChords + 1)):
        # create new grid
        grid = wingGrid()

        # calculate grid coordinates
        grid.y = (self.halfspanwidth / (self.numberOfGridChords-1)) * (i-1)
        grid.chord = self.rootchord*(1-self.overElipticOffset)*np.sqrt(1-(grid.y*grid.y/(self.halfspanwidth*self.halfspanwidth)))\
                    + self.rootchord*self.overElipticOffset
        grid.hingeDepth = (self.hingeDepthPercent/100)*grid.chord
        grid.hingeLine = (self.hingeOuterPoint-self.hingeInnerPoint)/(self.halfspanwidth) * (grid.y) + self.hingeInnerPoint
        grid.trailingEdge = grid.hingeLine + grid.hingeDepth
        grid.leadingEdge = grid.hingeLine -(grid.chord-grid.hingeDepth)
        grid.meanLine = (grid.leadingEdge + grid.trailingEdge)/2

        # append section to section-list of wing
        self.grid.append(grid)


  # calculate all sections of the wing, oriented at the grid
  def calculateSections(self):

    # calculate decrement of chord from section to section
    chord_decrement = (self.rootchord - self.tipDepth) / (self.numberOfSections)

    # set chord-length of root-section
    chord = self.rootchord

    # create all sections
    for i in range(1, (self.numberOfSections + 1)):
        # create new section
        section = wingSection()

        # append section to section-list of wing
        self.sections.append(section)

        # set number of the section
        section.number = i

        # set name of the airfoil
        section.airfoilName = self.rootAirfoilName + "_%s" % i

        # find grid-values matching the chordlength of the section
        grid = self.findGrid(chord)

        # copy grid-coordinates to section
        self.copyGridToSection(grid, section)

        # calculate chord for the next section
        if (i < len(self.valueList)):
            # calculate cordlangths from given valueList
            chord = (self.rootchord * self.valueList[i]) / self.valueList[0]
        else:
            # calculate chord with fixed difference
            chord = chord - chord_decrement


  # plot the wing planform
  def plotPlanform(self):
        #create empty lists
        xValues = []
        leadingEdge = []
        trailingeEge = []
        hingeLine = []
        meanLine = []

        # plot sections
        factor = 1
        offset = -60

        for element in self.sections:
            plt.plot([element.y, element.y] ,[element.leadingEdge, element.trailingEdge], 'b-')
            # insert text for section-name
            text = ("%s\n(%d mm)" % (element.airfoilName, int(round(element.chord*1000))))
            plt.annotate(text,
            xy=(element.y, element.leadingEdge), xycoords='data',
            xytext=(+(12*factor), offset), textcoords='offset points', fontsize=self.fontsize,
            arrowprops=dict(arrowstyle="->", connectionstyle="arc3, rad=.02"))

            # insert text for section-length
            text = ("%d mm" % (int(round(element.y*1000))))
            plt.annotate(text,
            xy=(element.y, element.trailingEdge), xycoords='data',
            xytext=(+(12*factor), -offset), textcoords='offset points', fontsize=self.fontsize,
            arrowprops=dict(arrowstyle="->", connectionstyle="arc, rad =0"))
            factor = factor + 1
            offset = offset + 5#12

        for element in self.grid:
            #build up list of x-values
            xValues.append(element.y)
            #build up lists of y-values
            leadingEdge.append(element.leadingEdge)
            meanLine.append(element.meanLine)
            hingeLine.append(element.hingeLine)
            trailingeEge.append(element.trailingEdge)

        # plot shape, mean-line and hinge-line
        plt.plot(xValues, leadingEdge, 'k-')
        plt.plot(xValues, meanLine, 'b-')
        plt.plot(xValues, hingeLine, 'r-')
        plt.plot(xValues, trailingeEge, 'k-')

        # insert text for mean-line
        plt.annotate('center line',
        xy=(xValues[10], meanLine[10]), xycoords='data',
        xytext=(40, -20), textcoords='offset points', fontsize=self.fontsize,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3, rad=-.2"))

        # insert text for hinge-line
        text = ("hinge line (%.1f %%)" % self.hingeDepthPercent)
        plt.annotate(text,
        xy=(xValues[10], hingeLine[10]), xycoords='data',
        xytext=(40, -20), textcoords='offset points', fontsize=self.fontsize,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3, rad=-.2"))

        # insert title
        spanwidth_mm = int(round(self.spanwidth*1000))
        text = "%s (%d mm / %d mm)" % (self.planformName, spanwidth_mm/2, spanwidth_mm)
        plt.title(text, fontsize = 20)

        # show grid
        plt.grid(True)

        # both axes shall be equal
        plt.axis('equal')

        # show diagram
        plt.show()


################################################################################

# insert the planform-data into XFLR5-xml-file
def insert_PlanformDataIntoXFLR5_File(data, inFileName, outFileName):

    # basically parse the XML-file
    tree = ET.parse(inFileName)

    # get root of XML-tree
    root = tree.getroot()
    bFound = 0

    # find wing-data
    for wing in root.iter('wing'):
        for XMLwingFinSwitch in wing.iter('isFin'):
            if (XMLwingFinSwitch.text == data.wingFinSwitch):
                # found the correct wing
                print ("Wing was found\n")
                bFound = 1
                break

    if (not bFound):
        print("Error, wing not found\n")
        return

    # find sections-data-template
    for sectionTemplate in wing.iter('Sections'):
        # copy the template
        newSection = deepcopy(sectionTemplate)

        # remove the template
        wing.remove(sectionTemplate)

    # write the new section-data to the wing
    for section in data.sections:
        # copy the template
        newSection = deepcopy(sectionTemplate)

        # enter the new data
        for yPosition in newSection.iter('y_position'):
            # convert float to text
            yPosition.text = str(section.y)

        for chord in newSection.iter('Chord'):
            # convert float to text
            chord.text = str(section.chord)

        for xOffset in newSection.iter('xOffset'):
            # convert float to text
            xOffset.text = str(section.leadingEdge)

        for dihedral in newSection.iter('Dihedral'):
            # convert float to text
            dihedral.text = str(section.dihedral)

        for foilName in newSection.iter('Left_Side_FoilName'):
            # convert float to text
            foilName.text = str(section.airfoilName)

        for foilName in newSection.iter('Right_Side_FoilName'):
            # convert float to text
            foilName.text = str(section.airfoilName)

        # add the new section to the tree
        wing.append(newSection)

    # write all data to the new file file
    tree.write(outFileName)

################################################################################
# function that gets the name of the strak-machine-data-file
def getInFileName(args):

    if args.input:
        inFileName = args.input
    else:
        # use Default-name
        inFileName = 'planformdata'

    inFileName = inFileName + '.txt'
    print("filename for planform input-data is: %s" % inFileName)
    return inFileName


################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    parser.add_argument("-input", "-i", help="filename of planformdata input"\
                        "-file (e.g. planformdata)")

    # read arguments from the command line
    args = parser.parse_args()
    return (getInFileName(args))


# Main program
if __name__ == "__main__":

  #get command-line-arguments or user-input
  planformDataFileName = getArguments()

  # create a new planform
  newWing = wing()

  #debug
  json.dump(PLanformDict, open("planformdata.txt",'w'))

  # try to open .json-file
  try:
     planform = open(planformDataFileName)
  except:
    print('Error, failed to open file %s' % planformDataFileName)
    exit(-1)

  # load dictionary from .json-file
  try:
    planformData = json.load( planform)
    planform.close()
  except:
    print('Error, failed to read data from file %s' % planformDataFileName)
    planform.close()
    exit(-1)


  try:
    planformData = json.load(open("planformdata.txt"))
  except:
    print('failed to open file \"planformdata.txt\"')
    planformData = PLanformDict

  # set data for the planform
  newWing.setData(planformData)

  # calculate the grid and sections
  newWing.calculateGrid()
  newWing.calculateSections()

  inputFileName =  './' + inputFolder + '/'\
                 + planformData["templateFileName"]
  print (inputFileName)


  outputFileName = './' + outputFolder + '/'\
                 + planformData["outFileName"]
  print (outputFileName)

  if not os.path.exists(outputFolder):
      os.makedirs(outputFolder)

 # insert the generated-data into the XML-File for XFLR5
  insert_PlanformDataIntoXFLR5_File(newWing, inputFileName, outputFileName)

  # plot the result
  newWing.plotPlanform()

  print("Ready.")
