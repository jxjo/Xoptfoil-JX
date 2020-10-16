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
from math import log10, floor, tan, pi
import json

################################################################################
# some global variables

# fonts
csfont = {'fontname':'DejaVu Sans'}

# fontsizes
fs_infotext = 7
fs_legend = 7

# colours, styles and linewidths
cl_centerLine = 'darkcyan'
ls_centerLine = 'dashdot'
lw_centerLine  = 0.8
cl_hingeLine = 'r'
ls_hingeLine = 'solid'
lw_hingeLine = 0.6
cl_planform = 'aqua'
ls_planform = 'solid'
lw_planform = 0.6
cl_sections = 'grey'
ls_sections = 'solid'
lw_sections = 0.4

fs_tick = 7
cl_infotext = 'aqua'

# folder containing the inputs-files
inputFolder = 'ressources'

# folder containing the output / result-files
outputFolder ='ressources'

# name of the strakdata-file
strakDataFileName = "strakdata.txt"

# dictionary, containing all data of the planform
PLanformDict =	{
            # name of XFLR5-template-xml-file
            "templateFileName": 'plane_template.xml',
            # name of the generated XFLR5-xml-file
            "outFileName": "rocketeerMainWing.xml",
            # name of the planform
            "planformName": 'main wing',
            # Wing or Fin
            "isFin": 'false',
            # wingspan in m
            "wingspan": 2.54,
            # overeliptic shaping of the wing
            "overElipticOffset": 0.08,
            # orientation of the wings leading edge
            "leadingEdgeOrientation": 'up',
            # length of the root-chord in m
            "rootchord": 0.223,
            # sweep of the tip of the wing in degrees
            "rootTipSweep": 4.2,
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
        self.Re = 0.0
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
        self.leadingEdgeOrientation = 'down'
        self.wingspan = 0.0
        self.overElipticOffset = 0.11
        self.halfwingspan = 0.0
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
        self.area = 0.0
        self.aspectRatio = 0.0

        # Fontsize for planform-plotting
        self.fontsize = 7


    # set basic data of the wing
    def setData(self, dictData, strakData):
        # evaluate strakdata
        self.rootAirfoilName = strakData["seedFoilName"]
        self.valueList = strakData["reynolds"]
        self.numberOfSections = len(self.valueList)
        self.rootReynolds = self.valueList[0]

        # evaluate planformdata
        self.rootchord = dictData["rootchord"]
        self.wingspan = dictData["wingspan"]
        self.overElipticOffset = dictData["overElipticOffset"]
        self.halfwingspan = (self.wingspan/2)
        self.numberOfGridChords = self.numberOfSections * 256
        self.rootTipSweep = dictData["rootTipSweep"]
        self.leadingEdgeOrientation = dictData["leadingEdgeOrientation"]
        self.hingeDepthPercent = dictData["hingeDepthPercent"]
        self.dihedral = dictData["dihedral"]
        self.planformName = dictData["planformName"]
        self.wingFinSwitch = dictData["isFin"]

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

        # set Re of the section (only for proper airfoil naming)
        section.Re = (section.chord / self.rootchord) * self.rootReynolds


    # sets the airfoilname of a section
    def setAirfoilName(self, section):
        if (section.number == 1):
            suffix = '-root'
        else:
            suffix = '-strak'

        section.airfoilName = (self.rootAirfoilName + "%s-%s.dat") % \
            (suffix ,get_ReString(section.Re))


    # calculate grid-values of the wing (high-resolution wing planform)
    def calculateGrid(self):
        self.hingeInnerPoint = (1 - (self.hingeDepthPercent/100)) * self.rootchord

        # calculate tip-depth
        self.tipDepth = self.rootchord * self.overElipticOffset

        # calculate tip-offset for sweep-angle
        tipOffset = tan((self.rootTipSweep*pi)/180)*(self.wingspan/2)

        # calculate hinge-offset
        hingeOffset = tipOffset*(self.hingeDepthPercent/100)

        # calculate hinge outer-point
        self.hingeOuterPoint= 0.5*self.rootchord +\
           (self.tipDepth*(1-self.hingeDepthPercent/100)) + hingeOffset

        # calculate all Grid-chords
        grid_delta_y = (self.halfwingspan / (self.numberOfGridChords-1))

        for i in range(1, (self.numberOfGridChords + 1)):
            # create new grid
            grid = wingGrid()

            # calculate grid coordinates
            grid.y = grid_delta_y * (i-1)
            grid.chord = self.rootchord*(1-self.overElipticOffset)*np.sqrt(1-(grid.y*grid.y/(self.halfwingspan*self.halfwingspan)))\
                        + self.rootchord*self.overElipticOffset
            grid.hingeDepth = (self.hingeDepthPercent/100)*grid.chord
            grid.hingeLine = (self.hingeOuterPoint-self.hingeInnerPoint)/(self.halfwingspan) * (grid.y) + self.hingeInnerPoint
            grid.trailingEdge = grid.hingeLine + grid.hingeDepth
            grid.leadingEdge = grid.hingeLine -(grid.chord-grid.hingeDepth)
            grid.meanLine = (grid.leadingEdge + grid.trailingEdge)/2

            # append section to section-list of wing
            self.grid.append(grid)

            # calculate area of the wing
            self.area = self.area + (grid_delta_y*10 * grid.chord*10)

        # calculate aspect ratio of the wing
        self.area = self.area * 2.0
        self.aspectRatio = self.wingspan*self.wingspan / (self.area/100)


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

            # find grid-values matching the chordlength of the section
            grid = self.findGrid(chord)

            # copy grid-coordinates to section
            self.copyGridToSection(grid, section)

            # set the airfoil-Name
            self.setAirfoilName(section)

            # store last Re value for the tip
            lastSectionRe = section.Re

            # calculate chord for the next section
            if (i < len(self.valueList)):
                # calculate cordlangths from given valueList
                chord = (self.rootchord * self.valueList[i]) / self.valueList[0]
            else:
                # calculate chord with fixed difference
                chord = chord - chord_decrement

        # create last section
        section = wingSection()

        # append section to section-list of wing
        self.sections.append(section)

        # set number of the section
        section.number = i+1

        # get the tip grid-values
        grid = self.grid[len(self.grid)-1]

        # copy grid-coordinates to section
        self.copyGridToSection(grid, section)

        # set same Re as for the last section so the same airfoil-name will be given
        section.Re = lastSectionRe

        # set the airfoil-Name
        self.setAirfoilName(section)

    # plot the wing planform
    def plotPlanform(self, ax):
        #create empty lists
        xValues = []
        leadingEdge = []
        trailingeEge = []
        hingeLine = []
        meanLine = []

        # setup empty list for new x-tick locations
        new_tick_locations = []

        # set axes and labels
        self.set_AxesAndLabels(ax, "Half-wing planform")

        # plot sections in reverse order
        for element in reversed(self.sections):
            ax.plot([element.y, element.y] ,[element.leadingEdge, element.trailingEdge],
            color=cl_sections, linestyle = ls_sections, linewidth = lw_sections)

            # determine x and y Positions of the labels
            xPos = element.y
            if (self.leadingEdgeOrientation == 'up'):
                yPosChordLabel = element.trailingEdge
                yPosOffsetSectionLabel = 12
            else:
                yPosChordLabel = element.leadingEdge
                yPosOffsetSectionLabel = -12

            yPosSectionLabel = element.leadingEdge

            # plot label for chordlength of section
            text = ("%d mm" % int(round(element.chord*1000)))
            ax.annotate(text,
            xy=(xPos, yPosChordLabel), xycoords='data',
            xytext=(2, 10), textcoords='offset points', color = cl_sections,
            fontsize=fs_infotext, rotation='vertical')

            # plot label for airfoil-name / section-name
            text = ("%s" % (element.airfoilName))
            props = dict(arrowstyle="-", connectionstyle="arc3, rad=-0.23", color = cl_sections)
            ax.annotate(text,
            xy=(xPos, yPosSectionLabel), xycoords='data',
            xytext=(10, yPosOffsetSectionLabel), textcoords='offset points', color = cl_sections,
            fontsize=fs_infotext, arrowprops=props)

            # append position of section to x-axis ticks
            new_tick_locations.append(xPos)

        # set new ticks for the x-axis according to the positions of the sections
        ax.set_xticks(new_tick_locations)

        # set new fontsize of the x-tick labels
        for tick in ax.xaxis.get_major_ticks():
            tick.label.set_fontsize(fs_tick)
            tick.label.set_rotation('vertical')

        # set new fontsize of the y-tick labels
        for tick in ax.yaxis.get_major_ticks():
            tick.label.set_fontsize(fs_tick)

        for element in self.grid:
            #build up list of x-values
            xValues.append(element.y)
            #build up lists of y-values
            leadingEdge.append(element.leadingEdge)
            meanLine.append(element.meanLine)
            hingeLine.append(element.hingeLine)
            trailingeEge.append(element.trailingEdge)

        # setup root- and tip-joint
        rootJoint_y = (leadingEdge[0],trailingeEge[0])
        rootJoint_x = (xValues[0], xValues[0])
        lastElementIndex = len(leadingEdge)-1
        tipJoint_y = (leadingEdge[lastElementIndex],trailingeEge[lastElementIndex])
        tipJoint_x = (xValues[lastElementIndex], xValues[lastElementIndex])

        # compose labels for legend
        labelHingeLine = ("hinge line (%.1f %%)" % self.hingeDepthPercent)

        # plot mean-line and hinge-line
        ax.plot(xValues, meanLine, color=cl_centerLine,
         linestyle = ls_centerLine, linewidth = lw_centerLine,
         label = "center line")

        # plot hinge-line
        ax.plot(xValues, hingeLine, color=cl_hingeLine,
         linestyle = ls_hingeLine, linewidth = lw_hingeLine,
        label = labelHingeLine)

        # plot the planform last
        ax.plot(xValues, leadingEdge, color=cl_planform, label = "planform")
        ax.plot(xValues, trailingeEge, color=cl_planform)
        ax.plot(rootJoint_x, rootJoint_y, color=cl_planform)
        ax.plot(tipJoint_x, tipJoint_y, color=cl_planform)

        # place legend inside subplot
        ax.legend(loc='best', fontsize = fs_legend)

        # show grid
        ax.grid(True)

        # both axes shall be equal
        ax.axis('equal')

        # revert y-axis on demand
        if (self.leadingEdgeOrientation == 'up'):
            ax.set_ylim(ax.get_ylim()[::-1])


    def set_AxesAndLabels(self, ax, title):

        # set title of the plot
        text = (title)
        #ax.set_title(text, fontsize = 30, color="darkgrey")

        # customize grid
        ax.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)


    # draw the graph
    def draw(self):

        # set 'dark' style
        plt.style.use('dark_background')

        # setup subplots
        fig, (upper,lower) = plt.subplots(2,1)

        # compose diagram-title
        wingspan_mm = int(round(self.wingspan*1000))
        text = "\"%s\"\n wingspan: %d mm, area: %.2f dm², aspect ratio: %.2f, root-tip sweep: %.2f°\n"\
         % (self.planformName, wingspan_mm, self.area, self.aspectRatio, self.rootTipSweep)

        fig.suptitle(text, fontsize = 12, color="darkgrey", **csfont)

        # first figure, display detailed half-wing
        self.plotPlanform(upper)

        # second figure, display
        self.plotPlanform(lower)

        # maximize window
        figManager = plt.get_current_fig_manager()
        try:
            figManager.window.Maximize(True)
        except:
            try:
                figManager.window.state('zoomed')
            except:
                pass

        # show diagram
        plt.show()

################################################################################
# find the wing in the XML-tree
def get_wing(root, wingFinSwitch):
    for wing in root.iter('wing'):
        for XMLwingFinSwitch in wing.iter('isFin'):
            if (XMLwingFinSwitch.text == wingFinSwitch):
                return wing


# insert the planform-data into XFLR5-xml-file
def insert_PlanformDataIntoXFLR5_File(data, inFileName, outFileName):

    # basically parse the XML-file
    tree = ET.parse(inFileName)

    # get root of XML-tree
    root = tree.getroot()

    # find wing-data
    wing = get_wing(root, data.wingFinSwitch)

    if (wing == None):
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

        print("Section %d: position: %.0f mm, chordlength %.0f mm, airfoilName %s was inserted" %
          (section.number, section.y*1000, section.chord*1000, section.airfoilName))

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


    # try to open .json-file
    try:
        strakDataFile = open('./' + inputFolder + '/' + strakDataFileName)
    except:
        print('failed to open file %s' % strakDataFileName)
        exit(-1)

    # load dictionary from .json-file
    try:
        strakdata = json.load(strakDataFile)
        strakDataFile.close()
    except:
        print('failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
        exit(-1)

    # set data for the planform
    newWing.setData(planformData, strakdata)

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
    newWing.draw()

    print("Ready.")
