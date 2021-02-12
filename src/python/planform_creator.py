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

import re
import xml.etree.ElementTree as ET
from copy import deepcopy
import argparse
from sys import version_info
import os
from shutil import copyfile
from matplotlib import pyplot as plt
from matplotlib import rcParams
import numpy as np
from math import log10, floor, tan, atan, pi
import json
import tkinter
from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
# Implement the default Matplotlib key bindings.
from matplotlib.backend_bases import key_press_handler
import tkinter.font as font
from strak_machineV2 import (copyAndSmooth_Airfoil, get_ReString,
                             ErrorMsg, WarningMsg, NoteMsg, DoneMsg,
                             remove_suffix, interpolate, round_Re,
                             bs, buildPath, ressourcesPath, airfoilPath,
                             scriptPath, exePath,
                             strakMachineInputFileName)
from colorama import init
from termcolor import colored

################################################################################
# some global variables


# folder containing the output / result-files
outputFolder = buildPath + bs +'planforms'

# fonts
csfont = {'fontname':'DejaVu Sans'}

# fontsizes
fs_infotext = 7
fs_legend = 7

# colours, styles and linewidths
cl_quarterChordLine = 'darkgreen'
ls_quarterChordLine = 'solid'
lw_quarterChordLine  = 0.8
cl_tipLine = 'blue'
ls_tipLine = 'solid'
lw_tipLine = 0.8
cl_hingeLine = 'r'
ls_hingeLine = 'solid'
lw_hingeLine = 0.6
cl_planform = 'gray'
ls_planform = 'solid'
lw_planform = 1.0
cl_sections = 'grey'
ls_sections = 'solid'
lw_sections = 0.4
cl_userAirfoil = 'aqua'
cl_optAirfoil = 'yellow'

fs_tick = 7
cl_infotext = 'aqua'


# example-dictionary, containing all data of the planform
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
            "wingspan": 2.98,#2.54,
            # width of fuselage in m
            "fuselageWidth": 0.035,
            # shape of the planform, elliptical / trapezoidal
            "planformShape": 'elliptical',
            # orientation of the wings leading edge
            "leadingEdgeOrientation": 'up',
            # length of the root-chord in m
            "rootchord": 0.223,
            # length of the tip-cord in m
            "tipchord": 0.002,
            # sweep of the tip of the wing in degrees
            "rootTipSweep": 3.2,
            # depth of the aileron / flap in percent of the chord-length
            "hingeDepthPercent": 23.5,
            # dihedral of the of the wing in degree
            "dihedral": 2.5,
            # whether to show the line at 25% of wing depth
            "showQuarterChordLine" : 'true',
            # whether to show line from root-chord to middle of the tip
            "showTipLine": 'true',
            # whether to show the hinge-line
            "showHingeLine" : 'true',
            # positions of the airfoils
            "airfoilPositions": [   0.0,     0.3,     0.6,     0.9,     1.2,    1.5,     1.6,    1.8],
            # user-defined names of the airfoils
            "airfoilNames":     ["SD-1",  "SD-2",  "SD-3",  "SD-4",  "SD-5", "SD-6",  "SD-7", "SD-8"],
            # types of the airfoils (user / blend / opt
            "airfoilTypes":     ["user", "blend", "blend", "blend", "blend", "user", "blend", "user"],
            # user-defined airfoils and where to find them
            "userAirfoils":     [".\\airfoil_library\\Scale_Glider\\SD\\SD-220.dat",
                                 ".\\airfoil_library\\Scale_Glider\\SD\\SD-150.dat",
                                 ".\\airfoil_library\\Scale_Glider\\SD\\SD-80.dat"]
            }



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
        self.quarterChordLine = 0
        self.tipLine = 0
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
        self.quarterChordLine = 0
        self.tipLine = 0

################################################################################
#
# Wing class
#
################################################################################
class wing:
    # class init
    def __init__(self):
        self.airfoilBasicName = ""
        self.airfoilNames = []
        self.rootchord = 0.223
        self.leadingEdgeOrientation = 'up'
        self.wingspan = 2.54
        self.fuselageWidth = 0.035
        self.planformShape = 'elliptical'
        self.halfwingspan = 0.0
        self.numberOfSections = 0
        self.numberOfGridChords = 2048
        self.hingeDepthPercent = 23.0
        self.tipDepthPercent = 8.0
        self.tipDepth = 0
        self.hingeInnerPoint = 0
        self.hingeOuterPoint = 0
        self.hingeLineAngle = 0.0
        self.showQuarterChordLine = 'true',
        self.showTipLine = 'true',
        self.showHingeLine = 'true',
        self.dihedral = 0.00
        self.sections = []
        self.grid = []
        self.valueList = []
        self.chords = []
        self.area = 0.0
        self.aspectRatio = 0.0


    # set airfoilnames from basic name and Re-number
    def set_AirfoilNamesFromRe(self):
        for Re in self.airfoilReynolds:
                airfoilName = (self.airfoilBasicName + "-%s.dat") % get_ReString(Re)
                self.airfoilNames.append(airfoilName)


    # set basic data of the wing
    def set_Data(self, dictData):
        self.airfoilPositions = dictData["airfoilPositions"]
        self.airfoilReynolds = dictData["airfoilReynolds"]

        # get user-defined list of airfoil-names
        try:
            self.airfoilNames = dictData["airfoilNames"]
        except:
            # no user defined list was found. In this case at least the
            # name of the root airfoil has to be specified.
            self.airfoilBasicName = dictData["airfoilBasicName"]
            self.airfoilNames = []

        self.airfoilTypes = dictData["airfoilTypes"]
        self.userAirfoils = dictData["userAirfoils"]

        # set number of sections to number of positions
        self.numberOfSections = len(self.airfoilPositions)

        # determine reynolds-number for root-airfoil
        self.rootReynolds = self.airfoilReynolds[0]

        # evaluate planformdata
        self.rootchord = dictData["rootchord"]
        self.wingspan = dictData["wingspan"]
        self.fuselageWidth = dictData["fuselageWidth"]
        self.planformShape = dictData["planformShape"]
        self.tipchord = dictData["tipchord"]
        self.tipDepthPercent = (self.tipchord/self.rootchord)*100
        self.halfwingspan = (self.wingspan/2)-(self.fuselageWidth/2)
        self.rootTipSweep = dictData["rootTipSweep"]
        self.leadingEdgeOrientation = dictData["leadingEdgeOrientation"]
        self.hingeDepthPercent = dictData["hingeDepthPercent"]
        self.dihedral = dictData["dihedral"]
        self.planformName = dictData["planformName"]
        self.wingFinSwitch = dictData["isFin"]

        # evaluate additional data
        try:
            self.showQuarterChordLine = dictData["showQuarterChordLine"]
            self.showTipLine = dictData["showTipLine"]
            self.showHingeLine = dictData["showHingeLine"]
        except:
            pass


    # get name of the user defined airfoil, as it will appear in the planform
    def get_UserAirfoilName(self, userAirfoil_idx):
        userAirfol_num = 0
        # loop through all airfoils
        for idx in range(len(self.airfoilTypes)):
            # Is the airfoil a user-defined airfoil ?
            if self.airfoilTypes[idx] == "user":
                # was the desired index found?
                if (userAirfoil_idx == userAirfol_num):
                    # Found
                    return self.airfoilNames[idx]
                else:
                    # only increment number of user-airfoil
                    userAirfol_num = userAirfol_num + 1

        # nothing was found
        return None



    # find planform-values for a given chord-length
    def find_PlanformData(self, chord):
        for element in self.grid:
            if (element.chord <= chord):
              return element


    # copy planform-values to section
    def copy_PlanformDataToSection(self, grid, section):
        section.y = grid.y
        section.chord = grid.chord
        section.hingeDepth = grid.hingeDepth
        section.hingeLine = grid.hingeLine
        section.trailingEdge = grid.trailingEdge
        section.leadingEdge = grid.leadingEdge
        section.quarterChordLine = grid.quarterChordLine
        section.tipLine = grid.tipLine
        section.dihedral = self.dihedral

        # set Re of the section (only for proper airfoil naming)
        section.Re = (section.chord / self.rootchord) * self.rootReynolds


    # sets the airfoilname of a section
    def set_AirfoilName(self, section):
        try:
           section.airfoilName = self.airfoilNames[section.number-1]
        except:
            section.airfoilName = (self.airfoilBasicName + "-%s.dat") % \
             (get_ReString(section.Re))


    def set_lastSectionAirfoilName(self, section):
        try:
            section.airfoilName = self.airfoilNames[section.number-2]
        except:
            section.airfoilName = (self.airfoilBasicName + "-%s.dat") % \
             (get_ReString(section.Re))


    # calculate planform-shape of the half-wing (high-resolution wing planform)
    def calculate_planform(self):
        self.hingeInnerPoint = (1-(self.hingeDepthPercent/100))*self.rootchord

        # calculate tip-depth
        self.tipDepth = self.rootchord*(self.tipDepthPercent/100)

        # calculate the depth of the hinge at the tip
        tipHingeDepth = self.tipDepth *(self.hingeDepthPercent/100)

        # calculate quarter-chord-lines
        rootQuarterChord = self.rootchord/4
        tipQuarterChord = self.tipDepth/4

        # calculate the tip offset according to the sweep-angle
        tipOffsetSweep = tan((self.rootTipSweep*pi)/180)*(self.halfwingspan)

        # calculate hinge-offset at the tip (without the tip-offset)
        tipHingeOffset = self.tipDepth - tipQuarterChord - tipHingeDepth

        # calculate offset of the middle of the tip
        tipMiddleOffset = self.tipDepth/2 - tipQuarterChord

        # calculate hinge outer-point
        self.hingeOuterPoint = rootQuarterChord + tipOffsetSweep + tipHingeOffset

        # calculate the outer point of the middle of the tip
        tippMiddleOuterPoint = rootQuarterChord + tipOffsetSweep + tipMiddleOffset

        # calculate hinge line angle
        AK = self.halfwingspan
        GK = self.hingeOuterPoint - self.hingeInnerPoint
        hingeLineAngle_radian = atan(GK/AK)

        # convert radian measure --> degree
        self.hingeLineAngle = (hingeLineAngle_radian / pi) * 180.0

        # calculate interval for setting up the grid
        grid_delta_y = (self.halfwingspan / (self.numberOfGridChords-1))

        # calculate all Grid-chords
        for i in range(1, (self.numberOfGridChords + 1)):
            # create new grid
            grid = wingGrid()

            # calculate grid coordinates
            grid.y = grid_delta_y * (i-1)

            # chord-length
            if self.planformShape == 'elliptical':
                # elliptical shaping of the wing with straight hing-line
                #self.rootchord*(1-self.overElipticOffset)
                grid.chord = (self.rootchord-self.tipDepth)\
                 *np.sqrt(1-(grid.y*grid.y/(self.halfwingspan*self.halfwingspan)))\
                 + self.tipDepth
            else:
                # trapezoidal shaping of the wing
                grid.chord = self.rootchord*(self.halfwingspan-grid.y)/self.halfwingspan \
                            + self.tipDepth* (grid.y/self.halfwingspan)

            grid.hingeDepth = (self.hingeDepthPercent/100)*grid.chord
            grid.hingeLine = (self.hingeOuterPoint-self.hingeInnerPoint)/(self.halfwingspan) * (grid.y) + self.hingeInnerPoint
            grid.trailingEdge = grid.hingeLine + grid.hingeDepth
            grid.leadingEdge = grid.hingeLine -(grid.chord-grid.hingeDepth)
            grid.quarterChordLine = grid.leadingEdge + (grid.trailingEdge-grid.leadingEdge)/4
            grid.tipLine = tippMiddleOuterPoint

            # append section to section-list of wing
            self.grid.append(grid)

            # calculate area of the wing
            self.area = self.area + (grid_delta_y*10 * grid.chord*10)

        # calculate aspect ratio of the wing
        self.area = self.area * 2.0
        self.aspectRatio = self.wingspan*self.wingspan / (self.area/100)

        # add offset of half of the fuselage-width to the y-coordinates
        for element in self.grid:
            element.y = element.y + self.fuselageWidth/2


    # get chordlength from position, according to the planform-data
    def get_chordFromPosition(self, position):
        # valid position specified ?
        if (position == None):
            ErrorMsg("invalid position")
            return None

        # get chord from planform
        for gridData in self.grid:
            if (gridData.y >= position):
                return gridData.chord
        ErrorMsg("no chordlength was found for position %f" % position)
        return None


    # get Re from position, according to the planform-data
    def get_ReFromPosition(self, position):
        chord = self.get_chordFromPosition(position)
        if (chord != None):
            chordRatio = chord / self.rootchord
            Re = self.airfoilReynolds[0] * chordRatio
            return Re
        else:
            ErrorMsg("no Re could not be caclulated for position %f" % position)
            return None


    # get chordlength from position, according to the planform-data
    def get_chordFromPositionOrReynolds(self, position, reynolds):
        # valid reynolds number specified ?
        if (reynolds != None):
            # calculate chord from ratio reynolds to rootReynolds
            return (reynolds / self.rootReynolds) * self.rootchord

        # valid position specified ?
        elif (position != None):
            # get chord from planform
            for gridData in self.grid:
                if (gridData.y >= position):
                    return gridData.chord

        # nothing was found
        ErrorMsg("position or reynolds not found inside planform")
        NoteMsg("position was: %f, reynolds was %d" % (position, reynolds))
        return None


    # calculate all chordlenghts from the list of airfoil positions
    # and the given planform-data
    def calculate_chordlengths(self):
        for idx in range(len(self.airfoilPositions)):
            position = self.airfoilPositions[idx]
            reynolds = self.airfoilReynolds[idx]
            chord = self.get_chordFromPositionOrReynolds(position, reynolds)
            self.chords.append(chord)


    # calculate missing Re-numbers from positions
    def calculate_ReNumbers(self):
        num = len(self.airfoilReynolds)

        # loop over list of specified Re-numbers
        for idx in range(num):
            if self.airfoilReynolds[idx] == None:
                # for this position no reynolds-number has been specified.
                # calculate the number from postition now
                Re = self.get_ReFromPosition(self.airfoilPositions[idx])
                self.airfoilReynolds[idx] = int(round(Re ,0))


    # calculate all sections of the wing, oriented at the grid
    def calculate_sections(self):
        # create all sections, according to the precalculated chords
        for chord in self.chords:
            # find grid-values matching the chordlength of the section
            grid = self.find_PlanformData(chord)

            if (grid == None):
                ErrorMsg("chord-length %f not found in planform-data\n")
                # end the loop
                break

            # create new section
            section = wingSection()

            # append section to section-list of wing
            self.sections.append(section)

            # set number of the section
            section.number = len(self.sections)

            # find grid-values matching the chordlength of the section
            grid = self.find_PlanformData(chord)

            # copy grid-coordinates to section
            self.copy_PlanformDataToSection(grid, section)

            # set the airfoil-Name
            self.set_AirfoilName(section)

            # store last Re value for the tip
            lastSectionRe = section.Re

        # create last section
        section = wingSection()

        # append section to section-list of wing
        self.sections.append(section)

        # set number of the section
        section.number = len(self.sections)

        # get the tip grid-values
        grid = self.grid[len(self.grid)-1]

        # copy grid-coordinates to section
        self.copy_PlanformDataToSection(grid, section)

        # set same Re as for the last section so the same airfoil-name will be given
        section.Re = lastSectionRe

        # set the airfoil-Name
        self.set_lastSectionAirfoilName(section)


    def get_colorFromAirfoilType(self, airfoilType):
        if (airfoilType == 'user'):
            color = cl_userAirfoil
        elif (airfoilType == 'opt'):
            color = cl_optAirfoil
        else:
            color = cl_sections

        return color


    # plot planform of the half-wing
    def plot_HalfWingPlanform(self, ax):
        # create empty lists
        xValues = []
        leadingEdge = []
        trailingeEge = []
        hingeLine = []
        quarterChordLine = []
        tipLine = []

        # setup empty lists for new tick locations
        x_tick_locations = []
        y_tick_locations = [self.rootchord]

        # set axes and labels
        self.set_AxesAndLabels(ax, "Half-wing planform")

        # plot sections in reverse order
        idx = len(self.airfoilTypes)
        for element in reversed(self.sections):
            # determine type of airfoil of this section
            try:
                airfoilType = self.airfoilTypes[idx]
            except:
                airfoilType = 'blend'

            labelColor = self.get_colorFromAirfoilType(airfoilType)

            ax.plot([element.y, element.y] ,[element.leadingEdge, element.trailingEdge],
            color=labelColor, linestyle = ls_sections, linewidth = lw_sections)

            # determine x and y Positions of the labels
            xPos = element.y
            if (self.leadingEdgeOrientation == 'up'):
                yPosChordLabel = element.trailingEdge
                yPosOffsetSectionLabel = 32
            else:
                yPosChordLabel = element.leadingEdge
                yPosOffsetSectionLabel = -32

            yPosSectionLabel = element.leadingEdge

            # plot label for chordlength of section
            try:
                text = ("%d mm" % int(round(element.chord*1000)))
            except:
                text = ("0 mm" )
                ErrorMsg("label for chordlength of section could not be plotted")

            ax.annotate(text,
            xy=(xPos, yPosChordLabel), xycoords='data',
            xytext=(2, 5), textcoords='offset points', color = 'white',
            fontsize=fs_infotext, rotation='vertical')


            # plot label for airfoil-name / section-name
            text = ("%s" % (remove_suffix(element.airfoilName,'.dat')))
            props=dict(arrowstyle="-", connectionstyle= "angle,angleA=-90,angleB=30,rad=10", color=labelColor)

            ax.annotate(text,
            xy=(xPos, yPosSectionLabel), xycoords='data',
            xytext=(8, yPosOffsetSectionLabel), textcoords='offset points', color = 'white',
            bbox=dict(boxstyle="round", facecolor = labelColor, alpha=0.5), fontsize=fs_infotext, rotation='vertical', arrowprops=props)

            # append position of section to x-axis ticks
            x_tick_locations.append(xPos)

            # append position of leading edge and trailing edge to y-axis-ticks
            y_tick_locations.append(element.leadingEdge)
            #y_tick_locations.append(element.trailingEdge)
            idx = idx - 1

        # set new ticks for the x-axis according to the positions of the sections
        ax.set_xticks(x_tick_locations)
        ax.set_yticks(y_tick_locations)

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
            quarterChordLine.append(element.quarterChordLine)
            tipLine.append(element.tipLine)
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

        # plot quarter-chord-line
        if (self.showQuarterChordLine == 'true'):
            ax.plot(xValues, quarterChordLine, color=cl_quarterChordLine,
              linestyle = ls_quarterChordLine, linewidth = lw_quarterChordLine,
              label = "quarter-chord line")

        if (self.showTipLine == 'true'):
            ax.plot(xValues, tipLine, color=cl_tipLine,
              linestyle = ls_tipLine, linewidth = lw_tipLine,
              label = "tip line")

        # plot hinge-line
        if (self.showHingeLine == 'true'):
            ax.plot(xValues, hingeLine, color=cl_hingeLine,
              linestyle = ls_hingeLine, linewidth = lw_hingeLine,
              label = labelHingeLine)

        # plot the planform last
        ax.plot(xValues, leadingEdge, color=cl_planform,
                linewidth = lw_planform)#, label = "planform")
        ax.plot(xValues, trailingeEge, color=cl_planform,
                linewidth = lw_planform)
        try:
            airfoilType = self.airfoilTypes[0]
        except:
             airfoilType = 'blend'

        ax.plot(rootJoint_x, rootJoint_y,
                color = self.get_colorFromAirfoilType(airfoilType),
                linewidth = lw_planform)

        ax.plot(tipJoint_x, tipJoint_y, color= cl_planform,
                linewidth = lw_planform)

        # plot additional point (invisible) to expand the y-axis and
        # get the labels inside the diagram
        ax.plot(xValues[0], -1*(self.rootchord/2))

        # place legend inside subplot
        ax.legend(loc='lower right', fontsize = fs_legend)

        # show grid
        ax.grid(True)

        # both axes shall be equal
        ax.axis('equal')

        # revert y-axis on demand
        if (self.leadingEdgeOrientation == 'up'):
            ax.set_ylim(ax.get_ylim()[::-1])


    # plot planform of the complete wing
    def plot_WingPlanform(self, ax):
        #create empty lists
        xValues = []
        leadingEdge = []
        trailingeEge = []
        hingeLine = []
        quarterChordLine = []
        tipLine = []

        # set axes and labels
        self.set_AxesAndLabels(ax, "Full-wing planform")


        # build up list of x-values,
        # first left half-wing
        for element in self.grid:
            xValues.append(element.y-(self.fuselageWidth/2))

        # offset for beginning of right half-wing
        xOffset = self.halfwingspan + self.fuselageWidth/2

        # center-section / fuselage (x)
        lastElement = len(xValues)-1
        Left_x = xValues[lastElement]
        Right_x = Left_x + self.fuselageWidth
        leftWingRoot_x = (Left_x, Left_x)
        rightWingRoot_x = (Right_x, Right_x)

        # right half-wing (=reversed right-half-wing)
        for element in self.grid:
            xValues.append(element.y + xOffset)

        # build up lists of y-values
        # left half wing
        for element in reversed(self.grid):
            leadingEdge.append(element.leadingEdge)
            hingeLine.append(element.hingeLine)
            trailingeEge.append(element.trailingEdge)

        # center-section / fuselage (y)
        wingRoot_y = (leadingEdge[lastElement],trailingeEge[lastElement])

        # right half wing
        for element in (self.grid):
            leadingEdge.append(element.leadingEdge)
            hingeLine.append(element.hingeLine)
            trailingeEge.append(element.trailingEdge)

        # setup root- and tip-joint
        lastElement = len(xValues)-1
        leftTipJoint_x = (xValues[0], xValues[0])
        leftTipJoint_y = (leadingEdge[0],trailingeEge[0])
        rightTipJoint_x = (xValues[lastElement], xValues[lastElement])
        rightTipJoint_y = (leadingEdge[lastElement],trailingeEge[lastElement])

        # plot left tip-joint
        ax.plot(leftTipJoint_x, leftTipJoint_y, color=cl_planform,
                linewidth = lw_planform)

        # plot the planform last
        ax.plot(xValues, leadingEdge, color=cl_planform,
                linewidth = lw_planform)#, label = "planform")
        ax.plot(xValues, trailingeEge, color=cl_planform,
                linewidth = lw_planform)

        # center-section
        ax.plot(leftWingRoot_x, wingRoot_y, color=cl_planform,
                linewidth = lw_planform)

        ax.arrow(self.wingspan/2, 0.0, 0.0, -1*(self.rootchord/3),head_width=self.fuselageWidth/4)

        ax.plot(rightWingRoot_x, wingRoot_y, color=cl_planform,
                linewidth = lw_planform)

        # plot right tip-joint
        ax.plot(rightTipJoint_x, rightTipJoint_y, color=cl_planform,
                linewidth = lw_planform)

        # both axes shall be equal
        ax.axis('equal')

        # revert y-axis on demand
        if (self.leadingEdgeOrientation == 'up'):
            ax.set_ylim(ax.get_ylim()[::-1])

        # plot hinge-line
        if (self.showHingeLine == 'true'):
            ax.plot(xValues, hingeLine, color=cl_hingeLine,
              linestyle = ls_hingeLine, linewidth = lw_hingeLine)

        # fill the wing
        ax.fill_between(xValues, leadingEdge, hingeLine, color=cl_planform, alpha=0.4)
        ax.fill_between(xValues, hingeLine, trailingeEge, color=cl_hingeLine, alpha=0.4)

        # setup list for new x-tick locations
        new_tick_locations = [0.0, self.halfwingspan, (self.halfwingspan + self.fuselageWidth/2),
                             (self.halfwingspan + self.fuselageWidth),self.wingspan]

        # set new ticks for the x-axis according to the positions of the sections
        ax.set_xticks(new_tick_locations)

        # set new fontsize of the x-tick labels
        for tick in ax.xaxis.get_major_ticks():
            tick.label.set_fontsize(fs_tick)
            tick.label.set_rotation('vertical')

        # set new fontsize of the y-tick labels
        for tick in ax.yaxis.get_major_ticks():
            tick.label.set_fontsize(fs_tick)


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
        rootchord_mm = int(round(self.rootchord*1000))
        text = "\"%s\"\n wingspan: %d mm, rootchord: %d mm, area: %.2f dm², "\
         "aspect ratio: %.2f, root-tip sweep: %.2f°, hinge line angle: %.2f°\n"\
         % (self.planformName, wingspan_mm, rootchord_mm, self.area,
         self.aspectRatio, self.rootTipSweep, self.hingeLineAngle)

        fig.suptitle(text, fontsize = 12, color="darkgrey", **csfont)

        # first figure, display detailed half-wing
        self.plot_HalfWingPlanform(upper)

        # second figure, display
        self.plot_WingPlanform(lower)

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

    def getFigure(self):
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
        self.plot_HalfWingPlanform(upper)

        # second figure, display
        self.plot_HalfWingPlanform(lower)

        return fig

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
        ErrorMsg("wing not found\n")
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

        NoteMsg("Section %d: position: %.0f mm, chordlength %.0f mm, airfoilName %s was inserted" %
          (section.number, section.y*1000, section.chord*1000, section.airfoilName))

    # write all data to the new file file
    tree.write(outFileName)

################################################################################
# function that gets the name of the planform-data-file
def get_planformDataFileName(args):

    if args.planforminput:
        inFileName = args.planforminput
    else:
        # use Default-name
        inFileName = ressourcesPath + bs + 'planformdata'

    inFileName = inFileName + '.txt'
    NoteMsg("filename for planform input-data is: %s" % inFileName)
    return inFileName


################################################################################
# function that gets the name of the strak-data-file
def get_strakDataFileName(args):

    if args.strakinput:
        inFileName = args.strakinput
    else:
        # use Default-name
        inFileName = ressourcesPath + bs + strakMachineInputFileName

    NoteMsg("filename for strak input-data is: %s" % inFileName)
    return inFileName


def copy_userAirfoils(wingData):
    userAirfoil_idx = 0

    for airfoil in wingData.userAirfoils:
        splitnames = airfoil.split("\\")
        airfoilName = splitnames[-1]
        airfoilName = remove_suffix(airfoilName, ".dat")
        srcPath = '\\'.join(splitnames[0:-1])
        destName = wingData.get_UserAirfoilName(userAirfoil_idx)
        destName = remove_suffix(destName, ".dat")

        copyAndSmooth_Airfoil(airfoilName, srcPath, destName, False)
        userAirfoil_idx = userAirfoil_idx + 1


def get_rightFoilData(wingData, start):
    end = len(wingData.airfoilTypes)

    # loop over all airfoil-Types
    for idx in range(start, end):
        # Get the first "non-blend" airfoil beginning from "start"
        if (wingData.airfoilTypes[idx] != "blend"):
            return ( wingData.airfoilNames[idx], wingData.chords[idx])

    # Nothing was found
    return (None, None)


def calculate_Blend(chord_left, chord_blend, chord_right):
    if (chord_left < chord_blend < chord_right):
        # decreasing chord from left to right (normal)
        diff = chord_right - chord_left
        diff_blend = chord_blend - chord_left
    elif (chord_left > chord_blend > chord_right):
        # increasing chord from left to right (unusual)
        diff = chord_left - chord_right
        diff_blend = chord_left - chord_blend
    else:
        # unknown behaviour
        ErrorMsg("calculate_Blend()")
        diff = diff_blend = 1

    # calculate blend now
    blend = (diff_blend / diff) * 100
    blend = round(blend, 0)
    return int(blend)


def check_airfFoilsExist(name_1, name_2):
    try:
        file = open(name_1)
        file.close()
        file = open(name_2)
        file.close()
    except:
        return False

    return True



def create_blendedArifoils(wingData):
    num = len(wingData.airfoilTypes)

    # loop over all airfoil-Types
    for idx in range(num):
        # not a "blend" airfoil ?
        if (wingData.airfoilTypes[idx] != "blend"):
            # yes, take this airfoil as the "left-side" airfoil for blending
            leftFoilName = wingData.airfoilNames[idx]
            leftFoilChord = wingData.chords[idx]
        else:
            # no, this is a "blend" airfoil that must be created
            blendFoilName = wingData.airfoilNames[idx]
            blendFoilName = remove_suffix(blendFoilName , ".dat")
            blendFoilChord = wingData.chords[idx]

            # get data of the "right-side" airfoil for blending
            (rightFoilName, rightFoilChord) = get_rightFoilData(wingData, idx+1)
            # check if left- and right-side airfoils exist

            if (check_airfFoilsExist(leftFoilName, rightFoilName) == True):
                NoteMsg("creating blended airfoil %s" % blendFoilName)

                # calculate the blend-factor
                blend = calculate_Blend(leftFoilChord, blendFoilChord, rightFoilChord)

                # compose XFOIL-worker-call
                worker_call = ".." + bs +".." + bs +"bin" + bs +\
                          "xfoil_worker.exe -w blend %d -a %s -a2 %s -o %s"\
                        % (blend, leftFoilName, rightFoilName, blendFoilName)

                # call worker now by system call
                os.system(worker_call)
            else:
                NoteMsg("at least one airfoil for blending does not exist,"\
                 "skipping blending for airfoil %s" % blendFoilName)


def determine_SeedFoilIdx(wingData):
    num = len(wingData.airfoilTypes)

    # loop backwards
    for idx in range(num-1,-1,-1):
        # first user-airfoil that is found (loop-backwards) will be the seedfoil
        if (wingData.airfoilTypes[idx] == "user"):
            return idx

    # at least the airfoil at idx 0 must be the seedfoil
    return 0


def update_seedfoilName(wingData, strakdata):
    idx = determine_SeedFoilIdx(wingData)
    seedFoilName = wingData.airfoilNames[idx]

    # set the new seedfoilname
    strakdata["seedFoilName"] = remove_suffix(seedFoilName, ".dat")


def update_airfoilNames(wingData, strakdata):
    num = len(wingData.airfoilTypes)
    airfoilNames = []

    # get index of the seedfoil
    seedIdx = determine_SeedFoilIdx(wingData)

    # first append name of the seedfoil
    airfoilNames.append(wingData.airfoilNames[seedIdx])

    # create list of airfoilnames that shall be created by the strak-machine
    for idx in range(num):
        if (wingData.airfoilTypes[idx] == "opt"):
            airfoilNames.append(wingData.airfoilNames[idx])

    # now set the new list in the strakdata-dictionary
    strakdata["airfoilNames"] = airfoilNames


def update_reynolds(wingData, strakdata):
    num = len(wingData.airfoilTypes)
    reynolds = []

    # get index of the seedfoil
    seedIdx = determine_SeedFoilIdx(wingData)

    # first append reynolds-number of the seedfoil
    reynolds.append(wingData.airfoilReynolds[seedIdx])

    # create list of reynolds-numbers for the airfoils that shall be created by
    # the strak-machine
    for idx in range(num):
        if (wingData.airfoilTypes[idx] == "opt"):
            reynolds.append(wingData.airfoilReynolds[idx])

    # now set the new list in the strakdata-dictionary
    strakdata["reynolds"] = reynolds


def create_strakdataFile(strakDataFileName):
    data = { "seedFoilName": " ", "reynolds": [0,0], "airfoilNames": [" "," "]}
    json.dump(data, open(strakDataFileName,'w'))
    NoteMsg("strakdata was successfully created")


# write Re-numbers and seedfoil to strakdata.txt
def update_strakdata(wingData):
    # try to open .json-file
    try:
        strakDataFile = open(strakDataFileName, "r")
    except:
        NoteMsg('failed to open file %s, creating new one' % strakDataFileName)
        create_strakdataFile(strakDataFileName)
        strakDataFile = open(strakDataFileName, "r")

    # load dictionary from .json-file
    try:
        strakdata = json.load(strakDataFile)
        strakDataFile.close()
    except:
        ErrorMsg('failed to read data from file %s' % strakDataFileName)
        return

    # update data coming from planform-creator
    update_seedfoilName(wingData, strakdata)
    update_airfoilNames(wingData, strakdata)
    update_reynolds(wingData, strakdata)

    # write json-File
    with open(strakDataFileName, "w") as write_file:
        json.dump(strakdata, write_file, indent=4, separators=(", ", ": "), sort_keys=False)
        write_file.close()
    NoteMsg("strakdata was successfully updated")



# GUI-Handling
def on_key_press(event):
        print("you pressed {}".format(event.key))
        key_press_handler(event, canvas, toolbar)

def _quit():
        root.quit()     # stops mainloop
        root.destroy()  # this is necessary on Windows to prevent
                        # Fatal Python Error: PyEval_RestoreThread: NULL tstate

################################################################################
# function that gets arguments from the commandline
def get_Arguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    parser.add_argument("-planforminput", "-p", help="filename of planformdata input"\
                        "-file (e.g. planformdata)")


    parser.add_argument("-strakinput", "-s", help="filename of strakdata input"\
                        "-file (e.g. strakdata)")

    # read arguments from the command line
    args = parser.parse_args()
    return (get_planformDataFileName(args), get_strakDataFileName(args))


def addEntryToGUI(entryName, xOffset, yOffset):
    canvas1 = tkinter.Canvas(root, width = 400, height = 300)
    canvas1.pack()

    # add label
    label2 = tkinter.Label(root, text=entryName)
    label2.config(font=('helvetica', 20))
    canvas1.create_window(xOffset, yOffset, window=label2)

    # add entry-field
    entry1 = tkinter.Entry (root)
    entry1.setvar("2.98")
    canvas1.create_window(xOffset + 100, yOffset, window=entry1)


def GUI():
    # !! WIP !!!
    # set up GUI
    root = tkinter.Tk()
    root.wm_title("Planform-Creator")

    # get Figure from wing-class
    fig = newWing.getFigure()
    canvas = FigureCanvasTkAgg(fig, master=root)  # A tk.DrawingArea.
    canvas.draw()
    canvas.get_tk_widget().pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=1)

    toolbar = NavigationToolbar2Tk(canvas, root)
    toolbar.update()
    canvas.get_tk_widget().pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=1)
    canvas.mpl_connect("key_press_event", on_key_press)

    # define font for button
    myFont = font.Font(size=30)
    # add the button
    button = tkinter.Button(master=root, text="Quit", command=_quit)
    # apply font to the button label
    button['font'] = myFont
    button.pack(side=tkinter.BOTTOM)

    # create parameter-GUI
    addEntryToGUI("Wing-span", 0, 20)

##
##"wingspan": 2.98,#2.54,
##            # shape of the planform, elliptical / trapezoidal
##            "planformShape": 'elliptical',
##            # orientation of the wings leading edge
##            "leadingEdgeOrientation": 'up',
##            # length of the root-chord in m
##            "rootchord": 0.223,
##            # depth of the tip in percent of the chord-length
##            "tipDepthPercent": 8.0,
##            # sweep of the tip of the wing in degrees
##            "rootTipSweep": 3.2,
##            # depth of the aileron / flap in percent of the chord-length
##            "hingeDepthPercent": 23.5,
##            # dihedral of the of the wing in degree
##            "dihedral": 2.5,


    tkinter.mainloop()

# Main program
if __name__ == "__main__":
    init()

    #get command-line-arguments or user-input
    (planformDataFileName, strakDataFileName) = get_Arguments()

    # create a new planform
    newWing = wing()

    #debug, generate example-dictionary
    #json.dump(PLanformDict, open("planformdata.txt",'w'))

    # check working-directory, have we been started from "scripts"-dir?
    if (os.getcwd().find("scripts")>=0):
        os.chdir("..")

    # try to open .json-file
    try:
     planform = open(planformDataFileName)
    except:
        ErrorMsg("failed to open file %s" % planformDataFileName)
        exit(-1)

    # load dictionary of planform-data from .json-file
    try:
        planformData = json.load( planform)
        planform.close()
    except:
        ErrorMsg('Error, failed to read data from file %s' % planformDataFileName)
        planform.close()
        exit(-1)

    # set data for the planform
    newWing.set_Data(planformData)

    # calculate the grid, the chordlengths of the airfoils and the sections
    newWing.calculate_planform()
    newWing.calculate_ReNumbers()
    newWing.calculate_chordlengths()
    newWing.set_AirfoilNamesFromRe()
    newWing.calculate_sections()

    # get filename of plane-template
    inputFileName =  ressourcesPath + bs + planformData["templateFileName"]

    # compose output-filename for planform-xml-file
    outputFileName = outputFolder + bs + planformData["outFileName"]

    # create outputfolder, if it does not exist
    if not os.path.exists(outputFolder):
        os.makedirs(outputFolder)

    # insert the generated-data into the XML-File for XFLR5
    insert_PlanformDataIntoXFLR5_File(newWing, inputFileName, outputFileName)

    # get current working dir
    workingDir = os.getcwd()

    # check if output-folder exists. If not, create folder.
    if not os.path.exists(buildPath):
        os.makedirs(buildPath)

    # check if airfoil-folder exists. If not, create folder.
    if not os.path.exists(buildPath + bs + airfoilPath):
        os.makedirs(buildPath + bs + airfoilPath)

    # change working-directory to output-directory
    os.chdir(workingDir + bs + buildPath + bs + airfoilPath)

    # copy and rename user-airfoils, the results will be copied to the
    # airfoil-folder specified in the strak-machine
    copy_userAirfoils(newWing)

    # create blended airfoils using XFOIL_worker
    create_blendedArifoils(newWing)

    # change working-directory back
    os.chdir(workingDir)

    # update "strakdata.txt" for the strakmachine
    update_strakdata(newWing)

    # plot the planform
    newWing.draw()
