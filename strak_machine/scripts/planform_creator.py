#!/usr/bin/env python

#  This file is part of "The Strak Machine".

#  "The Strak Machine" is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  "The Strak Machine" is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with "The Strak Machine".  If not, see <http://www.gnu.org/licenses/>.

#  Copyright (C) 2020-2022 Matthias Boese

import re
import xml.etree.ElementTree as ET
from copy import deepcopy
import argparse
from sys import version_info
import os
import f90nml
from shutil import copyfile
from matplotlib import pyplot as plt
from matplotlib import rcParams
import numpy as np
from scipy.interpolate import make_interp_spline
from scipy import interpolate as scipy_interpolate
from scipy.ndimage import gaussian_filter1d
from scipy.optimize import curve_fit
from math import log10, floor, tan, atan, sin, cos, pi, sqrt
import json
import tkinter
from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
# Implement the default Matplotlib key bindings.
from matplotlib.backend_bases import key_press_handler
import tkinter.font as font
from strak_machine import (copyAndSmooth_Airfoil, get_ReString,
                             ErrorMsg, WarningMsg, NoteMsg, DoneMsg,
                             remove_suffix, interpolate, round_Re,
                             bs, buildPath, ressourcesPath, airfoilPath,
                             scriptPath, exePath, smoothInputFile,
                             strakMachineInputFileName, xfoilWorkerName,
                             T1_polarInputFile)

from colorama import init
from termcolor import colored
from FLZ_Vortex_export import export_toFLZ
#import bezier FIXME BEZIER


################################################################################
# some global variables

# folder containing the output / result-files
outputFolder = buildPath + bs +'planforms'

# fonts
csfont = {'fontname':'DejaVu Sans'}

# fontsizes
fs_infotext = 7
fs_legend = 7

# colours, lineStyles
cl_background = 'dark_background'
cl_quarterChordLine = 'orange'
cl_areaCenterLine = 'blue'
cl_hingeLine = 'r'
cl_hingeLineFill ='r'
cl_planform = 'gray'
cl_planformFill = 'gray'
cl_sections = 'grey'
cl_userAirfoil = 'DeepSkyBlue'
cl_optAirfoil = 'yellow'
cl_infotext = 'DeepSkyBlue'
cl_chordlengths = 'darkgray'
cl_referenceChord = 'gray'
cl_normalizedChord = 'blue'
cl_controlPoints = 'red'

ls_quarterChordLine = 'solid'
lw_quarterChordLine  = 0.8
ls_areaCenterLine = 'solid'
lw_areaCenterLine = 0.8
ls_hingeLine = 'solid'
lw_hingeLine = 0.6
ls_planform = 'solid'
lw_planform = 1.0
ls_sections = 'solid'
lw_sections = 0.4
fs_tick = 7


xfoilWorkerCall = "..\\..\\bin\\" + xfoilWorkerName + '.exe'
inputFilename = "..\\..\\ressources\\" + smoothInputFile

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
            # depth of the aileron / flap in percent of the chord-length, root
            "hingeDepthRoot": 27.0,
            # depth of the aileron / flap in percent of the chord-length, tip
            "hingeDepthTip": 19.0,
            # correction value to avoid a part of the wing being swept forward
            "leadingEdgeCorrection": 0.0085,
            # dihedral of the of the wing in degree
            "dihedral": 2.5,
            # whether to show the line at 25% of wing depth
            "showQuarterChordLine" : 'True',
            # whether to show line from root-chord to middle of the tip
            "showTipLine": 'True',
            # whether to show the hinge-line
            "showHingeLine" : 'True',
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

# derivative function
def deriv(f,x):
     h = 0.000000001                 #step-size
     return (f(x+h) - f(x))/h        #definition of derivative



def plot_Line(x, y):
    plt.plot(x, y, color='blue', linewidth = lw_planform, solid_capstyle="round")

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



def objective_elliptical_wrapper(values, normalizedTipChord, tipSharpness,
                                 leadingEdgeCorrection):
    result = []

    for x in values:
        y = objective_elliptical(x, normalizedTipChord, tipSharpness,
                         leadingEdgeCorrection)

        result.append(y)

    return (result)


def objective_elliptical(x, normalizedTipChord, tipSharpness,
                         leadingEdgeCorrection):

    # calculate distance to tip, where rounding starts
    tipRoundingDistance = normalizedTipChord * tipSharpness

    # calculate actual distance to tip
    distanceToTip = 1.0 - x

   # calculate delta that will be added to pure ellipse
    if (distanceToTip > tipRoundingDistance):
        # add constant value, as we are far away from the tip
        delta = normalizedTipChord
    else:
        # add decreasing value according to quarter ellipse
        a = tipRoundingDistance
        x1 = tipRoundingDistance-distanceToTip
        b = normalizedTipChord
        radicand = (a*a)-(x1*x1)

        if radicand > 0:
            # quarter ellipse formula
            delta = (b/a) * np.sqrt(radicand)
        else:
            delta = 0

    # elliptical shaping of the wing plus additonal delta
    y = (1.0-delta) * np.sqrt(1.0-(x*x)) + delta

    # correct chord with leading edge correction
    y = y - leadingEdgeCorrection * sin(interpolate(0.0, 1.0, 0.0, pi, x))

    return y


##def objective_bezier_wrapper(values, b1_x, b1_y, b2_x, b2_y, b3_x, b3_y, b4_x, b4_y, b5_x, b5_y):
##    result = []
##
##    for x in values:
##        y = objective_bezier(x, b1_x, b1_y, b2_x, b2_y, b3_x, b3_y, b4_x, b4_y, b5_x, b5_y)
##
##        result.append(y)
##
##    return (result)
##
##
##def objective_bezier(x, b1_x, b1_y, b2_x, b2_y, b3_x, b3_y, b4_x, b4_y, b5_x, b5_y):
##
##    nodes = np.asfortranarray([
##    [0.0, b1_x, b2_x, b3_x, b4_x, b5_x, 1.0],
##    [1.0, b1_y, b2_y, b3_y, b4_y, b5_y, 0.0],
##    ])
##
##
##    deg = len(nodes[0])-1
##
##    curve = bezier.Curve(nodes, degree=deg)
##
##    result = curve.evaluate(b1_x)
##    if b1_y < result[1][0] :
##        return -1
##
##    result = curve.evaluate(b2_x)
##    if b2_y < result[1][0] :
##        return -1
##
##    result = curve.evaluate(b3_x)
##    if b3_y < result[1][0] :
##        return -1
##
##    result = curve.evaluate(b4_x)
##    if b4_y < result[1][0] :
##        return -1
##
##    result = curve.evaluate(b5_x)
##    if b5_y < result[1][0] :
##        return -1
##
##    result = curve.evaluate(x)
##    y = result[1][0]
##    return y

################################################################################
# function that gets a single boolean parameter from dictionary and returns a
#  default value in case of error
def get_booleanParameterFromDict(dict, key, default):
    value = default
    try:
        string = dict[key]
        if (string == 'true') or (string == 'True'):
            value = True
        else:
            value = False
    except:
        NoteMsg('parameter \'%s\' not specified, using' \
        ' default-value \'%s\'' % (key, str(value)))
    return value


################################################################################
# function that gets a single mandatory parameter from dictionary and exits, if
# the key was not found
def get_MandatoryParameterFromDict(dict, key):
    try:
        value = dict[key]
    except:
        ErrorMsg('parameter \'%s\' not specified, this key is mandatory!'% key)
        sys.exit(-1)
    return value


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
        self.areaCenterLine = 0
        self.dihedral= 3.00
        self.flapGroup = 0

        # name of the airfoil-file that shall be used for the section
        self.airfoilName = ""

################################################################################
#
# normalizedGrid class
#
################################################################################
class normalizedGrid:

    # class init
     def __init__(self):
        self.y = 0.0
        self.chord = 0.0
        self.referenceChord = 0.0
        self.hinge = 0.0

################################################################################
#
# wingGrid class
#
################################################################################
class wingGrid:

    # class init
     def __init__(self):
        self.y = 0.0
        self.chord = 0.0
        self.leadingEdge = 0.0
        self.trailingEdge = 0.0
        self.hingeDepth = 0.0
        self.hingeLine = 0.0
        self.quarterChordLine = 0.0
        self.LE_derivative = 0.0
        self.areaCenterLine = 0.0


################################################################################
#
# Wing class
#
################################################################################
class wing:
    # class init
    def __init__(self):
        self.theme = 'default'
        self.airfoilBasicName = ""
        self.airfoilNames = []
        self.numAirfoils= 0
        self.planformName= "Main Wing"
        self.rootchord = 0.223
        self.tipchord = 0.03
        self.leadingEdgeOrientation = 'up'
        self.leadingEdgeCorrection = 0.0
        self.wingspan = 2.54
        self.fuselageWidth = 0.035
        self.planformShape = 'elliptical'
        self.planform_chord = []
        self.planform_y = []
        self.halfwingspan = 0.0
        self.numberOfGridChords = 16384
        self.hingeDepthRoot = 23.0
        self.hingeDepthTip = 23.0
        self.tipDepthPercent = 8.0
        self.tipDepth = 0
        self.tipSharpness = 0.5
        self.blendingEndPoint = 0.5
        self.hingeInnerPoint = 0
        self.hingeOuterPoint = 0
        self.hingeLineAngle = 0.0
        self.dihedral = 0.00
        self.area = 0.0
        self.aspectRatio = 0.0
        self.interpolationSegments = 5
        self.NCrit = 9.0           # for polar creation
        self.polarReynolds = []    # for polar creation
        self.sections = []
        self.grid = []
        self.normalizedGrid = []
        self.valueList = []
        self.chords = []
        self.isFin = False
        self.showQuarterChordLine = False
        self.showTipLine = False
        self.showHingeLine = True
        self.smoothUserAirfoils = True
        self.use_bezier = False
        self.bezier_x = []
        self.bezier_y = []


    def set_colours(self):
        global cl_background
        global cl_quarterChordLine
        global cl_areaCenterLine
        global cl_hingeLine
        global cl_planform
        global cl_hingeLineFill
        global cl_planformFill
        global cl_sections
        global cl_userAirfoil
        global cl_optAirfoil
        global cl_infotext
        global cl_controlPoints

        #self.theme = ' '
        if self.theme == 'black_white':
            # black and white theme
            cl_background = 'default'
            cl_quarterChordLine = 'black'
            cl_areaCenterLine = 'black'
            cl_hingeLine = 'dimgray'
            cl_planform = 'black'
            cl_hingeLineFill = 'gray'
            cl_planformFill = 'lightgray'
            cl_sections = 'black'
            cl_userAirfoil = 'black'
            cl_optAirfoil = 'black'
            cl_infotext = 'black'
            cl_chordlengths = 'lightgray'
            cl_referenceChord = 'gray'
            cl_normalizedChord = 'black'
            cl_controlPoints = 'red'
        else:
            # dark theme
            cl_background = 'dark_background'
            cl_quarterChordLine = 'orange'
            cl_areaCenterLine = 'blue'
            cl_hingeLine = 'r'
            cl_planform = 'gray'
            cl_hingeLineFill = 'r'
            cl_planformFill = 'lightgray'
            cl_sections = 'grey'
            cl_userAirfoil = 'DeepSkyBlue'
            cl_optAirfoil = 'yellow'
            cl_infotext = 'DeepSkyBlue'
            cl_chordlengths = 'darkgray'
            cl_referenceChord = 'gray'
            cl_normalizedChord = 'blue'
            cl_controlPoints = 'red'


    # compose a name from the airfoil basic name and the Re-number
    def set_AirfoilNamesFromRe(self):
        # loop over all airfoils (without tip and fuselage section)
        for idx in range(self.numAirfoils):
            Re = self.airfoilReynolds[idx]
            airfoilName = (self.airfoilBasicName + "-%s.dat") % get_ReString(Re)
            self.airfoilNames.append(airfoilName)


    # set missing airfoilnames from basic name and Re-number
    def set_AirfoilNames(self):
        if (len(self.airfoilNames) == 0):
            # list is empty and has to be created
            self.set_AirfoilNamesFromRe()

        # check if the .dat ending was appended to all airfoils.
        # if not, append the ending
        for idx in range(self.numAirfoils):
            if (self.airfoilNames[idx].find('.dat')<0):
                self.airfoilNames[idx] = self.airfoilNames[idx] +'.dat'


    def insert_fuselageData(self):
        self.airfoilNames.insert(0, self.airfoilNames[0])

        # root airfoil must be of type "user", so always insert user-airfoil
        self.userAirfoils.insert(0, self.userAirfoils[0])
        self.airfoilTypes.insert(0, self.airfoilTypes[0])

        # section has same chord, same reynolds
        self.chords.insert(0, self.chords[0])
        self.airfoilPositions.insert(0, 0.0)
        self.airfoilReynolds.insert(0, self.airfoilReynolds[0])


    def insert_tipData(self):
        self.airfoilNames.append(self.airfoilNames[-1])
        self.airfoilTypes.append(self.airfoilTypes[-1])
        self.airfoilPositions.append(self.wingspan/2)

        # is last airfoil of type "user" ?
        if self.airfoilTypes[-1] == "user":
            # yes, so append user-airfoil
            self.userAirfoils.append(self.userAirfoils[-1])

        reynolds = (self.tipchord / self.chords[-1]) * self.airfoilReynolds[-1]
        self.airfoilReynolds.append(int(round(reynolds,0)))
        self.chords.append(self.tipchord)


    # get the number of user defined airfoils
    def get_numUserAirfoils(self):
        num = 0
        for foilType in self.airfoilTypes:
            if (foilType == "user"):
                num = num + 1

        return num


    # set basic data of the wing
    def set_Data(self, dictData):
        # -------------- get mandatory parameters --------------------

        # get basic planformdata
        self.planformName = get_MandatoryParameterFromDict(dictData, "planformName")
        self.rootchord =  get_MandatoryParameterFromDict(dictData, "rootchord")
        self.wingspan =  get_MandatoryParameterFromDict(dictData, "wingspan")
        self.fuselageWidth =  get_MandatoryParameterFromDict(dictData, "fuselageWidth")
        self.planformShape =  get_MandatoryParameterFromDict(dictData, "planformShape")

        if ((self.planformShape != 'elliptical') and\
            (self.planformShape != 'curve_fitting') and\
            (self.planformShape != 'trapezoidal')):
            ErrorMsg("planformshape must be elliptical or curve_fitting or trapezoidal")
            sys.exit(-1)

        if self.planformShape == 'elliptical':
            self.leadingEdgeCorrection = get_MandatoryParameterFromDict(dictData, "leadingEdgeCorrection")
            self.tipSharpness =  get_MandatoryParameterFromDict(dictData, "tipSharpness")
        elif self.planformShape == 'curve_fitting':
            self.planform_chord = get_MandatoryParameterFromDict(dictData, "planform_chord")
            self.planform_y = get_MandatoryParameterFromDict(dictData, "planform_y")

        if (self.planformShape == 'elliptical') or\
           (self.planformShape == 'trapezoidal'):
            self.tipchord =  get_MandatoryParameterFromDict(dictData, "tipchord")

        self.rootTipSweep =  get_MandatoryParameterFromDict(dictData, "rootTipSweep")
        self.hingeDepthRoot = get_MandatoryParameterFromDict(dictData, "hingeDepthRoot")
        self.hingeDepthTip = get_MandatoryParameterFromDict(dictData, "hingeDepthTip")
        self.dihedral = get_MandatoryParameterFromDict(dictData, "dihedral")

        # get airfoil- / section data
        self.airfoilTypes = get_MandatoryParameterFromDict(dictData, 'airfoilTypes')
        self.airfoilPositions = get_MandatoryParameterFromDict(dictData, 'airfoilPositions')
        self.airfoilReynolds = get_MandatoryParameterFromDict(dictData, 'airfoilReynolds')
        self.flapGroups = get_MandatoryParameterFromDict(dictData, 'flapGroup')

        # number of airfoils equals number of specified airfoil types
        self.numAirfoils = len(self.airfoilTypes)

        # check number of airfoils
        if (self.numAirfoils == 0):
            ErrorMsg("number of airfoils must be >= 1")
            exit(-1)

        # check if the above parameters have the same number of elements
        if ((self.numAirfoils != len(self.airfoilPositions)) or
            (self.numAirfoils != len(self.airfoilReynolds))):
            ErrorMsg("airfoilTypes, airfoilPositions and airfoilReynolds must have the same number of elements")
            exit(-1)

        # After all types are known, the number of user airfoils can be determined
        numUserAirfoils = self.get_numUserAirfoils()

        # check number of user-airfoils
        if (numUserAirfoils == 0):
            ErrorMsg("number of user-airfoils must be >= 1")
            exit(-1)

        # check if first airfoil is user-airfoil
        if (self.airfoilTypes[0] != "user"):
            ErrorMsg("type of first airfoils must \"user\"")
            exit(-1)

        # check if there is a valid reynolds number specified for the first airfoil
        if (self.airfoilReynolds[0] == None):
            ErrorMsg("reynolds of first airfoils must not be \"None\"")
            exit(-1)
        else:
            # determine reynolds-number for root-airfoil
            self.rootReynolds = self.airfoilReynolds[0]

        # get names and paths of user-defined airfoils
        self.userAirfoils = dictData["userAirfoils"]

        # check userAirfoil names against number
        numDefinedUserAirfoils = len(self.userAirfoils)
        if (numDefinedUserAirfoils < numUserAirfoils):
            ErrorMsg("%d airfoils have type \"user\", but only %d user-airfoils"\
            " were defined in \"user-airfoils\""\
             % (numUserAirfoils, numDefinedUserAirfoils))
            exit(-1)
        elif (numDefinedUserAirfoils > numUserAirfoils):
            WarningMsg("%d airfoils have type \"user\", but %d user-airfoils"\
            " were defined in \"user-airfoils\""\
             % (numUserAirfoils, numDefinedUserAirfoils))
            self.userAirfoils = self.userAirfoils[0:numUserAirfoils]

        # calculate dependent parameters
        self.tipDepthPercent = (self.tipchord/self.rootchord)*100
        self.halfwingspan = (self.wingspan/2)-(self.fuselageWidth/2)

        # -------------- get optional parameters --------------------
        try:
            self.leadingEdgeOrientation = dictData["leadingEdgeOrientation"]
        except:
              NoteMsg("leadingEdgeOrientation not defined")

        try:
            self.interpolationSegments= dictData["interpolationSegments"]
        except:
              NoteMsg("interpolationSegments not defined")

        try:
            self.theme = dictData["theme"]
        except:
            NoteMsg("theme not defined")

        # get user-defined list of airfoil-names
        try:
            self.airfoilNames = dictData["airfoilNames"]
        except:
            NoteMsg("No user-defined airfoil names specified")
            self.airfoilNames = []

        try:
            self.airfoilBasicName = dictData["airfoilBasicName"]
        except:
             NoteMsg("No basic airfoil name specified")
             self.airfoilBasicName = None

        # if there are user defined airfoilnames, check if every airfoil has a
        # user defined name
        if ((len(self.airfoilNames) != self.numAirfoils) and
            (len(self.airfoilNames) != 0)):
            ErrorMsg("number of airfoilNames does not match the number of airfoils, which is %d" % self.numAirfoils)
            exit(-1)

        # if no basic airfoil name was defined, check if there are user defined arifoil names
        if ((len(self.airfoilNames) == 0) and (self.airfoilBasicName == None)):
            ErrorMsg("\"airfoilBasicName\" not defined and also \"airfoilNames\" not defined")
            exit(-1)

        # polar-generation feature
        try:
            self.polarReynolds = dictData["polar_Reynolds"]
            try:
                self.NCrit = dictData["polar_Ncrit"]
            except:
                NoteMsg("polar_Ncrit not defined, using default-value %f" %\
                  self.NCrit)
        except:
            NoteMsg("polar_Reynolds not defined, no Information for polar creation available")

        # evaluate additional boolean data
        self.isFin = get_booleanParameterFromDict(dictData, "isFin", self.isFin)

        self.smoothUserAirfoils = get_booleanParameterFromDict(dictData,
                                  "smoothUserAirfoils", self.smoothUserAirfoils)

        self.showQuarterChordLine = get_booleanParameterFromDict(dictData,
                                  "showQuarterChordLine", self.showQuarterChordLine)

        self.showTipLine = get_booleanParameterFromDict(dictData,
                                  "showTipLine", self.showTipLine)

        self.showHingeLine = get_booleanParameterFromDict(dictData,
                                   "showHingeLine", self.showHingeLine)


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
        section.areaCenterLine = grid.areaCenterLine
        section.dihedral = self.dihedral

        # set Re of the section (only for proper airfoil naming)
        section.Re = (section.chord / self.rootchord) * self.rootReynolds


    # sets the airfoilname of a section
    def set_AirfoilName(self, section):
        try:
            section.airfoilName = self.airfoilNames[section.number-1]
        except:
            ErrorMsg("No airfoilName found for section %d" % section.number)

    def set_lastSectionAirfoilName(self, section):
            section.airfoilName = self.airfoilNames[section.number-2]

   # calculates a chord-distribution, which is normalized to root_chord = 1.0
   # half wingspan = 1
    def calculate_normalizedChordDistribution(self):
        # calculate interval for setting up the grid
        grid_delta_y = 1 / (self.numberOfGridChords-1)
        normalizedTipChord = self.tipDepthPercent / 100
        normalizedRootChord = normalizedTipChord

        # In case of curve fitting, determine the optimum parameters
        # automatically
        if self.planformShape == 'curve_fitting':
##            if self.use_bezier: FIXME BEZIER
##                # curve fitting with objective function
##                guess = (0.6,1.0,  0.5,1.0,  0.7,0.8,  0.9,0.5,  0.95,0.4)
##                popt, _ = curve_fit(objective_bezier_wrapper, self.planform_chord, self.planform_y, p0=guess)
##                b1_x, b1_y, b2_x, b2_y, b3_x, b3_y, b4_x, b4_y, b5_x, b5_y = popt
##
##                # store bezier control points
##                self.bezier_x = (b1_x, b2_x, b3_x, b4_x, b5_x)
##                self.bezier_y = (b1_y, b2_y, b3_y, b4_y, b5_y)
            #else:
            guess = (0.18, 0.1, 0.005)
            popt, _ = curve_fit(objective_elliptical_wrapper, self.planform_chord, self.planform_y, p0=guess, bounds=(0.0, 100.0))
            normalizedTipChord, self.tipSharpness, self.leadingEdgeCorrection = popt

        # calculate all Grid-chords
        for i in range(1, (self.numberOfGridChords + 1)):
            # create new normalized grid
            grid = normalizedGrid()

            # calculate grid coordinates
            grid.y = grid_delta_y * (i-1)

            # pure elliptical shaping of the wing as a reference
            grid.referenceChord = np.sqrt(1.0-(grid.y*grid.y))
            #if (grid.y % 0.1) < grid_delta_y:
             #   print("%1.5f\n" % grid.referenceChord)

            # normalized chord-length
            if (self.planformShape == 'elliptical'):
                # elliptical shaping of the wing
                grid.chord = objective_elliptical(grid.y, normalizedTipChord,
                                self.tipSharpness, self.leadingEdgeCorrection)

            elif(self.planformShape == 'curve_fitting'):
##                if self.use_bezier: FIXME BEZIER
##                    # curve fitting algorithm with bezier
##                    grid.chord = objective_bezier(grid.y, b1_x, b1_y, b2_x, b2_y, b3_x, b3_y, b4_x, b4_y, b5_x, b5_y)
##                else:
                grid.chord = objective_elliptical(grid.y, normalizedTipChord,
                                self.tipSharpness, self.leadingEdgeCorrection)

            elif self.planformShape == 'trapezoidal':
                # trapezoidal shaping of the wing
                grid.chord = (1.0-grid.y) \
                            + normalizedTipChord * (grid.y)

##            # calculate hinge line
##            grid.hinge = interpolate(0.0 , 1.0, (self.hingeDepthRoot/100),
##                   ((self.hingeDepthTip/100) * normalizedTipChord), grid.y)
##
##            # calculate leadingEdge, trailingedge
##            grid.leadingEdge = grid.hingeLine -(grid.chord-grid.hingeDepth

            # append section to section-list of wing
            self.normalizedGrid.append(grid)

    # calculate planform-shape of the half-wing (high-resolution wing planform)
    def calculate_planform(self):
        self.hingeInnerPoint = (1-(self.hingeDepthRoot/100))*self.rootchord

        # calculate tip-depth
        self.tipDepth = self.rootchord*(self.tipDepthPercent/100)

        # calculate the depth of the hinge at the tip
        tipHingeDepth = self.tipDepth *(self.hingeDepthTip/100)

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

        # init areaCenter
        area_Center = 0.0

        # track maximum value of leading edge derivative
        LE_derivative_max = 0.0

        # track maximum value of hingeDepth
        hingeDepthPercent_max = 0.0

        # calculate all Grid-chords
        for i in range(1, (self.numberOfGridChords + 1)):

            # Get normalized grid for chordlength calculation
            normalizedGrid = self.normalizedGrid[i-1]
            normalizedChord = normalizedGrid.chord

            # create new grid
            grid = wingGrid()

            # calculate grid coordinates
            grid.y = grid_delta_y * (i-1)

            # chord-length
            grid.chord = self.rootchord * normalizedChord

            # calculate hingeDepth in percent at this particular point along the wing
            hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                       self.hingeDepthRoot, self.hingeDepthTip,
                                       grid.y)

            # correction of leading edge for elliptical planform, avoid swept forward part of the wing
            #delta = self.leadingEdgeCorrection * sin(interpolate(0.0, self.halfwingspan, 0.0, pi, grid.y))#FIXME

            grid.hingeDepth = (hingeDepth_y/100)*grid.chord #+ delta FIXME
            grid.hingeLine = (self.hingeOuterPoint-self.hingeInnerPoint)/(self.halfwingspan) * (grid.y) + self.hingeInnerPoint
            grid.leadingEdge = grid.hingeLine -(grid.chord-grid.hingeDepth)

            # calculate trailing edge according to chordlength at this particular
            # point along the wing
            grid.trailingEdge = grid.leadingEdge + grid.chord

            # calculate centerLine, quarterChordLine
            grid.centerLine = grid.leadingEdge + (grid.chord/2)
            grid.quarterChordLine = grid.leadingEdge + (grid.trailingEdge-grid.leadingEdge)/4

            # Calculate derivative of Leading edge
            if (i>3):
                grid.LE_derivative = -1.0*(self.grid[i-2].leadingEdge - self.grid[i-3].leadingEdge) / (self.grid[i-2].y - self.grid[i-3].y)
            else:
                grid.LE_derivative = 0.0

            # Tracking of LE-derative Maximum-value
            if (grid.LE_derivative < LE_derivative_max):
                LE_derivative_max = grid.LE_derivative

            # calculate percentual hingeDepth
            grid.hingeDepthPercent = ((grid.trailingEdge - grid.hingeLine) / grid.chord) * 100.0

            # Tracking of hingeDepth maximum value
            if (grid.hingeDepthPercent > hingeDepthPercent_max):
                hingeDepthPercent_max = grid.hingeDepthPercent


            # append section to section-list of wing
            self.grid.append(grid)

        # calculate the area of the wing
        self.calculate_wingArea()

        # calculate aspect ratio of the wing
        self.aspectRatio = self.wingspan*self.wingspan / (self.area/100)

        # add offset of half of the fuselage-width to the y-coordinates
        for element in self.grid:
            element.y = element.y + self.fuselageWidth/2
            element.areaCenterLine = self.area_Center

        #self.draw_LE_derivative() #FIXME Debug-plot

        return (LE_derivative_max, hingeDepthPercent_max)


    def calculate_wingArea(self):
        grid_delta_y = self.grid[1].y - self.grid[0].y
        area_Center = 0.0
        self.area = 0.0

        for element in self.grid:
            # sum up area
            area = (grid_delta_y*10 * element.chord*10)
            area_Center = area_Center + element.centerLine*area

            # calculate area of the wing
            self.area = self.area + area

        # Calculate areaCenter (Flaechenschwerpunkt)
        self.area_Center = area_Center / self.area

        # calculate area of the whole wing
        self.area = self.area * 2.0


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


    def denormalize_positions(self):
        for idx in range(len(self.airfoilPositions)):
            if self.airfoilPositions[idx] != None:
                self.airfoilPositions[idx] = self.airfoilPositions[idx] * self.halfwingspan


    def calculate_positions(self):
        for idx in range(len(self.airfoilPositions)):
            if self.airfoilPositions[idx] == None:
                # no position defined yet, calculate now
                grid = self.find_PlanformData(self.chords[idx])
                self.airfoilPositions[idx] = grid.y


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


    # determine weather a fuselage shall be used
    def fuselageIsPresent(self):
        # check, if a fuselageWidth > 0 was defined
        if self.fuselageWidth >= 0.0001:
            return True
        else:
            return False

    def getFlapPartingLines(self):
        flapPositions_x = []
        flapPositions_y =[]
        flapPositions_x_left =[]
        flapPositions_x_right =[]

        # As we have a projected drawing of the wing, we must consider the
        # projection factor
        proj_fact = cos(self.dihedral*pi/180.0)
        proj_halfwingSpan = self.halfwingspan * proj_fact

        sections = self.sections
        numSections = len(sections)
        actualFlapGroup = 0

        # Calculate zero Axis (center of the wing)
        zeroAxis = proj_halfwingSpan + self.fuselageWidth/2

        # check all sections
        for idx in range (0, numSections):
            # Change of Flap-Group ? -->Parting line
            if (actualFlapGroup != sections[idx].flapGroup):
                # determine x_pos to zeroAxis
                x_pos_rightZeroAxis = sections[idx].y * proj_fact
                # Shift by offset of zeroAxis
                x_pos_right = x_pos_rightZeroAxis + zeroAxis
                x_pos_leftZeroAxis = -1.0*x_pos_rightZeroAxis
                x_pos_left = x_pos_leftZeroAxis + zeroAxis
                x_tupel_left = (x_pos_left, x_pos_left)
                x_tupel_right = (x_pos_right, x_pos_right)
                y_tupel = (sections[idx].hingeLine, sections[idx].trailingEdge)
                flapPositions_x_right.append(x_tupel_right)
                flapPositions_x_left.append(x_tupel_left)
                flapPositions_y.append(y_tupel)

            actualFlapGroup = sections[idx].flapGroup

        # join all values
        flapPositions_x = flapPositions_x_left + flapPositions_x_right
        flapPositions_y = flapPositions_y + flapPositions_y

        return (flapPositions_x, flapPositions_y)

    # adds a section using given grid-values
    def add_sectionFromGrid(self, grid):
         # create new section
        section = wingSection()

        # append section to section-list of wing
        self.sections.append(section)

        # set number of the section
        section.number = len(self.sections)

        # copy grid-coordinates to section
        self.copy_PlanformDataToSection(grid, section)

        # set the airfoil-Name
        self.set_AirfoilName(section)


    # adds a section using given chord
    def add_sectionFromChord(self, chord):
        # find grid-values matching the chordlength of the section
        grid = self.find_PlanformData(chord)

        if (grid == None):
            ErrorMsg("chord-length %f not found in planform-data\n" % chord)
            exit(-1)
        else:
            self.add_sectionFromGrid(grid)


    # add an own section for the fuselage and use rootchord
    def add_fuselageSection(self):
        # get the root grid-values
        grid = deepcopy(self.grid[0])

        # set offset to zero so the section will start exactly in the center
        grid.y = 0

        # add section now
        self.add_sectionFromGrid(grid)


    # calculate all sections of the wing, oriented at the grid
    def calculate_sections(self):
        self.sections = []
        # check if fuselageWidth is > 0
        if self.fuselageIsPresent():
            # first add section for fuselage
            self.add_fuselageSection()
            startIdx = 1
        else:
            startIdx = 0

        # create all sections, according to the precalculated chords
        for chord in self.chords[startIdx:]:
            startIdx = 0
            # add section according to chord
            self.add_sectionFromChord(chord)


    # function to interpolate within sections. This is very useful to get a very
    # accurate calculation of lift-distribution for elliptical wings
    # e.g in FLZ-Vortex or XFLR5.
    # Also other calculations, like wing area, aspect ratio etc. get
    # more accurate
    def interpolate_sections(self):
        if self.interpolationSegments < 1:
            # nothing to do
            return

        NoteMsg("Interpolation of sections was requested, interpolating each section with"\
                " additional %d steps" % self.interpolationSegments)

        new_positions = []
        new_chords = []
        new_airfoilNames = []
        new_airfoilTypes = []
        new_flapGroups = []

        num = len(self.airfoilPositions)

        if self.fuselageIsPresent():
            # do not interpolate fuselage section
            startIdx = 1
            new_positions.append(self.airfoilPositions[0])
            new_chords.append(self.chords[0])
            new_airfoilNames.append(self.airfoilNames[0])
            new_airfoilTypes.append(self.airfoilTypes[0])
            # assinging flapGroup of fuselage not necessary, will be done
            # in assignFlapGroups!
        else:
            startIdx = 0

        for idx in range(startIdx, num-1):
            # determine interpolation-distance
            posDelta = self.airfoilPositions[idx+1]-self.airfoilPositions[idx]
            posDelta = posDelta / float(self.interpolationSegments+1)

            # add existiong position and name
            new_positions.append(self.airfoilPositions[idx])
            new_chords.append(self.chords[idx])
            new_airfoilNames.append(self.airfoilNames[idx])
            new_airfoilTypes.append(self.airfoilTypes[idx])
            new_flapGroups.append(self.flapGroups[idx])

            # add interpolated position and name
            for n in range(self.interpolationSegments):
                position = self.airfoilPositions[idx] + (float(n+1)*posDelta)
                chord = self.get_chordFromPosition(position)
                new_positions.append(position)
                new_chords.append(chord)
                new_airfoilNames.append(self.airfoilNames[idx])
                new_flapGroups.append(self.flapGroups[idx])
                new_airfoilTypes.append("blend")

        # set Tip values
        new_positions.append(self.airfoilPositions[-1])
        new_chords.append(self.chords[-1])
        new_airfoilNames.append(self.airfoilNames[-1])
        new_airfoilTypes.append(self.airfoilTypes[-1])
        # assigning of flapGroup for tip not  not necessary, will be done in
        # assignFlapGroups!

        # assign interpolated lists
        self.airfoilPositions = new_positions
        self.airfoilTypes = new_airfoilTypes
        self.airfoilNames = new_airfoilNames
        self.chords = new_chords
        self.flapGroups = new_flapGroups

        # calculate the interpolated sections
        self.calculate_sections()

        # do not forget: assign the flap groups to the different sections
        self.assignFlapGroups()

    # assigns the user defined flap groups to the different sections
    def assignFlapGroups(self):
        if self.fuselageIsPresent():
            # add flapGroup 0 at the beginning of the list
            self.flapGroups.insert(0,0)

        # append flapGroup for the tip section, which is the same as for the section before
        self.flapGroups.append(self.flapGroups[-1])

        # assign flap groups now
        for idx in range (len(self.sections)):
            self.sections[idx].flapGroup = self.flapGroups[idx]

    # get color for plotting
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
        areaCenterLine = []

        # setup empty lists for new tick locations
        x_tick_locations = []
        y_tick_locations = [self.rootchord]

        # set axes and labels
        self.set_AxesAndLabels(ax, "Half-wing planform")

        # plot sections in reverse order
        idx = len(self.airfoilTypes) - 1

        # check if there is a fuselage section
        if self.fuselageIsPresent():
            # skip fuselage section
            sectionsList = self.sections[1:]
        else:
            # plot all sections
            sectionsList = self.sections

        for element in reversed(sectionsList):
            # determine type of airfoil of this section
            try:
                airfoilType = self.airfoilTypes[idx]
            except:
                airfoilType = 'blend'

            labelColor = self.get_colorFromAirfoilType(airfoilType)

            ax.plot([element.y, element.y] ,[element.leadingEdge, element.trailingEdge],
            color=labelColor, linestyle = ls_sections, linewidth = lw_sections,
            solid_capstyle="round")

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
            xytext=(2, 5), textcoords='offset points', color = cl_chordlengths,
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
            areaCenterLine.append(element.areaCenterLine)
            hingeLine.append(element.hingeLine)
            trailingeEge.append(element.trailingEdge)

        # setup root- and tip-joint
        trailingeEge[0] = leadingEdge[0]
        trailingeEge[-1] = leadingEdge[-1]

        # compose labels for legend
        labelHingeLine = ("hinge line (%.1f %% / %.1f %%)" %
                           (self.hingeDepthRoot, self.hingeDepthTip))

        # plot quarter-chord-line
        if (self.showQuarterChordLine == True):
            ax.plot(xValues, quarterChordLine, color=cl_quarterChordLine,
              linestyle = ls_quarterChordLine, linewidth = lw_quarterChordLine,
              solid_capstyle="round", label = "quarter-chord line")

        if (self.showTipLine == True):
            ax.plot(xValues, areaCenterLine, color=cl_areaCenterLine,
              linestyle = ls_areaCenterLine, linewidth = lw_areaCenterLine,
              solid_capstyle="round", label = "area CoG line")

        # plot hinge-line
        if (self.showHingeLine == True):
            ax.plot(xValues, hingeLine, color=cl_hingeLine,
              linestyle = ls_hingeLine, linewidth = lw_hingeLine,
              solid_capstyle="round", label = labelHingeLine)

        # plot the planform last
        ax.plot(xValues, leadingEdge, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")
        ax.plot(xValues, trailingeEge, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")
        try:
            airfoilType = self.airfoilTypes[0]
        except:
             airfoilType = 'blend'

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
        xValuesLeft = []
        xValuesRight = []
        leadingEdge = []
        trailingEdge = []
        hingeLine = []
        leadingEdgeLeft = []
        trailingEdgeLeft = []
        hingeLineLeft = []
        leadingEdgeRight = []
        trailingEdgeRight = []
        hingeLineRight = []

        # determine factor for projection of the wing according the dihedral
        proj_fact = cos(self.dihedral*pi/180.0)

        # Halfwingspan already without fuselage
        proj_halfwingSpan = self.halfwingspan * proj_fact

        # Caution, fuselageWidth does not change due to projection
        proj_wingspan = (2*proj_halfwingSpan) + self.fuselageWidth

        # set axes and labels
        self.set_AxesAndLabels(ax, "Full-wing planform")

        # build up list of x-values,
        # first left half-wing
        for element in self.grid:
            proj_y = (element.y- (self.fuselageWidth/2)) * proj_fact
            xVal = proj_y#-(self.fuselageWidth/2)
            #xValues.append(xVal)
            xValuesLeft.append(xVal)

        # offset for beginning of right half-wing
        xOffset = proj_halfwingSpan + self.fuselageWidth

        # center-section / fuselage (x)
        Left_x = xValuesLeft[-1]
        Right_x = Left_x + self.fuselageWidth
        leftWingRoot_x = (Left_x, Left_x)
        rightWingRoot_x = (Right_x, Right_x)

        # right half-wing (=reversed right-half-wing)
        for element in self.grid:
            proj_y = (element.y - (self.fuselageWidth/2)) * proj_fact
            xVal = proj_y + xOffset
            xValues.append(xVal)
            xValuesRight.append(xVal)

        # build up lists of y-values
        # left half wing
        for element in reversed(self.grid):
            leadingEdgeLeft.append(element.leadingEdge)
            hingeLineLeft.append(element.hingeLine)
            trailingEdgeLeft.append(element.trailingEdge)

        # center-section / fuselage (y)
        #wingRoot_y = (leadingEdgeLeft[lastElement],trailingEdgeLeft[lastElement])
        wingRoot_y = (leadingEdgeLeft[-1],trailingEdgeLeft[-1])

        # right half wing
        for element in (self.grid):
            leadingEdgeRight.append(element.leadingEdge)
            hingeLineRight.append(element.hingeLine)
            trailingEdgeRight.append(element.trailingEdge)

        # setup root- and tip-joint
        trailingEdgeLeft[0] = leadingEdgeLeft[0]
        trailingEdgeRight[-1] = leadingEdgeRight[-1]

        # get flap parting lines
        (flapPositions_x, flapPositions_y) = self.getFlapPartingLines()

        # plot the flap parting lines
        numLines = len(flapPositions_x)
        for idx in range(numLines):
            ax.plot(flapPositions_x[idx], flapPositions_y[idx],
            color=cl_hingeLine, linewidth = lw_hingeLine, solid_capstyle="round")

        # plot the hingeline
        ax.plot(xValuesLeft, hingeLineLeft, color=cl_hingeLine,
                linewidth = lw_hingeLine, solid_capstyle="round")
        ax.plot(xValuesRight, hingeLineRight, color=cl_hingeLine,
                linewidth = lw_hingeLine, solid_capstyle="round")

        # plot the planform last
        ax.plot(xValuesLeft, leadingEdgeLeft, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")
        ax.plot(xValuesRight, leadingEdgeRight, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")

        ax.plot(xValuesLeft, trailingEdgeLeft, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")
        ax.plot(xValuesRight, trailingEdgeRight, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")

        # center-section
        ax.plot(leftWingRoot_x, wingRoot_y, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")

        ax.arrow(proj_wingspan/2, 0.0, 0.0, -1*(self.rootchord/3),head_width=self.fuselageWidth/4)

        ax.plot(rightWingRoot_x, wingRoot_y, color=cl_planform,
                linewidth = lw_planform, solid_capstyle="round")

        # both axes shall be equal
        ax.axis('equal')

        # revert y-axis on demand
        if (self.leadingEdgeOrientation == 'up'):
            ax.set_ylim(ax.get_ylim()[::-1])

        # plot hinge-line
##        if (self.showHingeLine == 'true'):
##            ax.plot(xValuesLeft, hingeLine, color=cl_hingeLine,
##              linestyle = ls_hingeLine, linewidth = lw_hingeLine,
##              solid_capstyle="round")

        # fill the wing
        ax.fill_between(xValuesLeft, leadingEdgeLeft, hingeLineLeft, color=cl_planformFill, alpha=0.4)
        ax.fill_between(xValuesLeft, hingeLineLeft, trailingEdgeLeft, color=cl_hingeLineFill, alpha=0.4)
        ax.fill_between(xValuesRight, leadingEdgeRight, hingeLineRight, color=cl_planformFill, alpha=0.4)
        ax.fill_between(xValuesRight, hingeLineRight, trailingEdgeRight, color=cl_hingeLineFill, alpha=0.4)

        # setup list for new x-tick locations
        new_tick_locations = [0.0, proj_halfwingSpan, (proj_halfwingSpan + self.fuselageWidth/2),
                             (proj_halfwingSpan + self.fuselageWidth), proj_wingspan]

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


    def draw_NormalizedChordDistribution(self):
        # set background style
        plt.style.use(cl_background)

        # customize grid
        plt.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)

        normalizedHalfwingspan = []
        normalizedChord = []
        referenceChord = []
        hinge = []

        for element in self.normalizedGrid:
            normalizedHalfwingspan.append(element.y)
            normalizedChord.append(element.chord)
            referenceChord.append(element.referenceChord)
            #hinge.append(element.hinge)

        # set axes and labels
        #self.set_AxesAndLabels(ax, "Half-wing planform")


        plt.plot(normalizedHalfwingspan, normalizedChord, color=cl_normalizedChord,
                linewidth = lw_planform, solid_capstyle="round",
                label = "normalized chord")

        plt.plot(normalizedHalfwingspan, referenceChord, color=cl_referenceChord,
                linewidth = lw_planform, solid_capstyle="round",
                label = "pure ellipse")

##        plt.plot(normalizedHalfwingspan, hinge, color=cl_hingeLine,
##                linewidth = lw_planform, solid_capstyle="round",
##                label = "hinge line")

        if self.planformShape =='curve_fitting':
            plt.scatter(self.planform_chord, self.planform_y, color=cl_normalizedChord,
            label = "planform points points")

##            if self.use_bezier:FIXME BEZIER
##                 plt.scatter(self.bezier_x, self.bezier_y, color=cl_controlPoints,
##                 label = "control points")

        plt.title("Normalized chord distribution")
        plt.legend(loc='lower right', fontsize = fs_legend)

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


    def draw_LE_derivative(self):
        # set background style
        plt.style.use(cl_background)

        # customize grid
        plt.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)

        x = []
        y = []

        for element in self.grid:
            x.append(element.y)
            if (element.LE_derivative > 0.0):
                y.append(element.LE_derivative)
            else:
                y.append(0.0)


        plt.plot(x, y, color=cl_normalizedChord,
                linewidth = lw_planform, solid_capstyle="round",
                label = "LE derivative")

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


    # draw the graph
    def draw(self):
        # set background style
        plt.style.use(cl_background)

        # setup subplots
        fig, (upper,lower) = plt.subplots(2,1)

        # compose diagram-title
        wingspan_mm = int(round(self.wingspan*1000))
        rootchord_mm = int(round(self.rootchord*1000))
        proj_area = self.area * cos(self.dihedral*pi/180.0)

        text = "\"%s\"\n wingspan: %d mm, rootchord: %d mm, area: %.2f dm², proj. area: %.2f dm²\n"\
         "aspect ratio: %.2f, root-tip sweep: %.2f°, hinge line angle: %.2f°\n"\
         % (self.planformName, wingspan_mm, rootchord_mm, self.area, proj_area,
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
            # convert string to boolean value
            if (XMLwingFinSwitch.text == 'true') or (XMLwingFinSwitch.text == 'True'):
                value = True
            else:
                value = False

            # check against value of wingFinswitch
            if (value == wingFinSwitch):
                return wing


# insert the planform-data into XFLR5-xml-file
def insert_PlanformDataIntoXFLR5_File(data, inFileName, outFileName):

    # basically parse the XML-file
    tree = ET.parse(inFileName)

    # get root of XML-tree
    root = tree.getroot()

    # find wing-data
    wing = get_wing(root, data.isFin)

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
            foilName.text = remove_suffix(str(section.airfoilName), '.dat')

        for foilName in newSection.iter('Right_Side_FoilName'):
            # convert float to text
            foilName.text = remove_suffix(str(section.airfoilName), '.dat')

        # add the new section to the tree
        wing.append(newSection)
        hingeDepthPercent = (section.hingeDepth /section.chord )*100
        NoteMsg("Section %d: position: %.0f mm, chordlength %.0f mm, hingeDepth %.1f  %%, airfoilName %s was inserted" %
          (section.number, section.y*1000, section.chord*1000, hingeDepthPercent, section.airfoilName))

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

    if wingData.fuselageIsPresent():
        userAirfoils = wingData.userAirfoils[1:]
        userAirfoil_idx = 1
    else:
        userAirfoils = wingData.userAirfoils
        userAirfoil_idx = 0


    for airfoil in userAirfoils:
        splitnames = airfoil.split("\\")
        airfoilName = splitnames[-1]
        airfoilName = remove_suffix(airfoilName, ".dat")

        if (splitnames[0]=='.'):
            srcPath = "..\\..\\" + '\\'.join(splitnames[1:-1])
        else:
            srcPath = '\\'.join(splitnames[0:-1])

        destName = wingData.get_UserAirfoilName(userAirfoil_idx)
        destName = remove_suffix(destName, ".dat")

        copyAndSmooth_Airfoil(xfoilWorkerCall, inputFilename, airfoilName,
                              srcPath, destName, wingData.smoothUserAirfoils)
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
                worker_call = xfoilWorkerCall + " -w blend %d -a %s -a2 %s -o %s"\
                        % (blend, leftFoilName, rightFoilName, blendFoilName)
                print(worker_call) #Debug

                # call worker now by system call
                os.system(worker_call)
            else:
                NoteMsg("at least one airfoil for blending does not exist,"\
                 "skipping blending for airfoil %s" % blendFoilName)


def update_seedfoilName(wingData, strakdata):
    seedFoilName = wingData.airfoilNames[0]

    # set the new seedfoilname
    strakdata["seedFoilName"] = airfoilPath + bs + remove_suffix(seedFoilName, ".dat")


def update_airfoilNames(wingData, strakdata):
    # all airfoil without tip-airfoil
    num = len(wingData.airfoilTypes) - 1

    airfoilNames = []

    # first append name of the seedfoil
    foilName = remove_suffix(wingData.airfoilNames[0], ".dat")
    airfoilNames.append(foilName)

    # create list of airfoilnames that shall be created by the strak-machine
    for idx in range(num):
        if (wingData.airfoilTypes[idx] == "opt"):
            foilName = remove_suffix(wingData.airfoilNames[idx], ".dat")
            airfoilNames.append(foilName)

    # now set the new list in the strakdata-dictionary
    strakdata["airfoilNames"] = airfoilNames


def update_reynolds(wingData, strakdata):
    # all airfoil without tip-airfoil
    num = len(wingData.airfoilTypes) -1
    reynolds = []

    # first append reynolds-number of the seedfoil
    reynolds.append(wingData.airfoilReynolds[0])

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


# generates batchfile for polar-creation
def generate_polarCreationFile(wingData):
    # check, if polar- Re-numbers defined, determine number of polars for
    # each airfoil
    numPolars = len(wingData.polarReynolds)
    if numPolars == 0:
        NoteMsg("no reynolds numbers for polar creation defined, skipping"\
        " generation of polar creation file")
        return

    # set NCrit in T1-polar creation input-file
    set_NCrit(wingData.NCrit, T1_polarInputFile)

    # set name of polar creation file
    if (wingData.isFin):
        polarCreationFileName = "create_tail_polars.bat"
    else:
        polarCreationFileName = "create_wing_polars.bat"

    File = open(polarCreationFileName, 'w+')
    File.write("cd .\\build\n")

    # set list of re-numbers and airfoil names
    if wingData.fuselageIsPresent():
        airfoilNames = wingData.airfoilNames[1:-1]
        airfoilReynolds = wingData.airfoilReynolds[1:-1]
    else:
        airfoilNames = wingData.airfoilNames[:-1]
        airfoilReynolds = wingData.airfoilReynolds[:-1]

    # determine number of airfoils
    numAirfoils = len(airfoilNames)

    # determine Re-number or Resqrt(Cl) of root airfoil
    rootAirfoilRe = airfoilReynolds[0]

    # loop over all airfoils
    for idx in range(numAirfoils):
        airfoilName = remove_suffix(airfoilNames[idx], '.dat')
        airfoilRe = airfoilReynolds[idx]

        # determine factor, ratio of current airfoil-Re to rootAirfoil-Re
        ReFactor = airfoilRe / rootAirfoilRe
        ReList = []

        # build up List of Re-numbers for the airfoil
        for element in wingData.polarReynolds:
            ReList.append(element * ReFactor)

        # open File for polar creation for this airfoil
        AirfoilPolarCreationFileName = "create_%s_polars.bat" % airfoilName
        AirfoilPolarCreationFile = open((".\\build\\%s" %AirfoilPolarCreationFileName), 'w+')

        # generate lines of the batchfile for this airfoil, write to file
        for Re in ReList:
            workerCall = "echo y | ..\\bin\\xfoil_worker.exe -i \"..\\ressources\\i"\
            "Polars_T1.txt\" -a \".\\airfoils\%s.dat\" -w polar -o \"%s\" -r %d\n" \
            %(airfoilName, airfoilName, Re)

            # write worker call to batchfile
            AirfoilPolarCreationFile.write(workerCall)

        # append small timeout and close the window
        AirfoilPolarCreationFile.write("timeout 5 >nul\n")
        AirfoilPolarCreationFile.write("exit\n")

        # finished polar cration for this airfoil
        AirfoilPolarCreationFile.close()

        # append entry to main polar cration file
        File.write("Start %s\n" % AirfoilPolarCreationFileName)

    # change back directory
    File.write("cd ..\n")
    File.close()
    NoteMsg("polar creation file \"%s\" was successfully generated"\
             % polarCreationFileName)



def set_NCrit(NCrit, filename):
        fileNameAndPath = ressourcesPath + bs + filename

        # read namelist form file
        dictData = f90nml.read(fileNameAndPath)

        # change ncrit in namelist / dictionary
        xfoil_run_options = dictData["xfoil_run_options"]
        xfoil_run_options['ncrit'] = NCrit

        # delete file and writeback namelist
        os.remove(fileNameAndPath)
        f90nml.write(dictData, fileNameAndPath)


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
    except ValueError as e:
        ErrorMsg('invalid json: %s' % e)
        ErrorMsg('failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
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

    # get command-line-arguments or user-input
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
    except ValueError as e:
        ErrorMsg('invalid json: %s' % e)
        ErrorMsg('Error, failed to read data from file %s' % planformDataFileName)
        planform.close()
        exit(-1)

    # set data for the planform
    newWing.set_Data(planformData)

    # before calculating the planform with absolute numbers,
    # calculate normalized chord distribution
    newWing.calculate_normalizedChordDistribution()
    #newWing.draw_NormalizedChordDistribution()#FIXME

    # denormalize position / calculate absolute numbers
    newWing.denormalize_positions()

    # calculate the grid, the chordlengths of the airfoils and the sections
    newWing.calculate_planform()
    newWing.calculate_ReNumbers()
    newWing.calculate_chordlengths()
    newWing.calculate_positions() # must be done after chordlenghts ar known
    newWing.set_AirfoilNames()

    # if there is a fuselage, insert data for the fuselage section
    # at the beginning of the list.
    if newWing.fuselageIsPresent():
        newWing.insert_fuselageData()

    # always insert data for the wing tip
    newWing.insert_tipData()

    # calculate the sections now
    newWing.calculate_sections()

    # assign the flap groups to the different sections
    newWing.assignFlapGroups()

    # create outputfolder, if it does not exist
    if not os.path.exists(outputFolder):
        os.makedirs(outputFolder)

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

    # interpolation of sections, make complete 1:1 copy first
    # in the drawing we only want to see the non-interpolated sections
    interpolatedWing = deepcopy(newWing)
    interpolatedWing.interpolate_sections()

    # insert the generated-data into the XFLR5 File (interpolated data)
    try:
        # get filename of XFLR5-File
        XFLR5_inFileName =  planformData["XFLR5_inFileName"]
        if (XFLR5_inFileName.find(bs) < 0):
            XFLR5_inFileName = ressourcesPath + bs + XFLR5_inFileName

        # compose output-filename for planform-xml-file
        XFLR5_outFileName =  planformData["XFLR5_outFileName"]
        if (XFLR5_outFileName.find(bs) < 0):
            XFLR5_outFileName = outputFolder + bs + XFLR5_outFileName


        if ((XFLR5_inFileName != None) and (XFLR5_outFileName != None)):
            insert_PlanformDataIntoXFLR5_File(interpolatedWing, XFLR5_inFileName, XFLR5_outFileName)
            NoteMsg("XFLR5 file \"%s\" was successfully created" % XFLR5_outFileName)
    except:
        NoteMsg("creating XFLR5 file was skipped")

    # insert the generated-data into the FLZ-Vortex File (interpolated data)
    try:
        FLZ_inFileName  = planformData["FLZVortex_inFileName"]
        if (FLZ_inFileName.find(bs) < 0):
            FLZ_inFileName = ressourcesPath + bs + FLZ_inFileName

        FLZ_outFileName = planformData["FLZVortex_outFileName"]
        if (FLZ_outFileName.find(bs) < 0):
            FLZ_outFileName = outputFolder + bs + FLZ_outFileName

        export_toFLZ(interpolatedWing, FLZ_inFileName, FLZ_outFileName)
        NoteMsg("FLZ vortex file \"%s\" was successfully created" % FLZ_outFileName)
    except:
        NoteMsg("creating FLZ vortex file was skipped")

    # generate batchfile for polar creation (non interpolated data)
    generate_polarCreationFile(newWing)

    # set colours according to selected theme
    newWing.set_colours()

    # plot the normalized chord distribution
    newWing.draw_NormalizedChordDistribution()

    # plot the planform
    newWing.draw()
