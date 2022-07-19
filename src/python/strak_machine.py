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

# imports
import xml.etree.ElementTree as ET
import argparse
import sys
import json
from os import listdir, path, system, makedirs, chdir, getcwd, remove
from os.path import exists
from matplotlib import pyplot as plt
from matplotlib import image as mpimg
from math import pi, sin
import numpy as np
import f90nml
from copy import deepcopy
from colorama import init
from termcolor import colored
import change_airfoilname
import re
import importlib
visualizer = importlib.import_module("xoptfoil_visualizer-jx")

# paths and separators
bs = "\\"
ressourcesPath = 'ressources'
buildPath = 'build'
airfoilPath = 'airfoils'
scriptPath = 'scripts'
exePath = 'bin'

# fixed filenames
pythonInterpreterName = "python"
strakMachineName = "strak_machine"
xfoilWorkerName = "xfoil_worker"
xoptfoilName = "xoptfoil-jx"
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"
airfoilComparisonName = "best_airfoil"
showStatusName = "show_status"
strakMachineInputFileName = 'strakdata.txt'
T1_polarInputFile = 'iPolars_T1.txt'
T2_polarInputFile = 'iPolars_T2.txt'
smoothInputFile = 'iSmooth.txt'
DesignCoordinatesName = 'Design_Coordinates.dat'
AssessmentResultsName = 'Assessment_Results.dat'

# filename of progress-file
progressFileName = "progress.txt"

# fonts
csfont = {'fontname':'Segoe Print'}

# number of decimals in the generated input-files
CL_decimals = 5 # lift
CD_decimals = 6 # drag
CL_CD_decimals = 2 # lift/drag
AL_decimals = 5 # alpha
PER_decimals = 6

# decimals for camber and thickness etc.
camb_decimals = 2
thick_decimals = 2

# fontsizes and linewidths
fs_infotext = 9
fs_legend = 8
fs_axes = 20
fs_ticks = 10
fs_weightings = 6
lw_targetPolar = 0.6
lw_referencePolar  = 0.4
ms_oppoint = 7
ms_target = 5
scaled = False

# colours
cl_infotext = 'DeepSkyBlue'
cl_polar_change = 'orange'
cl_T1_polar = 'g'
cl_T2_polar = 'b'

# styles
opt_point_style_root = 'y.'
opt_point_style_strak = 'y-'
opt_point_style_reference = 'r-'
ls_targetPolar = 'solid'
ls_referencePolar = 'dashdot'


# types of diagrams
diagTypes = "CL_CD_diagram", "CL_alpha_diagram", "CLCD_CL_diagram"

# disables all print output to console
print_disabled = False

# default values
NCrit_Default = 9.0

def my_print(message):
    if print_disabled:
        return
    else:
       print(message)


################################################################################
#
# helper functions to put colored messages
#
################################################################################
def ErrorMsg(message):
    my_print(colored('Error: ', 'red') + message)

def WarningMsg(message):
    my_print(colored('Warning: ', 'yellow') + message)

def NoteMsg(message):
    my_print(colored('Note: ', 'cyan') + message)

def DoneMsg():
    my_print("Done.\n")


################################################################################
#
# more helper functions
#
################################################################################

# function that rounds Re and returns a rounded decimal number
def round_Re(Re):
    floatRe = Re/1000.0
    decRe = round(floatRe, 0)
    return int(decRe)


# transform reynolds-number into a string e.g. Re = 123500 -> string = 124k
def get_ReString(Re):
    return ("%03dk" % round_Re(Re))


# helper-function to perform a linear-interpolation
def interpolate(x1, x2, y1, y2, x):
    try:
        y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
    except:
        ErrorMsg("Division by zero, x1:%f, x2:%f", (x1, x2))
        y = 0.0
    return y


def interpolate_2(x1, x2, y1, y2, y):
    try:
        x = (y - y1)/((y2-y1)/(x2-x1)) + x1
    except:
        ErrorMsg("Division by zero!")
        x = 0.0
    return x


def remove_suffix(text, suffix):
    try:
        text = re.sub(suffix, '', text)
    except:
        ErrorMsg("remove_suffix failed, text was %s, suffix was %s" % (text, suffix))
    return text

# get the name and absolute path of an template xoptfoil-input-file, that
# resides in the 'presets'-folder.
def get_PresetInputFileName(xoptfoilTemplate):
    searchPaths = []
    searchPaths.append(".." + bs + ressourcesPath)

    for path in searchPaths:
        try:
            fileList = get_ListOfFiles(path)

            # search the whole list of files for the desired template-file
            for name in fileList:
                if name.find(xoptfoilTemplate) >= 0:
                    return name
        except:
            NoteMsg("xoptfoil-template-file not found, tying different directory for"\
            " searching xoptfoil-template-files")

    ErrorMsg("could not find xoptfoil-template-file %s" % xoptfoilTemplate)
    sys.exit(-1)



################################################################################
#
# inputfile class
#
################################################################################
class inputFile:
    def __init__(self, params):
        self.values = {}
        self.presetInputFileName = ""
        self.idx_CL0 = 0
        self.idx_maxSpeed = 0
        self.idx_maxGlide = 0
        self.idx_preClmax = 0
        self.idx_additionalOpPoints = []

        # get name and path of xoptfoil-inputfile
        self.presetInputFileName = get_PresetInputFileName(params.xoptfoilTemplate)

        # read input-file as a Fortan namelist
        self.values = f90nml.read(self.presetInputFileName)


    def __del__(self):
        class_name = self.__class__.__name__


    # writes contents to file, using f90nnml-parser
    def write_ToFile(self, fileName):
        # delete 'name'
        operatingConditions = self.values["operating_conditions"]
        operatingConditionsBackup = operatingConditions.copy()
        try:
            del(operatingConditions['name'])
        except:
            pass

        self.values["operating_conditions"] = operatingConditions

        # write to file
        my_print("writing input-file %s..." % fileName)
        f90nml.write(self.values, fileName, True)

        # restore 'name'
        self.values["operating_conditions"] = operatingConditionsBackup.copy()
        DoneMsg()


    # reads contents to file, using f90nnml-parser
    def read_FromFile(self, fileName):
        my_print("reading input-file %s..." % fileName)
        currentDir = getcwd()#FIXME Debug
        self.values = f90nml.read(fileName)


    # deletes all existing op-points in operating-conditions, but keeps
    # the keys of the dictionary (empty lists)
    def delete_AllOpPoints(self, operatingConditions):
        # clear operating conditions
        operatingConditions["name"] = []
        operatingConditions["op_mode"] = []
        operatingConditions["op_point"] = []
        operatingConditions["optimization_type"] = []
        operatingConditions["target_value"] = []
        operatingConditions["weighting"] = []
        operatingConditions["reynolds"] = []
        operatingConditions['noppoint'] = 0


    def change_TargetValue(self, keyName, targetValue):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        idx = 0
        for key in opPointNames:
            if key == keyName:
                # get type of op-point
                opPointType = operatingConditions['op_mode'][idx]

                # limit the number of decimals
                if (opPointType == 'spec-cl'):
                    # target-value is drag-value
                    targetValue = round(targetValue, CD_decimals)
                elif (opPointType == 'spec-al'):
                    # target-value is lift-value
                    targetValue = round(targetValue, CL_decimals)

                # change target value
                operatingConditions['target_value'][idx] = targetValue

                # write-back operatingConditions
                self.values["operating_conditions"] = operatingConditions
                return
            idx = idx + 1


    def change_OpPoint(self, keyName, op_point):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        idx = 0
        for key in opPointNames:
            if key == keyName:
                # get type of op-point
                opPointType = operatingConditions['op_mode'][idx]

                # limit the number of decimals
                if (opPointType == 'spec-cl'):
                    # opPoint-value is lift-value
                    op_point = round(op_point, CL_decimals)
                elif (opPointType == 'spec-al'):
                    # opPoint-value is alpha-value
                    op_point = round(op_point, AL_decimals)

                # change op_point
                operatingConditions['op_point'][idx] = op_point
                # write-back operatingConditions
                self.values["operating_conditions"] = operatingConditions
                return
            idx = idx + 1


    def change_Weighting(self, idx, new_weighting):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

         # set new weighting
        operatingConditions['weighting'][idx] = new_weighting


    def get_OperatingConditions(self):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        return operatingConditions


    def set_OperatingConditions(self, operatingConditions):
         # put operating-conditions into dictionary
        self.values["operating_conditions"] = operatingConditions


    def get_OpPoint(self, keyName):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        idx = 0
        for key in opPointNames:
            if key == keyName:
                # return op_point
                return operatingConditions['op_point'][idx]
            idx = idx + 1


    # get all targets, filtered by 'op_mode'
    def get_xyTargets(self, op_mode):
        x = []
        y = []

        # get operating-conditions from inputfile
        operatingConditions = self.get_OperatingConditions()
        target_values = operatingConditions["target_value"]
        op_points = operatingConditions["op_point"]
        op_modes =  operatingConditions["op_mode"]

        # get the number of op-points
        numOpPoints = len(op_points)

        for i in range(numOpPoints):
            # check if the oppoint has the requested op-mode
            if (op_modes[i] == op_mode):
                if (op_mode == 'spec-cl'):
                    # get CL, CD
                    x.append(target_values[i]) # CD
                    y.append(op_points[i])     # CL
                # check if the op-mode is 'spec-al'
                elif (op_modes[i] == 'spec-al'):
                    x.append(op_points[i])     # alpha
                    y.append(target_values[i]) # CL

        return (x, y)


    # get all weightings, filtered by 'op_mode'
    def get_weightings(self, op_mode):
        weightings = []

        # get operating-conditions from inputfile
        operatingConditions = self.get_OperatingConditions()
        op_modes =  operatingConditions["op_mode"]
        try:
            weightingList = operatingConditions["weighting"]
        except:
            # no weightings found
            pass

        # get the number of op-points
        numOpPoints = len(op_modes)

        for i in range(numOpPoints):
            # check if the oppoint has the requested op-mode
            if (op_modes[i] == op_mode):
                try:
                    weightings.append(weightingList[i])
                except:
                    # found no weighting for this oppoint
                    weightings.append(None)
                    pass

        return (weightings)


    # returns name and index of the last op-point of operating-conditions
    def get_LastOpPoint(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        numOpPoints = len(operatingConditions["op_point"])

        # return last opPoint
        name = operatingConditions["name"][numOpPoints-1]
        idx = numOpPoints-1
        return (name, idx)


    def get_TargetValue(self, keyName):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        idx = 0
        for key in opPointNames:
            if key == keyName:
                # return op_point
                return operatingConditions['target_value'][idx]
            idx = idx + 1


    # gets the type of an opPoint ('spec-cl' or 'spec-al')
    def get_OpPointType(self, keyName):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        idx = 0
        for key in opPointNames:
            if key == keyName:
                # return op_point-type (="op-mode")
                return operatingConditions['op_mode'][idx]
            idx = idx + 1


    def get_InitialPerturb(self):
        optimization_options = self.values["optimization_options"]
        return optimization_options['initial_perturb']


    def set_InitialPerturb(self, newValue):
        optimization_options = self.values["optimization_options"]
        optimization_options['initial_perturb'] = newValue


    def set_geometryTargets(self, newValues):
        (camber, thickness) = newValues
        try:
            geoTargets = self.values["geometry_targets"]
            geoTargets["target_geo"][0] = round(camber/100.0, 5)
            geoTargets["target_geo"][1] = round(thickness/100.0, 5)
        except:
            ErrorMsg("could not set geoTargets")


    def get_geometryTargets(self):
        try:
            geoTargets = self.values["geometry_targets"]
            camber = geoTargets["target_geo"][0] * 100.0
            thickness = geoTargets["target_geo"][1] * 100.0
        except:
            ErrorMsg("could not get geoTargets")
            camber = 0.0
            thickness = 0.0

        return (camber, thickness)


    def set_NCrit(self, newValue):
        try:
            # change ncrit in namelist / dictionary
            xfoil_run_options = self.values["xfoil_run_options"]
            xfoil_run_options['ncrit'] = newValue
        except:
            ErrorMsg("Unable to set NCrit in inputfile")


    def get_Ncrit(self):
        try:
            # get ncrit from namelist / dictionary
            xfoil_run_options = self.values["xfoil_run_options"]
            return xfoil_run_options['ncrit']
        except:
            ncrit
            ErrorMsg("Unable to get NCrit from inputfile, using default-value %f" % NCrit_Default)
            return NCrit_Default


    def set_reversals(self, reversals_top, reversals_bot):
        try:
            curvature = self.values['curvature']
        except:
            curvature = {}

        curvature['max_curv_reverse_top'] = reversals_top
        curvature['max_curv_reverse_bot'] = reversals_bot
        self.values['curvature'] = curvature


    def set_shape_functions (self, shape_functions):
        optimization_options = self.values["optimization_options"]
        optimization_options['shape_functions'] = shape_functions


    def set_maxIterations(self, newValue):
        particle_swarm_options = self.values["particle_swarm_options"]
        particle_swarm_options['pso_maxit'] = newValue


    def get_maxIterations(self):
        particle_swarm_options = self.values["particle_swarm_options"]
        return particle_swarm_options['pso_maxit']


    def calculate_InitialPerturb(self, Re, ReFactor):
        ReFactorList = [0.7, 0.5]
        perturbList = [0.0025, 0.0028]

        # limit to list boundaries
        if ReFactor < ReFactorList[0]:
            ReFactor = ReFactorList[0]
        elif ReFactor > ReFactorList[1]:
            ReFactor = ReFactorList[1]

        # calculate corresponding perturb according to Re-Factor
        perturb = interpolate(ReFactorList[0],ReFactorList[1],
                             perturbList[0],perturbList[1],ReFactor)

        return round(perturb, PER_decimals)


    def init_TargetValues(self, params, strakPolar):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]
        opModes = operatingConditions["op_mode"]
        optTypes = operatingConditions["optimization_type"]

        # init all target values with current value of strak polar
        for idx in range(len(opPoints)):
            if opModes[idx] == 'spec-cl':
                # opPoint is Cl value
                targetValues[idx] = strakPolar.find_CD_From_CL(opPoints[idx])
            elif opModes[idx] == 'spec-al':
                # opPoint is alpha value
                targetValues[idx] = strakPolar.find_CL_from_alpha(opPoints[idx])
            else:
                ErrMsg("unknown op_mode %s" % opModes[idx])


    # adapts 'reynolds'-value of all op-points, that are below a certain
    # CL-value. These op-points will be treated as "type1"-polar op-points.
    # All op-points above the CL-value will be treated as "type2"-polar
    # op-points.
    # "type2" op-points will have no 'reynolds' value, as the default-reSqrt(CL)
    # value for all "type2" op-points will be passed to xoptfoil via commandline
    def adapt_ReNumbers(self, polarData):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        reynolds = operatingConditions["reynolds"]
        op_points = operatingConditions["op_point"]
        op_modes = operatingConditions["op_mode"]

        # get number of op-points
        num = len(op_points)

       # walk through the opPoints
        for idx in range(num):
            if(op_modes[idx] == 'spec-cl'):
                # check the op-point-value
                CL = op_points[idx]
                # is the CL below the CL-switchpoint T1/T2-polar ?
                if (CL <= polarData.CL_merge):
                    # yes, adapt maxRe --> Type 1 oppoint
                    reynolds[idx] = int(polarData.maxRe)
                    my_print("adapted oppoint @ Cl = %0.3f, Type 1, Re = %d\n" % \
                          (CL, int(polarData.maxRe)))


    def find_ClosestClOpPoint(self, Cl):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        numOpPoints = len(operatingConditions["op_point"])
        name = None
        idx = -1

        for i in range(1, numOpPoints):
            value_left = operatingConditions["op_point"][i-1]
            value_right = operatingConditions["op_point"][i]
            name_left = operatingConditions["name"][i-1]
            name_right = operatingConditions["name"][i]

            if (Cl >= value_left) & (Cl <= value_right):
                # we found the correct interval. Which one is closer ?
                diff_To_left = Cl - value_left
                diff_To_right = value_right - Cl

                if (diff_To_left < diff_To_right):
                    return (name_left, i-1)
                else:
                    return (name_right, i)

        return (name, idx)


    # insert a new oppoint at the end of the list
    def add_Oppoint(self, name, op_mode, op_point, optimization_type,
                                            target_value, weighting, reynolds):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # append new oppoint
        operatingConditions["name"].append(name)
        operatingConditions["op_mode"].append(op_mode)
        operatingConditions["op_point"].append(op_point)
        operatingConditions["optimization_type"].append(optimization_type)
        operatingConditions["target_value"].append(target_value)
        operatingConditions["weighting"].append(weighting)
        operatingConditions["reynolds"].append(reynolds)
        operatingConditions['noppoint'] = operatingConditions['noppoint'] + 1

    # TODO insert 'spec-al' op-point
    # insert a new oppoint in the list
    def insert_OpPoint(self, name, op_mode, op_point, optimization_type,
                                            target_value, weighting, reynolds):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # find index
        num_opPoints = len(operatingConditions["op_point"])

        # determine the kind of op-point to be inserted
        if (op_mode == 'spec-cl'):
            CL = op_point
        else:
            CL = target_value

        # search the list of op-points for CL
        for idx in range(num_opPoints):
            op_mode_list = operatingConditions["op_mode"][idx]
            op_point_list = operatingConditions["op_point"][idx]
            target_value_List = operatingConditions["target_value"][idx]

            if (op_mode_list== 'spec-cl'):
                CL_List = op_point_list
            else:
                CL_List = target_value_List

            # found the right place for insertion
            if (CL_List >= CL):
                # insert new oppoint now
                operatingConditions["name"].insert(idx, name)
                operatingConditions["op_mode"].insert(idx, op_mode)
                operatingConditions["op_point"].insert(idx, op_point)
                operatingConditions["optimization_type"].insert(idx, optimization_type)
                operatingConditions["target_value"].insert(idx, target_value)
                operatingConditions["weighting"].insert(idx, weighting)
                operatingConditions["reynolds"].insert(idx, reynolds)
                operatingConditions['noppoint'] = operatingConditions['noppoint'] + 1

                return idx


        return None


    def generate_OpPoints(self, numOpPoints, CL_min, CL_max):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # clear operating conditions
        self.delete_AllOpPoints(operatingConditions)

        # last oppoint will be spec-al-oppoint and will be inserted afterwards,
        # using another function (alpha_CL0)
        lastOpPoint = numOpPoints-1

        # calculate the intervall
        diff = (CL_max - CL_min) / (lastOpPoint-1)

        # always start at CL_min for first opPoint
        op_point = CL_min
        op_mode = 'spec-cl'
        optimization_type = 'target-drag'
        target_value = 0.0
        weighting = None #1.0
        reynolds = None

        # now build up new opPoints
        for i in range(lastOpPoint):
            # set generic op-point-name
            name = "op_%s" % i

            # round opPoint
            op_point_value = round(op_point, CL_decimals)

            # add new opPoint to dictionary
            self.add_Oppoint(name, op_mode, op_point_value, optimization_type,
                                            target_value, weighting, reynolds)
            # increment op-point
            op_point = op_point + diff


    # Inserts additional op-points, that are passed by a list, into
    # operating-conditions.
    # The idx-values of fixed op-points will be corrected, if necessary
    def insert_AdditionalOpPoints(self, opPoints):
        if len(opPoints) == 0:
            # nothing to do
            return

        num = 0
        #self.my_print_OpPoints()#Debug

        for opPoint in opPoints:

            # compose name
            name = "add_op_%s" % num

            # insert new op-Point, get index
            idx = self.insert_OpPoint(name, 'spec-cl', opPoint, 'target-drag',
                                     0.0, None, None)

            # correct idx of main op-points
            if (idx <= self.idx_CL0):
                self.idx_CL0 = self.idx_CL0 + 1

            if (idx <= self.idx_maxSpeed):
                self.idx_maxSpeed = self.idx_maxSpeed + 1

            if (idx <= self.idx_preMaxSpeed):
                self.idx_preMaxSpeed = self.idx_preMaxSpeed + 1

            if (idx <= self.idx_maxGlide):
                self.idx_maxGlide = self.idx_maxGlide + 1

            if (idx <= self.idx_preClmax):
                self.idx_preClmax = self.idx_preClmax + 1

            # append idx to list of additional op-points
            self.idx_additionalOpPoints.append(idx)
            num = num + 1

        #self.my_print_OpPoints()#Debug

    def insert_alpha0_oppoint(self, params, strakPolar, i):
        # get maxRe
        maxRe = params.maxReNumbers[i]

        # get alpha0 - target
        alpha = round(params.targets["alpha0"][i], AL_decimals)

        # insert op-Point, get index
        idx = self.insert_OpPoint('alpha0', 'spec-al', alpha, 'target-lift',
                                     0.0, params.weight_spec_al, maxRe)


    def insert_alphaMaxGlide_oppoint(self, params, i):
        # get maxRe
        maxRe = params.maxReNumbers[i]
        rootPolar = params.merged_polars[0]

        # get alpha-maxGlide - target
        alpha = round(rootPolar.alpha_maxGlide, AL_decimals)
        CL = round(rootPolar.CL_maxGlide, CL_decimals)

        # insert op-Point, get index
        idx = self.insert_OpPoint('alphaMaxGlide', 'spec-al', alpha, 'target-lift',
                                     CL, params.weight_spec_al, None)

    def insert_alphaMaxLift_oppoint(self, params, i):
        # get maxRe
        maxRe = params.maxReNumbers[i]
        rootPolar = params.merged_polars[0]

        # get alpha-maxGlide - target
        alpha = round(params.targets["alpha_pre_maxLift"][i], AL_decimals)
        CL = round(params.targets["CL_pre_maxLift"][i], CL_decimals)

        # insert op-Point, get index
        idx = self.insert_OpPoint('alphaMaxLift', 'spec-al', alpha, 'target-lift',
                                     CL, params.weight_spec_al, None)


    # All op-points between start and end shall be distributed equally.
    # Equally means: the difference in CL will be constant
    # "start" and "end" are both fixed op-points.
    def distribute_OpPointsEqually(self, start, end):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # get Cl-values of start and end
        Cl_start = operatingConditions["op_point"][start]
        Cl_end = operatingConditions["op_point"][end]

        # calculate the interval
        num_intervals = end - start

        if (num_intervals <= 1):
            # nothing to do, both points are fixed
            return

        Cl_interval = (Cl_end - Cl_start) / num_intervals
        #my_print(Cl_start, Cl_end, Cl_interval, num_intervals) Debug

        num = 1
        for idx in range(start+1, end):
            newValue = round(Cl_start + (num*Cl_interval), CL_decimals)
            operatingConditions["op_point"][idx] = newValue
            num = num + 1


    # distributes main-oppoints
    def distribute_MainOpPoints(self, targets, i):

        # get all op-points and target-values
        CD_min = targets["CD_min"][i]
        CL0 = targets["CL0"][i]
        CD0 = targets["CD0"][i]
        CL_maxSpeed = targets["CL_maxSpeed"][i]
        CD_maxSpeed = targets["CD_maxSpeed"][i]
        CL_preMaxSpeed = targets["CL_preMaxSpeed"][i]
        CD_preMaxSpeed = targets["CD_preMaxSpeed"][i]
        CL_maxGlide = targets["CL_maxGlide"][i]
        CD_maxGlide = targets["CD_maxGlide"][i]
        CL_pre_maxLift = targets["CL_pre_maxLift"][i]
        CD_pre_maxLift = targets["CD_pre_maxLift"][i]

        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]

        # get opPoint
        (opPoint_maxLift, self.idx_preClmax) = self.get_LastOpPoint()
        opPoint_preClmax = opPointNames[self.idx_preClmax]

        # get opPoint
        (opPoint_maxGlide, self.idx_maxGlide) =\
                            self.find_ClosestClOpPoint(CL_maxGlide)

        # correct oppoint, if necessary
        if (self.idx_maxGlide >= self.idx_preClmax):
            self.idx_maxGlide = self.idx_preClmax -1
            opPoint_maxGlide = opPointNames[self.idx_maxGlide]

        # get opPoint
        (opPoint_preMaxSpeed, self.idx_preMaxSpeed) =\
                                 self.find_ClosestClOpPoint(CL_preMaxSpeed)

        # correct oppoint, if necessary
        if (self.idx_preMaxSpeed >= self.idx_maxGlide):
            self.idx_preMaxSpeed = self.idx_maxGlide -1
            opPoint_preMaxSpeed = opPointNames[self.idx_preMaxSpeed]

        # get opPoint
        (opPoint_maxSpeed, self.idx_maxSpeed) =\
                                 self.find_ClosestClOpPoint(CL_maxSpeed)

        # correct oppoint, if necessary
        if (self.idx_maxSpeed >= self.idx_preMaxSpeed):
            self.idx_maxSpeed = self.idx_preMaxSpeed -1
            opPoint_maxSpeed = opPointNames[self.idx_maxSpeed]

        # change values
        self.change_OpPoint(opPoint_preClmax, CL_pre_maxLift)
        self.change_OpPoint(opPoint_maxGlide, CL_maxGlide)
        self.change_OpPoint(opPoint_preMaxSpeed, CL_preMaxSpeed)
        self.change_OpPoint(opPoint_maxSpeed, CL_maxSpeed)


        # set remaining target-values of main-op-points
        # target-value of CL_pre_maxLift will be set later
        self.change_TargetValue(operatingConditions["name"][0], CD_min)
        self.change_TargetValue(opPoint_maxGlide, CD_maxGlide)
        self.change_TargetValue(opPoint_preMaxSpeed, CD_preMaxSpeed)
        self.change_TargetValue(opPoint_maxSpeed, CD_maxSpeed)

        # change names
        opPointNames[self.idx_preClmax] = 'preClmax'
        opPointNames[self.idx_maxGlide] = 'maxGlide'
        opPointNames[self.idx_preMaxSpeed] = 'preMaxSpeed'
        opPointNames[self.idx_maxSpeed] = 'maxSpeed'

        # always insert CL0 as new oppoint
        self.idx_CL0 = self.insert_OpPoint('CL0', 'spec-cl', CL0, 'target-drag',
                                           CD0, None, None)
        # correct idx of main op-points
        if (self.idx_CL0 <= self.idx_maxSpeed):
            self.idx_maxSpeed = self.idx_maxSpeed + 1
            self.idx_preMaxSpeed = self.idx_preMaxSpeed + 1
            self.idx_maxGlide = self.idx_maxGlide + 1
            self.idx_preClmax = self.idx_preClmax + 1
        else:
            # check order of idx-values. idx of CL0 must not be > idx maxSpeed !
            ErrorMsg("idx_CL0 > idx_maxSpeed")
            Exit(-1)

        #my_print (opPointNames)
        #my_print (opPoints)
        #my_print(operatingConditions['target_value'])
        #my_print ("Ready.")#Debug


    # Distribute all intermediate-oppoints
    def distribute_IntermediateOpPoints(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # first generate a index (!) list of all fixed op-points
        fixed_opPoints = []
        fixed_opPoints.append(self.idx_CL0)
        fixed_opPoints.append(self.idx_maxSpeed)
        fixed_opPoints.append(self.idx_preMaxSpeed)
        fixed_opPoints.append(self.idx_maxGlide)
        fixed_opPoints.append(self.idx_preClmax)

        # append the index-values of additional op-points (defined by the user)
        # to the list of fixed op-points. After this the list of indices may be
        # unsorted.
        for idx in self.idx_additionalOpPoints:
            fixed_opPoints.append(idx)

        # now sort the list again
        fixed_opPoints.sort()
        #my_print (fixed_opPoints) Debug

        # now distribute the intermediate opPoints between the fixed opPoints
        # equally. Therefore set up an interval from one fixed op-point to the
        # next one. Every intermediate op-point between "start" and "end"
        # will get the same distance to the op-point before and the op-point
        # afterwards.
        for idx in range(len(fixed_opPoints)-1):
            start = fixed_opPoints[idx]
            end = fixed_opPoints[idx+1]
            self.distribute_OpPointsEqually(start, end)

################################################################################
#
# strakData class
#
################################################################################
class strak_machineParams:
    def __init__(self, fileName):
        self.buildDir = ''
        self.workingDir = ''
        self.quality = 'medium'
        self.batchfileName = 'make_strak.bat'
        self.xoptfoilTemplate = "iOpt"
        self.operatingMode = 'default'
        self.seedFoilName = ""
        self.ReSqrtCl = 150000
        self.NCrit = NCrit_Default
        self.numOpPoints = 17
        self.alphaMin = -8.0
        self.alphaMax = 16.0
        self.CL_min = -0.1
        self.CL_preMaxSpeed = 0.2
        self.CL_merge = 0.05
        self.maxReFactor = 15.0
        self.maxLiftDistance = 0.03
        self.alpha_Resolution = 0.001
        self.optimizationPasses = 2
        self.activeTargetPolarIdx = 1
        self.scaleFactor = 1.0
        self.weight_spec_al = None# 2.0
        self.generateBatch = True
        self.showTargetPolars = True
        self.adaptInitialPerturb = True
        self.smoothSeedfoil = True
        self.smoothStrakFoils = True
        self.showReferencePolars = True
        self.ReNumbers = []
        self.geoParams = None
        self.rootGeoParams = None
        self.additionalOpPoints = [[]]
        self.maxReNumbers = []
        self.polarFileNames = []
        self.polarFileNames_T1 = []
        self.polarFileNames_T2 = []
        self.inputFileNames = []
        self.merged_polars = []
        self.seedfoil_polars = []
        self.target_polars = []
        self.strak_polars = []
        self.inputFiles = []
        self.airfoilNames = []
        self.visibleFlags = [True, True, True, True, True, True, True, True, True, True, True, True]
        self.targets ={
                        "CL_min": [],
                        "CD_min": [],
                        "alpha_min": [],
                        "CL_maxSpeed": [],
                        "CD_maxSpeed": [],
                        "alpha_maxSpeed": [],
                        "CL_preMaxSpeed": [],
                        "CD_preMaxSpeed": [],
                        "alpha_preMaxSpeed": [],
                        "CL_maxGlide": [],
                        "CL_CD_maxGlide": [],
                        "CD_maxGlide": [],
                        "alpha_maxGlide": [],
                        "CL_pre_maxLift": [],
                        "CD_pre_maxLift": [],
                        "alpha_pre_maxLift": [],
                        "CL0": [],
                        "CD0": [],
                        "alpha0": [],
                       }


        # read json-dictionary from file containing necessary parameters
        self.fileContent = self.read_paramsFromFile(fileName)

        # store filename locally
        self.fileName = fileName

        # evaluate file content
        self.get_Parameters(self.fileContent)

        # calculate further values like max Re-numbers etc., also setup
        # calls of further tools like xoptfoil
        self.calculate_DependendValues()


    def read_paramsFromFile(self, fileName):
        cwd = getcwd()
        # try to open .json-file
        try:
            parameterFile = open(fileName)
        except:
            ErrorMsg('failed to open file %s' % fileName)
            sys.exit(-1)

        # load dictionary from .json-file
        try:
            fileContent = json.load(parameterFile)
            parameterFile.close()
        except ValueError as e:
            ErrorMsg('invalid json: %s' % e)
            ErrorMsg('failed to read data from file %s' % fileName)
            parameterFile.close()
            sys.exit(-1)

        return fileContent


    def get_geoParamsOfAirfoil(self, airfoilIdx, geoParams):
        # separate tuple of lists
        (thick, thickPos, camb, cambPos) = geoParams

        # return tuple of single values
        return (thick[airfoilIdx], thickPos[airfoilIdx],
                camb[airfoilIdx], cambPos[airfoilIdx])


    def read_geoParamsfromFile(self, airfoilIdx):
        # try to open .json-file
        cwd = getcwd()# FIXME Debug
        fileName = '..' + bs + self.fileName
        try:
            parameterFile = open(fileName)
        except:
            ErrorMsg('failed to open file %s' % fileName)
            return(-1)

        # load dictionary from .json-file
        try:
            fileContent = json.load(parameterFile)
            parameterFile.close()
        except:
            ErrorMsg('failed to read data from file %s' % fileName)
            parameterFile.close()
            return(-2)
        try:
            # read complete geo params of all airfoils
            all_GeoParams = self.get_geoParamsFromDict(fileContent)

            # extract geo params of single airfoil
            airfoil_GeoParams = self.get_geoParamsOfAirfoil(airfoilIdx, all_GeoParams)

            # set geo params of single airfoil in strak machine data structure
            self.set_geoParameters(airfoilIdx, airfoil_GeoParams)
        except:
            ErrorMsg('unable to set geo params')
            return(-3)
        return 0


    def write_geoParamsToFile(self, airfoilIdx):
        cwd = getcwd()# FIXME Debug
        fileName = '..' + bs + self.fileName

        # read actual fileContent of parameter file
        fileContent = self.read_paramsFromFile(fileName)

        # get actual geo params from filecontent
        (thick, thickPos, camb, cambPos) = self.get_geoParamsFromDict(fileContent)

        # get actual geoparams stored in strak machine parameters (Ram)
        (new_thick, new_thickPos, new_camb, new_cambPos) = self.geoParams

        # insert new values at the correct array-position
        thick[airfoilIdx] =    new_thick[airfoilIdx]
        thickPos[airfoilIdx] = new_thickPos[airfoilIdx]
        camb[airfoilIdx] =     new_camb[airfoilIdx]
        cambPos[airfoilIdx] =  new_cambPos[airfoilIdx]

        # writeback geo parameters to file content
        self.set_geoParamsInDict(fileContent, (thick, thickPos, camb, cambPos))

        # writeback parameter file
        result = self.write_paramsToFile(fileName, fileContent)
        return result


    def write_paramsToFile(self, fileName, fileContent):
        cwd = getcwd()# FIXME Debug
        try:
            parameterFile = open(fileName, 'w')
        except:
            ErrorMsg('failed to open file %s for writing' % fileName)
            return(-1)

        # writeback dictionary to .json-file
        try:
            json.dump(fileContent, parameterFile, indent=2, separators=(',', ':'))
            parameterFile.close()
            NoteMsg('%s was successfully written' % fileName)
        except:
            ErrorMsg('failed to write data to file %s' % fileName)
            parameterFile.close()
            return(-2)
        return 0



    ################################################################################
    # function that gets a single parameter from dictionary and returns a
    # default value in case of error
    def get_ParameterFromDict(self, dict, key, default):
        res = type(default) is tuple
        # set default-value first
        if (res):
            value = None
            for element in default:
                value = element
        else:
            value = default

        try:
            value = dict[key]
        except:
            NoteMsg('parameter \'%s\' not specified, using default-value \'%s\'' %\
                  (key, str(value)))
        return value


    def get_geoParamsFromDict(self, dict):
        # read all parameters from dictionary
        try:
            geoParams = dict["geoParams"]
            thickness = geoParams["thickness"]
            thicknessPosition = geoParams["thicknessPosition"]
            camber = geoParams["camber"]
            camberPosition = geoParams["camberPosition"]
        except:
            WarningMsg("unable to read geo parameters")
            return None

        # check array size against number of airfoils
        num = len(self.ReNumbers)
        if ((len(thickness) != num) or
            (len(thicknessPosition) != num) or
            (len(camber) != num) or
            (len(camberPosition) != num)):
                ErrorMsg("expected geo parameters for % airfoils, but" \
                 "parameters for %d airfoils were found" % (num, len(thickness)))
                return None

        my_print("geo parameters for %d airfoils were successfully read" % num)
        return (thickness, thicknessPosition, camber, camberPosition)


    def set_geoParamsInDict(self, dict, geoParams):
        new_geoParams = {}

        # unpack tuple
        (thick, thickPos, camb, cambPos) = geoParams

        # create dictionary entries
        new_geoParams["thickness"] = thick
        new_geoParams["thicknessPosition"] = thickPos
        new_geoParams["camber"] = camb
        new_geoParams["camberPosition"] = cambPos

        # put geoParams dictionary into main dictionary
        dict["geoParams"] = new_geoParams



    ################################################################################
    # function that gets a single boolean parameter from dictionary and returns a
    #  default value in case of error
    def get_booleanParameterFromDict(self, dict, key, default):
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
    def get_MandatoryParameterFromDict(self, dict, key):
        try:
            value = dict[key]
        except:
            ErrorMsg('parameter \'%s\' not specified, this key is mandatory!'% key)
            sys.exit(-1)
        return value


    ################################################################################
    # function that checks validity of the 'quality'-input
    def check_quality(self):
        if ((self.quality != 'default') and
            (self.quality != 'high')):

            WarningMsg('quality = \'%s\' is not valid, setting quality'\
            ' to \'default\'' % self.quality)
            self.quality = 'default'

        if self.quality == 'default':
            # single-pass optimization, hicks-henne
            self.maxIterations = [350]
            self.numberOfCompetitors = [1]
            self.shape_functions = ['hicks-henne']
        else:
            # double-pass optimization, hicks-henne
            self.maxIterations = [80, 300]
            self.numberOfCompetitors = [3, 1]
            self.shape_functions = ['hicks-henne','hicks-henne']

        self.optimizationPasses = len(self.maxIterations)


    ################################################################################
    # function that checks validity of the number of op-points
    def check_NumOpPoints(self):
        if (self.numOpPoints < 6):
            WarningMsg('numOpPoints must be >= 6, setting numOpPoints to minimum-value of 6')
            self.numOpPoints = 6


    ################################################################################
    # function that gets parameters from dictionary
    def get_Parameters(self, fileContent):
        NoteMsg("getting parameters..")

        # get mandatory parameters first
        self.quality = self.get_MandatoryParameterFromDict(fileContent, 'quality')
        self.seedFoilName = self.get_MandatoryParameterFromDict(fileContent, 'seedFoilName')
        self.ReNumbers = self.get_MandatoryParameterFromDict(fileContent, 'reynolds')
        self.airfoilNames = self.get_MandatoryParameterFromDict(fileContent, "airfoilNames")

        # get optional parameters
        self.maxReFactor = self.get_ParameterFromDict(fileContent, "maxReynoldsFactor",
                                                            self.maxReFactor)

        self.additionalOpPoints[0] = self.get_ParameterFromDict(fileContent, "additionalOpPoints",
                                                       self.additionalOpPoints[0])

        self.numOpPoints = self.get_ParameterFromDict(fileContent, "numOpPoints",
                                                   self.numOpPoints)

        self.NCrit = self.get_ParameterFromDict(fileContent, "NCrit", self.NCrit)

        self.smoothSeedfoil = self.get_booleanParameterFromDict(fileContent,
                                 "smoothSeedfoil", self.smoothSeedfoil)

        self.smoothStrakFoils = self.get_booleanParameterFromDict(fileContent,
                                 "smoothStrakFoils", self.smoothStrakFoils)

        # perform parameter-checks now
        my_print("checking validity of all parameters..")
        self.check_NumOpPoints()
        self.check_quality()
        DoneMsg()


    def read_geoParameters(self):
        NoteMsg("getting geo parameters..")
        # read root geo parameters first
        self.read_rootGeoParameters()

       # get geoParameters from dictionary
        self.geoParams = self.get_geoParamsFromDict(self.fileContent)
        if (self.geoParams == None):
            my_print("Setting default values for geo parameters.")
            # set geoParameters to default values
            self.set_defaultGeoParams(self.fileContent)
            result = self.write_paramsToFile('..' + bs + self.fileName, self.fileContent)
            if (result != 0):
                sys.exit(-1)

        DoneMsg()

    ############################################################################
    # function that returns a list of Re-numbers
    def get_ReList(self):
        return self.ReNumbers

    def get_visibleFlags(self):
        return self.visibleFlags

    def set_visibleFlags(self, flags):
        self.visibleFlags.clear()
        self.visibleFlags = flags

    def set_referenceFlag(self, flag):
        self.showReferencePolars = flag

    def set_activeTargetPolarIdx(self, idx):
        self.activeTargetPolarIdx = idx

    ############################################################################
    # function that returns a list of max Re-numbers
    def get_maxReList(self):
        return self.maxReNumbers

    def get_geoParameters(self, airfoilIdx):
        # get parameters from instance data
        (thick, thickPos, camb, cambPos) = self.geoParams

        return (thick[airfoilIdx], thickPos[airfoilIdx],
                camb[airfoilIdx], cambPos[airfoilIdx])


    def set_geoParameters(self, airfoilIdx, geoParams):
        # separate input-tuple
        (new_thick, new_thickPos, new_camb, new_cambPos) = geoParams

        # get parameters from instance data
        (thick, thickPos, camb, cambPos) = self.geoParams

        # insert new values at the correct array-position
        thick[airfoilIdx] =    new_thick
        thickPos[airfoilIdx] = new_thickPos
        camb[airfoilIdx] =     new_camb
        cambPos[airfoilIdx] =  new_cambPos

        # writeback geo parameters to instance data
        self.geoParams = (thick, thickPos, camb, cambPos)

        try:
            # get inputfile, if there already is one
            inputFile = self.inputFiles[airfoilIdx]

            # set geometry targets in inputfile
            inputFile.set_geometryTargets((new_camb, new_thick))
        except:
            pass
        return 0


    def get_rootGeoParameters(self):
        return self.rootGeoParams


    def generate_CoordsAndAssessFile(self, airfoilName):
        filepath = airfoilName +'_temp' + bs
        coordfilename = filepath + DesignCoordinatesName
        assessfilename = filepath + AssessmentResultsName

        # check if output-folder exists. If not, create folder.
        if not path.exists(filepath):
            makedirs(filepath)

        if exists(assessfilename):
            # remove an existing assessment file in case it exists. Otherwise
            # the new assessment results would be appended
            remove(assessfilename)

        if exists(coordfilename):
            remove(coordfilename)

        # perform check of airfoil and generate some data that can be read
        # by the visualizer and additional assessment data of the airfoil
        systemString = ("%s -w check -v -a %s >%s\n" %\
            (self.xfoilWorkerCall, airfoilName+'.dat', assessfilename))
        system(systemString)


    def read_rootGeoParameters(self):
        coordfilename = self.airfoilNames[0]+'_temp' + bs + DesignCoordinatesName

        # generate the necessary files now
        self.generate_CoordsAndAssessFile(self.airfoilNames[0])

        # read design coordinates of root airfoil using the visualizer
        (x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3, name) =\
            visualizer.read_airfoil_coordinates(coordfilename, 'zone t="Seed airfoil', 0)

        thick_ref    = round( maxt*100, thick_decimals)
        thickPos_ref = round(xmaxt*100, thick_decimals)
        camb_ref     = round( maxc*100, camb_decimals)
        cambPos_ref  = round(xmaxc*100, camb_decimals)

        self.rootGeoParams = (thick_ref, thickPos_ref, camb_ref, cambPos_ref)

    def init_geoParams(self, airfoilIdx):
        # get absolute geo params for root airfoil
        root_geoParams = self.get_rootGeoParameters()

        # unpack tuple
        (thick_root, thickPos_root, camb_root, cambPos_root) = root_geoParams

        # init reference_geoParams-class
        reference_geoParams = reference_GeoParameters()

        # calulate ratio Re(strak) to Re(root)
        Re_ratio = self.ReNumbers[airfoilIdx]/self.ReNumbers[0]

        # get geo-ratios according to Re_ratio
        (thick_ratio, thickPos_ratio, camb_ratio, cambPos_ratio) =\
         reference_geoParams.get(Re_ratio)

        # calculate absolute values
        thick =     round((thick_root * thick_ratio), thick_decimals)
        thickPos  = round((thickPos_root * thickPos_ratio), thick_decimals)
        camb =      round((camb_root * camb_ratio), camb_decimals)
        cambPos =   round((cambPos_root * cambPos_ratio), camb_decimals)

        # compose tuple
        new_geoParams = (thick, thickPos, camb, cambPos)
        result = self.set_geoParameters(airfoilIdx, new_geoParams)
        return result


    def set_defaultGeoParams(self, fileContent):
        thick = []
        thickPos = []
        camb = []
        cambPos = []
        num = len(self.ReNumbers)

        # build up default lists
        for idx in range(num):
            thick.append(0.0)
            thickPos.append(0.0)
            camb.append(0.0)
            cambPos.append(0.0)

        # assign default lists
        self.geoParams = (thick, thickPos, camb, cambPos)

        # init geo params of each airfoil including root airfoil
        for idx in range(num):
            self.init_geoParams(idx)

        # write to dicitonary
        self.set_geoParamsInDict(fileContent, self.geoParams)


    def read_AssessmentData(self, airfoilName):
        NoteMsg ("reading assessment data of airfoil %s" % airfoilName)
        reversals = []
        smoothingNecessary = True
        filepath = airfoilName+'_temp' + bs
        assessfilename = filepath + AssessmentResultsName

        if exists(assessfilename):
            file = open(assessfilename)
            lines = file.readlines()
            file.close()
        else:
            ErrorMsg("unable to read assessment data of airfoil %s"  % airfoilName)
            return (0, 0, smoothingNecessary)

        for line in lines:
            if line.find('perfect surface quality')>=0:
                smoothingNecessary = False
            elif line.find('Reversals')>=0:
                splitlines = line.split('Reversals')
                reversalString = splitlines[1]
                splitlines = reversalString.split('Curvature ')
                reversalString = splitlines[0].strip()
                value = int(reversalString)
                reversals.append(value)

        # check how many reversal values were found (with or withou smoothing)
        if (len(reversals) == 2):
            # airfoil not smoothed
            reversals_top = reversals[0]
            reversals_bot = reversals[1]
        elif (len(reversals) == 4):
            # airfoil smoothed
            reversals_top = reversals[1]
            reversals_bot = reversals[3]
        else:
            ErrorMsg("number of reversals could not be determined")
            return (0, 0, smoothingNecessary)

        my_print("airfoil has %d reversals on top and %d reversals on bottom"\
                   %(reversals_top, reversals_bot))
        DoneMsg()

        return (reversals_top, reversals_bot, smoothingNecessary)


    def get_rootReversals(self):
        (rev_top, rev_bot, smooth) = self.read_AssessmentData(self.airfoilNames[0])
        return (rev_top, rev_bot)

    ############################################################################
    # function to change the NCrit-value in a given Namelist-file
    def change_NCritInNamelistFile(self, NCrit, filename):
        fileNameAndPath = ressourcesPath + bs + filename

        # read namelist form file
        dictData = f90nml.read(fileNameAndPath)

        # change ncrit in namelist / dictionary
        xfoil_run_options = dictData["xfoil_run_options"]
        xfoil_run_options['ncrit'] = NCrit

        # delete file and writeback namelist
        remove(fileNameAndPath)
        f90nml.write(dictData, fileNameAndPath)


    ############################################################################
    # function to set the NCrit-value in polar-creation-files
    def set_NCrit(self):
        # change both files
        self.change_NCritInNamelistFile(self.NCrit, T1_polarInputFile)
        self.change_NCritInNamelistFile(self.NCrit, T2_polarInputFile)


    ############################################################################
    # function that calculates dependend values
    def calculate_DependendValues(self):
        # setup tool-calls
        firstExeCallString =  " .." + bs + exePath + bs
        exeCallString =  "echo y | .." + bs + exePath + bs # This will automatically answer with 'yes'
        pythonCallString = pythonInterpreterName + ' ..' + bs + scriptPath + bs

        self.xfoilWorkerCall = exeCallString + xfoilWorkerName + '.exe'
        self.firstXoptfoilCall = firstExeCallString + xoptfoilName + '.exe'
        self.xoptfoilCall = exeCallString + xoptfoilName + '.exe'
        self.strakMachineCall = pythonCallString + strakMachineName + '.py'
        self.xoptfoilVisualizerCall = pythonCallString + xoptfoilVisualizerName + '.py'
        self.airfoilComparisonCall = pythonCallString + airfoilComparisonName + '.py'
        self.showStatusCall = "start \"\" \"%s\" %s\n" % (pythonInterpreterName +"w", \
                         (' ..' + bs + scriptPath + bs + showStatusName + '.py'))

        # set value of NCrit for polar creation
        self.set_NCrit()

        # calculate list of max Re-numbers
        for Re in self.ReNumbers:
            ReMax = int(round(Re * self.maxReFactor, 0))
            self.maxReNumbers.append(ReMax)

        # calculate Cl where T1 and T2 polars will be merged
        self.CL_merge =\
                   ((self.ReNumbers[0] * self.ReNumbers[0]))/\
                   ((self.maxReNumbers[0])*(self.maxReNumbers[0]))


    def calculate_CD_TargetValue(self, root, strak, gain):
        #target = (  (root * gain)           # part coming from root-airfoil
        #          + (strak * (1.0 - gain))) # part coming from strak-airfoil
        target = strak * (1.0 + gain)
        return round(target, CD_decimals)

    def clear_MainTargetValues(self):
        # clear all the targets
        self.targets["CL_min"].clear()
        self.targets["CD_min"].clear()
        self.targets["alpha_min"].clear()
        self.targets["CL_maxSpeed"].clear()
        self.targets["CD_maxSpeed"].clear()
        self.targets["alpha_maxSpeed"].clear()
        self.targets["CL_preMaxSpeed"].clear()
        self.targets["CD_preMaxSpeed"].clear()
        self.targets["alpha_preMaxSpeed"].clear()
        self.targets["CL_maxGlide"].clear()
        self.targets["CD_maxGlide"].clear()
        self.targets["CL_CD_maxGlide"].clear()
        self.targets["alpha_maxGlide"].clear()
        self.targets["CL_pre_maxLift"].clear()
        self.targets["CD_pre_maxLift"].clear()
        self.targets["alpha_pre_maxLift"].clear()
        self.targets["CL0"].clear()
        self.targets["CD0"].clear()
        self.targets["alpha0"].clear()

    def calculate_MainTargetValues(self):
        # get root-polar
        rootPolar = self.merged_polars[0]
        num = len(self.ReNumbers)

        # clear all targets
        self.clear_MainTargetValues()

        for idx in range(num):
            if (idx == 0):
                polar = self.merged_polars[idx]
            else:
                polar = self.seedfoil_polars[idx-1]

            # Get root-polar for this operation
            rootPolar = self.merged_polars[0]

            # get gain and loss values
            minCLGain = 0.0
            CL0Gain = 0.0
            maxSpeedGain = 0.0
            maxSpeedShift = 0.0
            preMaxSpeedGain = 0.0
            maxGlideGain = 0.0
            maxGlideShift = 0.0
            maxLiftGain = 0.0

            #---------------------- CL_min-targets ----------------------------
            target_CL_min = rootPolar.CL_min
            target_alpha_min = rootPolar.alpha[rootPolar.min_idx]
            #polar_CD_min = polar.find_CD_From_CL(target_CL_min)
            polar_CD_min = polar.find_CD_From_alpha(target_alpha_min)

            # now calculate CD-target-value
            target_CD_min = self.calculate_CD_TargetValue(
            rootPolar.CD_min, polar_CD_min, minCLGain)

            # append the targets
            self.targets["CL_min"].append(target_CL_min)
            self.targets["CD_min"].append(target_CD_min)
            self.targets["alpha_min"].append(target_alpha_min)

            #---------------------- maxSpeed-targets --------------------------
            target_CL_maxSpeed = rootPolar.CL_maxSpeed + maxSpeedShift
            maxSpeedIdx = polar.find_index_From_CL(target_CL_maxSpeed)
            target_alpha_maxSpeed = polar.alpha[maxSpeedIdx]
            CD_maxSpeed = polar.CD[maxSpeedIdx]

            # now calculate CD-target-value
            target_CD_maxSpeed = self.calculate_CD_TargetValue(
            rootPolar.CD_maxSpeed, CD_maxSpeed, maxSpeedGain)

            # append the targets
            self.targets["CL_maxSpeed"].append(target_CL_maxSpeed)
            self.targets["CD_maxSpeed"].append(target_CD_maxSpeed)
            self.targets["alpha_maxSpeed"].append(target_alpha_maxSpeed)

#---------------------- preMaxSpeed-targets --------------------------
            target_CL_preMaxSpeed = rootPolar.CL_preMaxSpeed
            target_alpha_preMaxSpeed = rootPolar.alpha[rootPolar.preMaxSpeed_idx]

            # now calculate CD-target-value
            target_CD_preMaxSpeed = self.calculate_CD_TargetValue(
            rootPolar.CD_preMaxSpeed, polar.CD_preMaxSpeed, preMaxSpeedGain)

            # append the targets
            self.targets["CL_preMaxSpeed"].append(target_CL_preMaxSpeed)
            self.targets["CD_preMaxSpeed"].append(target_CD_preMaxSpeed)
            self.targets["alpha_preMaxSpeed"].append(target_alpha_preMaxSpeed)

            #---------------------- maxGlide-targets --------------------------
            # Caution: use CL-target from shifted root-polar
            target_CL_maxGlide = rootPolar.CL_maxGlide
            target_alpha_maxGlide = rootPolar.alpha[rootPolar.maxGlide_idx]
            #polar_CD_maxGlide = polar.find_CD_From_CL(target_CL_maxGlide)
            polar_CD_maxGlide = polar.find_CD_From_alpha(target_alpha_maxGlide)

            # now calculate CD-target-value
            target_CD_maxGlide = self.calculate_CD_TargetValue(
            rootPolar.CD_maxGlide, polar_CD_maxGlide, maxGlideGain)

            # append the targets
            self.targets["CL_maxGlide"].append(target_CL_maxGlide)
            self.targets["CD_maxGlide"].append(target_CD_maxGlide)
            self.targets["CL_CD_maxGlide"].append(target_CL_maxGlide/target_CD_maxGlide)
            self.targets["alpha_maxGlide"].append(target_alpha_maxGlide)

            #---------------------- maxLift-targets --------------------------
            target_CL_pre_maxLift = polar.CL_pre_maxLift
            target_alpha_pre_maxLift = rootPolar.alpha[polar.pre_maxLift_idx]
            #rootPolar_CD_pre_maxLift = rootPolar.find_CD_From_CL(polar.CL_pre_maxLift)
            rootPolar_CD_pre_maxLift = rootPolar.find_CD_From_alpha(target_alpha_pre_maxLift)

            # now calculate CD-target-value
            target_CD_pre_maxLift = self.calculate_CD_TargetValue(
            rootPolar_CD_pre_maxLift, polar.CD_pre_maxLift, maxLiftGain)

            # append the targets
            self.targets["CL_pre_maxLift"].append(target_CL_pre_maxLift)
            self.targets["CD_pre_maxLift"].append(target_CD_pre_maxLift)
            self.targets["alpha_pre_maxLift"].append(target_alpha_pre_maxLift)

            #---------------------- CL0-targets ----------------------------
            target_CL0 = 0.0001#rootPolar.find_CL_From_alpha(rootPolar.alpha_CL0)
            rootPolar_CD0 = rootPolar.find_CD_From_alpha(rootPolar.alpha_CL0)
            polar_CD0 = polar.find_CD_From_alpha(rootPolar.alpha_CL0)

            # now calculate CD-target-value
            target_CD0 = self.calculate_CD_TargetValue(
            rootPolar_CD0, polar_CD0, CL0Gain)
            target_CD0 = rootPolar_CD0/(1+CL0Gain) #Test

            # append the targets
            self.targets["CL0"].append(target_CL0)
            self.targets["CD0"].append(target_CD0)
            self.targets["alpha0"].append(rootPolar.alpha_CL0)

            idx = idx + 1


    def correctOpPoint_left(self, opPoint, CL_maxSpeed_root,
                    CL_maxSpeed_strak, CL_maxGlide_root, CL_maxGlide_strak):
        # distances of maxSpeed, root / strak to maxGlide as a fixed op-point
        delta_root = CL_maxGlide_root - CL_maxSpeed_root
        delta_strak = CL_maxGlide_strak - CL_maxSpeed_strak
        # factor between distances
        factor = delta_strak / delta_root
        # distance of op-point (root) to maxGlide
        delta_opPoint = CL_maxGlide_root - opPoint
        # new distance of op-point (strak) to maxGlide
        delta_opPoint = delta_opPoint * factor
        # new op-point (strak)
        correctedOpPoint = CL_maxGlide_strak - delta_opPoint

        return round(correctedOpPoint, CL_decimals)


    def correctOpPoint_right(self, opPoint, CL_maxLift_root,
                  CL_maxLift_strak, CL_maxGlide_root, CL_maxGlide_strak):

        # distances of maxLift, root / strak to maxGlide as a fixed op-point
        delta_root = CL_maxLift_root - CL_maxGlide_root
        delta_strak = CL_maxLift_strak - CL_maxGlide_strak
        # factor between distances
        factor = delta_strak / delta_root
        # distance of op-point (root) to maxGlide
        delta_opPoint = opPoint - CL_maxGlide_root
        # new distance of op-point (strak) to maxGlide
        delta_opPoint = delta_opPoint * factor
        # new op-point (strak)
        correctedOpPoint = CL_maxGlide_strak + delta_opPoint

        return round(correctedOpPoint, CL_decimals)


################################################################################
#
# polarGraph class
#
################################################################################
class polarGraph:
    def __init__(self):
        self.visibleFlags = []
        return


    def check_polarVisibility(self, params, polarIdx):
        visibleFlags = params.get_visibleFlags()
        return (visibleFlags[polarIdx])


    def check_onlyRootPolarVisible(self, params):
        visibleFlags = params.get_visibleFlags()
        if visibleFlags[0] == False:
            return False

        for flag in visibleFlags[1:]:
            if flag:
                return False

        return True

    def set_AxesAndLabels(self, ax, title, xlabel, ylabel):
        global fs_axes
        global fs_ticks

        # set axis-labels
        ax.set_xlabel(xlabel, fontsize = fs_axes, color="darkgrey")
        ax.set_ylabel(ylabel, fontsize = fs_axes, color="darkgrey")

        for tick in ax.xaxis.get_major_ticks():
            tick.label.set_fontsize(fs_ticks)

        for tick in ax.yaxis.get_major_ticks():
            tick.label.set_fontsize(fs_ticks)

        # customize grid
        ax.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)


    def plot_weightings(self, params, ax, weightings, x, y):
        if (weightings == None):
            return

        for i in range(len(weightings)):
            weight = weightings[i]
            if weight == None:
                continue

            # determine colour
            if (weight >= 1.0):
                cl = 'green'
            else:
                cl = 'red'
            try:
                x_off = 10 * params.scaleFactor
                ax.annotate('%.2f' % weight, xy=(x[i], y[i]),
                        fontsize = fs_weightings, color='white', bbox=dict(facecolor=cl),
                        xytext=(x_off, 0), textcoords='offset points', arrowprops=dict(arrowstyle="->"))
            except:
                pass


    # plots lift/drag-polars (Xfoil-worker-polars and target-polars)
    def plot_LiftDragPolars(self, ax, x_limits, y_limits, polars, targetPolars, params):
        T1_label = None
        T1T2_labelOk = False
        Target_labelOk = False
        Reference_labelOk = False

        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL, CD', 'CD', 'CL')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # get number of polars to plot
        numPolars = len(polars)

        if ((x_limits == None) or (y_limits == None)):
            # get maximum CD-value that shall be visible in the plot
            max_CD = round(rootPolar.CD_maxLift * 1.1, CD_decimals)
            # set x-axis manually
            ax.set_xlim(right = max_CD)
            # set y-axis manually
            ax.set_ylim(min(rootPolar.CL) - 0.2, max(rootPolar.CL) + 0.2)
        else:
            ax.set_xlim(x_limits)
            ax.set_ylim(y_limits)

        # all polars
        for polarIdx in range(numPolars):
            if (self.check_polarVisibility(params, polarIdx) == False):
                # do not plot this polar
                continue

            #  get polar and target-polar to plot
            polar = polars[polarIdx]
            targetPolar = targetPolars[polarIdx]

            # determine idx for changing colors
            switchIdx = polar.T2_T1_switchIdx

            # set label only once
            if (T1T2_labelOk == False):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
                T1T2_labelOk = True
            else:
                T1_label = None
                T2_label = None

            # plot lower (T1)-part of polar
            x = polar.CD[0:switchIdx+1]
            y = polar.CL[0:switchIdx+1]
            # plot CL, CD
            ax.plot(x, y, (cl_T1_polar+'-'), label=T1_label)

            # plot upper (T2)-part of polar
            x = polar.CD[switchIdx:len(polar.CD)]
            y = polar.CL[switchIdx:len(polar.CL)]
            # plot CL, CD
            ax.plot(x, y, (cl_T2_polar+'-'), label=T2_label)

            # plot main oppoints for root polar only
            if (polar == rootPolar):
                # plot CD @CL = 0
                x_CL0 = polar.CD_CL0
                y_CL0 = 0.0
                ax.plot(x_CL0, y_CL0, marker='.',
                    markersize=ms_oppoint, color=cl_infotext)

                # plot max_speed
                x_maxSpeed = polar.CD[polar.maxSpeed_idx]
                y_maxSpeed = polar.CL[polar.maxSpeed_idx]
                ax.plot(x_maxSpeed, y_maxSpeed, marker='.',
                        markersize=ms_oppoint, color=cl_infotext)

                # plot max_glide
                x_maxGlide = polar.CD[polar.maxGlide_idx]
                y_maxGlide = polar.CL[polar.maxGlide_idx]
                ax.plot(x_maxGlide, y_maxGlide, marker='.',
                        markersize=ms_oppoint, color=cl_infotext)

                # plot max lift
                x_maxLift = polar.CD[polar.maxLift_idx]
                y_maxLift = polar.CL[polar.maxLift_idx]
                ax.plot(x_maxLift, y_maxLift, marker='.',
                         markersize=ms_oppoint, color=cl_infotext)

                # Is this the only visible polar ?
                if self.check_onlyRootPolarVisible(params):
                    # determine some text-offsets
                    CL0TextOffset_x = rootPolar.CD_CL0 * 1.1
                    CL0TextOffset_y = 0
                    maxSpeedTextOffset_x = rootPolar.CD_maxSpeed * 1.1
                    maxSpeedTextOffset_y = rootPolar.CL_maxSpeed
                    maxGlideTextOffset_x = rootPolar.CD_maxGlide * 1.1
                    maxGlideTextOffset_y = rootPolar.CL_maxGlide

                    ax.annotate('CL=0 @ CD = %.4f' % x_CL0, xy=(x_CL0,y_CL0),
                      xytext=(CL0TextOffset_x, CL0TextOffset_y),
                      textcoords='data',
                      fontsize = fs_infotext, color=cl_infotext)

                    ax.annotate('maxSpeed @ CL = %.2f, CD = %.4f' % (y_maxSpeed, x_maxSpeed),
                     xy=(x_maxSpeed,y_maxSpeed), xytext=(maxSpeedTextOffset_x, maxSpeedTextOffset_y),
                     textcoords='data', fontsize = fs_infotext,
                     color=cl_infotext)

                    ax.annotate('maxGlide @ CL = %.2f, CD = %.4f' % (y_maxGlide, x_maxGlide),
                     xy=(x_maxGlide,y_maxGlide), xytext=(maxGlideTextOffset_x, maxGlideTextOffset_y),
                      textcoords='data', fontsize = fs_infotext, color=cl_infotext)

                    x_off = -130 * params.scaleFactor
                    y_off = 10 * params.scaleFactor
                    ax.annotate('maxLift @ CL = %.2f, CD = %.4f' %(y_maxLift,x_maxLift),
                      xy=(x_maxLift,y_maxLift), xytext=(x_off,y_off), textcoords='offset points',
                        fontsize = fs_infotext, color=cl_infotext)
            else:
                # plot target-polar
                if (Target_labelOk == False):
                    label = 'target-polar'
                    Target_labelOk = True
                else:
                    label = None

                # is this the selected target polar for editing ?
                if (polarIdx == params.activeTargetPolarIdx):
                    style = opt_point_style_root
                    # get inputfile for the weightings
                    inputFile = params.inputFiles[polarIdx]
                    weightings = inputFile.get_weightings('spec-cl')
                else:
                    style = opt_point_style_strak
                    weightings = None

                linewidth = lw_targetPolar

                # remove last elements, as they are dummies
                x = deepcopy(targetPolar.CD)
                x.pop()
                y = deepcopy(targetPolar.CL)
                y.pop()

                ax.plot(x, y, style, linestyle = ls_targetPolar,
                     linewidth = linewidth, markersize=ms_target, label = label)

                # plot weightings, if any
                self.plot_weightings(params, ax, weightings, x, y)

        # plot strak-polars
        if params.showReferencePolars:
            strakPolars = params.strak_polars
            numPolars = len(strakPolars)

            for i in range(numPolars):
                if ((self.check_polarVisibility(params, i) == False) or
                    (strakPolars[i] == None)):
                        continue

                x = strakPolars[i].CD
                y = strakPolars[i].CL

                # set label only once
                if (Reference_labelOk == False):
                    label = 'reference (polar of previous strak-airfoil)'
                    Reference_labelOk = True
                else:
                    label = None

                ax.plot(x, y, linestyle = ls_referencePolar, color = 'gray',
                                linewidth = lw_referencePolar, label = label)

        if (T1T2_labelOk):
            ax.legend(loc='upper left', fontsize = fs_legend)


    # plots lift/alpha-polars (Xfoil-worker-polars and target-polars)
    def plot_LiftAlphaPolars(self, ax, x_limits, y_limits, polars, targetPolars, params):
        T1_label = None
        T1T2_labelOk = False
        Target_labelOk = False

        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL, alpha', 'alpha', 'CL')

        # get polar of root-airfoil
        rootPolar = polars[0]

        if ((x_limits == None) or (y_limits == None)):
            # set y-axis manually
            ax.set_ylim(min(rootPolar.CL) - 0.1, max(rootPolar.CL) + 0.2)
        else:
            ax.set_xlim(x_limits)
            ax.set_ylim(y_limits)


        # get number of polars to plot
        numPolars = len(polars)

        # all polars
        for polarIdx in range(numPolars):
            if (self.check_polarVisibility(params, polarIdx) == False):
                # do not plot this polar
                continue

            #  get polar to plot
            polar = polars[polarIdx]

            # get inputfile. We can only determine the alpha targets
            # from the inputfile, not the target polar. Also get weightings from here
            inputFile = params.inputFiles[polarIdx]

            # set label only once
            if (T1T2_labelOk == False):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
                T1T2_labelOk = True
            else:
                T1_label = None
                T2_label = None

            # determine idx for changing colors
            switchIdx = polar.T2_T1_switchIdx

            # plot lower (T1)-part of polar
            x = polar.alpha[0:switchIdx+1]
            y = polar.CL[0:switchIdx+1]
            # plot CL, CD
            ax.plot(x, y, (cl_T1_polar+'-'), label=T1_label)

            # plot upper (T2)-part of polar
            x = polar.alpha[switchIdx:len(polar.CD)]
            y = polar.CL[switchIdx:len(polar.CL)]
            # plot CL, CD
            ax.plot(x, y, (cl_T2_polar+'-'), label=T2_label)

            if (polar == rootPolar):
                 # plot alpha @CL = 0
                x = polar.alpha_CL0
                y = 0.0
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                rootPolarOnly = self.check_onlyRootPolarVisible(params)
                if rootPolarOnly:
                    ax.annotate('CL=0 @ alpha = %.2f' % x,
                      xy=(x,y), xytext=(20,-15), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)

                # plot max Speed
                x = polar.alpha[polar.maxSpeed_idx]
                y = polar.CL[polar.maxSpeed_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                if rootPolarOnly:
                    ax.annotate('maxSpeed @ alpha = %.2f, CL = %.2f' %\
                      (x, y), xy=(x,y),
                      xytext=(20,-5), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)

                # plot max Glide
                x = polar.alpha[polar.maxGlide_idx]
                y = polar.CL[polar.maxGlide_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                if rootPolarOnly:
                    ax.annotate('maxGlide @ alpha = %.2f, CL = %.2f' %\
                      (x, y), xy=(x,y),
                      xytext=(20,-5), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)

                # plot max lift
                x = polar.alpha[polar.maxLift_idx]
                y = polar.CL[polar.maxLift_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                if rootPolarOnly:
                    ax.annotate('maxLift @ alpha = %.2f, CL = %.2f' %\
                      (x, y), xy=(x,y),
                      xytext=(-140,10), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)
            else:
                # plot target-polar
                if (Target_labelOk == False):
                    label = 'target-polar'
                    Target_labelOk = True
                else:
                    label = None

                # is this the selected target polar for editing ?
                if (polarIdx == params.activeTargetPolarIdx):
                    # get the x,y values
                    (x, y) = inputFile.get_xyTargets('spec-al')
                    weightings = inputFile.get_weightings('spec-al')

                    # plot
                    ax.plot(x, y, opt_point_style_root,
                          markersize=ms_oppoint, label = label)

                    # plot weightings, if any
                    self.plot_weightings(params, ax, weightings, x, y)

        if (T1T2_labelOk):
            ax.legend(loc='upper left', fontsize = fs_legend)


    # plots glide-polars (Xfoil-worker-polars and target-polars)
    def plot_GlidePolars(self, ax, x_limits, y_limits, polars, targetPolars, params):
        T1_label = None
        T1T2_labelOk = False
        Target_labelOk = False
        Reference_labelOk = False

        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL/CD, CL', 'CL', 'CL/CD')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # get number of polars to plot
        numPolars = len(polars)

        if ((x_limits == None) or (y_limits == None)):
            # set y-axis manually
            ax.set_xlim(-0.1, max(rootPolar.CL) + 0.05)
            ax.set_ylim(-5, max(rootPolar.CL_CD) + 5)
        else:
            ax.set_xlim(x_limits)
            ax.set_ylim(y_limits)

        # all polars
        for polarIdx in range(numPolars):
            if (self.check_polarVisibility(params, polarIdx) == False):
                # do not plot this polar
                continue

            #  get polar and target-polar to plot
            polar = polars[polarIdx]
            targetPolar = targetPolars[polarIdx]

            # set label only once
            if (T1T2_labelOk == False):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
                T1T2_labelOk = True
            else:
                T1_label = None
                T2_label = None

            # determine idx for changing colors
            switchIdx = polar.T2_T1_switchIdx

            # plot lower (T1)-part of polar
            x = polar.CL[0:switchIdx+1]
            y = polar.CL_CD[0:switchIdx+1]
            # plot CL, CD
            ax.plot(x, y, (cl_T1_polar+'-'), label=T1_label)

            # plot upper (T2)-part of polar
            x = polar.CL[switchIdx:len(polar.CD)]
            y = polar.CL_CD[switchIdx:len(polar.CL)]

            # plot CL, CD
            ax.plot(x, y, (cl_T2_polar+'-'), label=T2_label)

            # main oppoints for root polar only
            if (polar == rootPolar):
                # plot Cl/CD @CL = 0
                x = 0.0
                y = 0.0
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                rootPolarOnly = self.check_onlyRootPolarVisible(params)
                if rootPolarOnly:
                    x_off = int(20*params.scaleFactor)
                    y_off = int(-5*params.scaleFactor)
                    ax.annotate('CL=0 @ CL/CD = %.2f' % y, xy=(x,y),
                    xytext=(x_off,y_off), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)

                # plot max_speed
                x = polar.CL[polar.maxSpeed_idx]
                y = polar.CL_CD[polar.maxSpeed_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                 # Is this the only polar ?
                if rootPolarOnly:
                    x_off = int(20*params.scaleFactor)
                    y_off = 0
                    ax.annotate('maxSpeed @\nCL = %.2f,\nCL/CD = %.2f' %\
                    (x, y), xy=(x,y), xytext=(x_off,y_off), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)

                # plot max_glide
                x = polar.CL[polar.maxGlide_idx]
                y = polar.CL_CD[polar.maxGlide_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                # Is this the only polar ?
                if rootPolarOnly:
                   x_off = int(-60*params.scaleFactor)
                   y_off = int(15*params.scaleFactor)
                   ax.annotate('maxGlide @ CL = %.2f, CL/CD = %.2f' %\
                      (x, y), xy=(x,y), xytext=(x_off,y_off), textcoords='offset points',
                       fontsize = fs_infotext, color=cl_infotext)

                # plot max Lift
                x = polar.CL[polar.maxLift_idx]
                y = polar.CL_CD[polar.maxLift_idx]
                ax.plot(x, y, '.', markersize=ms_oppoint, color=cl_infotext)

                 # Is this the only polar ?
                if rootPolarOnly:
                    x_off = int(-80*params.scaleFactor)
                    y_off = 0
                    ax.annotate('maxLift @\nCL = %.2f,\nCL/CD = %.2f' %\
                    (x, y), xy=(x,y), xytext=(x_off, y_off), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)
            else:
                # plot target-polar
                if (Target_labelOk == False):
                    label = 'target-polar'
                    Target_labelOk = True
                else:
                    label = None

                # is this the selected target polar for editing ?
                if (polarIdx == params.activeTargetPolarIdx):
                    style = opt_point_style_root
                    # get inputfile for the weightings
                    inputFile = params.inputFiles[polarIdx]
                    # get all weightings of 'spec-cl' oppoints
                    weightings = inputFile.get_weightings('spec-cl')
                else:
                    style = opt_point_style_strak
                    weightings = None

                linewidth = lw_targetPolar

                x = deepcopy(targetPolar.CL)
                x.pop()
                y = deepcopy(targetPolar.CL_CD)
                y.pop()

                # plot
                ax.plot(x, y, style, linestyle = ls_targetPolar,
                    linewidth = linewidth, markersize=ms_target, label = label)

                # plot weightings, if any
                self.plot_weightings(params, ax, weightings, x, y)

        # plot strak-polars
        if params.showReferencePolars:
            strakPolars = params.strak_polars
            numPolars = len(strakPolars)

            for i in range(numPolars):
                if ((self.check_polarVisibility(params, i) == False) or
                    (strakPolars[i] == None)):
                    # do not plot this polar
                    continue

                # set style
                style = "r-"

                x = strakPolars[i].CL
                y = strakPolars[i].CL_CD

                # set label only once
                if (Reference_labelOk == False):
                    label = 'reference (polar of previous strak-airfoil)'
                    Reference_labelOk = True
                else:
                    label = None

                ax.plot(x, y, linestyle = ls_referencePolar, color = 'gray',
                                linewidth = lw_referencePolar, label = label)
        # Legend
        if (T1T2_labelOk):
            ax.legend(loc='upper left', fontsize = fs_legend)


    def draw_diagram(self, params, diagramType, ax, x_limits, y_limits):
        # get polars
        polars = params.merged_polars
        targetPolars = params.target_polars

        if diagramType == "CL_CD_diagram":
            # plot Glide polar
            self.plot_LiftDragPolars(ax, x_limits, y_limits, polars, targetPolars, params)
        elif diagramType == "CL_alpha_diagram":
            # plot Glide polar
            self.plot_LiftAlphaPolars(ax, x_limits, y_limits, polars, targetPolars, params)
        elif diagramType == "CLCD_CL_diagram":
            # plot Glide polar
            self.plot_GlidePolars(ax, x_limits, y_limits, polars, targetPolars, params)
        else:
            ErrorMsg("undefined diagramtype")



################################################################################
#
# polarData class
#
################################################################################
class polarData:
    def __init__(self):
        self.polarName = ''
        self.airfoilname = "airfoil"
        self.polarType = 2
        self.Re = 0
        self.maxRe = 0
        self.NCrit = NCrit_Default
        self.Mach = 0.0
        self.alpha = []
        self.CL = []
        self.CD = []
        self.CL_CD = []
        self.CDp = []
        self.Cm = []
        self.Top_Xtr = []
        self.Bot_Xtr= []
        self.CD_min = 0.0
        self.CL_min = 0.0
        self.alpha_min = 0.0
        self.min_idx= 0
        self.CD_maxSpeed = 0.0
        self.CL_maxSpeed = 0.0
        self.alpha_maxSpeed = 0.0
        self.CL_CD_maxSpeed = 0.0
        self.maxSpeed_idx = 0
        self.CD_preMaxSpeed = 0.0
        self.CL_preMaxSpeed = 0.0
        self.alpha_preMaxSpeed = 0.0
        self.CL_CD_preMaxSpeed = 0.0
        self.preMaxSpeed_idx = 0
        self.CL_CD_maxGlide = 0.0
        self.maxGlide_idx = 0
        self.alpha_maxGlide= 0.0
        self.CL_maxGlide = 0.0
        self.CD_maxGlide = 0.0
        self.CL_maxLift = 0.0
        self.CD_maxLift = 0.0
        self.alpha_maxLift = 0.0
        self.alpha_CL0 = 0.0
        self.CD_CL0 = 0.0
        self.maxLift_idx = 0
        self.CL_pre_maxLift = 0.0
        self.CD_pre_maxLift= 0.0
        self.pre_maxLift_idx = 0
        self.alpha_pre_maxLift = 0.0
        self.operatingConditions = None
        self.CL_merge = 999999
        self.T2_T1_switchIdx = 0


    # function to get alpha @ CL_min and alpha @ CL_maxLift
    def get_alphaMin_alphaMaxLift(self):
        return (self.alpha_min, self.alpha_maxLift)


    def get_alphaMerge(self):
        return (self.alpha[self.T2_T1_switchIdx])


    def import_FromFile(self, fileName):
        BeginOfDataSectionTag = "-------"
        airfoilNameTag = "Calculated polar for:"
        ReTag = "Re ="
        parseInDataPoints = 0
        my_print("importing polar %s..." %fileName)

        # open file
        fileHandle = open(fileName)

        # parse all lines
        for line in fileHandle:

            # scan for airfoil-name
            if  line.find(airfoilNameTag) >= 0:
                splitline = line.split(airfoilNameTag)
                self.airfoilname = splitline[1]
                self.airfoilname = self.airfoilname.strip()

           # scan for Re-Number
            if  line.find(ReTag) >= 0:
                splitline = line.split(ReTag)
                splitline = splitline[1].split("Ncrit")
                Re_string = splitline[0].strip()
                splitstring = Re_string.split("e")
                faktor = float(splitstring[0].strip())
                Exponent = float(splitstring[1].strip())
                self.Re = faktor * (10**Exponent)
                self.airfoilname = self.airfoilname.strip()

            # scan for start of data-section
            if line.find(BeginOfDataSectionTag) >= 0:
                parseInDataPoints = 1
            else:
                # get all Data-points from this line
                if parseInDataPoints == 1:
                    # split up line detecting white-spaces
                    splittedLine = line.split(" ")
                    # remove white-space-elements, build up list of data-points
                    dataPoints = []
                    for element in splittedLine:
                        if element != '':
                            dataPoints.append(element)
                    # get data points
                    alpha = (float(dataPoints[0]))
                    CL = float(dataPoints[1])
                    CD = float(dataPoints[2])
                    CDp = float(dataPoints[3])
                    Cm = float(dataPoints[4])
                    Top_Xtr = float(dataPoints[5])
                    Bot_Xtr = float(dataPoints[6])

                    # determine index where to insert new data points
                    idx = 0
                    for element in self.alpha:
                        if alpha < element:
                            break
                        else:
                            idx = idx + 1

                    self.alpha.insert(idx, alpha)
                    self.CL.insert(idx, CL)
                    self.CD.insert(idx, CD)
                    self.CL_CD.insert(idx, CL/CD)
                    self.CDp.insert(idx, CDp)
                    self.Cm.insert(idx, Cm)
                    self.Top_Xtr.insert(idx, Top_Xtr)
                    self.Bot_Xtr.insert(idx, Bot_Xtr)

        fileHandle.close()


    # write polar to file with a given filename (and -path)
    def write_ToFile(self, fileName):
        # get some local variables
        polarType = self.polarType
        airfoilname = self.airfoilname
        Re = float(self.Re)/1000000
        Mach = self.Mach
        NCrit = self.NCrit

        if (polarType == 1):
            ReString = 'fixed         '
            MachString = 'fixed'
        elif(polarType == 2):
            ReString = '~ 1/sqrt(CL)  '
            MachString = '~ 1/sqrt(CL)'
        else:
            ReString = 'fixed / ~ 1/sqrt(CL)'
            MachString = 'fixed / ~ 1/sqrt(CL)'

        my_print("writing polar to file %s..." %fileName)

        # open file
        fileHandle = open(fileName, 'w+')

        # write header
        fileHandle.write("Xoptfoil-JX\n\n")
        fileHandle.write(" Calculated polar for: %s\n\n" % airfoilname)
        fileHandle.write(" %d %d Reynolds number %s Mach number %s\n\n" %\
         (polarType, polarType, ReString, MachString))

        fileHandle.write(" xtrf =   1.000 (top)        1.000 (bottom)\n")
        fileHandle.write(" Mach = %7.3f     Re = %9.3f e 6     Ncrit = %7.3f\n\n" %\
                        (Mach, Re, NCrit))

        fileHandle.write("  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr \n")
        fileHandle.write(" ------- -------- --------- --------- -------- ------- ------- \n")

        # more local variables
        alpha = self.alpha
        CL = self.CL
        CD = self.CD
        CDp = self.CDp
        Cm = self.Cm
        Top_Xtr = self.Top_Xtr
        Bot_Xtr = self.Bot_Xtr

        # write data
        for i in range(len(alpha)):
            fileHandle.write(" %7.3f %8.4f %9.5f %9.5f %8.4f %7.4f %7.4f\n"\
            % (alpha[i], CL[i], CD[i], CDp[i], Cm[i], Top_Xtr[i], Bot_Xtr[i]))

        fileHandle.close()


    # analyses a polar
    def analyze(self, params):
        my_print("analysing polar \'%s\'..." % self.polarName)

        self.determine_MaxSpeed()
        self.determine_CLmin(params)
        self.determine_MaxGlide()
        self.determine_preMaxSpeed(params)
        self.determine_MaxLift(params)
        self.determine_alpha_CL0(params)


    # this function must be called after reading a merged polar from file
    def restore_mergeData(self, CL_merge, maxRe):
        self.CL_merge = CL_merge
        self.maxRe = maxRe
        self.polarName = 'merged_polar_%s' % get_ReString(self.Re)

        for idx in range(len(self.CL)):
            if (self.CL[idx] <= CL_merge):
                self.T2_T1_switchIdx = idx

    # merge two polars at a certain CL-value, return a merged-polar
    # mergePolar_1 will be the "lower" part of the merged-polar from
    # minimum CL up to the CL-value where the merge happens.
    # "self" will be the upper part of the merged-polar
    def merge(self, mergePolar_1, CL_merge, maxRe):
        # create a new, empty polar
        mergedPolar = polarData()

        # copy some information from mergePolar_1
        mergedPolar.airfoilname = self.airfoilname
        mergedPolar.polarType = 12
        mergedPolar.Re = self.Re
        mergedPolar.NCrit = 1.0
        mergedPolar.CL_merge = CL_merge
        mergedPolar.maxRe = maxRe
        mergedPolar.polarName = 'merged_polar_%s' % get_ReString(self.Re)

        # merge first polar from start Cl to CL_merge
        for idx in range(len(mergePolar_1.CL)):
            if (mergePolar_1.CL[idx] <= CL_merge):
                mergedPolar.alpha.append(mergePolar_1.alpha[idx])
                mergedPolar.CL.append(mergePolar_1.CL[idx])
                mergedPolar.CD.append(mergePolar_1.CD[idx])
                mergedPolar.CL_CD.append(mergePolar_1.CL_CD[idx])
                mergedPolar.CDp.append(mergePolar_1.CDp[idx])
                mergedPolar.Cm.append(mergePolar_1.Cm[idx])
                mergedPolar.Top_Xtr.append(mergePolar_1.Top_Xtr[idx])
                mergedPolar.Bot_Xtr.append(mergePolar_1.Bot_Xtr[idx])
                mergedPolar.T2_T1_switchIdx = idx

        # merge second polar from switching_Cl to end Cl
        for idx in range(len(self.CL)):
            if (self.CL[idx] > CL_merge):
                mergedPolar.alpha.append(self.alpha[idx])
                mergedPolar.CL.append(self.CL[idx])
                mergedPolar.CD.append(self.CD[idx])
                mergedPolar.CL_CD.append(self.CL_CD[idx])
                mergedPolar.CDp.append(self.CDp[idx])
                mergedPolar.Cm.append(self.Cm[idx])
                mergedPolar.Top_Xtr.append(self.Top_Xtr[idx])
                mergedPolar.Bot_Xtr.append(self.Bot_Xtr[idx])

        DoneMsg()
        return mergedPolar


    def set_alphaResolution(self, newResolution):
        # create empty lists
        new_alpha =[]
        new_CL = []
        new_CD = []
        new_CL_CD = []
        new_CDp = []
        new_Cm = []
        new_Top_Xtr = []
        new_Bot_Xtr = []

        # determine actual resoultion of alpha
        actualResolution = round((self.alpha[1] - self.alpha[0]), 10)
        if (actualResolution < 0.0):
            ErrorMsg("set_alphaResolution: negative value was determinded for actual resolution")
            return

        # number of increments must be an integer
        num_increments = int(round(actualResolution / newResolution, 0))

        # check number of increments, must be > 1
        if (num_increments <= 1):
            # Error-message and return
            NoteMsg("set_alphaResolution: newResolution is less than or equal actual resolution")
            NoteMsg("actual resolution: %f, new resolution: %f" % (actualResolution, newResolution) )
            return

        # determine size of an increment
        increment = actualResolution / float(num_increments)

        # loop over all list elements
        num_values = len(self.alpha)
        for i in range(num_values - 1):
            alpha_left = self.alpha[i]
            alpha_right = self.alpha[i+1]

            for n in range(num_increments):
                # calculate new values using linear interpolation
                alpha = round((alpha_left + n*increment), 10)
                CL = interpolate(alpha_left, alpha_right, self.CL[i], self.CL[i+1], alpha)
                CD = interpolate(alpha_left, alpha_right, self.CD[i], self.CD[i+1], alpha)
                CL_CD = interpolate(alpha_left, alpha_right, self.CL_CD[i], self.CL_CD[i+1], alpha)
                CDp = interpolate(alpha_left, alpha_right, self.CDp[i], self.CDp[i+1], alpha)
                Cm = interpolate(alpha_left, alpha_right, self.Cm[i], self.Cm[i+1], alpha)
                Top_Xtr = interpolate(alpha_left, alpha_right, self.Top_Xtr[i], self.Top_Xtr[i+1], alpha)
                Bot_Xtr = interpolate(alpha_left, alpha_right, self.Bot_Xtr[i], self.Bot_Xtr[i+1], alpha)

                new_alpha.append(alpha)
                new_CL.append(CL)
                new_CD.append(CD)
                new_CL_CD.append(CL_CD)
                new_CDp.append(CDp)
                new_Cm.append(Cm)
                new_Top_Xtr.append(Top_Xtr)
                new_Bot_Xtr.append(Bot_Xtr)

        # append last values
        new_alpha.append(self.alpha[num_values-1])
        new_CL.append(self.CL[num_values-1])
        new_CD.append(self.CD[num_values-1])
        new_CL_CD.append(self.CL_CD[num_values-1])
        new_CDp.append(self.CDp[num_values-1])
        new_Cm.append(self.Cm[num_values-1])
        new_Top_Xtr.append(self.Top_Xtr[num_values-1])
        new_Bot_Xtr.append(self.Bot_Xtr[num_values-1])

        # now set new values/ overwrite old values
        self.alpha = new_alpha
        self.CL = new_CL
        self.CD = new_CD
        self.CL_CD = new_CL_CD
        self.CDp = new_CDp
        self.Cm = new_Cm
        self.Top_Xtr = new_Top_Xtr
        self.Bot_Xtr = new_Bot_Xtr

        # correct the switching-idx between T1 / T2-polar
        self.T2_T1_switchIdx = self.find_index_From_CL(self.CL_merge)
        #my_print("Ready")#Debug


    # determines the first minimum CD-value of a given polar, starting at
    # the point of highest lift coefficent, descending
    def determine_MaxSpeed(self):
        # initialize minimum
        minimum = self.CD[-1]

        # find the first minimum CD starting from the highest lift coefficient
        for idx in reversed(range(len(self.CD)-1)):
            if (self.CD[idx] <= minimum):
                # found new minimum
                minimum = self.CD[idx]
            else:
                # CD starts incresing again, so we have found the first minimum
                break

        self.CD_maxSpeed = minimum
        self.maxSpeed_idx = idx
        self.CL_maxSpeed = self.CL[self.maxSpeed_idx]
        self.alpha_maxSpeed = self.alpha[self.maxSpeed_idx]
        self.CL_CD_maxSpeed = self.CL_maxSpeed / self.CD_maxSpeed

        #my_print("max Speed, CD = %f @ CL = %f" %\
        #                         (self.CD_maxSpeed, self.CL_maxSpeed))


    def determine_preMaxSpeed(self, params):
        self.CL_preMaxSpeed = params.CL_preMaxSpeed
        self.preMaxSpeed_idx = self.find_index_From_CL(self.CL_preMaxSpeed)
        self.CD_preMaxSpeed = self.CD[self.preMaxSpeed_idx]
        self.alpha_preMaxSpeed = self.alpha[self.preMaxSpeed_idx]
        self.CL_CD_preMaxSpeed = self.CL_preMaxSpeed / self.CD_preMaxSpeed
        #my_print("pre max Speed, CD = %f @ CL = %f" %\
        #                          (self.CD_preMaxSpeed, self.CL_preMaxSpeed))


    # determines the overall max-value for Cl/Cd (max glide) of a given polar
    # and some corresponding values
    def determine_MaxGlide(self):
        self.CL_CD_maxGlide = max(self.CL_CD)
        self.maxGlide_idx = self.find_index_From_CL_CD(self.CL_CD_maxGlide)
        self.CL_maxGlide = self.CL[self.maxGlide_idx]
        self.CD_maxGlide = self.CD[self.maxGlide_idx]
        self.alpha_maxGlide = self.alpha[self.maxGlide_idx]

        #my_print("max Glide, CL/CD = %f @ CL = %f" %
        #                          (self.CL_CD_maxGlide, self.CL_maxGlide))

    def determine_CLmin(self, params):
        self.CL_min = params.CL_min
        self.min_idx = self.find_index_From_CL(params.CL_min)
        self.CD_min = self.CD[self.min_idx]#self.find_CD_From_CL(params.CL_min)
        self.alpha_min = self.alpha[self.min_idx]


    # determines the max-value for Cl (max lift) of a given polar and some
    # corresponding values
    def determine_MaxLift(self, params):
        self.CL_maxLift = max(self.CL)
        self.maxLift_idx = self.find_index_From_CL(self.CL_maxLift)
        self.CD_maxLift = self.CD[self.maxLift_idx]
        self.alpha_maxLift = self.alpha[self.maxLift_idx]

        # also calculate opPoint before maxLift that can be reached by the
        # optimizer
        self.CL_pre_maxLift = self.CL_maxLift - params.maxLiftDistance
        self.pre_maxLift_idx = self.find_index_From_CL(self.CL_pre_maxLift)
        self.CD_pre_maxLift = self.CD[self.pre_maxLift_idx]
        self.alpha_pre_maxLift = self.alpha[self.pre_maxLift_idx]

        #my_print("max Lift, CL = %f @ alpha = %f" %
        #                          (self.CL_maxLift, self.alpha_maxLift))
        #my_print("last op-point before max Lift will be set to CL = %f @ alpha"\
        #      " = %f, keeping a CL-distance of %f" %\
        #  (self.CL_pre_maxLift, self.alpha_pre_maxLift, params.maxLiftDistance))

    # determines alpha @ CL = 0
    def determine_alpha_CL0(self, params):
        num = len(self.alpha)

        for idx in range(num-1):
            # find CL-values left and right from CL = 0
            if (self.CL[idx]<=0 and self.CL[idx+1]>=0):
                # interpolate between CL-values, calculate alpha @CL = 0
                self.alpha_CL0 = interpolate(self.CL[idx], self.CL[idx+1],\
                                      self.alpha[idx], self.alpha[idx+1], 0)

        # also determine CD @ CL = 0
        self.CD_CL0 = self.find_CD_From_CL(0.0)

        #my_print("alpha_CL0 = %f" % self.alpha_CL0)

    # local helper-functions
    def find_index_From_CL(self, CL):
        for i in range(len(self.CL)):
            if (self.CL[i] >= CL):
                return i
        ErrorMsg("index not found, CL was %f" % CL)
        return None

    def find_index_From_CD(self, CD):
        for i in range(len(self.CD)):
            if (self.CD[i] == CD):
                return i
        ErrorMsg("index not found, CD was %f" % CD)
        return None

    def find_index_From_CL_CD(self, CL_CD):
        for i in range(len(self.CL_CD)):
            if (self.CL_CD[i] >= CL_CD):
                return i
        ErrorMsg("index not found, CL_CD was %f" % CL_CD)
        return None

    def find_CD_From_CL(self, CL):
        num = len(self.CL)
        for idx in range(num-1):
            # find CL-values left and right from CL
            if (self.CL[idx]<=CL and self.CL[idx+1]>=CL):
                # interpolate between CL-values
                CD = interpolate(self.CL[idx], self.CL[idx+1],\
                                      self.CD[idx], self.CD[idx+1], CL)
                return CD

        ErrorMsg("CD not found, CL was %f" % CL)
        return None

    def find_CL_From_alpha(self, alpha):
        num = len(self.alpha)
        for idx in range(num-1):
            # find CL-values left and right from CL
            if (self.alpha[idx]<=alpha and self.alpha[idx+1]>=alpha):
                # interpolate between alpha-values
                CL = interpolate(self.alpha[idx], self.alpha[idx+1],\
                                      self.CL[idx], self.CL[idx+1], alpha)
                return CL

        ErrorMsg("CL not found, alpha was %f" % alpha)
        return None

    def find_CD_From_alpha(self, alpha):
        num = len(self.alpha)
        for idx in range(num-1):
            # find CL-values left and right from CL
            if (self.alpha[idx]<=alpha and self.alpha[idx+1]>=alpha):
                # interpolate between alpha-values
                CD = interpolate(self.alpha[idx], self.alpha[idx+1],\
                                      self.CD[idx], self.CD[idx+1], alpha)
                return CD

        ErrorMsg("CD not found, alpha was %f" % alpha)
        return None

    def find_alpha_From_CL(self, CL):
        num = len(self.CL)
        for idx in range(num-1):
            # find CL-values left and right from CL
            if (self.CL[idx]<=CL and self.CL[idx+1]>=CL):
                # interpolate between CL-values
                alpha = interpolate(self.CL[idx], self.CL[idx+1],\
                                      self.alpha[idx], self.alpha[idx+1], CL)
                return alpha
##
##        for i in range(len(self.CL)):
##            if (self.CL[i] >= CL):
##                return self.alpha[i]
        ErrorMsg("alpha not found, CL was %f" % CL)
        return None


################################################################################
# function that generates commandlines to create and merge polars
def generate_polarCreationCommandLines(commandlines, params, strakFoilName, ReT1, ReT2):
    airfoilName = remove_suffix(strakFoilName, '.dat')
    polarDir = airfoilName + '_polars'
    T1_fileName = 'iPolars_T1_%s.txt' % airfoilName
    T2_fileName = 'iPolars_T2_%s.txt' % airfoilName
    num = len(ReT1)

    # worker call T1-polar (multiple polars with one call)
    commandline = params.xfoilWorkerCall +  " -i \"%s\" -w polar -a \"%s\"\n" %\
                                  (T1_fileName, airfoilName+'.dat')
    commandlines.append(commandline)

    # worker call T2-polar (multiple polars with one call)
    commandline = params.xfoilWorkerCall +  " -i \"%s\" -w polar -a \"%s\"\n" %\
                                  (T2_fileName, airfoilName+'.dat')
    commandlines.append(commandline)

    # merge command (only one merged polar with one call)
    for i in range(num):
        polarFileName_T1 = compose_Polarfilename_T1(ReT1[i], params.NCrit)
        polarFileNameAndPath_T1 = polarDir + bs + polarFileName_T1

        polarFileName_T2 = compose_Polarfilename_T2(ReT2[i], params.NCrit)
        polarFileNameAndPath_T2 = polarDir + bs + polarFileName_T2

        mergedPolarFileName =  polarDir + bs +\
                 ('merged_polar_%s.txt' % get_ReString(ReT2[i]))

        commandline = params.strakMachineCall + " -w merge -p1 \"%s\"  -p2 \"%s\""\
                 " -m \"%s\" -c %f\n" %\
              (polarFileNameAndPath_T1, polarFileNameAndPath_T2,
               mergedPolarFileName, params.CL_merge)
        commandlines.append(commandline)


def delete_progressFile(commandLines, filename):
    commandLines.append("del %s\n" % filename)


def insert_MainTaskProgress(commandLines, fileName, progress):
    commandLines.append("echo main-task progress: %.1f >> %s\n" % (progress,fileName))


def insert_SubTaskProgress(commandLines, fileName, progress):
    commandLines.append("echo sub-task progress: %.1f >> %s\n" % (progress,fileName))


def insert_preliminaryAirfoilName(commandLines, filename, airfoilname):
    commandLines.append("echo %%TIME%%   creating preliminary-airfoil: %s >> %s\n" % (airfoilname, filename))

def insert_airfoilName(commandLines, filename, airfoilname):
    commandLines.append("echo %%TIME%%   finalizing airfoil: %s >> %s\n" % (airfoilname, filename))

def insert_finishedAirfoil(commandLines, filename ):
     commandLines.append("echo %%TIME%%   finished airfoil >> %s\n" % filename)

def insert_calculate_polars(commandLines, filename, airfoilname):
     commandLines.append("echo %%TIME%%   calculating polars for airfoil: %s >> %s\n" % (airfoilname, filename))

def insert_calculate_polars_finished(commandLines, filename):
     commandLines.append("echo %%TIME%%   finished calculating polars >> %s\n" % filename)

def insert_MainTaskStart(commandLines, filename, rootfoilName, ReList):
    line = "echo main-task start: create whole set of airfoils "
    splitlines = rootfoilName.split("root")
    rootfoilName = splitlines[0]
    numStrakfoils = len(ReList)

    for i in range(1, numStrakfoils):
        reString = get_ReString(ReList[i])
        strakfoilname = rootfoilName + reString
        line = line +"%s" % strakfoilname

        if (i < (numStrakfoils-1)):
            # not the last airfoil, append comma
            line = line +", "

    line = line + ">> %s\n\n" % (filename)
    commandLines.append(line)


def insert_MainTaskEnd(commandLines, filename):
    commandLines.append("echo main-task end >> %s\n" % (filename))


def insert_SubTaskStart(commandLines, filename, airfoilname):
    commandLines.append("echo sub-task start: create airfoil %s >> %s\n" % (airfoilname, filename))


def insert_SubTaskEnd(commandLines, filename):
    commandLines.append("echo sub-task end >> %s\n\n" % (filename))


def insert_StatusCall(commandLines, params):
    commandLines.append("\n" + params.showStatusCall +"\n")


def calculate_MainTaskProgress(params, i):
    # get number of airfoils without root-airfoil
    numFoils = len(params.ReNumbers) - 1
    if (numFoils > 0):
        progress = (i*100.0)/numFoils
    else:
        progress = 100.0
    return progress


def calculate_SubTaskProgress(params, n, c):
    overall_iterations = 0
    iterations_elapsed = 0

    # multi-pass-optimization ?
    if (params.optimizationPasses > 1):
        # loop over all optimization-passes
        for idx in range(0, params.optimizationPasses):
            num_competitors = params.numberOfCompetitors[idx]
            iterations_per_competitor = params.maxIterations[idx]
            iterations_per_pass = num_competitors * iterations_per_competitor
            overall_iterations = overall_iterations + iterations_per_pass

        for idx in range(0, n+1):
            num_competitors = params.numberOfCompetitors[idx]
            iterations_per_competitor = params.maxIterations[idx]
            if (n > idx):
                iterations_per_pass = num_competitors * iterations_per_competitor
            else:
                iterations_per_pass = (c+1) * iterations_per_competitor
            iterations_elapsed = iterations_elapsed + iterations_per_pass

        progress = (iterations_elapsed * 100.0) / overall_iterations
    else:
        # singlepass-optimization
        progress = 100.0

    return progress


def progressfile_preamble(commandLines, progressFileName):
    # delete progress-file
    delete_progressFile(commandLines, progressFileName)

################################################################################
# function that generates commandlines to run Xoptfoil, create and merge polars
# etc.
def generate_Commandlines(params):
    NoteMsg("Generating commandlines...")

    # create an empty list of commandlines
    commandLines = []

    # do some initializations / set local variables
    rootfoilName = params.airfoilNames[0] + ".dat"

    numFoils = len(params.ReNumbers)
    ReList = params.get_ReList()
    maxReList = params.get_maxReList()

    # change current working dir to output folder
    commandline = "cd %s\n\n" % buildPath
    commandLines.append(commandline)

    # do some initialisations for progress-file
    progressfile_preamble(commandLines, progressFileName)

    # insert specification of main task
    insert_MainTaskStart(commandLines, progressFileName, rootfoilName, ReList)

    # set timestamp and progress
    insert_MainTaskProgress(commandLines, progressFileName, 0.0)

    # call status-monitoring-script
    insert_StatusCall(commandLines, params)

    # store rootfoilname
    strakFoilName = rootfoilName
    previousFoilname = rootfoilName

    # add command-lines for each strak-airfoil
    # skip the root airfoil (as it was already copied)
    for i in range (1, numFoils):

        # get name of the airfoil
        strakFoilName = params.airfoilNames[i] + ".dat"
        seedfoilName = 'seed_%s.dat' % get_ReString(params.ReNumbers[i])

        # insert specification of sub-task
        insert_SubTaskStart(commandLines, progressFileName, strakFoilName)

        # set progress of sub-task to 0
        insert_SubTaskProgress(commandLines, progressFileName, 0.0)

        # multi-pass-optimization:
        # generate commandlines for intermediate airfoils
        for n in range(0, params.optimizationPasses-1):
            iFileIndex = i*(params.optimizationPasses) + n

            # set input-file name for Xoptfoil
            iFile = params.inputFileNames[iFileIndex]

            # generate name of the intermediate airfoil
            intermediateFoilName = remove_suffix(strakFoilName, '.dat')
            intermediateFoilName = intermediateFoilName + ("_%d.dat" % (n+1))

            # check, if there is more than one competitor for this intermediate stage
            num = params.numberOfCompetitors[n]

            for c in range(num):
                # append competitor-number to name of intermediate airfoil
                competitorName = remove_suffix(intermediateFoilName, '.dat') + ("_%d" % (c+1))

                # insert name of airfoil to be processes into progress-file
                insert_preliminaryAirfoilName(commandLines, progressFileName, competitorName)

                # set callstring for calling xoptfoil
                if (c==0):
                    # first call
                    xoptfoilCall = params.firstXoptfoilCall
                else:
                    xoptfoilCall = params.xoptfoilCall

                # generate commandline for competitor-intermediate strak-airfoil
                commandline = xoptfoilCall + " -i %s -r %d -a %s -o %s\n" %\
                (iFile, ReList[i], seedfoilName, competitorName)
                commandLines.append(commandline)

                # check wheather the strak-airfoils shall be smoothed after their
                # creation
                if (params.smoothStrakFoils):
                    # smooth the airfoil
                    smoothFileName = get_PresetInputFileName(smoothInputFile)

                    # compose commandline for smoothing the airfoil
                    commandline = params.xfoilWorkerCall + " -w smooth -i %s -a %s -o %s\n" % \
                       (smoothFileName, competitorName + '.dat', competitorName)
                    commandLines.append(commandline)

                # set timestamp and progress
                progress = calculate_SubTaskProgress(params, n, c)
                insert_SubTaskProgress(commandLines, progressFileName, progress)

            # generate commandline for selecting the best airfoil among all
            # competitors
            commandline = params.airfoilComparisonCall + " -a %s -n %d\n" %\
               (remove_suffix(intermediateFoilName, '.dat') , num)
            commandLines.append(commandline)

            # the output-airfoil is the new seedfoil
            seedfoilName = intermediateFoilName


        # generate commandline for final strak-airfoil
        if (params.optimizationPasses > 1):
            iFileIndex = i*(params.optimizationPasses) + (n+1)
        else:
            iFileIndex = i

        # insert name of airfoil to be processes into progress-file
        insert_airfoilName(commandLines, progressFileName, remove_suffix(strakFoilName, '.dat'))

        iFile = params.inputFileNames[iFileIndex]
        commandline = params.xoptfoilCall + " -i %s -r %d -a %s -o %s\n" %\
                    (iFile, ReList[i], seedfoilName,
                      remove_suffix(strakFoilName, '.dat'))
        commandLines.append(commandline)

        # check wheather the strak-airfoils shall be smoothed after their
        # creation
        if (params.smoothStrakFoils):
            # smooth the airfoil
            inputFilename = get_PresetInputFileName(smoothInputFile)

            # compose commandline for smoothing the airfoil
            commandline = params.xfoilWorkerCall + " -w smooth -i %s -a %s -o %s\n" % \
                       (inputFilename, strakFoilName, remove_suffix(strakFoilName, '.dat'))
            commandLines.append(commandline)

        # set timestamp and progress
        insert_SubTaskProgress(commandLines, progressFileName, 100.0)

        # insert message that strak-airfoil was finished
        insert_finishedAirfoil(commandLines, progressFileName)

        # insert message for polar-calculation
        insert_calculate_polars(commandLines, progressFileName, strakFoilName)

        # if not being the last strak-airfoil, also create T1 / T2 / merged
        # polars for the Re-numbers that were specified for the next
        # strak-airfoil, to have a kind of "benchmark" or at least
        # orientation for the next strak-airfoil
        if (i<(numFoils-1)):
            ReT1 = [maxReList[i], maxReList[i+1]]
            ReT2 = [ReList[i], ReList[i+1]]
        else:
            ReT1 = [maxReList[i]]
            ReT2 = [ReList[i]]

        # create T1 / T2 / merged polars for the specified Re-numbers of the
        # generated strak-airfoil
        generate_polarCreationCommandLines(commandLines, params, strakFoilName,
                                           ReT1, ReT2)

        # insert message for polar-calculation
        insert_calculate_polars_finished(commandLines, progressFileName)

        # copy strak-airfoil to airfoil-folder
        commandline = ("copy %s %s" + bs +"%s\n\n") % \
            (strakFoilName , airfoilPath, strakFoilName)
        commandLines.append(commandline)

        # insert end of sub-task
        insert_SubTaskEnd(commandLines, progressFileName)

        # set timestamp and progress
        progress = calculate_MainTaskProgress(params, i)
        insert_MainTaskProgress(commandLines, progressFileName, progress)

    # set end of main-task
    insert_MainTaskEnd(commandLines, progressFileName)

    # change current working dir back
    commandline = "cd..\n"
    commandLines.append(commandline)

    # pause in the end
    commandline = "pause\n"
    commandLines.append(commandline)

    DoneMsg()
    return commandLines


################################################################################
# function that generates a Xoptfoil-batchfile
def generate_Batchfile(batchFileName, commandlines):
    try:
        # create a new file
        outputfile = open(batchFileName, "w+")
    except:
        ErrorMsg('file %s could not be opened' % batchFileName)
        return

    # write Xoptfoil-commandline to outputfile
    for element in commandlines:
        outputfile.write(element)

    # close the outputfile
    outputfile.close()


################################################################################
# function that gets commandlines to generate one strak-airfoil
def get_strak_commandlines(params, commandlines, idx):
    strak_commandlines = []
    ReString = get_ReString(params.ReNumbers[idx])
    airfoilName = params.airfoilNames[idx]

    # change current working dir to output folder
    strak_commandlines.append("cd %s\n\n" % buildPath)
    progressfile_preamble(strak_commandlines, progressFileName)

    # set progress of main-task to 0 percent
    insert_MainTaskProgress(strak_commandlines, progressFileName, 0.0)

    # call status monitoring
    insert_StatusCall(strak_commandlines, params)

    start = False

    for line_idx in range(len(commandlines)):
        # determine start-line
        if ((commandlines[line_idx].find(airfoilName)>=0) and
            (commandlines[line_idx].find( 'sub-task start')>=0)):
            start = True

        if (start and (commandlines[line_idx].find('sub-task end')>=0)):
            # everything found, append last line
            strak_commandlines.append(commandlines[line_idx])
            break

        if (start):
            # append line
            strak_commandlines.append(commandlines[line_idx])

    # set progress of main-task to 100 percent
    insert_MainTaskProgress(strak_commandlines, progressFileName, 100.0)

    # change back directory
    strak_commandlines.append("cd..\n")
    return strak_commandlines


################################################################################
# function that generates a Xoptfoil-batchfile for one strak airfoil
def generate_StrakBatchfiles(params, commandlines):
    for i in range(1, len(params.ReNumbers)):
        batchFileName = "make_%s.bat" % (get_ReString(params.ReNumbers[i]))

        try:
            # create a new file
            outputfile = open(batchFileName, "w+")
        except:
            ErrorMsg('file %s could not be opened' % batchFileName)
            return
        # get commandlines to generate the strak-airfoil
        strak_commandlines = get_strak_commandlines(params, commandlines, i)

        # write commandlines to outputfile
        for element in strak_commandlines:
            outputfile.write(element)

        # close the outputfile
        outputfile.close()


################################################################################
# function that gets the name of the strak-machine-data-file
def get_InFileName(args):

    if args.input:
        inFileName = args.input
    else:
        # use Default-name
        inFileName = '.' + bs + ressourcesPath + bs + strakMachineInputFileName

    my_print("filename for strak-machine input-data is: %s\n" % inFileName)
    return inFileName


################################################################################
# function that gets the filenname of the first polar to merge
def get_workerAction(args):
    if args.work:
        return args.work
    else:
        return None

################################################################################
# function that gets the filenname of the first polar to merge
def get_firstMergePolarFileName(args):
    if args.p1:
        return args.p1
    else:
        return None

################################################################################
# function that gets the filename of the second polar to merge
def get_secondMergePolarFileName(args):
    if args.p2:
        return args.p2
    else:
        return None

################################################################################
# function that gets the filenname of the first polar to merge
def get_mergedPolarFileName(args):
    if args.m:
        return args.m
    else:
        return None

################################################################################
# function that gets the filenname of the first polar to merge
def get_mergeCL(args):
    if args.c:
        return float(args.c)
    else:
        return None

################################################################################
# function that gets arguments from the commandline
def get_Arguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    helptext = "filename of strak-machine input-file (e.g. strak_data)"
    parser.add_argument("-input", "-i", help = helptext)

    helptext = "worker action, e.g. -w merge (to merge two polars)"
    parser.add_argument("-work", "-w", help = helptext)

    helptext = "filename of first polar to merge)"
    parser.add_argument("-p1", help = helptext)

    helptext = "filename of second polar to merge)"
    parser.add_argument("-p2", help = helptext)

    helptext = "filename of merged polar"
    parser.add_argument("-m", help = helptext)

    helptext = "CL-value at which to merge the two polars"
    parser.add_argument("-c", help = helptext)

    # read arguments from the command line
    args = parser.parse_args()

    return (get_InFileName(args),
            get_workerAction(args),
            get_firstMergePolarFileName(args),
            get_secondMergePolarFileName(args),
            get_mergedPolarFileName(args),
            get_mergeCL(args))






def get_ListOfFiles(dirName):
    # create a list of files in the given directory
    listOfFile = listdir(dirName)
    allFiles = list()

    # Iterate over all the entries
    for entry in listOfFile:
        # Create full path
        fullPath = path.join(dirName, entry)
        allFiles.append(fullPath)

    return allFiles


def copyAndSmooth_Airfoil(xfoilWorkerCall, inputFilename, srcName, srcPath, destName, smooth):
    srcfoilNameAndPath = srcPath + bs + srcName + '.dat'

    # first always rename and copy the airfoil
    NoteMsg("Renaming airfoil \'%s\' to \'%s\'" % (srcName, destName))
    change_airfoilname.change_airfoilName(srcfoilNameAndPath, destName + '.dat')

    if (smooth):
        NoteMsg("Smoothing airfoil \'%s\'" % destName)

        # compose system-string for smoothing the airfoil
        systemString = xfoilWorkerCall + " -w smooth -i %s -a %s -o %s" % \
                       (inputFilename, destName +'.dat', destName)

        # execute xfoil-worker / create the smoothed root-airfoil
        system(systemString)

    DoneMsg()



def compose_Polarfilename_T1(Re, NCrit):
    return ("T1_Re%d.%03d_M0.00_N%.1f.txt"\
        % (round_Re(Re)/1000, round_Re(Re)%1000, NCrit))


def compose_Polarfilename_T2(ReSqrt_Cl, NCrit):
    return ("T2_Re%d.%03d_M0.00_N%.1f.txt"\
 % (round_Re(ReSqrt_Cl)/1000, round_Re(ReSqrt_Cl)%1000, NCrit))


def set_PolarDataFromInputFile(polarData, rootPolar, inputFile,
                              airfoilname, Re, idx):
    # set some variables in the polar-header
    polarData.polarName = 'target-polar for airfoil %s' % airfoilname
    polarData.airfoilname = airfoilname
    polarData.polarType = 12
    polarData.Re = Re
    polarData.NCrit = 0.0

    # get operating-conditions from inputfile
    operatingConditions = inputFile.get_OperatingConditions()

    target_values = operatingConditions["target_value"]
    op_points = operatingConditions["op_point"]
    op_modes =  operatingConditions["op_mode"]
    #names = operatingConditions["name"] #FIXME is this neccessary any more ?

    # get the number of op-points
    numOpPoints = len(op_points)

    for i in range(numOpPoints):
        # check if the op-mode is 'spec-cl'
        op_mode = op_modes[i]
        op_point = op_points[i]
        target_value = target_values[i]

        if (op_mode == 'spec-cl'):
            # if op_mode is 'spec-cl', get alpha from root-polar, as we have no
            # alpha-information for this oppoint in the input-file
            alpha = rootPolar.find_alpha_From_CL(op_points[i])
            # get CL, CD
            CL = op_point
            CD = target_value
        else:
            # op-mode is 'spec-al', another interpretation of values is needed
            alpha = op_point
            CL = target_value
            #CD = polarData.find_CD_From_CL(CL) #TODO does not work, needs complete target-polar

        # append only 'spec-cl'-data
        if (op_mode == 'spec-cl'):# TODO append all data
            # append values to polar
            polarData.alpha.append(alpha)
            polarData.CL.append(CL)
            polarData.CD.append(CD)
            try:
                polarData.CL_CD.append(CL/CD)
            except:
                ErrorMsg("CD is 0.0, division by zero!")
            polarData.CDp.append(0.0)
            polarData.Cm.append(0.0)
            polarData.Top_Xtr.append(0.0)
            polarData.Bot_Xtr.append(0.0)

    # Bugfix: The last line of the target-polar-file will not be shown in XFLR5,
    # add a dummy-line here
    polarData.alpha.append(0.0)
    polarData.CL.append(0.0)
    polarData.CD.append(0.0)
    polarData.CL_CD.append(0.0)
    polarData.CDp.append(0.0)
    polarData.Cm.append(0.0)
    polarData.Top_Xtr.append(0.0)
    polarData.Bot_Xtr.append(0.0)


def generate_TargetPolars(params, writeToDisk):
     # clear all previsously generated target polars
    params.target_polars.clear()

    # local variables
    inputFiles = params.inputFiles
    Re = params.ReNumbers
    numTargetPolars = len(Re)
    rootPolar = params.merged_polars[0]

    # get name of the root-airfoil
    airfoilName = params.airfoilNames[0]
    NoteMsg("Generating target polars for airfoil %s..." % airfoilName)

    for i in range(numTargetPolars):

        # get inputfile
        inputFile = inputFiles[i]

        # create new target polar
        targetPolar = polarData()

        # put the necessary data into the polar
        set_PolarDataFromInputFile(targetPolar, rootPolar, inputFile,
                                  airfoilName, Re[i], i)

        # append the new target polar to list of target_polars
        params.target_polars.append(targetPolar)

        if writeToDisk:
            # compose polar-dir
            polarDir = params.buildDir + bs + airfoilName + '_polars'

            # check if output-folder exists. If not, create folder.
            if not path.exists(polarDir):
                makedirs(polarDir)

            # compose filename and path
            polarFileNameAndPath = polarDir + bs + ('target_polar_%s.txt' %\
                                   get_ReString(Re[i]))

            # write polar to file
            targetPolar.write_ToFile(polarFileNameAndPath)

    DoneMsg()


# merge two polar files, the merging-point will be specified as a CL-value.
# generate a new file containing the data of the merged polar
def merge_Polars(polarFile_1, polarFile_2 , mergedPolarFile, mergeCL):
    # import polars from file
    try:
        polar_1 = polarData()
        polar_1.import_FromFile(polarFile_1)
    except:
        ErrorMsg("polarfile \'%s\' could not be imported" % polarFile_1)
        sys.exit(-1)

    try:
        polar_2 = polarData()
        polar_2.import_FromFile(polarFile_2)
    except:
        ErrorMsg("polarfile \'%s\' could not be imported" % polarFile_2)
        sys.exit(-1)

    # merge polars and write to file.
    # lower part (CL_min..mergeCL) comes from polar_1.
    # upper part (mergeCL..CL_max) comes from polar_2.
    # the merged values will be stored in mergedPolar.
    try:
        mergedPolar = polar_2.merge(polar_1, mergeCL, 0)
        mergedPolar.write_ToFile(mergedPolarFile)
    except:
        ErrorMsg("polarfile \'%s\' could not be generated" % mergedPolarFile)
        sys.exit(-1)



class polar_worker:
    def __init__(self, params):
        self.params = params
        self.NCrit = params.NCrit
        self.xfoilWorkerCall = params.xfoilWorkerCall
        self.alpha_Resolution = params.alpha_Resolution
        self.alphaMin_T1 = params.alphaMin
        self.alphaMin_T2 = params.alphaMin
        self.alphaMax_T1 = params.alphaMax
        self.alphaMax_T2 = params.alphaMax
        self.CL_merge = params.CL_merge


    def set_alphaMinMax(self, alphaMin_T1, alphaMax_T1, alphaMin_T2, alphaMax_T2):
        self.alphaMin_T1 = alphaMin_T1
        self.alphaMax_T1 = alphaMax_T1
        self.alphaMin_T2 = alphaMin_T2
        self.alphaMax_T2 = alphaMax_T2


    def generate_PolarCreationFile(self, fileName, polarType, ReList):
        if polarType == 'T1':
            inputFilename = get_PresetInputFileName(T1_polarInputFile)
            alphaMin = self.alphaMin_T1
            alphaMax = self.alphaMax_T1
        elif polarType == 'T2':
            inputFilename = get_PresetInputFileName(T2_polarInputFile)
            alphaMin = self.alphaMin_T2
            alphaMax = self.alphaMax_T2
        else:
            ErrorMsg("unknown polarType : %s" % polarType)

        # read template file
        fileContent = f90nml.read(inputFilename)

        # get polar generation options from dictionary
        polarGenerationOptions = fileContent['polar_generation']

        # get oppoint range, example: op_point_range = -4, 12, 0.1
        op_point_range = polarGenerationOptions['op_point_range']

        # set alpha min/max
        op_point_range[0] = alphaMin
        op_point_range[1] = alphaMax

        # writeback
        polarGenerationOptions['op_point_range'] = op_point_range

        # add list of Re-numbers
        polarGenerationOptions['polar_reynolds'] = ReList

        # write new file
        f90nml.write(fileContent, fileName, True)


    def get_polarfileName_T1(self, Re):

        # create polar-file-Name T1-polar from Re-Number
        polarfileName_T1 = "T1_Re%d.%03d_M0.00_N%.1f.txt"\
                          % (round_Re(Re)/1000, round_Re(Re)%1000, self.NCrit)

        return polarfileName_T1


    def get_polarfileName_T2(self, ReSqrt_Cl):
        # create polar-file-Name T2-polar from Re-Number
        polarfileName_T2 = "T2_Re%d.%03d_M0.00_N%.1f.txt"\
                 % (round_Re(ReSqrt_Cl)/1000, round_Re(ReSqrt_Cl)%1000, self.NCrit)

        return polarfileName_T2


    def get_missingPolars(self, airfoilName, ReList_T1, ReList_T2):
        # compose polar-dir
        polarDir = '..' + bs + buildPath + bs + airfoilName + '_polars'
        ReList_T1_missing = []
        ReList_T2_missing = []

        for Re in ReList_T1:
            fileName = polarDir + bs + self.get_polarfileName_T1(Re)
            if (not exists(fileName)):
                ReList_T1_missing.append(Re)

        for Re in ReList_T2:
            fileName = polarDir + bs + self.get_polarfileName_T2(Re)
            if (not exists(fileName)):
                ReList_T2_missing.append(Re)

        return (ReList_T1_missing, ReList_T2_missing)


    def import_polars(self, airfoilName, ReList_T1, ReList_T2):
        # import polars of airfoil
        NoteMsg("importing polars for airfoil %s..." % airfoilName)

        merged_polars = []
        num = len(ReList_T1)
        polarDir = '..' + bs + buildPath + bs + airfoilName + '_polars'

        if (num != len(ReList_T2)):
            ErrorMsg("number of list elements for T1 and T2 must be equal!")
            return None

        for idx in range(num):
            Re_T1 = ReList_T1[idx]
            Re_T2 = ReList_T2[idx]

            # get filename of merged polar
            mergedPolarfileName = polarDir + bs + ('merged_polar_%3s.txt' %\
                                      get_ReString(Re_T2))

            # check if merged polar already exists as a file
            if exists(mergedPolarfileName):
                # yes, import from file
                mergedPolar = polarData()
                mergedPolar.import_FromFile(mergedPolarfileName)
                mergedPolar.restore_mergeData(self.CL_merge, Re_T1)
            else:
                # does not exist. Read corresponding T1 polar from file
                fileName = polarDir + bs + self.get_polarfileName_T1(Re_T1)
                newPolar_T1 = polarData()
                newPolar_T1.import_FromFile(fileName)

                # read corresponding T2 polar from file
                fileName = polarDir + bs + self.get_polarfileName_T2(Re_T2)
                newPolar_T2 = polarData()
                newPolar_T2.import_FromFile(fileName)

                # merge T1/T2 polars at Cl_merge
                mergedPolar = newPolar_T2.merge(newPolar_T1,
                                                self.CL_merge, Re_T1)

                # write merged polar to file (with original alpha resolution
                # to save disk space)
                mergedPolar.write_ToFile(mergedPolarfileName)

            # change resolution of alpha for accurate conversion between CL /CD/ alpha
            mergedPolar.set_alphaResolution(self.alpha_Resolution)

            # analyze merged polar
            mergedPolar.analyze(self.params)

            # add merged polar to list
            merged_polars.append(mergedPolar)

        DoneMsg()
        return merged_polars


    def generate_initialPolar(self, airfoilName, Re_T1, Re_T2):
        initial_airfoilName = 'initial_' + airfoilName

        # create a dummy airfoil, which is a copy of the root-airfoil
        # just to have the polars in a different folder
        change_airfoilname.change_airfoilName(airfoilName + '.dat',
                                          initial_airfoilName + '.dat')

        # generate missing polars now
        generate_polars(self, initial_airfoilName, [Re_T1], [Re_T2])


    def generate_polars(self, airfoilName, ReList_T1, ReList_T2):
        # get list of T1 polars that have to be generated
        (ReList_T1_missing, ReList_T2_missing) =\
             self.get_missingPolars(airfoilName, ReList_T1, ReList_T2)

        if (len(ReList_T1_missing) > 0):
            my_print("generating missing T1 polars for airfoil %s..." % airfoilName)
            # create inputfile for worker
            T1_fileName = 'iPolars_T1_%s.txt' % airfoilName
            self.generate_PolarCreationFile(T1_fileName, 'T1', ReList_T1_missing)

            # compose string for system-call of XFOIL-worker for T1-polar generation
            systemString = self.xfoilWorkerCall + " -i \"%s\" -w polar -a \"%s\"" %\
                                  (T1_fileName, airfoilName+'.dat')
            system(systemString)
            DoneMsg()


        if (len(ReList_T2_missing) > 0):
            my_print("generating missing T2 polars for airfoil %s..." % airfoilName)
            # create inputfile for worker
            T2_fileName = 'iPolars_T2_%s.txt' % airfoilName
            self.generate_PolarCreationFile(T2_fileName, 'T2', ReList_T2_missing)

            # compose string for system-call of XFOIL-worker for T1-polar generation
            systemString = self.xfoilWorkerCall + " -i \"%s\" -w polar -a \"%s\"" %\
                                  (T2_fileName, airfoilName+'.dat')
            system(systemString)
            DoneMsg()


    def import_strakPolars(self):
        NoteMsg("trying to import polars of previous strak airfoils as a reference")

        # the first two are entries are dummy entries, one for root polar,
        # one for first strak airfoil
        # the first strak airfoil uses the polar of root airfoil as a reference
        strak_polars = [None, None]

        for i in range(1, len(self.params.ReNumbers)-1):
            # get name of the strak-airfoil
            strakFoilName = self.params.airfoilNames[i]

            # compose polar-dir of strak-airfoil-polars
            polarDir = '.' + bs + strakFoilName + '_polars'
            fileName = "merged_polar_%s.txt" % get_ReString(self.params.ReNumbers[i+1])
            polarFileNameAndPath = polarDir + bs + fileName

            try:
                newPolar = polarData()
                newPolar.import_FromFile(polarFileNameAndPath)
                strak_polars.append(newPolar)
                DoneMsg()
            except:
                strak_polars.append(None)
                ErrorMsg("failed")
                pass

        DoneMsg()
        return strak_polars


class reference_GeoParameters:
    def __init__(self):
        # reference data, this data may have to changed depending on the type of strak
        Re_ref =      150000
        thick_ref =     7.60
        thickPos_ref = 27.83
        camb_ref =      1.46
        cambPos_ref =  40.04

        self.Re_ratio       = [      40000/Re_ref,       60000/Re_ref,      100000/Re_ref,      138000/Re_ref,      150000/Re_ref]
        self.thick_ratio    = [    6.90/thick_ref ,    7.10/thick_ref,     7.41/thick_ref,     7.55/thick_ref,     7.60/thick_ref]
        self.thickPos_ratio = [23.22/thickPos_ref, 25.03/thickPos_ref, 26.83/thickPos_ref, 27.83/thickPos_ref, 27.83/thickPos_ref]
        self.camb_ratio     = [     1.49/camb_ref,      1.47/camb_ref,      1.47/camb_ref,      1.46/camb_ref,      1.46/camb_ref]
        self.cambPos_ratio  = [ 28.63/cambPos_ref,  32.63/cambPos_ref,  36.34/cambPos_ref,  39.54/cambPos_ref,  40.04/cambPos_ref]


    def get(self, Re_ratio):

        # limit to list boundaries
        if (Re_ratio < self.Re_ratio[0]):
            Re_ratio = self.Re_ratio[0]
        elif (Re_ratio > self.Re_ratio[-1]):
            Re_ratio = self.Re_ratio[-1]

        # calculate corresponding perturb according to Re-Factor
        thick_ratio = np.interp(Re_ratio, self.Re_ratio, self.thick_ratio)
        thickPos_ratio = np.interp(Re_ratio, self.Re_ratio, self.thickPos_ratio)
        camb_ratio = np.interp(Re_ratio, self.Re_ratio, self.camb_ratio)
        cambPos_ratio = np.interp(Re_ratio, self.Re_ratio, self.cambPos_ratio)

        return (thick_ratio, thickPos_ratio, camb_ratio, cambPos_ratio)


class strak_machine:
    def __init__(self, parameterFileName):
        global print_disabled

        # check working-directory, have we been started from "scripts"-dir? (Debugging)
        currentDir = getcwd()
        if (currentDir.find("scripts")>=0):
            self.startedFromScriptsFolder = True
            chdir("..")
        else:
            self.startedFromScriptsFolder = False

        # get strak-machine-parameters from file
        self.params = strak_machineParams(parameterFileName)

        # get current working dir
        self.params.workingDir = getcwd()

        # check if output-folder exists. If not, create folder.
        if not path.exists(buildPath):
            makedirs(buildPath)

        # check if airfoil-folder exists. If not, create folder.
        if not path.exists(buildPath + bs + airfoilPath):
            makedirs(buildPath + bs + airfoilPath)

        # change working-directory to output-directory
        chdir(self.params.workingDir + bs + buildPath)

        # get current working dir again
        self.params.buildDir = getcwd()

        # generate rootfoil from seedfoil (will perform airfoil assessment)
        rootfoilName = self.generate_rootfoil()

        # copy root-foil to airfoil-folder, as it can be used
        # as the root airfoil without optimization
        systemString = ("copy %s %s" + bs + "%s\n\n") % \
        (rootfoilName +'.dat', airfoilPath, rootfoilName + '.dat')
        system(systemString)

        # after root-airfoil data was generated, we can read geo parameters.
        # in case there are no geo parameters, we can initially create them using
        # data of root airfoil
        self.params.read_geoParameters()

        # afer we have the root-airfiol and, we can init polar generation and
        # get more specific alpha min/max for generating further polars
        self.init_polarGeneration()

        # check if all seedfoils are there, generate missing seedfoils
        self.check_andGenerateSeedfoils()

        # check existing polars and create them, if missing
        self.check_andGeneratePolars()

        # import existing polars
        self.import_polars()

        # calculate target-values for the main op-points
        self.params.calculate_MainTargetValues()

        # read input files / create new ones
        self.read_InputFiles()

        # generate target polars and write to file
        generate_TargetPolars(self.params, True)

        # generate Xoptfoil command-lines
        commandlines = generate_Commandlines(self.params)

        # change working-directory
        chdir(".." + bs)

        if (self.params.generateBatch == True):
            NoteMsg('Generating batchfiles')
            generate_Batchfile(self.params.batchfileName, commandlines)
            generate_StrakBatchfiles(self.params, commandlines)
            DoneMsg()

        # change working-directory to output-directory
        chdir(self.params.workingDir + bs + buildPath)

        # create an instance of polar graph
        self.graph = polarGraph()

        NoteMsg('Strak Machine was successfully started!\n')

        # disable further console print output
        print_disabled = True


    def read_InputFiles(self):
        NoteMsg("Reading inputfiles...")

        # clear are previsously generated inputfiles and -names
        self.params.inputFiles = []
        self.params.inputFileNames = []

        # first generate inputfile names
        for Re in self.params.ReNumbers:
            # generate inputfilename from Re-number
            inputFilename = self.params.xoptfoilTemplate + ("_%s.txt" % get_ReString(Re))

            # multi-pass-optimization: generate input-filenames for intermediate-airfoils
            for n in range(1, (self.params.optimizationPasses)):
                name = inputFilename
                name = remove_suffix(name, ".txt")
                name = name + ("_%d.txt" % n)
                self.params.inputFileNames.append(name)

            # append name of inputfile for final airfoil
            self.params.inputFileNames.append(inputFilename)

        for idx in range(len(self.params.ReNumbers)):
            # create input-file and append to list
            new_inputFile = inputFile(self.params)
            self.params.inputFiles.append(new_inputFile)
            try:
                # read contents of existing inputfile, if possible
                new_inputFile.read_FromFile(self.get_inputfileName(idx))
            except:
                # could not read inputfile, create new one
                self.generate_InputFile(idx, True)

        DoneMsg()


    def check_andGenerateSeedfoils(self):
        num = len(self.params.ReNumbers)
        self.params.seedfoilNames = []

        for idx in range(1, num):
            Re = self.params.ReNumbers[idx]
            seedfoilName = 'seed_%s' % get_ReString(Re)
            self.params.seedfoilNames.append(seedfoilName)

            if (not exists(seedfoilName + '.dat')):
                self.generate_seedfoil(idx)


    # this function will initialize the polar worker, create and import the
    # first polar of the root airfoil and determine alpha min and max values for
    # further polar generation
    def init_polarGeneration(self):
        # create polar worker
        self.polarWorker = polar_worker(self.params)

        # some local variables
        rootfoilName = self.params.airfoilNames[0]
        dummy_airfoilName = 'dummy'
        Re_T1 = [self.params.maxReNumbers[0]]
        Re_T2 = [self.params.ReNumbers[0]]

        # create a dummy airfoil, which is a copy of the root-airfoil with
        # another name, just to have the polars in a different folder
        change_airfoilname.change_airfoilName(rootfoilName + '.dat',
                                              dummy_airfoilName + '.dat')

        # generate missing polars for this airfoil now
        self.polarWorker.generate_polars(dummy_airfoilName, Re_T1, Re_T2)

        # import polar and analyse (caution, return value is a list!)
        polarList =\
            self.polarWorker.import_polars(dummy_airfoilName, Re_T1, Re_T2)

        # get polar from list
        initialPolar = polarList[0]

        # determine alphaMin/-Max of the initial polar
        (alphaMin, alphaMax) = initialPolar.get_alphaMin_alphaMaxLift()

        # expand alpha min/max
        alphaMinExpand = (alphaMax - alphaMin) * 0.05
        alphaMaxExpand = (alphaMax - alphaMin) * 0.08
        alphaMerge = initialPolar.get_alphaMerge()

        # set min/max alpha for T1 / T2
        alphaMin_T1 = round((alphaMin - alphaMinExpand), 1)
        alphaMin_T2 = round((alphaMerge - alphaMinExpand), 1)
        alphaMax_T1 = round((alphaMerge + alphaMaxExpand), 1)
        alphaMax_T2 = round((alphaMax + alphaMaxExpand), 1)

        # set new values in polar worker for further polar generation
        self.polarWorker.set_alphaMinMax(alphaMin_T1, alphaMax_T1,
                                         alphaMin_T2, alphaMax_T2)


    def check_andGeneratePolars(self):
        # create further necessary polars of root airfoil
        self.polarWorker.generate_polars(self.params.airfoilNames[0],
                          self.params.maxReNumbers, self.params.ReNumbers)

        # generate polars of seedfoils
        num = len(self.params.ReNumbers)

        for idx in range(1, num):
            Re_T1 = [self.params.maxReNumbers[idx]]
            Re_T2 = [self.params.ReNumbers[idx]]

            self.polarWorker.generate_polars(self.params.seedfoilNames[idx-1],
                                             Re_T1, Re_T2)

        # generate polar-creation files of strakfoils that will be used
        # later in the commandlines
        num = len(self.params.ReNumbers)
        for i in range(1, num):
            airfoilName = self.params.airfoilNames[i]
            T1_fileName = 'iPolars_T1_%s.txt' % airfoilName
            T2_fileName = 'iPolars_T2_%s.txt' % airfoilName

            if (i<(num-1)):
                # generate two polars
                Re_T1 = [self.params.maxReNumbers[i], self.params.maxReNumbers[i+1]]
                Re_T2 = [self.params.ReNumbers[i], self.params.ReNumbers[i+1]]
            else:
                # generate one polar
                Re_T1 = [self.params.maxReNumbers[i]]
                Re_T2 = [self.params.ReNumbers[i]]

            self.polarWorker.generate_PolarCreationFile(T1_fileName, 'T1', Re_T1)
            self.polarWorker.generate_PolarCreationFile(T2_fileName, 'T2', Re_T2)


    def import_polars(self):
        # import polars of root airfoil
        self.params.merged_polars =\
             self.polarWorker.import_polars(self.params.airfoilNames[0],
                             self.params.maxReNumbers, self.params.ReNumbers)

        # import polars of seedfoils
        num = len(self.params.ReNumbers)
        self.params.seedfoil_polars = []

        for idx in range(1, num):
            Re_T1 = [self.params.maxReNumbers[idx]]
            Re_T2 = [self.params.ReNumbers[idx]]

            merged_polars =\
                self.polarWorker.import_polars(self.params.seedfoilNames[idx-1],
                                               Re_T1, Re_T2)
            # worker call will return list, containing only one element
            self.params.seedfoil_polars.append(merged_polars[0])

        # import polars of strak-airfoils, if they exist
        self.params.strak_polars = self.polarWorker.import_strakPolars()


    def generate_MultiPassInputFiles(self, airfoilIdx, writeToDisk, inputFile):
        i = airfoilIdx

        # get default-value of initialPerturb from template
        initialPerturb = inputFile.get_InitialPerturb()

        if (self.params.adaptInitialPerturb and (i>0)):
            # factor calculated to Re-number of root-airfoil
            ReFactor = self.params.ReNumbers[i] / self.params.ReNumbers[0]

            # calculate initial perturb now.
            initialPerturb = inputFile.calculate_InitialPerturb(self.params.ReNumbers[i],
                              ReFactor)

        # get Default-value for max iterations
        maxIterationsDefault = inputFile.get_maxIterations()

        # multi-pass-optimization:
        # generate input-files for intermediate strak-airfoils
        for n in range(0, self.params.optimizationPasses):
            iFileIndex = i*(self.params.optimizationPasses) + n

            # set input-file name
            iFile = self.params.inputFileNames[iFileIndex]

            # set max number of iterations
            maxIterations = self.params.maxIterations[n]
            if (maxIterations == 0):
                maxIterations = maxIterationsDefault
            inputFile.set_maxIterations(maxIterations)

            # set initialPerturb
            inputFile.set_InitialPerturb(initialPerturb)

            # set shape_functions
            inputFile.set_shape_functions(self.params.shape_functions[n])

            if writeToDisk:
                #NCrit = inputFile.get_Ncrit()#FIXME Debug
                # physically create the file
                inputFile.write_ToFile(iFile)

            # reduce initial perturb for the next pass
            initialPerturb = initialPerturb*0.5


    def create_new_inputFile(self, i):
         # get strak-polar
        if (i>0):
            # use polar of respective seedfoil to initialize target values
            strakPolar = self.params.seedfoil_polars[i-1]
        else:
            # use polar of rootfoil to initialize target values
            strakPolar = self.params.merged_polars[i]

        # create new inputfile from template
        newFile = inputFile(self.params)

        # get the target-values
        targets = self.params.targets
        CL_pre_maxLift = targets["CL_pre_maxLift"][i]

        # generate op-points in the range CL_min..CL_max
        # the CL0-oppoint will be inserted later, so generate numOpPoints-1
        newFile.generate_OpPoints(self.params.numOpPoints-1, self.params.CL_min,
                               CL_pre_maxLift)

        # distribute main opPoints, also set the target-values
        newFile.distribute_MainOpPoints(targets, i)

        # insert additional opPoints (if there are any):
        if len(self.params.additionalOpPoints[0])>0:
            newFile.insert_AdditionalOpPoints(self.params.additionalOpPoints[0])
            # The below line will use "adjusted" additional oppoints for each
            # strak-polar. Sometimes this behaviour is not desired, bcause this means
            # that the CL-value is changed.
            #newFile.insert_AdditionalOpPoints(self.params.additionalOpPoints[i])

        # now distribute the opPoints between the main opPoints and additional
        # oppoints equally
        newFile.distribute_IntermediateOpPoints()

        # initialize all target-valuesnow
        newFile.init_TargetValues(self.params, strakPolar)

        return newFile


    def generate_InputFile(self, airfoilIdx, writeToDisk):
        # get number of airfoils
        num = len(self.params.ReNumbers)

        # check airfoilIdx
        if (airfoilIdx >= num):
            ErrorMsg("Invalid airfoilIdx %d" % airfoilIdx)
            return
        else:
            i = airfoilIdx
            NoteMsg("Generating inputfile(s) for airfoil %s" % self.params.airfoilNames[i])

        # create new file
        newFile = self.create_new_inputFile(i)

        # set NCrit
        newFile.set_NCrit(self.params.NCrit)

        # adapt reynolds()-values, get strak-polar
        strakPolar = self.params.merged_polars[i]
        newFile.adapt_ReNumbers(strakPolar)

        # insert oppoint for alpha @ CL = 0
        newFile.insert_alpha0_oppoint(self.params, strakPolar,i)

        # insert oppoints for alpha @maxGlide, maxLift
        newFile.insert_alphaMaxGlide_oppoint(self.params, i)
        newFile.insert_alphaMaxLift_oppoint(self.params, i)

        # get geo params of single airfoil
        airfoil_geoParams =\
                self.params.get_geoParamsOfAirfoil(i, self.params.geoParams)

        # separate tuple
        (thick, thickPos, camb, cambPos) = airfoil_geoParams

        # insert geo targets into inputfile
        newFile.set_geometryTargets((camb, thick))

        # get reversals of root airfoil
        (reversals_top, reversals_bot) = self.params.get_rootReversals()
        newFile.set_reversals(reversals_top, reversals_bot)


        # multi-pass-optimization:
        # generate input-files for intermediate strak-airfoils
        self.generate_MultiPassInputFiles(airfoilIdx, writeToDisk, newFile)

        # append only input-file of final strak-airfoil to params
        self.params.inputFiles[i] = newFile

    def generate_rootfoil(self):
        # get name of seed-airfoil
        seedFoilName = self.params.seedFoilName

        # get name of root-airfoil
        rootfoilName = self.params.airfoilNames[0]

        # get the path where the seed-airfoil can be found
        srcPath = "." + bs

        inputFilename = ".." + bs + ressourcesPath + bs + smoothInputFile

        # copy and smooth the airfoil, also rename
        copyAndSmooth_Airfoil(self.params.xfoilWorkerCall, inputFilename,
           seedFoilName, srcPath, rootfoilName, self.params.smoothSeedfoil)

        return rootfoilName

    def generate_seedfoil(self, airfoilIdx):
        rootfoilName = self.params.airfoilNames[0] + '.dat'
        # determine name of seedfoil according to Re-number
        seedfoilPrefix = 'seed_%s' % get_ReString(self.params.ReNumbers[airfoilIdx])
        seedfoilName = seedfoilPrefix + '.dat'
        # get geoParams for the airfoil
        geoParams = self.params.get_geoParamsOfAirfoil(airfoilIdx, self.params.geoParams)

        # unpack tuple
        (thick, thickPos, camb, cambPos) = geoParams

        # worker commands are:
        # t=yy Set thickness to xx%
        # xt=xx Set location of maximum thickness to xx% of chord
        # c=yy Set camber to xx%
        # xc=xx

        # set thickness position
        systemString = ("%s -w set xt=%.2f -a %s -o %s\n\n" %\
        (self.params.xfoilWorkerCall, thickPos, rootfoilName, seedfoilPrefix))
        system(systemString)

        # normalize
        systemString = ("%s -w norm -a %s -o %s\n\n" %\
        (self.params.xfoilWorkerCall, seedfoilName, seedfoilPrefix))
        system(systemString)

        # set thickness
        systemString = ("%s -w set t=%.2f -a %s -o %s\n\n" %\
        (self.params.xfoilWorkerCall, thick, seedfoilName, seedfoilPrefix))
        system(systemString)

##        # normalize
##        systemString = ("%s -w norm -a %s -o %s\n\n" %\
##        (self.params.xfoilWorkerCall, seedfoilName, seedfoilPrefix))
##        system(systemString)

        # set camber
        systemString = ("%s -w set c=%.2f -a %s -o %s\n\n" %\
        (self.params.xfoilWorkerCall, camb, seedfoilName, seedfoilPrefix))
        system(systemString)

##        # normalize
##        systemString = ("%s -w norm -a %s -o %s\n\n" %\
##        (self.params.xfoilWorkerCall, seedfoilName, seedfoilPrefix))
##        system(systemString)

        # set camber position
        systemString = ("%s -w set xc=%.2f -a %s -o %s\n\n" %\
        (self.params.xfoilWorkerCall, cambPos, seedfoilName, seedfoilPrefix))
        system(systemString)

##        # normalize
##        systemString = ("%s -w norm -a %s -o %s\n\n" %\
##        (self.params.xfoilWorkerCall, seedfoilName, seedfoilPrefix))
##        system(systemString)

        NoteMsg("seedfoil %s was successfully generated" % seedfoilName)


    def exit_action(self, value):
        global print_disabled
        print_disabled = True

        return value


    def entry_action(self, airfoilIdx):
        global print_disabled
        print_disabled = False

        # check if airfoilIdx exceeds number of airfoils handled by parameters
        if airfoilIdx > len(self.params.ReNumbers):
            ErrorMsg("set_targetValues: invalid airfoilIdx :%d" % airfoilIdx)
            return self.exit_action(-1)


    def plot_diagram(self, diagramType, ax, x_limits, y_limits):
        # draw the graph
        self.graph.draw_diagram(self.params, diagramType, ax, x_limits, y_limits)


    def get_airfoilNames(self):
        return self.params.airfoilNames


    def set_visiblePolars(self, visibleFlags):
        self.params.set_visibleFlags(visibleFlags)


    def set_referencePolarsVisibility(self, referenceFlag):
        self.params.set_referenceFlag(referenceFlag)


    def set_activeTargetPolarIdx(self, airfoilIdx):
        self.params.set_activeTargetPolarIdx(airfoilIdx)


    def get_targetValues(self, airfoilIdx):
        self.entry_action(airfoilIdx)

        targetValues = []
        inputFile = self.params.inputFiles[airfoilIdx]
        operatingConditions = inputFile.get_OperatingConditions()
        num = len(operatingConditions['op_point'])

        for idx in range(num):
            # type of op-point
            mode = operatingConditions['op_mode'][idx]
            # x-value of op-point
            oppoint = operatingConditions['op_point'][idx]
            # target value of op-point
            target = operatingConditions['target_value'][idx]
            try:
                # weighting of op-point
                weighting = operatingConditions['weighting'][idx]
            except:
                weighting = None

            targetValues.append({"type": mode, "oppoint" : oppoint,
                                 "target" : target, "weighting" : weighting})

        return self.exit_action(targetValues)


    def set_targetValues(self, airfoilIdx, targetValues):
        self.entry_action(airfoilIdx)

        inputFile = self.params.inputFiles[airfoilIdx]
        operatingConditions = inputFile.get_OperatingConditions()
        num = len(operatingConditions['op_point'])

        for idx in range(num):
            target = targetValues[idx]
            # type of op-point
            operatingConditions['op_mode'][idx] = target["type"]
            # x-value of op-point
            operatingConditions['op_point'][idx] = target["oppoint"]
            # target value of op-point
            operatingConditions['target_value'][idx] = target["target"]
            # weighting of op-point
            try:
                weighting = operatingConditions['weighting']
            except:
                # init weighting
                emptyList = []
                for i in range(num):
                    emptyList.append(None)
                    operatingConditions['weighting'] = emptyList

            # weighting of op-point
            operatingConditions['weighting'][idx] = target["weighting"]


        # writeback operating-conditions
        inputFile.set_OperatingConditions(operatingConditions)

        return self.exit_action(0)


    def get_geoParams(self, airfoilIdx):
        self.entry_action(airfoilIdx)
        geoParams = self.params.get_geoParameters(airfoilIdx)
        return self.exit_action(geoParams)


    def set_geoParams(self, airfoilIdx, geoParams):
        self.entry_action(airfoilIdx)
        result = self.params.set_geoParameters(airfoilIdx, geoParams)
        return self.exit_action(result)


    def set_screenParams(self, width, height):
        global fs_infotext
        global fs_legend
        global fs_axes
        global fs_ticks
        global fs_weightings
        global lw_targetPolar
        global lw_referencePolar
        global ms_oppoint
        global ms_target
        global scaled

        if (scaled == False):
            # scale font sizes (1920 being default screen width)
            scaleFactor = int(width/1920)
            if (scaleFactor < 1):
                scaleFactor = 1

            fs_infotext = fs_infotext * scaleFactor
            fs_legend = fs_legend * scaleFactor
            fs_axes = fs_axes * scaleFactor
            fs_ticks = fs_ticks * scaleFactor
            fs_weightings = fs_weightings * scaleFactor
            lw_targetPolar = lw_targetPolar * scaleFactor
            lw_referencePolar = lw_referencePolar * scaleFactor
            ms_oppoint = ms_oppoint * scaleFactor
            ms_target = ms_target * scaleFactor

            self.params.scaleFactor = scaleFactor
            scaled = True

    def update_targetPolars(self):
        try:
            # generate target polars and write to file
            generate_TargetPolars(self.params, True)
            NoteMsg("TargetPolars were updated")
        except:
            ErrorMsg("Unable to generate target polars")


    def get_inputfileName(self, airfoilIdx):
        idx = ((airfoilIdx + 1) * self.params.optimizationPasses) - 1
        fileName = self.params.inputFileNames[idx]
        return fileName


    def load(self, airfoilIdx):
        self.entry_action(airfoilIdx)
        try:
            # get input file from params
            inputFile = self.params.inputFiles[airfoilIdx]
            fileName = self.get_inputfileName(airfoilIdx)
            inputFile.read_FromFile(fileName)
        except:
            ErrorMsg("Unable to load input-file %s" % fileName)
            return self.exit_action(-1)

        # read geometry params of the airfoil from parameterfile
        result = self.params.read_geoParamsfromFile(airfoilIdx)
        return self.exit_action(result)


    def save(self, airfoilIdx):
        self.entry_action(airfoilIdx)

        try:
            # get input file from params
            inputFile = self.params.inputFiles[airfoilIdx]
            # generate input-files for intermediate strak-airfoils
            self.generate_MultiPassInputFiles(airfoilIdx, True, inputFile)
        except:
            ErrorMsg("Unable to save input-file %s" % self.get_inputfileName(airfoilIdx))
            return self.exit_action(-1)

        # write geometry params of the airfoil to parameterfile
        result = self.params.write_geoParamsToFile(airfoilIdx)

        # generate seedfoil now
        if result == 0:
            result = self.generate_seedfoil(airfoilIdx)

        return self.exit_action(result)


    def reset(self, airfoilIdx):
        self.entry_action(airfoilIdx)
        fileName = self.get_inputfileName(airfoilIdx)

        try:
            self.generate_InputFile(airfoilIdx, False)
        except:
            ErrorMsg("Unable to reset input-file %s" % fileName)
            return self.exit_action(-1)

        result = self.params.init_geoParams(airfoilIdx)
        return self.exit_action(result)

################################################################################
# Main program
if __name__ == "__main__":
    init()

    # get command-line-arguments or user-input
    (strakDataFileName, workerAction, polarFile_1, polarFile_2,
      mergedPolarFile, mergeCL) = get_Arguments()

    # decide what action to perform.
    if (workerAction == 'merge'):
        # do nothing else but merging the polars
        merge_Polars(polarFile_1, polarFile_2 , mergedPolarFile, mergeCL)
        exit(0)


