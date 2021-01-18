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

# imports
import xml.etree.ElementTree as ET
import argparse
import sys
from json import load
from os import listdir, path, system, makedirs, chdir, getcwd
from matplotlib import pyplot as plt
from matplotlib import image as mpimg
from math import pi, sin
import numpy as np
import f90nml
from copy import deepcopy
from colorama import init
from termcolor import colored
import change_airfoilname

# paths and separators
bs = "\\"
ressourcesPath = 'ressources'
buildPath = 'build'
airfoilPath = 'airfoils'
scriptPath = 'scripts'
exePath = 'bin'

# fixed filenames
pythonInterpreterName = "python"
strakMachineName = "strak_machineV2"
xfoilWorkerName = "xfoil_worker"
xoptfoilName = "xoptfoil-jx"
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"
airfoilComparisonName = "best_airfoil"
showStatusName = "show_status"
logoName = 'strakmachine.jpg'
strakMachineInputFileName = 'strakdata.txt'
T1_polarInputFile = 'iPolars_T1.txt'
T2_polarInputFile = 'iPolars_T2.txt'
smoothInputFile = 'iSmooth.txt'
# filename of progress-file
progressFileName = "progress.txt"


# fonts
csfont = {'fontname':'Segoe Print'}

# number of decimals in the generated input-files
CL_decimals = 4 # lift
CD_decimals = 6 # drag
CL_CD_decimals = 2 # lift/drag
AL_decimals = 4 # alpha
PER_decimals = 6

# fontsizes
fs_infotext = 9
fs_legend = 8

# colours
cl_infotext = 'aqua'
cl_polar_change = 'orange'
cl_T1_polar = 'g'
cl_T2_polar = 'b'

# styles
opt_point_style_root = 'y.'
opt_point_style_strak = 'y-'
ls_targetPolar = 'solid'
lw_targetPolar = 0.6
ls_strakPolar = 'dashdot'
lw_strakPolar  = 0.4


################################################################################
#
# helper functions to put colored messages
#
################################################################################
def ErrorMsg(message):
    print(colored(' Error: ', 'red') + message)

def WarningMsg(message):
    print(colored(' Warning: ', 'yellow') + message)

def NoteMsg(message):
    print(colored(' Note: ', 'cyan') + message)

def DoneMsg():
    print("Done.\n")


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


def sigmoid(z):
    return 1/(1 + np.exp(-z))

# get the name and absolute path of an template xoptfoil-input-file, that
# resides in the 'presets'-folder.
def get_PresetInputFileName(xoptfoilTemplate, params):
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
        self.presetInputFileName = get_PresetInputFileName(params.xoptfoilTemplate,
                                                           params)

        # read input-file as a Fortan namelist
        self.values = f90nml.read(self.presetInputFileName)
        #operatingConditions = self.values["operating_conditions"]#Debug
        #print (operatingConditions)#Debug

        # clean-up file
        #self.remove_DeactivatedOpPoints() #TODO remove


    def __del__(self):
        class_name = self.__class__.__name__
        #print (class_name, "destroyed")#Debug


    # prints all op-points for debugging-purposes
    def print_OpPoints(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]

        print(opPointNames)
        print(opPoints)
        print(targetValues)


    # writes contents to file, using f90nnml-parser
    def write_ToFile(self, fileName):
        # delete 'name'
        operatingConditions = self.values["operating_conditions"]
        operatingConditionsBackup = operatingConditions.copy()
        del(operatingConditions['name'])
        self.values["operating_conditions"] = operatingConditions

        # write to file
        print("writing input-file %s..." % fileName)
        f90nml.write(self.values, fileName, True)

        # restore 'name'
        self.values["operating_conditions"] = operatingConditionsBackup.copy()
        DoneMsg()


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


    # deletes the key 'geometry_targets' in dictionary
    def clear_GeoTargets(self):
        if 'geometry_targets' in self.values:
            del self.values['geometry_targets']


    # removes an op-Point if weighting is beyond a certain limit
    def remove_DeactivatedOpPoints(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # make a copy
        newOperatingConditions = operatingConditions.copy()

        # clear all opPoints of the copy
        self.delete_AllOpPoints(newOperatingConditions)

       # walk through the opPoints
        for idx in range(len(operatingConditions["weighting"])):
            # get OpPoint-weight

            if (operatingConditions["weighting"][idx] >= 0.001):
                # copy this opPoint to the new operating-conditions
                newOperatingConditions["name"].append(operatingConditions["name"][idx])
                newOperatingConditions["op_mode"].append(operatingConditions["op_mode"][idx])
                newOperatingConditions["op_point"].append(operatingConditions["op_point"][idx])
                newOperatingConditions["optimization_type"].append(operatingConditions["optimization_type"][idx])
                newOperatingConditions["target_value"].append(operatingConditions["target_value"][idx])
                newOperatingConditions["weighting"].append(operatingConditions["weighting"][idx])
                newOperatingConditions['noppoint'] = newOperatingConditions['noppoint'] + 1

        # write-back operatingConditions
        self.values["operating_conditions"] = newOperatingConditions


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

    def set_shape_functions (self, shape_functions):
        optimization_options = self.values["optimization_options"]
        optimization_options['shape_functions'] = shape_functions

    def set_maxIterations(self, newValue):
        particle_swarm_options = self.values["particle_swarm_options"]
        particle_swarm_options['pso_maxit'] = newValue

    def get_maxIterations(self):
        particle_swarm_options = self.values["particle_swarm_options"]
        return particle_swarm_options['pso_maxit']

    def calculate_InitialPerturb(self, Re, ReDiff, ReFactor):
        # TODO: not sure what is the best algorithm:
        # use Difference in Re or use Re-factor?
        ReDiffList =  [(70000/4), 70000]
        perturbList_ReDiff = [(0.0025/2), 0.0025]

        ReFactorList = [(150/220),(80/150)]
        perturbList_ReFactor = [(0.0025), (0.0025*1.5)]

        # calculate corresponding perturb according to Re-Diff
        perturb_fromDiff = interpolate(ReDiffList[0],ReDiffList[1],
                           perturbList_ReDiff[0],perturbList_ReDiff[1],ReDiff)

        # calculate corresponding perturb according to Re-Factor
        perturb_fromFactor = interpolate(ReFactorList[0],ReFactorList[1],
                             perturbList_ReFactor[0],perturbList_ReFactor[1],ReFactor)

        # return perturb, based on Re-Factor
        return round(perturb_fromFactor, PER_decimals)


    def set_NewTargetValues(self, start, end, rootPolar, x1, x2, y1, y2):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # get op-points / names
        opPoints = operatingConditions["op_point"]
        opPointNames = operatingConditions["name"]

        for idx in range(start, end):
            # get opPoint
            opPoint = opPoints[idx]
            #print(opPointNames[idx])#Debug

            # find CD value of root-polar
            CD = rootPolar.find_CD_From_CL(opPoint)

            # determine the factor for scaling the CD-values by
            # linear interpolation
            factor = interpolate(x1, x2, y1, y2, opPoint)

            # calculate new target-value
            CD_new = CD * factor

            # set new target-value
            self.change_TargetValue(opPointNames[idx], CD_new)

    def linearizeTargetValues(self, start, end, factor):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # get op-points / names
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]
        opPointNames = operatingConditions["name"]

        x1 = targetValues[start]
        x2 = targetValues[end]
        y1 = opPoints[start]
        y2 = opPoints[end]

        for idx in range(start+1, end):
            #print ("changing opPoint: %s" % opPointNames[idx]) #Debug

            # get opPoint
            opPoint = opPoints[idx]

            # calculate target-value by linear-interpolation between start and
            # end
            linearTargetValue = interpolate_2(x1, x2, y1, y2, opPoint)

            # get actual target-value of target-polar
            targetValue = targetValues[idx]

            # calculate new target-value
            new_targetValue = (factor * linearTargetValue)\
                              + (1-factor) * targetValue

            # set new target-value
            self.change_TargetValue(opPointNames[idx], new_targetValue)


    def set_IntermediateOpPointTargetValues(self, params, targets,
                                            shifted_rootPolar, strakPolar, i):
        # get root-polar
        rootPolar = params.merged_polars[0]

        # shift the CD-values of the root-polar according to change in CD at the
        # maxGlide-point
        target_CD = round(targets["CD_maxGlide"][i], CD_decimals)
        CD_shiftValue = target_CD - shifted_rootPolar.CD_maxGlide
        shifted_rootPolar.shift_CD(CD_shiftValue)

        # determine factors for the main op-points
        # CL_min
        CL_min_strak = targets["CL_min"][i]
        CD_min_strak = targets["CD_min"][i]
        # important: use root-polar, as this is polar is closer to the desired target-polar
        CD_min_root = rootPolar.find_CD_From_CL(CL_min_strak)
        factor_min = CD_min_strak / CD_min_root

        # CL0
        CL0_strak = targets["CL0"][i]
        CD0_strak = targets["CD0"][i]
        CD0_root = rootPolar.find_CD_From_CL(CL0_strak)
        factor0 = CD0_strak / CD0_root

        # maxSpeed
        CL_maxSpeed_strak = targets["CL_maxSpeed"][i]
        CD_maxSpeed_strak = targets["CD_maxSpeed"][i]
        alpha_maxSpeed_strak = targets["alpha_maxSpeed"][i]
        CD_strak = targets["CD_maxSpeed"][i]
        CD_maxSpeed_root_1 = rootPolar.find_CD_From_alpha(alpha_maxSpeed_strak)
        CD_maxSpeed_root = shifted_rootPolar.find_CD_From_alpha(alpha_maxSpeed_strak)
        factor_maxSpeed_1 = CD_maxSpeed_strak / CD_maxSpeed_root_1
        factor_maxSpeed = CD_maxSpeed_strak / CD_maxSpeed_root

        # preMaxSpeed
        CL_preMaxSpeed_strak = targets["CL_preMaxSpeed"][i]
        CD_preMaxSpeed_strak = targets["CD_preMaxSpeed"][i]
        alpha_preMaxSpeed_strak = targets["alpha_preMaxSpeed"][i]
        CD_strak = targets["CD_preMaxSpeed"][i]
        CD_preMaxSpeed_root = shifted_rootPolar.find_CD_From_alpha(alpha_preMaxSpeed_strak)
        factor_preMaxSpeed = CD_preMaxSpeed_strak / CD_preMaxSpeed_root

        # maxGlide
        CL_maxGlide_strak = targets["CL_maxGlide"][i]
        CD_maxGlide_strak = targets["CD_maxGlide"][i]
        alpha_maxGlide_strak = targets["alpha_maxGlide"][i]
        # factor for maxGlide is alway 1.0, because this correction in the polar
        # was done by shifting the whole polar.
        factor_maxGlide = 1.0

        # maxLift
        # get strak-polar-values (target-values)
        CL_pre_maxLift_strak = targets["CL_pre_maxLift"][i]
        CD_pre_maxLift_strak = targets["CD_pre_maxLift"][i]
        alpha_pre_maxLift_strak = targets["alpha_pre_maxLift"][i]
        CD_pre_maxLift_root = shifted_rootPolar.find_CD_From_alpha(alpha_pre_maxLift_strak)

        factor_maxLift = CD_pre_maxLift_strak / CD_pre_maxLift_root

        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]

        # check if there are intermediate oppoints
        if (self.idx_CL0 > 1):
            # determine start and end-index for all op-points between
            # CL_min and CL0
            start = 1
            end = self.idx_CL0

            # now change all target-values of these op-points
            y1 = factor_min
            y2 = factor0
            x1 = CL_min_strak
            x2 = CL0_strak

            # important: use root-polar, as this is polar is closer to the desired target-polar
            self.set_NewTargetValues(start, end, rootPolar, x1, x2, y1, y2)

            # now linearize the values by a certain factor
            linearFactor = params.linearFactor_0[i]
            self.linearizeTargetValues(0, self.idx_CL0, linearFactor)

        # check if there are intermediate oppoints
        if ((self.idx_maxSpeed - self.idx_CL0) > 1):
            # determine start and end-index for all op-points between
            # CL0 and maxSpeed
            start =  self.idx_CL0 + 1
            end = self.idx_maxSpeed

            # now change all target-values of these op-points
            y1 = factor0
            y2 = factor_maxSpeed_1
            x1 = CL0_strak
            x2 = CL_maxSpeed_strak

            # important: use root-polar, as this is polar is closer to the desired target-polar
            self.set_NewTargetValues(start, end, rootPolar, x1, x2, y1, y2)

            # now linearize the values by a certain factor
            linearFactor = params.linearFactor_1[i]
            self.linearizeTargetValues(self.idx_CL0, self.idx_maxSpeed, linearFactor)

        # check if there are intermediate oppoints
        if ((self.idx_preMaxSpeed - self.idx_maxSpeed) > 1):
            # determine start and end-index for all op-points between
            # maxSpeed and preMaxSpeed
            start = self.idx_maxSpeed + 1
            end = self.idx_preMaxSpeed

            # now change all target-values of these op-points
            y1 = factor_maxSpeed
            y2 = factor_preMaxSpeed
            x1 = CL_maxSpeed_strak
            x2 = CL_preMaxSpeed_strak

            self.set_NewTargetValues(start, end, shifted_rootPolar, x1, x2, y1, y2)

            # now linearize the values by a certain factor
            linearFactor = params.linearFactor_2[i]
            self.linearizeTargetValues(self.idx_maxSpeed, self.idx_preMaxSpeed, linearFactor)

        # check if there are intermediate oppoints
        if ((self.idx_maxGlide - self.idx_preMaxSpeed) > 1):
            # determine start and end-index for all op-points between
            # preMaxSpeed and maxGlide
            start = self.idx_preMaxSpeed + 1
            end = self.idx_maxGlide

            # now change all target-values of these op-points
            y1 = factor_preMaxSpeed
            y2 = factor_maxGlide
            x1 = CL_preMaxSpeed_strak
            x2 = CL_maxGlide_strak

            self.set_NewTargetValues(start, end, shifted_rootPolar, x1, x2, y1, y2)

            # now linearize the values by a certain factor
            linearFactor = params.linearFactor_3[i]
            self.linearizeTargetValues(self.idx_preMaxSpeed, self.idx_maxGlide, linearFactor)

        # check if there are intermediate oppoints
        if ((self.idx_preClmax - self.idx_maxGlide) > 1):
            # determine start and end-index for all op-points between
            # maxGlide and pre_maxLift
            start = self.idx_maxGlide + 1
            end = self.idx_preClmax + 1

            # now change all target-values of these op-points
            y1 = factor_maxGlide
            y2 = factor_maxLift
            x1 = CL_maxGlide_strak
            x2 = CL_pre_maxLift_strak

            self.set_NewTargetValues(start, end, shifted_rootPolar, x1, x2, y1, y2)

            # now linearize the values by a certain factor
            linearFactor = params.linearFactor_4[i]
            self.linearizeTargetValues(self.idx_maxGlide, self.idx_preClmax, linearFactor)


    def apply_maxGlideFactor(self, params, i):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

         # get op-points / names
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]
        opPointNames = operatingConditions["name"]

        # get actual target-value of target-polar
        targetValue = targetValues[self.idx_maxGlide]

        # calculate new target-value
        new_targetValue = params.maxGlideFactor[i] * targetValue

        # set new target-value
        self.change_TargetValue(opPointNames[self.idx_maxGlide], new_targetValue)


    def set_constantWeighting(self, opPoints, minWeight, maxWeight):
        # set every op-point to constant minWeight (but not max-Lift-opPoint)
        for idx in range(self.idx_preClmax):
            self.change_Weighting(idx, minWeight)


    def set_linearProgressionWeighting(self, opPoints, minWeight, maxWeight):
        # increment weighting from minWeight to maxWeight
        # do not change alpha_preClmax
        num_intervals = self.idx_preClmax+1
        diff = (maxWeight - minWeight) / num_intervals

        for idx in range(num_intervals):
            weight = round(minWeight + (idx*diff), 2)
            self.change_Weighting(idx, weight)


    def set_sinusWeighting(self, opPoints, minWeight, maxWeight, start, end, y1, y2):
        for idx in range(start, end):
            # set up x/y-points for linear-interpolation.
            # x-values are CL-values of op-points
            # y-values are 0..pi/2 for sinus- calculation
            x1 = opPoints[start]
            x2 = opPoints[end]

            # calculate y by linear interpolation
            y = interpolate(x1, x2, y1, y2, opPoints[idx])

            # calculate sinus-function. The result is a "delta-" value
            # that will be added to minWeight
            diff = (maxWeight - minWeight) * sin(y)

            # calculate new weight
            weight = round((minWeight + diff), 2)

            # set the new weighting for the op-point now
            self.change_Weighting(idx, weight)


    # Set weighting of all op-points according to the parameters
    # 'weightingMode', 'minWeight' and 'maxWeight'
    def set_Weightings(self, params):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        opPoints = operatingConditions["op_point"]

         # determine min and max weight
        minWeight = params.minWeight
        maxWeight = params.maxWeight

        # evaluate the weighting-mode
        if (params.weightingMode == 'constant'):
            self.set_constantWeighting(opPoints, minWeight, maxWeight)

        elif (params.weightingMode == 'linear_progression'):
           set_linearProgressionWeighting(self, opPoints, minWeight, maxWeight)

        elif (params.weightingMode == 'sinus'):
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, 0, self.idx_maxGlide-2, 0.0, pi/2)
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, self.idx_maxGlide-2, self.idx_preClmax, pi/2, 0.0)

        elif (params.weightingMode == 'doubleSinus'):
            # set constant weight to all oppoints first.
            self.set_constantWeighting(opPoints, minWeight, maxWeight)

            # calculate node-point and endpoint
            nodePoint = self.idx_maxGlide - 1
            #endPoint = self.idx_maxGlide + int(((self.idx_preClmax - self.idx_maxGlide)/2))
            endPoint = self.idx_preClmax

            # now set sinus-weighting to oppoints from Cl_min to node-point.
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, 0, int(nodePoint/2), 0.0, pi/2)
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, int(nodePoint/2), nodePoint, pi/2, 0.0)

            # last set sinus-weighting from node-point to end-point
            diff = endPoint - nodePoint
            peak = nodePoint + int(diff/2)
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, nodePoint, peak, 0.0, pi/2)
            self.set_sinusWeighting(opPoints, minWeight, maxWeight, peak, endPoint, pi/2, 0.0)

        # set weighting of max-Lift op-point to maxWeight
        self.change_Weighting(self.idx_preClmax, maxWeight)

        #print(operatingConditions["weighting"])#Debug
        #print(operatingConditions["op_point"])
        #print("Done.")#Debug


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
                if (CL <= polarData.CL_switchpoint_Type2_Type1_polar):
                    # yes, adapt maxRe --> Type 1 oppoint
                    reynolds[idx] = int(polarData.maxRe)
                    print("adapted oppoint @ Cl = %0.3f, Type 1, Re = %d\n" % \
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
        weighting = 1.0
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
        #self.print_OpPoints()#Debug

        for opPoint in opPoints:

            # compose name
            name = "add_op_%s" % num

            # insert new op-Point, get index
            idx = self.insert_OpPoint(name, 'spec-cl', opPoint, 'target-drag',
                                     0.0, 1.0, None)

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

        #self.print_OpPoints()#Debug

    def insert_alpha0_oppoint(self, params, strakPolar, i):
        # get maxRe
        maxRe = params.maxReNumbers[i]

        # get alpha0 - target
        alpha = round(params.targets["alpha0"][i], AL_decimals)

        # set weighting to 2 times maxWeight
        weighting = 2*params.maxWeight

        # set reynolds
        if params.ReAlpha0 > 0:
            reynolds = params.ReAlpha0
        else:
            reynolds = maxRe #800000

        # insert op-Point, get index
        idx = self.insert_OpPoint('alpha0', 'spec-al', alpha, 'target-lift',
                                     0.0, weighting, reynolds)

            #print (idx)#Debug


    # This function will append an additonal oppoint, that will prevent
    # that the point of maxLift drops too much at lower reynolds-numbers
    def add_maxLift_protection_oppoint(self, params, strakPolar, i):
        # get Re of strak-airfoil
        Re = params.ReNumbers[i]

        # calculate reynolds-number that shall be watched
        reynolds = int(round((Re*2/3), 0))

        # set weighting of oppoint to half between min and maxWeight
        weighting = (params.minWeight + params.maxWeight) / 2

        # get data of last oppoint (that is always the point of maximum lift)
        (lastOppointName, idx) = self.get_LastOpPoint()
        CL =  self.get_OpPoint(lastOppointName)

        # calculate target-value of maximum Lift-oppoint at lower
        # reynolds-number (only roughly estimated)
        targetValue = self.get_TargetValue(lastOppointName) * 1.2

        # append oppoint now
        self.add_Oppoint('maxLiftGuard', 'spec-cl', CL, 'target-drag',
                                     targetValue, weighting, reynolds)


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
        #print(Cl_start, Cl_end, Cl_interval, num_intervals) Debug

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
                                           CD0, 1.0, None)
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

        #print (opPointNames)
        #print (opPoints)
        #print(operatingConditions['target_value'])
        #print ("Ready.")#Debug


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
        #print (fixed_opPoints) Debug

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
class strakData:
    def __init__(self):
        self.buildDir = ''
        self.workingDir = ''
        self.quality = 'medium'
        self.strakMachineCall = "strak_machineV2.py"
        self.xfoilWorkerCall = "xfoil_worker.exe"
        self.xoptfoilCall = "xoptfoil-jx.exe"
        self.showStatusCall = "show_status.py"
        self.xoptfoilVisualizerCall = "xoptfoil_visualizer-jx.exe"
        self.airfoilComparisonCall = "best_airfoil.py"
        self.xoptfoilInputFileName = 'istrak.txt'
        self.weightingMode = 'sinus'
        self.batchfileName = 'make_strak.bat'
        self.xoptfoilTemplate = "iOpt"
        self.operatingMode = 'default'
        self.seedFoilName = ""
        self.matchPolarFoilName = ""
        self.ReSqrtCl = 150000
        self.ReAlpha0 = 0
        self.NCrit = 9.0
        self.numOpPoints = 16
        self.minWeight = 0.7
        self.maxWeight = 2.1
        self.CL_min = -0.1
        self.CL_preMaxSpeed = 0.2
        self.intersectionPoint_CL = 0.0
        self.intersectionPoint_CL_CD = 99.0 # Deactivated
        self.intersection_Hysteresis= 0.001
        self.CL_switchpoint_Type2_Type1_polar = 0.05
        self.maxReFactor = 15.0
        self.maxLiftDistance = 0.02
        self.alpha_Resolution = 0.001
        self.optimizationPasses = 3
        self.allGraphs = True
        self.scriptsAsExe = False
        self.generateBatch = True
        self.xmlFileName = None
        self.wingData = None
        self.useWingPlanform = True
        self.useAlwaysRootfoil = True
        self.showTargetPolars = True
        self.adaptInitialPerturb = True
        self.smoothSeedfoil = True
        self.smoothStrakFoils = False
        self.smoothMatchPolarFoil = False
        self.plotStrakPolars = True
        self.ReNumbers = []
        self.additionalOpPoints = [[]]
        self.chordLengths = []
        self.maxReNumbers = []
        self.polarFileNames = []
        self.polarFileNames_T1 = []
        self.polarFileNames_T2 = []
        self.inputFileNames = []
        self.T1_polars = []
        self.T2_polars = []
        self.merged_polars = []
        self.shifted_rootPolars = []
        self.target_polars = []
        self.strak_polars = []
        self.inputFiles = []
        self.maxIterations = [30,40,160], # multi-pass optimization
        self.numberOfCompetitors = [1, 3, 1], # multi-pass optimization
        self.shape_functions = ['camb-thick-plus','hicks-henne','hicks-henne'],
        self.optimizeAlpha0 = [True, True, True, True, True, True, True, True, True, True, True, True]
        self.minCLGain = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
        self.CL0Gain = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.maxGlideShift = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.maxGlideGain = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.maxGlideFactor = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
        self.maxSpeedGain = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.maxSpeedShift = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.preMaxSpeedGain = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.maxLiftGain = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.linearFactor_0 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.linearFactor_1 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.linearFactor_2 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.linearFactor_3 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        self.linearFactor_4 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
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


    ############################################################################
    # function that returns a list of Re-numbers
    def get_ReList(params):
        return params.ReNumbers


    ############################################################################
    # function that returns a list of max Re-numbers
    def get_maxReList(params):
        return params.maxReNumbers


    ############################################################################
    # function that calculates dependend values
    def calculate_DependendValues(self):
        # setup tool-calls
        #exeCallString =  " .." + bs + exePath + bs
        exeCallString =  "echo y | .." + bs + exePath + bs

        pythonCallString = pythonInterpreterName + ' ..' + bs + scriptPath + bs

        self.xfoilWorkerCall = exeCallString + xfoilWorkerName + '.exe'
        self.xoptfoilCall = exeCallString + xoptfoilName + '.exe'

        if (params.scriptsAsExe):
            self.strakMachineCall = exeCallString + strakMachineName + '.exe'
            self.xoptfoilVisualizerCall = exeCallString + xoptfoilVisualizerName + '.exe'
            self.airfoilComparisonCall = exeCallString + airfoilComparisonName + '.exe'
            self.showStatusCall = "start \"\" \"%s\"\n" % (exeCallString + showStatusName + '.exe')
        else:
            self.strakMachineCall = pythonCallString + strakMachineName + '.py'
            self.xoptfoilVisualizerCall = pythonCallString + xoptfoilVisualizerName + '.py'
            self.airfoilComparisonCall = pythonCallString + airfoilComparisonName + '.py'
            self.showStatusCall = "start \"\" \"%s\" %s\n" % (pythonInterpreterName +"w", \
                         (' ..' + bs + scriptPath + bs + showStatusName + '.py'))


        # calculate List of Re-numers, if wingdata available
        if (self.wingData != None):
            # clear the list of chord-lenghts
            self.chordLengths = []

            # get list of all chord-lengths of the wing
            chordLengths = params.wingData.get('chordLengths')

            # copy the list
            self.chordLengths = chordLengths

        # is there a list of chord-lengths available ?
        if (self.chordLengths != []):
            # clear the list of Re-numbers
            self.ReNumbers = []

            # get Re-number of root-airfoil
            rootRe = params.ReSqrtCl

            # get chord-length of root-airfoil
            rootChord = self.chordLengths[0]

            # calculate list of Re-numbers
            for chord in chordLengths:
                Re = (rootRe * chord) / rootChord
                self.ReNumbers.append(Re)

        # calculate list of max Re-numbers
        for Re in self.ReNumbers:
            ReMax = int(round(Re * self.maxReFactor, 0))
            self.maxReNumbers.append(ReMax)

        # calculate Cl where polar-generation is going to switch from
        # type2- to type1-polar
        self.CL_switchpoint_Type2_Type1_polar =\
                   ((self.ReNumbers[0] * self.ReNumbers[0]))/\
                   ((self.maxReNumbers[0])*(self.maxReNumbers[0]))


    def calculate_CD_TargetValue(self, root, strak, gain):
        target = (  (root * gain)           # part coming from root-airfoil
                  + (strak * (1.0 - gain))) # part coming from strak-airfoil

        return round(target, CD_decimals)


    def calculate_MainTargetValues(self):
        # get root-polar
        rootPolar = self.merged_polars[0]
        num = len(self.merged_polars)

        for idx in range(num):
            # get polar
            polar = self.merged_polars[idx]
            shifted_root_polar = params.shifted_rootPolars[idx]

            # get gain and loss values
            if (polar == rootPolar):
                # no gain / loss / shift for root-polar
                minCLGain = 0.0
                CL0Gain = 0.0
                maxSpeedGain = 0.0
                maxSpeedShift = 0.0
                preMaxSpeedGain = 0.0
                maxGlideGain = 0.0
                maxGlideShift = 0.0
                maxLiftGain = 0.0
            else:
                minCLGain = params.minCLGain[idx]
                CL0Gain = params.CL0Gain[idx]
                maxSpeedGain = params.maxSpeedGain[idx]
                maxSpeedShift = params.maxSpeedShift[idx]
                preMaxSpeedGain = params.preMaxSpeedGain[idx]
                maxGlideGain = params.maxGlideGain[idx]
                maxGlideShift = params.maxGlideShift[idx]
                maxLiftGain = params.maxLiftGain[idx]

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
            target_CL_maxGlide = shifted_root_polar.CL_maxGlide
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

        #print(self.targets)#Debug
        #DoneMsg()#Debug


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


##    # correct additional op-Points for strak-airfoils
##    def calculate_AdditionalOpPoints(self):
##        # get and additional op-points of root
##        rootAddOpPoints = self.additionalOpPoints[0]
##
##        if (len(rootAddOpPoints) == 0):
##            # nothing to do
##            return
##
##        # get target-values
##        targets = self.targets
##        CL0_root = targets["CL0"][0]
##        CL_maxSpeed_root = targets["CL_maxSpeed"][0]
##        CL_maxGlide_root = targets["CL_maxGlide"][0]
##        CL_pre_maxLift_root = targets["CL_pre_maxLift"][0]
##
##        # all polars behind the root polar
##        num = len(self.merged_polars)
##        for idx in range (1, num):
##            strakAddOpPoints = []
##
##            # get strak-target-values
##            CL0_strak = targets["CL0"][idx]
##            CL_maxSpeed_strak = targets["CL_maxSpeed"][idx]
##            CL_maxGlide_strak = targets["CL_maxGlide"][idx]
##            CL_pre_maxLift_strak = targets["CL_pre_maxLift"][idx]
##
##            # all additional opPoints
##            for opPoint in rootAddOpPoints:
##                if (CL_maxSpeed_root <= opPoint) and (CL_maxGlide_root >= opPoint):
##                    # correct the opPoint
##                    correctedOpPoint = self.correctOpPoint_left(opPoint,
##                    CL_maxSpeed_root, CL_maxSpeed_strak,
##                    CL_maxGlide_root, CL_maxGlide_strak)
##                else:
##                    # correct the opPoint
##                    correctedOpPoint = self.correctOpPoint_right(opPoint,
##                    CL_pre_maxLift_root, CL_pre_maxLift_strak,
##                    CL_maxGlide_root, CL_maxGlide_strak)
##
##                # append corrected opPoint to additional strakop-points
##                strakAddOpPoints.append(correctedOpPoint)
##
##            # append list of additional strak op-points to params
##            self.additionalOpPoints.append(strakAddOpPoints)


################################################################################
#
# polarGraph class
#
################################################################################
class polarGraph:
    def __init__(self):
        return


    def set_AxesAndLabels(self, ax, title, xlabel, ylabel):

        # set title of the plot
        text = (title)
        #ax.set_title(text, fontsize = 30, color="darkgrey")

        # set axis-labels
        ax.set_xlabel(xlabel, fontsize = 20, color="darkgrey")
        ax.set_ylabel(ylabel, fontsize = 20, color="darkgrey")

        # customize grid
        ax.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)


    # reverts list and returns reverted list
    def get_ReverseList(self, list):
        reverseList = []
        idx = len(list)-1

        for element in list:
            reverseList.append(list[idx])
            idx = idx -1

        return reverseList

    # plots an image
    def plot_Logo(self, ax, params):
        searchPaths = []
        searchPaths.append(ressourcesPath + bs + logoName)
        # further Search-paths can be added here

        for path in searchPaths:
            try:
                image = mpimg.imread(path)
            except:
                NoteMsg("strak-machine-image was not found in path %s,"\
                         "trying different path" % path)
        try:
            ax.imshow(image)
            ax.set_axis_off()
        except:
            ErrorMsg("could not find strak-machine-image, plotting of image was"
                     " skipped")


    # plots lift/drag-polars (Xfoil-worker-polars and target-polars)
    def plot_LiftDragPolars(self, ax, polars, targetPolars):
        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL, CD', 'CD', 'CL')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars (for graphical reasons: plot root-polar last)
        polars = self.get_ReverseList(polars)
        targetPolars = self.get_ReverseList(targetPolars)

        # get number of polars to plot
        numPolars = len(polars)

        # get maximum CD-value that shall be visible in the plot
        max_CD = round(rootPolar.CD_maxLift * 1.1, CD_decimals)

        # set x-axis manually
        ax.set_xlim(right = max_CD)

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.2, max(rootPolar.CL) + 0.2)

        # determine some text-offsets
        CL0TextOffset_x = polars[0].find_CD_From_CL(0.0) * 1.1
        CL0TextOffset_y = -0.2
        maxSpeedTextOffset_x = polars[0].CD_maxSpeed * 1.1
        maxSpeedTextOffset_y = rootPolar.CL_maxSpeed
        maxGlideTextOffset_x = polars[0].find_CD_From_CL(rootPolar.CL_maxGlide) * 1.1
        maxGlideTextOffset_y = rootPolar.CL_maxGlide

        # all polars
        for polarIdx in range(numPolars):
            #  get polar and target-polar to plot
            polar = polars[polarIdx]
            targetPolar = targetPolars[polarIdx]

            # determine idx for changing colors
            switchIdx = polar.T2_T1_switchIdx

            # set label only for root-polar
            if (polar == rootPolar):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
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

            # plot CD @CL = 0
            x = polar.CD_CL0
            y = 0.0

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('CL=0 (root) @ CD = %.4f' % x, xy=(x,y),
                  xytext=(CL0TextOffset_x, CL0TextOffset_y),
                  textcoords='data',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max_speed
            x = polar.CD[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o',color=cl_infotext)
                ax.annotate('maxSpeed (root) @ CL = %.2f, CD = %.4f' % (y, x),
                 xy=(x,y), xytext=(maxSpeedTextOffset_x, maxSpeedTextOffset_y),
                 textcoords='data', fontsize = fs_infotext,
                 color=cl_infotext)

            # plot preMax_speed
            x = polar.CD[polar.preMaxSpeed_idx]
            y = polar.CL[polar.preMaxSpeed_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o',color=cl_infotext)
##                ax.annotate('preMaxSpeed (root) @ CL = %.2f, CD = %.4f' % (y, x),
##                 xy=(x,y), xytext=(maxSpeedTextOffset_x, maxSpeedTextOffset_y),
##                 textcoords='data', fontsize = fs_infotext,
##                 color=cl_infotext)

            # plot max_glide
            x = polar.CD[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxGlide (root) @ CL = %.2f, CD = %.4f' % (y, x),
                 xy=(x,y), xytext=(maxGlideTextOffset_x, maxGlideTextOffset_y),
                  textcoords='data', fontsize = fs_infotext, color=cl_infotext)

            # plot max lift
            x = polar.CD[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxLift (root) @ CL = %.2f, CD = %.4f' %(y,x),
                  xy=(x,y), xytext=(-160,10), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)

            # plot target-polar
            label = None
            if (polar == rootPolar):
                # style for target-polar of root-airfoil
                style = opt_point_style_root
                linewidth = 0.0
            else:
                # style for target-polar of strak-airfoil
                style = opt_point_style_strak
                linewidth = lw_targetPolar

                # set label only for one of the strak-polars tp avoid multiple
                # labels that are all the same
                if (polar == polars[1]):
                    label = 'target-polar'

            if (polar == rootPolar) or (params.showTargetPolars == True):
                # remove last elements, as they are dummies
                x = deepcopy(targetPolar.CD)
                x.pop()
                y = deepcopy(targetPolar.CL)
                y.pop()

            ax.plot(x, y, style, linestyle = ls_targetPolar,
                            linewidth = linewidth, label = label)

            ax.legend(loc='upper left', fontsize = fs_legend)

        # plot strak-polars
        if params.plotStrakPolars:
            strakPolars = params.strak_polars
            numPolars = len(strakPolars)

            for i in range(numPolars):
                # set style
                style = "r-"
                linewidth = lw_strakPolar
                x = strakPolars[i].CD
                y = strakPolars[i].CL

                # set label only for one of the strak-polars tp avoid multiple
                # labels that are all the same
                if (i == 0):
                    label = 'polar of previous strak-airfoil'
                else:
                    label = None

                ax.plot(x, y, style, linestyle = ls_strakPolar,
                                linewidth = lw_strakPolar, label = label)

                ax.legend(loc='upper left', fontsize = fs_legend)


    # plots lift/alpha-polars (Xfoil-worker-polars and target-polars)
    def plot_LiftAlphaPolars(self, ax, polars, targetPolars):
        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL, alpha', 'alpha', 'CL')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars (for graphical reasons: plot root-polar last)
        polars = self.get_ReverseList(polars)
        targetPolars = self.get_ReverseList(targetPolars)

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.1, max(rootPolar.CL) + 0.2)

        # get number of polars to plot
        numPolars = len(polars)

        # all polars
        for polarIdx in range(numPolars):
            #  get polar and target-polar to plot
            polar = polars[polarIdx]
            targetPolar = targetPolars[polarIdx]

            # set label only for root-polar
            if (polar == rootPolar):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
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
            ax.legend(loc='upper left', fontsize = fs_legend)

            # plot alpha @CL = 0
            x = polar.alpha_CL0
            y = 0.0

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('CL=0 (root) @ alpha = %.2f' % x,
                  xy=(x,y), xytext=(20,-15), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max Speed
            x = polar.alpha[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @ alpha = %.2f, CL = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(20,-5), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max Glide
            x = polar.alpha[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]
            if (polar == rootPolar):
               ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ alpha = %.2f, CL = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(20,-5), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max lift
            x = polar.alpha[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @ alpha = %.2f, CL = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(-140,10), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot target-polar, root-polar only
            label = None
            if (polar == rootPolar):
                # style for target-polar of root-airfoil
                style = opt_point_style_root
                linewidth = 0.0

                # remove last dummy-values
                x = deepcopy(targetPolar.alpha)
                x.pop()
                y = deepcopy(targetPolar.CL)
                y.pop()

                # plot
                ax.plot(x, y, style, linestyle = ls_targetPolar,
                        linewidth = linewidth, label = label)


    # plots glide-polars (Xfoil-worker-polars and target-polars)
    def plot_GlidePolars(self, ax, polars, targetPolars, allGraphs):
        # set axes and labels
        self.set_AxesAndLabels(ax, 'CL/CD, CL', 'CL', 'CL/CD')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars (for graphical reasons: plot root-polar last)
        polars = self.get_ReverseList(polars)
        targetPolars = self.get_ReverseList(targetPolars)


        # get number of polars to plot
        numPolars = len(polars)

        # set y-axis manually
        if (allGraphs == True):
            ax.set_ylim(min(rootPolar.CL_CD) - 10, max(rootPolar.CL_CD) + 10)
        else:
            ax.set_ylim(-5, max(rootPolar.CL_CD) + 5)

        # all polars
        for polarIdx in range(numPolars):
            #  get polar and target-polar to plot
            polar = polars[polarIdx]
            targetPolar = targetPolars[polarIdx]

            # set label only for root-polar
            if (polar == rootPolar):
                T1_label = 'T1-polar'
                T2_label = 'T2-polar'
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
            ax.legend(loc='upper left', fontsize = fs_legend)

            # plot Cl/CD @CL = 0
            x = 0.0
            y = 0.0

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('CL=0 (root) @ CL/CD = %.2f' % y, xy=(x,y),
                  xytext=(20,-5), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max_speed
            x = polar.CL[polar.maxSpeed_idx]
            y = polar.CL_CD[polar.maxSpeed_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @\nCL = %.2f,\nCL/CD = %.2f' %\
                 (x, y), xy=(x,y), xytext=(-80,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot preMax_speed
            x = polar.CL[polar.preMaxSpeed_idx]
            y = polar.CL_CD[polar.preMaxSpeed_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o',color=cl_infotext)


            # plot max_glide
            x = polar.CL[polar.maxGlide_idx]
            y = polar.CL_CD[polar.maxGlide_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ CL = %.2f, CL/CD = %.2f' %\
                  (x, y), xy=(x,y), xytext=(-60,7), textcoords='offset points',
                   fontsize = fs_infotext, color=cl_infotext)

            # plot max Lift
            x = polar.CL[polar.maxLift_idx]
            y = polar.CL_CD[polar.maxLift_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @\nCL = %.2f,\nCL/CD = %.2f' %\
                 (x, y), xy=(x,y), xytext=(10,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)


            # plot target-polar
            label = None
            if (polar == rootPolar):
                # style for target-polar of root-airfoil
                style = opt_point_style_root
                linewidth = 0.0
            else:
                # style for target-polar of strak-airfoil
                style = opt_point_style_strak
                linewidth = lw_targetPolar

                # set label only for one of the strak-polars tp avoid multiple
                # labels that are all the same
                if (polar == polars[1]):
                    label = 'target-polar'

            if (polar == rootPolar) or (params.showTargetPolars == True):
                x = deepcopy(targetPolar.CL)
                x.pop()
                y = deepcopy(targetPolar.CL_CD)
                y.pop()

                # plot
                ax.plot(x, y, style, linestyle = ls_targetPolar,
                        linewidth = linewidth, label = label)

        # plot strak-polars
        if params.plotStrakPolars:
            strakPolars = params.strak_polars
            numPolars = len(strakPolars)

            for i in range(numPolars):
                # set style
                style = "r-"

                x = strakPolars[i].CL
                y = strakPolars[i].CL_CD

                # set label only for one of the strak-polars tp avoid multiple
                # labels that are all the same
                if (i == 0):
                    label = 'polar of previous strak-airfoil'
                else:
                    label = None

                ax.plot(x, y, style, linestyle = ls_strakPolar,
                                linewidth = lw_strakPolar, label = label)

                ax.legend(loc='upper left', fontsize = fs_legend)


    # draw the graph
    def draw(self, params):
        # get polars
        polars = params.merged_polars
        targetPolars = params.target_polars
        T1_polars = params.T1_polars
        T2_polars = params.T2_polars

        # get polar of root-airfoil
        rootPolar = polars[0]
        rootPolar_T1 = T1_polars[0]
        rootPolar_T2 = T2_polars[0]

        if (params.operatingMode == 'matchpolarfoils'):
            airfoilName = params.matchPolarFoilName
        else:
            airfoilName = params.seedFoilName

        print("plotting polars of airfoil %s..." % (airfoilName))

        # set 'dark' style
        plt.style.use('dark_background')

        # setup subplots
        if (params.allGraphs == True):
            fig, (upper,lower) = plt.subplots(2,2)
        else:
            fig, upper = plt.subplots(1)

        # compose diagram-title
        text = ("Analysis of airfoil \"%s\"\n" % airfoilName)
        text = text + "T1-polars, Re = "

        # add Re-numbers
        num_polars = len(T1_polars)
        for i in range(num_polars):
            Re = T1_polars[i].Re
            if (i == num_polars-1):
                text = text + ("%d\n" % Re)
            else:
                text = text + ("%d, " % Re)

        text = text + "T2-polars, ReSqrt(CL) = "

         # add Re-numbers
        for i in range(num_polars):
            Re = T2_polars[i].Re
            if (i == num_polars-1):
                text = text + ("%d\n" % Re)
            else:
                text = text + ("%d, " % Re)

        fig.suptitle(text, fontsize = 12, color="darkgrey", **csfont)

        if (params.allGraphs == True):
            # first figure, display strak-machine-logo
            self.plot_Logo(upper[0], params)

            # second figure, display the Lift / Drag-Polar
            self.plot_LiftDragPolars(lower[0], polars, targetPolars)

            # third figure, display the Lift / alpha-Polar
            self.plot_LiftAlphaPolars(upper[1], polars, targetPolars)

            # fourth figure, display the Glide polar
            self.plot_GlidePolars(lower[1], polars, targetPolars, params.allGraphs)
        else:
            # plot only Glide polar
            self.plot_GlidePolars(upper, polars, targetPolars, params.allGraphs)

        # maximize window
        figManager = plt.get_current_fig_manager()
        try:
            figManager.window.state('zoomed')
        except:
            try:
                figManager.window.Maximize(True)
            except:
                pass

        # show diagram
        plt.show()


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
        self.NCrit = 9.0
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
        self.CL_switchpoint_Type2_Type1_polar = 999999
        self.T2_T1_switchIdx = 0


    def import_FromFile(self, fileName):
        BeginOfDataSectionTag = "-------"
        airfoilNameTag = "Calculated polar for:"
        ReTag = "Re ="
        parseInDataPoints = 0
        print("importing polar %s..." %fileName)

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
                    splittedLine = line.split("  ")
                    self.alpha.append(float(splittedLine[1]))
                    self.CL.append(float(splittedLine[2]))
                    self.CD.append(float(splittedLine[3]))
                    CL_CD = float(splittedLine[2])/float(splittedLine[3])
                    self.CL_CD.append(CL_CD)
                    self.CDp.append(float(splittedLine[4]))
                    self.Cm.append(float(splittedLine[5]))
                    self.Top_Xtr.append(float(splittedLine[6]))
                    self.Bot_Xtr.append(float(splittedLine[7]))

        fileHandle.close()
        DoneMsg()


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

        print("writing polar to file %s..." %fileName)

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
        DoneMsg()


    # analyses a polar
    def analyze(self, params):
        print("analysing polar \'%s\'..." % self.polarName)

        self.determine_MaxSpeed()
        self.determine_CLmin(params)
        self.determine_MaxGlide()
        self.determine_preMaxSpeed(params)
        self.determine_MaxLift(params)
        self.determine_alpha_CL0(params)
        DoneMsg()


    # merge two polars at a certain CL-value, return a merged-polar
    # mergePolar_1 will be the "lower" part of the merged-polar from
    # minimum CL up to the CL-value where the merge happens.
    # "self" will be the upper part of the merged-polar
    def merge(self, mergePolar_1, switching_CL, maxRe):
        print ("merging polars at CL = %s" % switching_CL)

        # create a new, empty polar
        mergedPolar = polarData()

        # copy some information from mergePolar_1
        mergedPolar.airfoilname = self.airfoilname
        mergedPolar.polarType = 12
        mergedPolar.Re = self.Re
        mergedPolar.NCrit = 1.0
        mergedPolar.CL_switchpoint_Type2_Type1_polar = switching_CL
        mergedPolar.maxRe = maxRe
        mergedPolar.polarName = 'merged_polar_%s' % get_ReString(self.Re)

        # merge first polar from start Cl to switching_Cl
        for idx in range(len(mergePolar_1.CL)):
            if (mergePolar_1.CL[idx] <= switching_CL):
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
            if (self.CL[idx] > switching_CL):
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

        # number of increments must be an integer
        num_increments = int(round(actualResolution / newResolution, 0))

        # check number of increments, must be > 1
        if (num_increments <= 1):
            # Error-message and return
            ErrorMsg("set_alphaResolution: newResolution is less than or equal actual resolution")
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
        self.T2_T1_switchIdx = self.find_index_From_CL(self.CL_switchpoint_Type2_Type1_polar)
        #print("Ready")#Debug



    # generate a new shifted polar. The max-glide-point (this means maximum CL/CD)
    # will be shifted left or right. The max Lift point will remain the same.
    # The max Speed-point will be influenced in some kind.
    # The alpha_CL0 point will remain the same
    def get_shiftedPolar(self, shiftValue, params):
        # copy existing polar
        shiftedPolar = deepcopy(self)

       # check whether to shift the polar
        if abs(shiftValue) < 0.000001:
            # nothing to do
            return shiftedPolar

        # determine stretch-factor to strech the complete polar
        CL_factor = (self.CL_maxGlide + shiftValue) / self.CL_maxGlide

        # determine number of values
        num = len(shiftedPolar.CL)

        # stretch the whole polar by factor, changing CL-values
        for i in range(num):
            shiftedPolar.CL[i] = shiftedPolar.CL[i] * CL_factor
            # calculate new drag-value
            CD_old = shiftedPolar.CD[i]
            shiftedPolar.CD[i] = shiftedPolar.CL[i] / shiftedPolar.CL_CD[i]
            #print(CD_old, shiftedPolar.CD[i])#Debug

        # analyze streched-polar, determine max-lift
        shiftedPolar.analyze(params)

        # determine factor to correct aerea between max-glide and maxLift,
        # so max-lift has the same value than before stretching
        maxLift_factor = self.CL_maxLift / shiftedPolar.CL_maxLift

        y1 = 1.0
        y2 = maxLift_factor

        # now linear correct all CL-values after maxGlide, get the same max-lift as before
        for i in range(shiftedPolar.maxGlide_idx, num):
            factor = interpolate(shiftedPolar.maxGlide_idx, shiftedPolar.maxLift_idx, y1, y2, i)
            shiftedPolar.CL[i] = shiftedPolar.CL[i] * factor
            # calculate new drag-value
            shiftedPolar.CD[i] = shiftedPolar.CL[i] / shiftedPolar.CL_CD[i]

        # return the polar
        return shiftedPolar


    # all CD-values will be shifted by the given shiftValue.
    # all CL_CD-values will be recalculated
    def shift_CD(self, shiftValue):
        # determine number of values
        num = len(self.CD)

        # now shift all CD-values, recalculate CL_CD
        for i in range(num):
            self.CD[i] = self.CD[i] + shiftValue
            self.CL_CD[i] = self.CL[i] / self.CD[i]


    # all CL_CD-values will be sscaled by the given scale-factor.
    # all CD-values will be recalculated
    def scale_CL_CD(self, scaleFactor):
        # determine number of values
        num = len(self.CL_CD)

        # now scale all CL_CD-values, recalculate CD
        for i in range(num):
            self.CL_CD[i] = self.CL_CD[i] * scaleFactor
            self.CD[i] = self.CL[i] / self.CL_CD[i]


    # determines the overall minimum CL-value of a given polar and some
    # corresponding values
    def determine_MaxSpeed(self):
        self.CD_maxSpeed = min(self.CD)
        self.maxSpeed_idx = self.find_index_From_CD(self.CD_maxSpeed)
        self.CL_maxSpeed = self.CL[self.maxSpeed_idx]
        self.alpha_maxSpeed = self.alpha[self.maxSpeed_idx]
        self.CL_CD_maxSpeed = self.CL_maxSpeed / self.CD_maxSpeed

        print("max Speed, CD = %f @ CL = %f" %\
                                  (self.CD_maxSpeed, self.CL_maxSpeed))


    def determine_preMaxSpeed(self, params):
        self.CL_preMaxSpeed = params.CL_preMaxSpeed
        self.preMaxSpeed_idx = self.find_index_From_CL(self.CL_preMaxSpeed)
        self.CD_preMaxSpeed = self.CD[self.preMaxSpeed_idx]
        self.alpha_preMaxSpeed = self.alpha[self.preMaxSpeed_idx]
        self.CL_CD_preMaxSpeed = self.CL_preMaxSpeed / self.CD_preMaxSpeed
        print("pre max Speed, CD = %f @ CL = %f" %\
                                  (self.CD_preMaxSpeed, self.CL_preMaxSpeed))


    # determines the overall max-value for Cl/Cd (max glide) of a given polar
    # and some corresponding values
    def determine_MaxGlide(self):
        self.CL_CD_maxGlide = max(self.CL_CD)
        self.maxGlide_idx = self.find_index_From_CL_CD(self.CL_CD_maxGlide)
        self.CL_maxGlide = self.CL[self.maxGlide_idx]
        self.CD_maxGlide = self.CD[self.maxGlide_idx]
        self.alpha_maxGlide = self.alpha[self.maxGlide_idx]

        print("max Glide, CL/CD = %f @ CL = %f" %
                                  (self.CL_CD_maxGlide, self.CL_maxGlide))

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

        print("max Lift, CL = %f @ alpha = %f" %
                                  (self.CL_maxLift, self.alpha_maxLift))
        print("last op-point before max Lift will be set to CL = %f @ alpha"\
              " = %f, keeping a CL-distance of %f" %\
          (self.CL_pre_maxLift, self.alpha_pre_maxLift, params.maxLiftDistance))

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

        print("alpha_CL0 = %f" % self.alpha_CL0)

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
def get_FoilName(params, index):

    # is there wingdata available ?
    if (params.wingData != None):
        # yes
        wing = params.wingData
        # get airfoil-names from wing-dictionary
        airfoilNames = wing.get('airfoilNames')
        foilName = airfoilNames[index]
    else:
        # compose foilname with seedfoilname and Re-number
        Re = params.ReNumbers[index]

        if (params.operatingMode == 'matchpolarfoils'):
            foilName = params.matchPolarFoilName
        else:
            foilName = params.seedFoilName

        if (index == 0):
            suffix = '-root'
        else:
            suffix = '-strak'

        foilName = (foilName + "%s-%s.dat") % (suffix ,get_ReString(Re))

    return (foilName)

################################################################################
# function that gets the number of chords
def get_NumberOfAirfoils(params):

    # is there wingdata available ?
    if (params.wingData != None):
        # get number of chords from wing-data
        num = len(params.wingData.get('chordLengths'))
    else:
        # get number of chords from ReNumbers
        num = len(params.ReNumbers)

    return num


################################################################################
# function that generates commandlines to create and merge polars
def generate_polarCreationCommandLines(commandlines, params, strakFoilName, maxRe, Re):

    polarDir = strakFoilName.strip('.dat') + '_polars'

    polarFileName_T1 = compose_Polarfilename_T1(maxRe, params.NCrit)
    polarFileNameAndPath_T1 = polarDir + bs + polarFileName_T1

    polarFileName_T2 = compose_Polarfilename_T2(Re, params.NCrit)
    polarFileNameAndPath_T2 = polarDir + bs + polarFileName_T2

    mergedPolarFileName =  polarDir + bs +\
                 ('merged_polar_%s.txt' % get_ReString(Re))

    # T1-polar
    inputFilename = get_PresetInputFileName('iPolars_T1', params)
    commandline = params.xfoilWorkerCall + " -i \"%s\" -a \"%s\" -w polar -o \"%s\" -r %d\n" %\
                             (inputFilename, strakFoilName,
                              strakFoilName.strip('.dat'), maxRe)
    commandlines.append(commandline)

    # T2-polar
    inputFilename = get_PresetInputFileName('iPolars_T2', params)
    commandline = params.xfoilWorkerCall + " -i \"%s\" -a \"%s\" -w polar -o \"%s\" -r %d\n" %\
                             (inputFilename, strakFoilName,
                              strakFoilName.strip('.dat'), Re)
    commandlines.append(commandline)

    # merge polars
    commandline = params.strakMachineCall + " -w merge -p1 \"%s\"  -p2 \"%s\""\
                 " -m \"%s\" -c %f\n" %\
              (polarFileNameAndPath_T1, polarFileNameAndPath_T2,
               mergedPolarFileName, params.CL_switchpoint_Type2_Type1_polar)
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
    commandLines.append("echo %%TIME%%   finalizing strak-airfoil: %s >> %s\n" % (airfoilname, filename))

def insert_finishedAirfoil(commandLines, filename ):
     commandLines.append("echo %%TIME%%   finished strak-airfoil >> %s\n" % filename)

def insert_calculate_polars(commandLines, filename, airfoilname):
     commandLines.append("echo %%TIME%%   calculating polars for strak-airfoil: %s >> %s\n" % (airfoilname, filename))

def insert_calculate_polars_finished(commandLines, filename):
     commandLines.append("echo %%TIME%%   finished calculating polars >> %s\n" % filename)

def insert_MainTaskStart(commandLines, filename, rootfoilName, ReList):
    line = "echo main-task start: create whole set of strak-airfoils "
    splitlines = rootfoilName.split("root")
    rootfoilName = splitlines[0]
    numStrakfoils = len(ReList)

    for i in range(1, numStrakfoils):
        reString = get_ReString(ReList[i])
        strakfoilname = rootfoilName + "strak-" + reString
        line = line +"%s" % strakfoilname

        if (i < (numStrakfoils-1)):
            # not the last airfoil, append comma
            line = line +", "

    line = line + ">> %s\n\n" % (filename)
    commandLines.append(line)


def insert_MainTaskEnd(commandLines, filename):
    commandLines.append("echo main-task end >> %s\n" % (filename))


def insert_SubTaskStart(commandLines, filename, airfoilname):
    commandLines.append("echo sub-task start: create strak-airfoil %s >> %s\n" % (airfoilname, filename))


def insert_SubTaskEnd(commandLines, filename):
    commandLines.append("echo sub-task end >> %s\n\n" % (filename))


def insert_StatusCall(commandLines, params):
    commandLines.append("\n" + params.showStatusCall +"\n")


def calculate_MainTaskProgress(params, i):
    # get number of airfoils without root-airfoil
    numFoils = get_NumberOfAirfoils(params)-1
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
    print("Generating commandlines...")

    # create an empty list of commandlines
    commandLines = []

    # do some initializations / set local variables
    if (params.operatingMode != 'matchpolarfoils'):
        rootfoilName = get_FoilName(params, 0)
        firstIdx = 1
    else:
        rootfoilName = params.seedFoilName +'.dat'
        firstIdx = 0

    numFoils = get_NumberOfAirfoils(params)#TODO refactor
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
    for i in range (firstIdx, numFoils):

        if (params.useAlwaysRootfoil == False):
            # store previous airfoil-name
            previousFoilname = strakFoilName

        # get name of the airfoil
        strakFoilName = get_FoilName(params, i)
        seedfoilName = previousFoilname

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
            intermediateFoilName = strakFoilName.strip('.dat')
            intermediateFoilName = intermediateFoilName + ("_%d.dat" % (n+1))

            # check, if there is more than one competitor for this intermediate stage
            num = params.numberOfCompetitors[n]

            for c in range(num):
                # append competitor-number to name of intermediate airfoil
                competitorName = intermediateFoilName.strip('.dat') + ("_%d" % (c+1))

                # insert name of airfoil to be processes into progress-file
                insert_preliminaryAirfoilName(commandLines, progressFileName, competitorName)

                # generate commandline for competitor-intermediate strak-airfoil
                commandline = params.xoptfoilCall + " -i %s -r %d -a %s -o %s\n" %\
                (iFile, ReList[i], seedfoilName, competitorName)
                commandLines.append(commandline)

                # check wheather the strak-airfoils shall be smoothed after their
                # creation
                if (params.smoothStrakFoils):
                    # smooth the airfoil
                    smoothFileName = get_PresetInputFileName(smoothInputFile, params)

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
                (intermediateFoilName.strip('.dat'), num)
            commandLines.append(commandline)

            # the output-airfoil is the new seedfoil
            seedfoilName = intermediateFoilName


        # generate commandline for final strak-airfoil
        if (params.optimizationPasses > 1):
            iFileIndex = i*(params.optimizationPasses) + (n+1)
        else:
            iFileIndex = i

        # insert name of airfoil to be processes into progress-file
        insert_airfoilName(commandLines, progressFileName, strakFoilName.strip('.dat'))

        iFile = params.inputFileNames[iFileIndex]
        commandline = params.xoptfoilCall + " -i %s -r %d -a %s -o %s\n" %\
                    (iFile, ReList[i], seedfoilName,
                      strakFoilName.strip('.dat'))
        commandLines.append(commandline)

        # check wheather the strak-airfoils shall be smoothed after their
        # creation
        if (params.smoothStrakFoils):
            # smooth the airfoil
            inputFilename = get_PresetInputFileName(smoothInputFile, params)

            # compose commandline for smoothing the airfoil
            commandline = params.xfoilWorkerCall + " -w smooth -i %s -a %s -o %s\n" % \
                       (inputFilename, strakFoilName, strakFoilName.strip('.dat'))
            commandLines.append(commandline)

        # set timestamp and progress
        insert_SubTaskProgress(commandLines, progressFileName, 100.0)

        # insert message that strak-airfoil was finished
        insert_finishedAirfoil(commandLines, progressFileName)

        # insert message for polar-calculation
        insert_calculate_polars(commandLines, progressFileName, strakFoilName)

        # create T1 / T2 / merged polars for the specified Re-numbers of the
        # generated strak-airfoil
        generate_polarCreationCommandLines(commandLines, params, strakFoilName,
                                           maxReList[i], ReList[i])

        if ((i<numFoils-1)):
            # if not being the last strak-airfoil, also create T1 / T2 / merged
            # polars for the Re-numbers that were specified for the next
            # strak-airfoil, to have a kind of "benchmark" or at least
            # orientation for the next strak-airfoil
            generate_polarCreationCommandLines(commandLines, params, strakFoilName,
                                               maxReList[i+1], ReList[i+1])

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
        if ((commandlines[line_idx].find(ReString)>=0) and
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
    if (params.operatingMode == 'matchpolarfoils'):
        # nothing to do
        return

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

    print("filename for strak-machine input-data is: %s\n" % inFileName)
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



################################################################################
# function that gets a single parameter from dictionary and returns a
# default value in case of error
def get_ParameterFromDict(dict, key, default):
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
# function that gets a single boolean parameter from dictionary and returns a
#  default value in case of error
def get_booleanParameterListFromDict(dict, key, default):
    try:
        stringList = dict[key]
        # Is entry a list-entry?
        if type(stringList) is list:
            # Yes, it is a list
            result = []
            for string in stringList:
                if (string == 'true') or (string == 'True'):
                    result.append(True)
                else:
                    result.append(False)
        else:
            if (stringList == 'true') or (stringList == 'True'):
                result = True
            else:
                result = False

        # No, not a list
    except:
        result = default
        NoteMsg('parameter \'%s\' not specified, using' \
        ' default-value' % key)

    return result


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
# function that checks validity of the 'weighting-mode'-input
def check_WeightingMode(params):
    if ((params.weightingMode != 'linear_progression') &
        (params.weightingMode != 'constant') &
        (params.weightingMode != 'sinus') &
        (params.weightingMode != 'doubleSinus')):

        WarningMsg('weightingMode = \'%s\' is not valid, setting weightingMode'\
        ' to \'constant\'' % params.weightingMode)
        params.weightingMode = 'constant'


################################################################################
# function that checks validity of the 'quality'-input
def check_quality(params):
    if ((params.quality != 'low') &
        (params.quality != 'medium') &
        (params.quality != 'high')):

        WarningMsg('quality = \'%s\' is not valid, setting quality'\
        ' to \'medium\'' % params.quality)
        params.quality = 'medium'

    if params.quality == 'low':
        # double-pass optimization, camb-thick-plus / hicks-henne
        params.maxIterations = [10, 30]
        params.numberOfCompetitors = [1, 1]
        params.shape_functions = ['camb-thick-plus', 'hicks-henne'],
    elif params.quality == 'medium':
        # double-pass optimization, camb-thick-plus / hicks-henne
        params.maxIterations = [10, 120]
        params.numberOfCompetitors = [1]
        params.shape_functions = ['camb-thick-plus', 'hicks-henne'],
    else:
        # multi-pass optimization, camb-thick-plus and hicks-henne
        params.maxIterations = [10,60,160]
        params.numberOfCompetitors = [1, 3, 1]
        params.shape_functions = ['camb-thick-plus','hicks-henne','hicks-henne']

    params.optimizationPasses = len(params.maxIterations)


################################################################################
# function that checks validity of the number of op-points
def check_NumOpPoints(params):
    if (params.numOpPoints < 6):
        WarningMsg('numOpPoints must be >= 6, setting numOpPoints to minimum-value of 6')
        params.numOpPoints = 6


################################################################################
# function that checks validity of the operating-mode
def check_operatingMode(params, dict):
    if ((params.operatingMode != 'default') &
        (params.operatingMode != 'matchpolarfoils')):

        WarningMsg('operatingMode = \'%s\' is not valid, setting operatingMode'\
        ' to \'default\'' % params.operatingMode)
        params.operatingMode = 'default'

    # get matchpolarfoilname only if operating-mode is set to "matchpolarfoils"
    if (params.operatingMode == 'matchpolarfoils'):
        params.matchPolarFoilName = get_MandatoryParameterFromDict(dict,
                                                      "matchPolarFoilName")
        params.useAlwaysRootfoil = True


################################################################################
# function that gets parameters from dictionary
def get_Parameters(dict):

    # create new instance of parameters
    params = strakData()

    print("getting parameters..\n")

    # get program-call from arguments
    call = sys.argv[0]

    # was it an .exe-call ?
    if call.find('.exe') >= 0:
        # yes, perform all following calls as exe-calls
        params.scriptsAsExe = True
    else:
        # yes, perform all following calls as python-calls
        params.scriptsAsExe = False

    # get mandatory parameters first
    params.quality = get_MandatoryParameterFromDict(dict, 'quality')

    params.seedFoilName = get_MandatoryParameterFromDict(dict, 'seedFoilName')

    params.ReNumbers = get_MandatoryParameterFromDict(dict, 'reynolds')

    params.maxGlideGain = get_MandatoryParameterFromDict(dict, "maxGlideGain")

    params.CL0Gain = get_MandatoryParameterFromDict(dict, "CL0Gain")

    params.minCLGain  = get_MandatoryParameterFromDict(dict, "minCLGain")

    params.maxSpeedGain = get_MandatoryParameterFromDict(dict, "maxSpeedGain")

    params.maxSpeedShift = get_MandatoryParameterFromDict(dict, "maxSpeedShift")

    params.preMaxSpeedGain = get_MandatoryParameterFromDict(dict, "preMaxSpeedGain")

    params.maxLiftGain = get_MandatoryParameterFromDict(dict, "maxLiftGain")

    # get optional parameters
    params.maxGlideShift = get_ParameterFromDict(dict, "maxGlideShift",
                                                        params.maxGlideShift)

    params.maxGlideFactor  = get_ParameterFromDict(dict, "maxGlideFactor",
                                                        params.maxGlideFactor)

    params.linearFactor_0 = get_ParameterFromDict(dict, "linearFactor_0",
                                                        params.linearFactor_0)

    params.linearFactor_1 = get_ParameterFromDict(dict, "linearFactor_1",
                                                        params.linearFactor_1)

    params.linearFactor_2 = get_ParameterFromDict(dict, "linearFactor_2",
                                                        params.linearFactor_2)

    params.linearFactor_3 = get_ParameterFromDict(dict, "linearFactor_3",
                                                        params.linearFactor_3)

    params.linearFactor_4 = get_ParameterFromDict(dict, "linearFactor_4",
                                                        params.linearFactor_4)

    params.intersectionPoint_CL_CD = get_ParameterFromDict(dict, "intersectionPoint_CL_CD",
                                                        params.intersectionPoint_CL_CD)

    params.maxReFactor = get_ParameterFromDict(dict, "maxReynoldsFactor",
                                                        params.maxReFactor)

    params.xoptfoilTemplate = get_ParameterFromDict(dict, 'xoptfoilTemplate',
                                 params.xoptfoilTemplate)

    params.additionalOpPoints[0] = get_ParameterFromDict(dict, "additionalOpPoints",
                                                   params.additionalOpPoints[0])

    params.ReAlpha0 = get_ParameterFromDict(dict, "ReAlpha0",
                                                   params.ReAlpha0)

    params.operatingMode = get_ParameterFromDict(dict, "operatingMode",
                                              params.operatingMode)

    params.xmlFileName = get_ParameterFromDict(dict, "xmlFileName",
                                                  params.xmlFileName)

    params.numOpPoints = get_ParameterFromDict(dict, "numOpPoints",
                                               params.numOpPoints)

    params.NCrit = get_ParameterFromDict(dict, "NCrit", params.NCrit)

    params.CL_preMaxSpeed = get_ParameterFromDict(dict, "CL_preMaxSpeed",
                                                    params.CL_preMaxSpeed)

    params.CL_min = get_ParameterFromDict(dict, "CL_min", params.CL_min)

    params.weightingMode = get_ParameterFromDict(dict, "weightingMode",
                                                  params.weightingMode)

    params.minWeight = get_ParameterFromDict(dict, "minWeight",
                                                  params.minWeight)

    params.maxWeight = get_ParameterFromDict(dict, "maxWeight",
                                                  params.maxWeight)

    params.maxLiftDistance = get_ParameterFromDict(dict, "maxLiftDistance",
                                                params.maxLiftDistance)


     # get optional boolean parameters
    params.allGraphs = get_booleanParameterFromDict(dict,
                             "allGraphs", params.allGraphs)

    params.optimizeAlpha0 = get_booleanParameterListFromDict(dict,
                             "optimizeAlpha0", params.optimizeAlpha0)

    params.useAlwaysRootfoil = get_booleanParameterFromDict(dict,
                             "useAlwaysRootfoil", params.useAlwaysRootfoil)

    params.adaptInitialPerturb = get_booleanParameterFromDict(dict,
                             "adaptInitialPerturb", params.adaptInitialPerturb)

    params.smoothSeedfoil = get_booleanParameterFromDict(dict,
                             "smoothSeedfoil", params.smoothSeedfoil)

    params.smoothStrakFoils = get_booleanParameterFromDict(dict,
                             "smoothStrakFoils", params.smoothStrakFoils)

    params.smoothMatchPolarFoil = get_booleanParameterFromDict(dict,
                             "smoothMatchPolarFoil", params.smoothMatchPolarFoil)

    params.plotStrakPolars = get_booleanParameterFromDict(dict,
                             "plotStrakPolars", params.plotStrakPolars)
    DoneMsg()

    # perform parameter-checks now
    print("checking validity of all parameters..")
    check_operatingMode(params, dict)
    check_WeightingMode(params)
    check_NumOpPoints(params)
    check_quality(params)

    DoneMsg()
    return params


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


def get_WingDataFromXML(params):

    xmlFileName = ressourcesPath + '/' + params.xmlFileName
    try:
        planeData = read_planeDataFile(xmlFileName)
    except:
        ErrorMsg("file \"%s\" could not be opened.") % xmlFileName
        sys.exit(-1)

    # return data
    return planeData[0]


def copyAndSmooth_Airfoil(srcName, srcPath, destName, smooth):
    srcfoilNameAndPath = srcPath + bs + srcName + '.dat'

    if (smooth):
        print("Smoothing airfoil \'%s\', creating airfoil \'%s\'\n" %\
                       (srcName, destName))
        # smooth, rename and copy the airfoil
        inputFilename = get_PresetInputFileName(smoothInputFile, params)

        # compose system-string for smoothing the seed-airfoil
        systemString = params.xfoilWorkerCall + " -w smooth -i %s -a %s -o %s" % \
                       (inputFilename, srcfoilNameAndPath, destName)

        # execute xfoil-worker / create the smoothed root-airfoil
        system(systemString)
    else:
        print("Renaming airfoil \'%s\' to \'%s\'\n" % (srcName, destName))
        # only rename and copy the airfoil
        change_airfoilname.change_airfoilName(srcfoilNameAndPath, destName + '.dat')
        DoneMsg()


def copy_Matchpolarfoils(params):
    # get the name of the matchfoil
    matchfoilName = params.matchPolarFoilName

    # get name of seed-airfoil
    seedFoilName = params.seedFoilName

    # get the path where the airfoil can be found
    srcPath = ".." + bs + ressourcesPath

    # copy and smooth the matchfoil
    copyAndSmooth_Airfoil(matchfoilName, srcPath, matchfoilName,
                                             params.smoothMatchPolarFoil)

    # copy and smooth the seedfoil
    copyAndSmooth_Airfoil(seedFoilName, srcPath, seedFoilName,
                                             params.smoothSeedfoil)

    return matchfoilName


def generate_rootfoil(params):
    # get name of seed-airfoil
    seedFoilName = params.seedFoilName

    # get name of root-airfoil
    rootfoilName = get_FoilName(params, 0).strip('.dat')

    # get the path where the seed-airfoil can be found
    srcPath = ".." + bs + ressourcesPath

    # copy and smooth the airfoil, also rename
    copyAndSmooth_Airfoil(seedFoilName, srcPath, rootfoilName,
                                           params.smoothSeedfoil)

    return rootfoilName


def calculate_intersectionPoint(params, inputfile):
    # get operating-conditions
    operatingConditions = inputfile.get_OperatingConditions()
    #inputfile.print_OpPoints()#Debug

    CL_list = operatingConditions["op_point"]
    CD_list = operatingConditions["target_value"]
    num = len(CL_list)
    x1 = CL_list[num-2]
    x2 = CL_list[num-1]
    y1 = x1/CD_list[num-2]
    y2 = x2/CD_list[num-1]

    # extrapolate CL_CD up to CL_CD_intersection-coordinate
    result = interpolate_2(x1, x2, y1, y2, params.intersectionPoint_CL_CD)
    result = round(result, CL_decimals)
    return result


def create_new_inputFile(params, i):
     # get strak-polar
    strakPolar = params.merged_polars[i]

    # get shifted root-polar (with shifted max-glide point).
    # all target-values will be derived from the shifted root-polar.
    shifted_rootPolar = deepcopy(params.shifted_rootPolars[i])

    # create new inputfile from template
    newFile = inputFile(params)

    # get the target-values
    targets = params.targets
    CL_pre_maxLift = targets["CL_pre_maxLift"][i]

    # generate op-points in the range CL_min..CL_max
    # the CL0-oppoint will be inserted later, so generate numOpPoints-1
    newFile.generate_OpPoints(params.numOpPoints-1, params.CL_min,
                           CL_pre_maxLift)

    # distribute main opPoints, also set the target-values
    newFile.distribute_MainOpPoints(targets, i)

    # insert additional opPoints (if there are any):
    if len(params.additionalOpPoints[0])>0:
        newFile.insert_AdditionalOpPoints(params.additionalOpPoints[0])
        # The below line will use "adjusted" additional oppoints for each
        # strak-polar. Sometimes this behaviour is not desired, bcause this means
        # that the CL-value is changed.
        #newFile.insert_AdditionalOpPoints(params.additionalOpPoints[i])

    # now distribute the opPoints between the main opPoints and additional
    # oppoints equally
    newFile.distribute_IntermediateOpPoints()

    # set the target-values of all intermediate-op-points now
    newFile.set_IntermediateOpPointTargetValues(params, targets, shifted_rootPolar,
                                                strakPolar, i)

    newFile.apply_maxGlideFactor(params, i)

    # not needed anymore
    del shifted_rootPolar

    return newFile


def createAdjustedInputFile(params, i):
    adjust = True
    while (adjust):
        # create initial, unadjusted  inputFile
        newFile = create_new_inputFile(params, i)

        targets = params.targets

        # automatic adjustment of max-Lift target-value
        # for all strak-polars, adjust CD-target-value, so the intersection-point will
        # be hit, leaving a small error
        intersection_CL = calculate_intersectionPoint(params, newFile)

        if (intersection_CL > (params.intersectionPoint_CL + params.intersection_Hysteresis)):
            # increase target-value
            targets["CD_pre_maxLift"][i] = round(targets["CD_pre_maxLift"][i] + 0.00005, CD_decimals)
            # destroy the instance of newFile and shifted polar
            del newFile
        elif (intersection_CL < (params.intersectionPoint_CL - params.intersection_Hysteresis)):
            # decrease target-value
            targets["CD_pre_maxLift"][i] = round(targets["CD_pre_maxLift"][i] - 0.00005, CD_decimals)
            # destroy the instance of newFile and shifted polar
            del newFile

        else:
            # everything o.k., clear adjustment-Flag
            adjust = False

    return newFile


def generate_InputFiles(params):
    print("Generating inputfiles...")

    # calculate number of files to be created
    num_files = len(params.ReNumbers)

    # create inputFile of root-airfoil
    newFile = create_new_inputFile(params, 0)

##    # append input-file to params
##    params.inputFiles.append(newFile)
##
##    # calculate the common intersectionPoint, so maxLiftGain can be adjusted
##    # automatically
##    params.intersectionPoint_CL = calculate_intersectionPoint(params, newFile)


    # generate files for all Re-numbers
    for i in range(0, num_files):
        if (i == 0):
            # create inputFile of root-airfoil
            newFile = create_new_inputFile(params, 0)

            if (params.intersectionPoint_CL_CD != 99.0):
                # calculate the common intersectionPoint, so maxLiftGain can be adjusted
                # automatically
                params.intersectionPoint_CL = calculate_intersectionPoint(params, newFile)

        else:
            # generate file that has an adjusted maxLift-Target
            if (params.intersectionPoint_CL_CD != 99.0):
                newFile = createAdjustedInputFile(params, i)
            else:
                newFile = create_new_inputFile(params, i)

        # set the importance / weightings of the op-points
        newFile.set_Weightings(params)

        # adapt reynolds()-values, get strak-polar
        strakPolar = params.merged_polars[i]
        newFile.adapt_ReNumbers(strakPolar)

        # insert oppoint for alpha @ CL = 0
        if params.optimizeAlpha0[i]:
            newFile.insert_alpha0_oppoint(params, strakPolar,i)

        # get default-value of initialPerturb from template
        initialPerturb = newFile.get_InitialPerturb()

        if (params.adaptInitialPerturb and (i>0)):
            # calculate the initial perturb according to the change in
            # Re-number
            if (params.useAlwaysRootfoil):
                # difference calculated to Re-number of root-airfoil
                ReDiff = params.ReNumbers[0] - params.ReNumbers[i]
                # factor calculated to Re-number of root-airfoil
                ReFactor = params.ReNumbers[i] / params.ReNumbers[0]
            else:
                # difference calculated to Re-number of previous-airfoil
                ReDiff = params.ReNumbers[i-1] - params.ReNumbers[i]
                ReFactor = params.ReNumbers[i] / params.ReNumbers[i-1]

            # calculate initial perturb now.
            initialPerturb = newFile.calculate_InitialPerturb(params.ReNumbers[i],
                              ReDiff, ReFactor)

        # get Default-value for max iterations
        maxIterationsDefault = newFile.get_maxIterations()

        # multi-pass-optimization:
        # generate input-files for intermediate strak-airfoils
        for n in range(0, params.optimizationPasses):
            iFileIndex = i*(params.optimizationPasses) + n
            # set input-file name
            iFile = params.inputFileNames[iFileIndex]

            # set max number of iterations
            maxIterations = params.maxIterations[n]
            if (maxIterations == 0):
                maxIterations = maxIterationsDefault

            newFile.set_maxIterations(maxIterations)
            # set initialPerturb
            newFile.set_InitialPerturb(initialPerturb)
            # set shape_functions
            newFile.set_shape_functions (params.shape_functions [n])
            # physically create the file
            newFile.write_ToFile(iFile)
            # reduce initial perturb for the next pass
            initialPerturb = initialPerturb/2

        # append only input-file of final strak-airfoil to params
        params.inputFiles.append(newFile)


def compose_Polarfilename_T1(Re, NCrit):
    return ("T1_Re%d.%03d_M0.00_N%.1f.txt"\
        % (round_Re(Re)/1000, round_Re(Re)%1000, NCrit))


def compose_Polarfilename_T2(ReSqrt_Cl, NCrit):
    return ("T2_Re%d.%03d_M0.00_N%.1f.txt"\
 % (round_Re(ReSqrt_Cl)/1000, round_Re(ReSqrt_Cl)%1000, NCrit))


def generate_Polars(params, rootfoilName):
    # generate polars of seedfoil / root-airfoil:
    print("Generating polars for airfoil %s..." % rootfoilName)

    # compose polar-dir
    polarDir = '.' + bs + rootfoilName + '_polars'

    # create polars, polar-file-Names and input-file-names from Re-Numbers
    for ReIdx in range(len(params.ReNumbers)):
        # get Re, maxRe
        Re = params.ReNumbers[ReIdx]
        maxRe = params.maxReNumbers[ReIdx]

        # create polar-file-Name T1-polar from maxRe-Number
        polarFileName_T1 = compose_Polarfilename_T1(maxRe, params.NCrit)
        polarFileNameAndPath_T1 = polarDir + bs + polarFileName_T1
        params.polarFileNames_T1.append(polarFileNameAndPath_T1)

        # create polar-file-Name T2-polar from Re-Number
        polarFileName_T2 = compose_Polarfilename_T2(Re, params.NCrit)
        polarFileNameAndPath_T2 = polarDir + bs + polarFileName_T2
        params.polarFileNames_T2.append(polarFileNameAndPath_T2)

        # generate inputfilename from Re-number
        inputFilename = params.xoptfoilInputFileName.strip('.txt')
        inputFilename = inputFilename + ("_%s.txt" % get_ReString(Re))

        # multi-pass-optimization: generate input-filenames for intermediate-airfoils
        for n in range(1, (params.optimizationPasses)):
            name = inputFilename.strip(".txt")
            name = name + ("_%d.txt" % n)
            params.inputFileNames.append(name)

        # append name of inputfile for final airfoil
        params.inputFileNames.append(inputFilename)

        # compose string for system-call of XFOIL-worker for T1-polar generation
        airfoilName = rootfoilName + '.dat'
        inputFilename = get_PresetInputFileName(T1_polarInputFile, params)
        systemString_T1 = params.xfoilWorkerCall + " -i \"%s\" -o \"%s\" -w polar -a \"%s\" -r %d" %\
                              (inputFilename, rootfoilName, airfoilName, maxRe)

        # compose string for system-call of XFOIL-worker for T2-polar generation
        inputFilename = get_PresetInputFileName(T2_polarInputFile, params)
        systemString_T2 = params.xfoilWorkerCall + " -i \"%s\" -o \"%s\" -w polar -a \"%s\" -r %d" %\
                                 (inputFilename, rootfoilName, airfoilName, Re)

        # import polar type 1
        newPolar_T1 = polarData()
        try:
            newPolar_T1.import_FromFile(polarFileNameAndPath_T1)
        except:
            # execute xfoil-worker / create T1 polar-file
            print("Generating polar %s" % polarFileName_T1)
            system(systemString_T1)
            newPolar_T1.import_FromFile(polarFileNameAndPath_T1)

        params.T1_polars.append(newPolar_T1)

        # import polar type 2
        newPolar_T2 = polarData()
        try:
            newPolar_T2.import_FromFile(polarFileNameAndPath_T2)
        except:
            # execute xfoil-worker / create T2 polar-file
            print("Generating polar %s" % polarFileName_T2)
            system(systemString_T2)
            newPolar_T2.import_FromFile(polarFileNameAndPath_T2)

        params.T2_polars.append(newPolar_T2)

        # merge T1/T2 polars at Cl switching-point
        mergedPolar = newPolar_T2.merge(newPolar_T1,
                         params.CL_switchpoint_Type2_Type1_polar, maxRe)

        # write merged polar to file
        polarFileNameAndPath = polarDir + bs + ('merged_polar_%3s.txt' %\
                              get_ReString(newPolar_T2.Re))
        mergedPolar.write_ToFile(polarFileNameAndPath)

        # change resolution of alpha for accurate conversion between CL /CD/ alpha
        mergedPolar.set_alphaResolution(params.alpha_Resolution)

        # analyze merged polar
        mergedPolar.analyze(params)

        # add merged polar to params
        params.merged_polars.append(mergedPolar)

        # create shifted root-polars. This means that the max-glide-point
        # (maximum CL/CD-value) will be shifted left or right by a certain
        # shift-factor that comes from user-parameters

        # Use always root-polar for this operation
        rootPolar = params.merged_polars[0]

        # get shift-value according to strak-airfoil / index
        shiftValue = params.maxGlideShift[ReIdx]

        # shift the max-glide-point now, create a new polar
        shiftedPolar = rootPolar.get_shiftedPolar(shiftValue, params)

        # also update values like maxSpeed, maxGlide etc.
        shiftedPolar.analyze(params)

        # append the new, shifted root-polar to params
        params.shifted_rootPolars.append(shiftedPolar)

    DoneMsg()


def import_strakPolars(params):
    for i in range(1, len(params.ReNumbers)-1):
        # get name of the strak-airfoil
        strakFoilName = get_FoilName(params, i)

        # compose polar-dir of strak-airfoil-polars
        polarDir = '.' + bs + strakFoilName.strip('.dat') + '_polars'
        fileName = "merged_polar_%s.txt" % get_ReString(params.ReNumbers[i+1])
        polarFileNameAndPath = polarDir + bs + fileName

        try:
            newPolar = polarData()
            newPolar.import_FromFile(polarFileNameAndPath)
            params.strak_polars.append(newPolar)
        except:
            pass


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
    names = operatingConditions["name"]

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


def generate_TargetPolars(params):
    # local variable
    targetPolars = params.target_polars
    inputFiles = params.inputFiles
    Re = params.ReNumbers
    numTargetPolars = len(Re)
    rootPolar = params.merged_polars[0]

    # get name of the root-airfoil
    airfoilName = (get_FoilName(params, 0)).strip('.dat')
    print("Generating target polars for airfoil %s..." % airfoilName)

    for i in range(numTargetPolars):

        # get inputfile
        inputFile = inputFiles[i]

        # create new target polar
        targetPolar = polarData()

        # get strak-Polar
        strakPolar = params.merged_polars[i]

        # put the necessary data into the polar
        set_PolarDataFromInputFile(targetPolar, rootPolar, inputFile,
                                  airfoilName, Re[i], i)

        # append the new target polar to list of target_polars
        params.target_polars.append(targetPolar)

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

################################################################################
# Main program
if __name__ == "__main__":
    init()

    # get command-line-arguments or user-input
    (strakDataFileName, workerAction, polarFile_1, polarFile_2,
      mergedPolarFile, mergeCL) = get_Arguments()

   ## check working-directory, have we been started from "scripts"-dir?
   # if (!getcwd().find("scripts")>=0):
   # chdir("..")

    # decide what action to perform.
    if (workerAction == 'merge'):
        # do nothing else but merging the polars
        merge_Polars(polarFile_1, polarFile_2 , mergedPolarFile, mergeCL)
        exit(0)

    # try to open .json-file
    try:
        strakDataFile = open(strakDataFileName)
    except:
        ErrorMsg('failed to open file %s' % strakDataFileName)
        sys.exit(-1)

    # load dictionary from .json-file
    try:
        strakdata = load(strakDataFile)
        strakDataFile.close()
    except:
        ErrorMsg('failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
        sys.exit(-1)

    # get strak-machine-parameters from dictionary
    params = get_Parameters(strakdata)

    # read plane-data from XML-File, if requested //TODO: only wing-data
    if (params.xmlFileName != None):
        params.wingData = get_WingDataFromXML(params)

    # calculate further values like max Re-numbers etc., also setup
    # calls of further tools like xoptfoil
    params.calculate_DependendValues()

    # get current working dir
    params.workingDir = getcwd()

    # check if output-folder exists. If not, create folder.
    if not path.exists(buildPath):
        makedirs(buildPath)

    # check if airfoil-folder exists. If not, create folder.
    if not path.exists(buildPath + bs + airfoilPath):
        makedirs(buildPath + bs + airfoilPath)

    # change working-directory to output-directory
    chdir(params.workingDir + bs + buildPath)

    # get current working dir again
    params.buildDir = getcwd()

    # get name of root-airfoil according to operating-mode
    if (params.operatingMode == 'matchpolarfoils'):
        rootfoilName = copy_Matchpolarfoils(params)
    else:
        rootfoilName = generate_rootfoil(params)
        # copy root-foil to airfoil-folder, as it can be used
        # as the root airfoil without optimization
        systemString = ("copy %s %s" + bs + "%s\n\n") % \
        (rootfoilName +'.dat', airfoilPath, rootfoilName + '.dat')
        system(systemString)

    if (params.operatingMode == 'fromtargetpolar'):
        ErrorMsg("Not implemented yet")
        sys.exit(-1)
        # TODO implement
        # read target-polars from file
        # generate input-Files directly from target-polars
        # (no) plotting option (?)
    else:
        # generate polars of root-airfoil, also analyze
        generate_Polars(params, rootfoilName)

        # import polars of strak-airfoils, if they exist
        import_strakPolars(params)

        # calculate target-values for the main op-points
        params.calculate_MainTargetValues()

        # calculate the additional op-points
        #params.calculate_AdditionalOpPoints()#TODO remove

        # generate input-Files
        generate_InputFiles(params)

        # generate target polars and write to file
        generate_TargetPolars(params)

    # generate Xoptfoil command-lines
    commandlines = generate_Commandlines(params)

    # change working-directory
    chdir(".." + bs)

    # generate batchfile
    print("Generating batchfiles...")
    if (params.generateBatch == True):
        print ('generating batchfile \'%s\'' % params.batchfileName)
        generate_Batchfile(params.batchfileName, commandlines)

        print ('generating batchfiles for each single airfoil of the strak')
        generate_StrakBatchfiles(params, commandlines)
    DoneMsg()

    # create an instance of polar graph
    graph = polarGraph()

    # show graph
    graph.draw(params)

    print("Ready.")
