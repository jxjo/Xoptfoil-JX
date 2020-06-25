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
import json
import sys, os
from matplotlib import pyplot as plt
import matplotlib.image as mpimg
import numpy as np
import math
import pip
import f90nml
from copy import deepcopy
from scipy.signal import savgol_filter

# paths and separators
bs = "\\"
presetsPath = 'ressources' + bs + 'presets'
imagesPath = 'ressources' + bs + 'images'
logoName = 'strakmachine.png'

# fixed filenames
T1_polarInputFile = 'iPolars_T1.txt'
T2_polarInputFile = 'iPolars_T2.txt'

# fonts
csfont = {'fontname':'Segoe Print'}

# number of decimals in the generated input-files
Cl_decimals = 4 # lift
Cd_decimals = 6 # drag
Cl_Cd_decimals = 2 # lift/drag
Al_decimals = 4 # alpha

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
ls_targetPolar = 'dotted'
lw_targetPolar = 0.6

pre_CL_maxLift_factor = 0.96

################################################################################
#
# install missing packages
#
################################################################################
def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        import pip
        pip.main(['install', package])
    finally:
        globals()[package] = importlib.import_module(package)

#TODO improve !! e.g. use linear interpolation etc.
################################################################################
#
# helper function that finds a peak
#
################################################################################
def findPeak(list, height):
        # init peak-searcher
        peak_max = 0.0
        peak_max_idx = 0
        peak_left_idx = 0
        peak_right_idx = 0
        searchLeftBorder = True
        searchRightBorder = False

        #first: find absolute maximum
        idx = 0
        for value in list:
            if value > peak_max:
                peak_max = value
                peak_max_idx = idx
            idx = idx+1

        peak_limit = peak_max - height

        # second: find left and right border
        idx = 0

        # walk through the list
        for value in list:
            if (searchLeftBorder == True):
                #searching for peak_left_idx
                if value >= peak_limit:
                    peak_left_idx = idx
                    peak_right_idx = idx
                    searchLeftBorder = False
                    searchRightBorder = True
            if (searchRightBorder == True):
                #searching for peak_right_idx
                if value <= peak_limit:
                    peak_right_idx = idx
                    searchRightBorder = False
            idx = idx + 1

         # calculate new maximum idx as the idx in the middle between the borders
        #print (peak_max_idx) #Debug
        #peak_max_idx = (peak_right_idx + peak_left_idx)/2 Debug
        #print (peak_max_idx, peak_left_idx, peak_right_idx) #Debug

        return peak_max_idx



################################################################################
#
# example-dictionary for creating .json-file
#
################################################################################
strakdata = {
            # folder containing the inputs-files
            "inputFolder": 'ressources',
            # folder containing the output / result-files
            "outputFolder": 'build',
            # name of XFLR5-xml-file
            "XMLfileName": 'wing.xml',
            # ReSqrt(Cl)-numbers of the strak
            "ReNumbers": [220000, 190000, 160000, 130000, 100000, 70500],
            # determines max Re-number to use for Type2 polar. Will switch to
            # Type1-polar at maxReFactor * ReNumber[i]
            "maxReFactor": 2.5,
            # list of chord-lenghts
            "chordlengths": [],
            # ReSqrt(Cl) of root airfoil if using chord-lenghts instead of Re-numbers
            "ReSqrtCl": '150000',
            # root airfoil name
            "seedFoilName": 'rg15.dat',
            # type of the strak that shall be developed
            "strakType":  'Generic',
             # name of the xoptfoil-inputfile for strak-airfoil(s)
            "strakInputFileName": 'istrak.txt',
            # generate batchfile for running Xoptfoil
            "generateBatchfile" : 'true',
            # name of the batchfile
            "batchfileName" : 'make_strak.bat',
            # operating-mode for strakmachine
            "operatingMode" : 'default',
            # use always root-airfoil or use predecessing airfoil
            "useAlwaysRootfoil" : 'false',
            # adapt initial_perturb in input file according to differenc in Re-numbers
            "adaptInitialPerturb": 'true',
            # projected maxGlide loss (percent), absolte value
            "maxGlideLoss": 0.008,
            # projected maxSpeed gain between root and strak-airfoil (percent)
            "maxSpeedGain": 0.5,
            # projected maxLift gain between root and strak-airfoil (percent)
            "maxLiftGain": 0.3
            }

def getPresetInputFileName(strakType):
    # get real path of the script
    pathname = os.path.dirname(sys.argv[0])
    scriptPath = os.path.abspath(pathname)

    # get list of all existing files
    fileList = getListOfFiles(scriptPath + bs + presetsPath)

    # search the whole list of files for the desired strak-type
    for name in fileList:
        if name.find(strakType) >= 0:
            return name

################################################################################
#
# inputfile class
#
################################################################################
class inputFile:
    def __init__(self, strakType):
        self.values = {}
        self.presetInputFileName = ""
        self.idx_maxSpeed = 0
        self.idx_maxGlide = 0
        self.idx_preClmax = 0
        self.idx_Clmax = 0
        self.idx_additionalOpPoints = []

        # get real path of the script
        pathname = os.path.dirname(sys.argv[0])
        scriptPath = os.path.abspath(pathname)
        presetInputFiles = getListOfFiles(scriptPath + bs + presetsPath)
        self.getInputFileName(presetInputFiles, strakType)

        # read input-file as a Fortan namelist
        self.values = f90nml.read(self.presetInputFileName)
        #operatingConditions = self.values["operating_conditions"]#Debug
        #print (operatingConditions)#Debug

        # clean-up file
        self.removeDeactivatedOpPoints() #TODO remove


    # removes an op-Point if weighting is beyond a certain limit
    def removeDeactivatedOpPoints(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # make a copy
        newOperatingConditions = operatingConditions.copy()

        # clear all opPoints of the copy
        self.deleteAllOpPoints(newOperatingConditions)

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


    def getInputFileName(self, fileList, strakType):
        # search the whole list of files for the desired strak-type
        for name in fileList:
            if name.find(strakType) >= 0:
                self.presetInputFileName = name
                return

##    def getPresetInputFileName(self):
##        return self.presetInputFileName


    def changeTargetValue(self, keyName, targetValue):
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
                    targetValue = round(targetValue, Cd_decimals)
                elif (opPointType == 'spec-al'):
                    # target-value is lift-value
                    targetValue = round(targetValue, Cl_decimals)

                # change target value
                operatingConditions['target_value'][idx] = targetValue

                # write-back operatingConditions
                self.values["operating_conditions"] = operatingConditions
                return
            idx = idx + 1


    def changeOpPoint(self, keyName, op_point):
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
                    op_point = round(op_point, Cl_decimals)
                elif (opPointType == 'spec-al'):
                    # opPoint-value is alpha-value
                    op_point = round(op_point, Al_decimals)

                # change op_point
                operatingConditions['op_point'][idx] = op_point
                # write-back operatingConditions
                self.values["operating_conditions"] = operatingConditions
                return
            idx = idx + 1


    def changeWeighting(self, idx, new_weighting):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

         # set new weighting
        operatingConditions['weighting'][idx] = new_weighting



    def getOpPoint(self, keyName):
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


    def getTargetValue(self, keyName):
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
    def getOpPointType(self, keyName):
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


    def setInitialPerturb(self, ReDiff):
        ReDiffList =  [(150000/5), 150000]
        perturbList = [(0.01/5), 0.01]
        pso_tolList = [(0.0015/5), 0.0015]

        # calculate corresponding perturb
        perturb = np.interp(ReDiff, ReDiffList, perturbList)
        optimization_options = self.values["optimization_options"]
        optimization_options['initial_perturb'] = perturb

        # also adapt pso_tol!!!
        pso_tol = round(np.interp(ReDiff, ReDiffList, pso_tolList),6)
        particle_swarm_options = self.values["particle_swarm_options"]
        particle_swarm_options['pso_tol'] = pso_tol
        print("Re-Diff is %d, setting initial_perturb to %.4f and pso_tol to %.5f" %\
         (ReDiff, perturb, pso_tol))


    def setNewTargetValues(self, start, end, rootPolar, x1, x2, y1, y2):
        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # get op-points / names
        opPoints = operatingConditions["op_point"]
        opPointNames = operatingConditions["name"]

        for idx in range(start, end):
            # get opPoint
            opPoint = opPoints[idx]

            # find CD value of root-polar
            CL_CD = rootPolar.find_CL_CD(opPoint)

            # determine the factor for scaling the CD-values by
            # linear interpolation
            factor = self.linearEquation(x1, x2, y1, y2, opPoint)

            # calculate new target-value
            CL_CD_new = round((CL_CD * factor) , Cl_Cd_decimals)
            CD_new = opPoint / CL_CD_new

            # set new target-value
            self.changeTargetValue(opPointNames[idx], CD_new)


    def SetIntermediateOpPointTargetValues(self, targets, rootPolar,
                                           strakPolar, i):
        # determine factors (CL/CD) for the main op-points
        # maxGlide
        CL_maxGlide = targets["CL_maxGlide"][0]
        CD_maxGlide_root = targets["CD_maxGlide"][0]
        CD_maxGlide_strak = targets["CD_maxGlide"][i]
        CL_CD_maxGlide_root = CL_maxGlide / CD_maxGlide_root
        CL_CD_maxGlide_strak = CL_maxGlide / CD_maxGlide_strak
        factor_maxGlide = CL_CD_maxGlide_strak / CL_CD_maxGlide_root

        # maxSpeed
        CL_maxSpeed_root = targets["CL_maxSpeed"][0]
        CL_maxSpeed_strak = targets["CL_maxSpeed"][i]
        CD_maxSpeed_root = targets["CD_maxSpeed"][0]
        CD_maxSpeed_strak = targets["CD_maxSpeed"][i]
        CL_CD_maxSpeed_root = CL_maxSpeed_root / CD_maxSpeed_root
        CL_CD_maxSpeed_strak = CL_maxSpeed_strak / CD_maxSpeed_strak
        factor_maxSpeed = CL_CD_maxSpeed_strak / CL_CD_maxSpeed_root

        # maxLift
        pre_CL_maxLift_root = targets["pre_CL_maxLift"][0]
        pre_CL_maxLift_strak = targets["pre_CL_maxLift"][i]
        pre_CD_maxLift_root = targets["pre_CD_maxLift"][0]
        pre_CD_maxLift_strak = targets["pre_CD_maxLift"][i]
        pre_CL_CD_maxLift_root = pre_CL_maxLift_root / pre_CD_maxLift_root
        pre_CL_CD_maxLift_strak = pre_CL_maxLift_strak / pre_CD_maxLift_strak
        factor_maxLift = pre_CL_CD_maxLift_strak / pre_CL_CD_maxLift_root

        # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # get OpPoint-names
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]

        #TODO: all op-points before maxSpeed!!

        # determine start and end-index for all op-points between
        # maxSpeed and maxGlide
        start = 0
        end = self.idx_maxSpeed

        # now change all target-values of these op-points
        y1 = factor_maxSpeed
        y2 = factor_maxSpeed
        x1 = params.CL_min
        x2 = CL_maxSpeed_strak

        self.setNewTargetValues(start, end, rootPolar, x1, x2, y1, y2)

        # determine start and end-index for all op-points between
        # maxSpeed and maxGlide
        start = self.idx_maxSpeed + 1
        end = self.idx_maxGlide

        # now change all target-values of these op-points
        y1 = factor_maxSpeed
        y2 = factor_maxGlide
        x1 = CL_maxSpeed_strak
        x2 = CL_maxGlide

        self.setNewTargetValues(start, end, rootPolar, x1, x2, y1, y2)

        # determine start and end-index for all op-points between
        # maxGlide and pre_maxLift
        start = self.idx_maxGlide + 1
        end = self.idx_preClmax + 1

        # now change all target-values of these op-points
        y1 = factor_maxGlide
        y2 = factor_maxGlide#factor_maxLift#
        x1 = CL_maxGlide
        x2 = pre_CL_maxLift_strak

        #print(factor_maxSpeed, factor_maxGlide, factor_maxLift)#Debug

        self.setNewTargetValues(start, end, rootPolar, x1, x2, y1, y2)


    def generateOpPoints(self, numOpPoints, Cl_min, Cl_max, alpha_Cl_max):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # clear operating conditions
        self.deleteAllOpPoints(operatingConditions)

        # calculate the intervall
        diff = (Cl_max - Cl_min) / (numOpPoints-1)

        # always start at Cl_min for first opPoint
        op_point = Cl_min
        op_mode = 'spec-cl'
        optimization_type = 'target-drag'
        target_value = 0.0
        weighting = 1.0
        reynolds = None

        # now build up new opPoints
        for i in range(numOpPoints):
            # set generic op-point-name
            name = "op_%s" % i

            # last opPoint has always lift-target
            if (i == (numOpPoints-1)):
                op_point_value = round(alpha_Cl_max, Al_decimals)
                op_mode = 'spec-al'
                optimization_type = 'target-lift'
                target_value = Cl_max
            else:
                # round opPoint
                op_point_value = round(op_point, Cl_decimals)

            # add new opPoint to dictionary
            self.addOppoint(name, op_mode, op_point_value, optimization_type,
                                            target_value, weighting, reynolds)
            # increment op-point
            op_point = op_point + diff

        # set new number of opPoints
        operatingConditions['noppoint'] = numOpPoints
#        print(self.values["operating_conditions"])#Debug
#        print("Done.")#Debug


    def findClosestClOpPoint(self, Cl):
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


    def getLastOpPoint(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        numOpPoints = len(operatingConditions["op_point"])

        # return last opPoint
        name = operatingConditions["name"][numOpPoints-1]
        idx = numOpPoints-1
        return (name, idx)


    def printOpPoints(self):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]
        targetValues = operatingConditions["target_value"]

        print(opPointNames)
        print(opPoints)
        print(targetValues)
        print("Done.")


    # distributes main-oppoints
    def distributeMainOpPoints(self, targets, i):

        # get all op-points and target-values
        CL_maxSpeed = targets["CL_maxSpeed"][i]
        CD_maxSpeed = targets["CD_maxSpeed"][i]
        CL_maxGlide = targets["CL_maxGlide"][i]
        CD_maxGlide = targets["CD_maxGlide"][i]
        pre_CL_maxLift = targets["pre_CL_maxLift"][i]
        pre_alpha_maxLift = targets["pre_alpha_maxLift"][i]
        CL_maxLift = targets["CL_maxLift"][i]
        alpha_maxLift = targets["alpha_maxLift"][i]

        print(targets["CD_maxGlide"])#Debug

        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        opPointNames = operatingConditions["name"]
        opPoints = operatingConditions["op_point"]

        # get opPoint
        (opPoint_maxLift, self.idx_Clmax) = self.getLastOpPoint()

        # change value
        self.changeOpPoint(opPoint_maxLift, pre_alpha_maxLift)
        self.changeTargetValue(opPoint_maxLift, pre_CL_maxLift)

        self.idx_preClmax = self.idx_Clmax-1
        opPoint_preClmax = opPointNames[self.idx_preClmax]

        # get opPoint
        (opPoint_maxGlide, self.idx_maxGlide) =\
                            self.findClosestClOpPoint(CL_maxGlide)

        # correct oppoint, if necessary
        if (self.idx_maxGlide >= self.idx_preClmax):
            self.idx_maxGlide = self.idx_preClmax -1
            opPoint_maxGlide = opPointNames[self.idx_maxGlide]

        # get opPoint
        (opPoint_maxSpeed, self.idx_maxSpeed) =\
                                 self.findClosestClOpPoint(CL_maxSpeed)

        # correct oppoint, if necessary
        if (self.idx_maxSpeed >= self.idx_maxGlide):
            self.idx_maxSpeed = self.idx_maxGlide -1
            opPoint_maxSpeed = opPointNames[self.idx_maxSpeed]

        # change values
        self.changeOpPoint(opPoint_preClmax, (pre_CL_maxLift*pre_CL_maxLift_factor))
        self.changeOpPoint(opPoint_maxGlide, CL_maxGlide)
        self.changeOpPoint(opPoint_maxSpeed, CL_maxSpeed)


        # set ramaining target-values of main-op-points
        # target-value of pre_CL_maxLift will be set later
        self.changeTargetValue(opPoint_maxGlide, CD_maxGlide)
        self.changeTargetValue(opPoint_maxSpeed, CD_maxSpeed)

        # change names
        opPointNames[self.idx_Clmax] = 'alphaClmax'
        opPointNames[self.idx_preClmax] = 'preClmax'
        opPointNames[self.idx_maxGlide] = 'maxGlide'
        opPointNames[self.idx_maxSpeed] = 'maxSpeed'


    def insertAdditionalOpPoints(self, opPoints):
        if len(opPoints) == 0:
            # nothing to do
            return

        num = 0
        #self.printOpPoints()#Debug

        for opPoint in opPoints:

            # compose name
            name = "add_op_%s" % num

            # insert new op-Point, get index
            idx = self.insertOpPoint(name, 'spec-cl', opPoint, 'target-drag',
             0.0, 1.0, None)

            # correct idx of main op-points
            if (idx <= self.idx_maxSpeed):
                self.idx_maxSpeed = self.idx_maxSpeed + 1

            if (idx <= self.idx_maxGlide):
                self.idx_maxGlide = self.idx_maxGlide + 1

            if (idx <= self.idx_preClmax):
                self.idx_preClmax = self.idx_preClmax + 1

            if (idx <= self.idx_Clmax):
                self.idx_Clmax = self.idx_Clmax + 1

            # append idx to list of additional op-points
            self.idx_additionalOpPoints.append(idx)
            num = num + 1

        #self.printOpPoints()#Debug


    # "start" and "end" are both fixed op-points. All op-points between
    # start and end shall be distributed equally.
    def distributeEqually(self, start, end):
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
            newValue = round(Cl_start + (num*Cl_interval), Cl_decimals)
            operatingConditions["op_point"][idx] = newValue
            num = num + 1


    # distribute all intermediate-oppoints
    def distributeIntermediateOpPoints(self, polarData):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

        # generate a sorted list of all fixed op-point-idx
        fixed_opPoints = []
        fixed_opPoints.append(self.idx_maxSpeed)
        fixed_opPoints.append(self.idx_maxGlide)
        fixed_opPoints.append(self.idx_preClmax)

        for idx in self.idx_additionalOpPoints:
            fixed_opPoints.append(idx)

        fixed_opPoints.sort()
        #print (fixed_opPoints) Debug

        # distribute the opPoints between the fixed opPoints equally
        for idx in range(len(fixed_opPoints)-1):
            start = fixed_opPoints[idx]
            end = fixed_opPoints[idx+1]
            self.distributeEqually(start, end)

        #print(self.values["operating_conditions"])#Debug
        #print("Done.")#Debug


    def linearEquation(self, x1, x2, y1, y2, x):
        y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
        return y


    def SetWeightings(self, params):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]#Debug
        opPoints = operatingConditions["op_point"]

        # determine min and max weight
        max_weigth = params.max_weight
        min_weight = params.min_weight

        # set weight of CLmax to max_weight
        self.changeWeighting(self.idx_Clmax, max_weigth)

        # evaluate the weighting-mode
        if (params.weighting_mode == 'constant'):
            # set every op-point to constant min_weight (but not Clmax)
            for idx in range((self.idx_Clmax)):
                self.changeWeighting(idx, min_weight)

        elif (params.weighting_mode == 'linear_progression'):
            # increment weighting from min_weight to max_weight
            # do not change Clmax
            num_intervals = self.idx_Clmax
            diff = (max_weigth - min_weight) / num_intervals

            for idx in range(num_intervals):
                weight = round(min_weight + (idx*diff), 2)
                self.changeWeighting(idx, weight)

        elif (params.weighting_mode == 'sinus'):
            # change weighting with a sinusoidal shape
            # do not change Clmax

            # all op-points from Cl-min up to maxGlide
            for idx in range(self.idx_maxGlide):
                x1 = opPoints[0]
                x2 = opPoints[self.idx_maxGlide]
                y1 = 0.0
                y2 = math.pi/2
                y = self.linearEquation(x1, x2, y1, y2, opPoints[idx])

                diff = (max_weigth - min_weight) * math.sin(y)
                weight = round((min_weight + diff), 2)
                self.changeWeighting(idx, weight)

            # all op-points from maxGlide up to Clmax
            for idx in range(self.idx_maxGlide, self.idx_preClmax+1):
                x1 = opPoints[self.idx_maxGlide]
                x2 = opPoints[self.idx_preClmax]
                y1 = math.pi/2
                y2 = 0.0
                y = self.linearEquation(x1, x2, y1, y2, opPoints[idx])

                diff = (max_weigth - min_weight) * math.sin(y)
                weight = round((min_weight + diff), 2)
                self.changeWeighting(idx, weight)

        #print(operatingConditions["weighting"])#Debug
        #print("Done.")#Debug


    def adaptReNumbers(self, polarData):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]

       # walk through the opPoints
        for idx in range(len(operatingConditions["weighting"])):#TODO use other key
            if(operatingConditions["op_mode"][idx] == 'spec-cl'):
                # check the op-point-value
                Cl = operatingConditions["op_point"][idx]
                if (Cl <= polarData.Cl_switchpoint_Type2_Type1_polar):
                    # adapt maxRe --> Type 1 oppoint
                    operatingConditions["reynolds"][idx] = int(polarData.maxRe)
                    print("adapted oppoint @ Cl = %0.3f, Type 1, Re = %d\n" % \
                          (Cl, int(polarData.maxRe)))


    # scales the target-values of a list of oppoints by a certain factor.
    def scaleTargetValues(self, factor, opPointList):
         # scale all target-values in list
        for opPointName in opPointList:
            try:
                value = self.getTargetValue(opPointName)
                value = value * factor
                self.changeTargetValue(opPointName, value)
            except:
                print("opPoint %s was skipped" % opPointName)


    # shifts the target-values of a list of oppoints by a certain difference.
    def shiftTargetValues(self, diff, opPointList):
         # scale all target-values in list
        for opPointName in opPointList:
            try:
                value = self.getTargetValue(opPointName)
                self.changeTargetValue(opPointName, value)
            except:
                print("opPoint %s was skipped" % opPointName)


    # the target-value of the given oppoint will be set according to the
    # value that is found in the polar
    def adaptTargetValueToPolar(self, opPointName, polar):
        # get value of opPoint
        opPointValue = self.getOpPoint(opPointName)
        print(polar.polarName)#debug

        # what kind of value is it?
        opPointType = self.getOpPointType(opPointName)

        if (opPointType == 'spec-al'):
            # oppoint is alpha-value, get target-value from polar
            targetValue = polar.find_CL(opPointValue)
        else:
            # oppoint is Cl-value, get target-value from polar
            targetValue = polar.find_CD(opPointValue)

        # set new target-value of oppoint
        self.changeTargetValue(opPointName, targetValue)


    # adapt all target-values to the given polar-data
    def adaptAllOppointsToPolar(self, polarData):
        # get operating-conditions
        operatingConditions = self.values["operating_conditions"]
        num_points = operatingConditions['noppoint']

        # all target-values will be set to the corresponding polar-value
        for i in range(num_points):
            name = operatingConditions["name"][i]
            self.adaptTargetValueToPolar(name, polarData)

        # adapt Re-numbers for Type2 / Type1 oppoints
        self.adaptReNumbers(polarData)


    def deleteAllOpPoints(self, operatingConditions):
        # clear operating conditions
        operatingConditions["name"] = []
        operatingConditions["op_mode"] = []
        operatingConditions["op_point"] = []
        operatingConditions["optimization_type"] = []
        operatingConditions["target_value"] = []
        operatingConditions["weighting"] = []
        operatingConditions["reynolds"] = []
        operatingConditions['noppoint'] = 0


    def clearGeoTargets(self):
        if 'geometry_targets' in self.values:
            del self.values['geometry_targets']



    def addOppoint(self, name, op_mode, op_point, optimization_type,
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


    # insert a new oppoint in the list
    def insertOpPoint(self, name, op_mode, op_point, optimization_type,
                                            target_value, weighting, reynolds):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        # find index
        num_opPoints = len(operatingConditions["op_point"])

        for idx in range(num_opPoints):
            op_mode_list = operatingConditions["op_mode"][idx]
            op_point_list = operatingConditions["op_point"][idx]

            if ((op_mode_list== 'spec-cl') & (op_point_list >= op_point)):

                # insert new oppoint
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


##    # add a "target-drag" oppoint to operating-conditions
##    def addTargetPolarOppoint(self, Cl, Cd):
##        self.addOppoint('target_polar', 'spec-cl', Cl, 'target-drag', Cd, 1.0)
##
##
##    # delete all existing oppoints and set new ones from polar-data
##    def SetOppointsFromPolar(self, polarData, numOppoints):
##        Cl_min = polarData.CL[0]
##        Cl_max = polarData.CL_maxLift
##        Cl_increment = (Cl_max - Cl_min) / numOppoints
##
##        # clear operating conditions
##        self.deleteAllOpPoints(self.values["operating_conditions"])
##
##        # clear any existing geo-targets
##        self.clearGeoTargets()
##
##        # add new oppoints
##        for i in range (numOppoints):
##            Cl = round(Cl_min + (i * Cl_increment), Cl_decimals)
##            Cd = round(polarData.find_CD(Cl), Cd_decimals)
##            #print "Cl:%f, Cd:%f" % (Cl, Cd) #Debug
##            self.addTargetPolarOppoint(Cl, Cd)


    def getOperatingConditions(self):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        return operatingConditions

    def writeToFile(self, fileName):
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
        print("Done.")


################################################################################
#
# strakData class
#
################################################################################
class strakData:
    def __init__(self):
        self.inputFolder = ''
        self.outputFolder = ''
        self.airfoilFolder = ''
        self.xmlFileName = None
        self.strakInputFileName = 'i-strak.txt'
        self.ReSqrtCl = 150000
        self.numOpPoints = 16
        self.weighting_mode = 'constant'
        self.min_weight = 1.0
        self.max_weight = 1.0
        self.useWingPlanform = True
        self.generateBatch = True
        self.batchfileName = 'make_strak.bat'
        self.wingData = None
        self.strakType = "F3F"
        self.operatingMode = 'default'
        self.useAlwaysRootfoil = False
        self.showTargetPolars = True
        self.adaptInitialPerturb = True
        self.seedFoilName = ""
        self.matchPolarFoilName = ""
        self.smoothSeedfoil = True
        self.smoothMatchPolarFoil = True
        self.ReNumbers = []
        self.additionalOpPoints = [[]]
        self.CL_min = -0.1
        self.chordLengths = []
        self.maxReFactor = 3.0
        self.maxReNumbers = []
        self.Cl_switchpoint_Type2_Type1_polar = 0.05
        self.polarFileNames = []
        self.polarFileNames_T1 = []
        self.polarFileNames_T2 = []
        self.inputFileNames = []
        self.T1_polars = []
        self.T2_polars = []
        self.merged_polars = []
        self.target_polars = []
        self.inputFiles = []
        self.maxGlideLoss = [0.008]
        self.maxSpeedGain = [0.5]
        self.maxLiftGain = [0.3]
        self.targets ={
                        "CL_maxSpeed": [],
                        "CD_maxSpeed": [],
                        "CL_maxGlide": [],
                        "CL_CD_maxGlide": [],
                        "CD_maxGlide": [],
                        "pre_CL_maxLift": [],
                        "pre_CD_maxLift": [],
                        "pre_alpha_maxLift": [],
                        "CL_maxLift": [],
                        "alpha_maxLift": []
                       }


    ############################################################################
    # function that returns a list of Re-numbers
    def get_ReList(params):
        return params.ReNumbers


    ############################################################################
    # function that calculates dependend values
    def calculateDependendValues(self):

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
            ReMax = Re * self.maxReFactor
            self.maxReNumbers.append(ReMax)

        # calculate Cl where polar-generation is going to switch from
        # type2- to type1-polar
        self.Cl_switchpoint_Type2_Type1_polar =\
                   ((self.ReNumbers[0] * self.ReNumbers[0]))/\
                   ((self.maxReNumbers[0])*(self.maxReNumbers[0]))

        print("polar-generation will switch vom type2 to type1 at Cl = %.3f\n"\
         % self.Cl_switchpoint_Type2_Type1_polar)


    def calculate_CD_TargetValue(self, root, strak, gain):
        target = (  (root * gain)           # part coming from root-airfoil
                  + (strak * (1.0 - gain))) # part coming from strak-airfoil

        return round(target, Cd_decimals)

    def calculate_CL_TargetValue(self, root, strak, gain):
        target = (  (root * gain)           # part coming from root-airfoil
                  + (strak * (1.0 - gain))) # part coming from strak-airfoil

        return round(target, Cl_decimals)


    def calculateMainTargetValues(self):
        # get root-polar
        rootPolar = self.merged_polars[0]
        num = len(self.merged_polars)

        for idx in range(num):
            # get polar
            polar = self.merged_polars[idx]

            # get gain and loss values
            if (polar == rootPolar):
                # no gain / loss for root-polar
                maxSpeedGain = 0.0
                maxGlideLoss = 0.0
                maxLiftGain = 0.0
            else:
                maxSpeedGain = params.maxSpeedGain[idx]
                maxGlideLoss = params.maxGlideLoss[idx]
                maxLiftGain = params.maxLiftGain[idx]

            #---------------------- maxSpeed-targets --------------------------
            # keep Cl-max-speed equal for all airfoils along the strak
            self.targets["CL_maxSpeed"].append(rootPolar.CL_maxSpeed)

            # calculate CD-max-speed target-value for each airfoil along the strak
            target_CD_maxSpeed = self.calculate_CD_TargetValue(
                 rootPolar.CD_maxSpeed, polar.CD_maxSpeed, maxSpeedGain)

            self.targets["CD_maxSpeed"].append(target_CD_maxSpeed)


            #---------------------- maxGlide-targets --------------------------
            # keep Cl-maxGlide equal for all airfoils along the strak
            self.targets["CL_maxGlide"].append(rootPolar.CL_maxGlide)

            # calculate CL/CD-target-value for each airfoil along the strak
            factor = 1.00 - maxGlideLoss
            target_CL_CD_maxGlide =  polar.CL_CD_maxGlide * factor

            # CL_CD = CL/CD -> CD = CL/CL_CD
            target_CD_maxGlide = rootPolar.CL_maxGlide/target_CL_CD_maxGlide
            target_CD_maxGlide = round(target_CD_maxGlide, Cd_decimals)
            target_CL_CD_maxGlide = round(target_CL_CD_maxGlide, Cl_Cd_decimals)

            self.targets["CL_CD_maxGlide"].append(target_CL_CD_maxGlide)
            self.targets["CD_maxGlide"].append(target_CD_maxGlide)

            #---------------------- maxLift-targets --------------------------
            target_pre_CL_maxLift = self.calculate_CL_TargetValue(
             rootPolar.pre_CL_maxLift, polar.pre_CL_maxLift, maxLiftGain)

            target_pre_alpha_maxLift = rootPolar.find_alpha(target_pre_CL_maxLift)
            target_pre_alpha_maxLift = round(target_pre_alpha_maxLift, Al_decimals)

            # get corresponding CD-value from root-polar
            rootPolar_pre_CD_maxLift = rootPolar.find_CD(target_pre_CL_maxLift)
            # get corresponding CD-value from strak-polar
            polar_pre_CD_maxLift = polar.find_CD(target_pre_CL_maxLift)

            target_pre_CD_maxLift  = self.calculate_CD_TargetValue(
                 rootPolar_pre_CD_maxLift, polar_pre_CD_maxLift, maxLiftGain)#TODO kann derselbe Faktor genommen werden wie für CL ???

            self.targets["pre_CL_maxLift"].append(target_pre_CL_maxLift)
            self.targets["pre_CD_maxLift"].append(target_pre_CD_maxLift)
            self.targets["pre_alpha_maxLift"].append(target_pre_alpha_maxLift)

            target_CL_maxLift = self.calculate_CL_TargetValue(
                  rootPolar.CL_maxLift, polar.CL_maxLift, maxLiftGain)

            target_alpha_maxLift = rootPolar.find_alpha(target_CL_maxLift)
            target_alpha_maxLift = round(target_alpha_maxLift, Al_decimals)

            self.targets["CL_maxLift"].append(target_CL_maxLift)
            self.targets["alpha_maxLift"].append(target_alpha_maxLift)
            idx = idx+1

        #print(self.targets)#Debug
        print("Done.")

    def correctOpPoint_left(self, opPoint, CL_maxSpeed_root,
                            CL_maxSpeed_strak, CL_maxGlide):
        # distances of maxSpeed, root / strak to maxGlide as a fixed op-point
        delta_root = CL_maxGlide - CL_maxSpeed_root
        delta_strak = CL_maxGlide - CL_maxSpeed_strak
        # factor between distances
        factor = delta_strak / delta_root
        # distance of op-point (root) to maxGlide
        delta_opPoint = CL_maxGlide - opPoint
        # new distance of op-point (strak) to maxGlide
        delta_opPoint = delta_opPoint * factor
        # new op-point (strak)
        correctedOpPoint = CL_maxGlide - delta_opPoint

        return round(correctedOpPoint, Cl_decimals)


    def correctOpPoint_right(self, opPoint, CL_maxLift_root,
                             CL_maxLift_strak, CL_maxGlide):

        # distances of maxLift, root / strak to maxGlide as a fixed op-point
        delta_root = CL_maxLift_root - CL_maxGlide
        delta_strak = CL_maxLift_strak - CL_maxGlide
        # factor between distances
        factor = delta_strak / delta_root
        # distance of op-point (root) to maxGlide
        delta_opPoint = opPoint - CL_maxGlide
        # new distance of op-point (strak) to maxGlide
        delta_opPoint = delta_opPoint * factor
        # new op-point (strak)
        correctedOpPoint = CL_maxGlide + delta_opPoint

        return round(correctedOpPoint, Cl_decimals)


    # correct additional op-Points for strak-airfoils
    def calculateAdditionalOpPoints(self):
        # get and additional op-points of root
        rootAddOpPoints = self.additionalOpPoints[0]

        if (len(rootAddOpPoints) == 0):
            # nothing to do
            return

        # get target-values
        targets = self.targets
        CL_maxSpeed_root = targets["CL_maxSpeed"][0]
        CL_maxGlide = targets["CL_maxGlide"][0]
        CL_maxLift_root = targets["CL_maxLift"][0]

        # all polars behind the root polar
        num = len(self.merged_polars)
        for idx in range (1, num):
            strakAddOpPoints = []

            # get strak-target-values
            CL_maxSpeed_strak = targets["CL_maxSpeed"][idx]
            CL_maxLift_strak = targets["CL_maxLift"][idx]

            # all additional opPoints
            for opPoint in rootAddOpPoints:
                if (CL_maxSpeed_root <= opPoint) and (CL_maxGlide >= opPoint):
                    # correct the opPoint
                    correctedOpPoint = self.correctOpPoint_left(opPoint,
                    CL_maxSpeed_root, CL_maxSpeed_strak, CL_maxGlide)
                else:
                    # correct the opPoint
                    correctedOpPoint = self.correctOpPoint_right(opPoint,
                    CL_maxLift_root, CL_maxLift_strak, CL_maxGlide)

                # append corrected opPoint to additional strakop-points
                strakAddOpPoints.append(correctedOpPoint)

            # append list of additional strak op-points to params
            self.additionalOpPoints.append(strakAddOpPoints)


################################################################################
#
# polarGraph class
#
################################################################################
class polarGraph:
    def __init__(self):
        return

    def plotLogo(self, ax, scriptDir):
        image = mpimg.imread(scriptDir + bs + imagesPath + bs + logoName)
        ax.imshow(image)
        ax.set_axis_off()

    def plotLiftDragOptimizationPoints(self, ax, polar, opt_point_style,
                                       linewidth, label):
        print("plotting CL over CD target-op-points for Re = %.0f...\n"\
              % (polar.Re))

        # check if there are operationg-conditions available
        if (polar.operatingConditions == None):
            return
        else:
            operatingConditions = polar.operatingConditions
            numOpPoints = len(operatingConditions["op_point"])

        x = []
        y = []

        for idx in range(numOpPoints):
            # get op-mode and type
            op_mode = operatingConditions["op_mode"][idx]
            op_type = operatingConditions["optimization_type"][idx]
            op_name = operatingConditions["name"][idx]

            if (op_mode == 'spec-cl') and (op_type != 'min-glide-slope'):

                # get CD from target-value
                x.append(operatingConditions["target_value"][idx])

                # get CL
                y.append(operatingConditions["op_point"][idx])

                print("target-op-point[%d] \'%s\', CL: %f, CD:%f" % \
                        (idx, op_name, x[idx], y[idx]))

        # plot
        ax.plot(x, y, opt_point_style, linestyle=ls_targetPolar,
                linewidth=linewidth, label = label)

        print("Done.\n\n")


    def plotLiftDragPolar(self, ax, polars):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl, Cd', 'Cd', 'Cl')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars
        polars = self.getReverseList(polars)

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.2, max(rootPolar.CL) + 0.2)

        # all polars
        for polar in polars:
            # determine idx for changing colors
            switchIdx = polar.T2_T1_switchIdx

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

            # plot max_speed
            x = polar.CD[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o',color=cl_infotext)
                ax.annotate('maxSpeed (root) @ Cl = %.2f, Cd = %.4f' % (y, x),
                 xy=(x,y), xytext=(40,0), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)

            # plot max_glide
            x = polar.CD[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxGlide (root) @ Cl = %.2f, Cd = %.4f' % (y, x),
                 xy=(x,y), xytext=(40,0), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)

            # plot max lift
            x = polar.CD[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]

            #set style for optimization points
            label = None
            style = opt_point_style_root
            linewidth = 0.0

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxLift (root) @ Cl = %.2f, Cd = %.4f' %(y,x),
                  xy=(x,y), xytext=(-20,10), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)
            else:
                style = opt_point_style_strak
                linewidth = lw_targetPolar
                if (polar == polars[1]):
                    label = 'target-polar'

            # plot optimization points
            if (polar == rootPolar) or (params.showTargetPolars == True):
                self.plotLiftDragOptimizationPoints(ax, polar, style, linewidth,
                                                    label)

            ax.legend(loc='upper left', fontsize = fs_legend)


    def plotLiftOverAlphaOptimizationPoints(self, ax, polar, opt_point_style):
        print("plotting CL over alpha target-op-points for Re = %.0f...\n"\
              % (polar.Re))

        # check if there are operationg-conditions available
        if (polar.operatingConditions == None):
            return
        else:
            operatingConditions = polar.operatingConditions
            numOpPoints = len(operatingConditions["op_point"])

        for idx in range(numOpPoints):
            # get op-mode
            op_mode = operatingConditions["op_mode"][idx]
            op_name = operatingConditions["name"][idx]

            if (op_mode == 'spec-al'):
                # get CL
                x = operatingConditions["op_point"][idx]

                # get alpha from target-value
                y = operatingConditions["target_value"][idx]

                print("target-op-point[%d] \'%s\', alpha:%f, CL: %f,"\
                 % (idx, op_name, x, y))
                # plot
                ax.plot(x, y, opt_point_style)

        print("Done.\n\n")

    def plotLiftOverAlphaPolar(self, ax, polars):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl, alpha', 'alpha', 'Cl')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars
        polars = self.getReverseList(polars)

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.1, max(rootPolar.CL) + 0.2)

        # all polars
        for polar in polars:

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

            # plot max Speed
            x = polar.alpha[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(40,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max Glide
            x = polar.alpha[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]
            if (polar == rootPolar):
               ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(20,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max lift
            x = polar.alpha[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(-140,10), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

                #set style for optimization points
                style = opt_point_style_root
            else:
                style = opt_point_style_strak

            # plot optimizationPoints
            if (polar == rootPolar) or (params.showTargetPolars == True):
                self.plotLiftOverAlphaOptimizationPoints(ax, polar, style)


    def setAxesAndLabels(self, ax, title, xlabel, ylabel):

        # set title of the plot
        text = (title)
        #ax.set_title(text, fontsize = 30, color="darkgrey")

        # set axis-labels
        ax.set_xlabel(xlabel, fontsize = 20, color="darkgrey")
        ax.set_ylabel(ylabel, fontsize = 20, color="darkgrey")

        # customize grid
        ax.grid(True, color='dimgrey',  linestyle='dotted', linewidth=0.4)


    def plotLiftDragOverLiftOptimizationPoints(self, ax, polar, opt_point_style,
                                               linewidth, label):
        print("plotting CL/CD over CL target-op-points for Re = %.0f...\n"\
              % (polar.Re))

        # check if there are operationg-conditions available
        if (polar.operatingConditions == None):
            return
        else:
            operatingConditions =polar.operatingConditions
            numOpPoints = len(operatingConditions["op_point"])
        x = []
        y = []

        for idx in range(numOpPoints):
            # get op-mode and -type
            op_mode = operatingConditions["op_mode"][idx]
            op_type = operatingConditions["optimization_type"][idx]
            op_name = operatingConditions["name"][idx]

            if (op_mode == 'spec-cl') and (op_type != 'min-glide-slope'):
                # get CL
                x.append(operatingConditions["op_point"][idx])

                # get CD from target-value
                Cd = operatingConditions["target_value"][idx]

                # calculate Cl/Cd
                if (Cd == 0):
                    print("Error, Cd is zero, op-point:%s" % op_name)

                y.append(x[idx]/Cd)

                print("target-op-point[%d] \'%s\', CL/CD: %f, CL:%f" %\
                   (idx, op_name, x[idx], y[idx]))

        # plot
        ax.plot(x, y, opt_point_style, linestyle=ls_targetPolar,
                linewidth=linewidth, label = label)

        print("Done.\n\n")


    def getReverseList(self, list):
        reverseList = []
        idx = len(list)-1

        for element in list:
            reverseList.append(list[idx])
            idx = idx -1

        return reverseList


    def plotLiftDragOverLiftPolar(self, ax, polars):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl/Cd, Cl', 'Cl', 'Cl/Cd')

        # get polar of root-airfoil
        rootPolar = polars[0]

        # revert List of polars
        polars = self.getReverseList(polars)

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL_CD) - 10, max(rootPolar.CL_CD) + 10)

        # all polars
        for polar in polars:

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

            # plot max_speed
            x = polar.CL[polar.maxSpeed_idx]
            y = polar.CL_CD[polar.maxSpeed_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @\nCl = %.2f,\nCl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(-80,0), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)

            # plot max_glide
            x = polar.CL[polar.maxGlide_idx]
            y = polar.CL_CD[polar.maxGlide_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ Cl = %.2f, Cl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(-60,7), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)

            # plot max Lift
            x = polar.CL[polar.maxLift_idx]
            y = polar.CL_CD[polar.maxLift_idx]

            if (polar == rootPolar):
                ax.plot(x, y, 'o', color=cl_infotext)

            #set style for optimization points
            label = None
            style = opt_point_style_root
            linewidth = 0.0

            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @\nCl = %.2f,\nCl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(10,0), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)
            else:
                style = opt_point_style_strak
                linewidth = lw_targetPolar
                if (polar == polars[1]):
                    label = 'target-polar'


            # plot optimizationPoints
            if (polar == rootPolar) or (params.showTargetPolars == True):
                self.plotLiftDragOverLiftOptimizationPoints(ax, polar, style,
                                                            linewidth, label)


    def draw(self, scriptDir, params):
        # get polars
        polars = params.merged_polars
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
        fig, (upper,lower) = plt.subplots(2,2)

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

        text = text + "T2-polars, ReSqrt(Cl) = "

         # add Re-numbers
        for i in range(num_polars):
            Re = T2_polars[i].Re
            if (i == num_polars-1):
                text = text + ("%d\n" % Re)
            else:
                text = text + ("%d, " % Re)

        fig.suptitle(text, fontsize = 12, color="darkgrey", **csfont)

        # first figure, display strak-machine-logo
        self.plotLogo(upper[0], scriptDir)

        # second figure, display the Lift / Drag-Polar
        self.plotLiftDragPolar(lower[0], polars)

        # third figure, display the Lift / alpha-Polar
        self.plotLiftOverAlphaPolar(upper[1], polars)

        # fourth figure, display the lift/drag /Lift polar
        self.plotLiftDragOverLiftPolar(lower[1], polars)

        # maximize window
        figManager = plt.get_current_fig_manager()
        figManager.window.showMaximized()

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
        self.CD_maxSpeed = 0.0
        self.CL_maxSpeed = 0.0
        self.maxSpeed_idx = 0
        self.CL_CD_maxGlide = 0.0
        self.maxGlide_idx = 0
        self.alpha_maxGlide= 0.0
        self.CL_maxGlide = 0.0
        self.CL_maxLift = 0.0
        self.alpha_maxLift = 0.0
        self.maxLift_idx = 0
        self.pre_CL_maxLift = 0.0
        self.pre_CD_maxLift= 0.0
        self.pre_maxLift_idx = 0
        self.pre_alpha_maxLift = 0.0
        self.operatingConditions = None
        self.Cl_switchpoint_Type2_Type1_polar = 999999
        self.T2_T1_switchIdx = 0

    def addOperatingConditions(self, opConditions):
        self.operatingConditions = opConditions.copy()

    def importFromFile(self, fileName):
        BeginOfDataSectionTag = "-------"
        airfoilNameTag = "Calculated polar for:"
        ReTag = "Re ="
        parseInDataPoints = 0
        print("importing polar %s...\n" %fileName)

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
        print("done.\n")

    def writeToFile(self, fileName):
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

        print("writing polar to file %s...\n" %fileName)

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
        print("done.\n")

    def merge(self, mergePolar_1, switching_Cl, maxRe):
        print ("merging polars at Cl = %s.." % switching_Cl)

        # create a new, empty polar
        mergedPolar = polarData()

        # copy some information from mergePolar_1
        mergedPolar.airfoilname = self.airfoilname
        mergedPolar.polarType = 12
        mergedPolar.Re = self.Re
        mergedPolar.NCrit = 1.0
        mergedPolar.Cl_switchpoint_Type2_Type1_polar = switching_Cl
        mergedPolar.maxRe = maxRe

        # merge first polar from start Cl to switching_Cl
        for idx in range(len(mergePolar_1.CL)):
            if (mergePolar_1.CL[idx] <= switching_Cl):
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
            if (self.CL[idx] > switching_Cl):
                mergedPolar.alpha.append(self.alpha[idx])
                mergedPolar.CL.append(self.CL[idx])
                mergedPolar.CD.append(self.CD[idx])
                mergedPolar.CL_CD.append(self.CL_CD[idx])
                mergedPolar.CDp.append(self.CDp[idx])
                mergedPolar.Cm.append(self.Cm[idx])
                mergedPolar.Top_Xtr.append(self.Top_Xtr[idx])
                mergedPolar.Bot_Xtr.append(self.Bot_Xtr[idx])

        print("done.\n")
        return mergedPolar


    def determineMaxSpeed(self):
        self.CL_maxSpeed = 0.0
        self.CD_maxSpeed = 1000000.0
        self.maxSpeed_idx = 0

        # find absolute minimum of Cd
        for idx in range(len(self.CD)):
            if (self.CD[idx] < self.CD_maxSpeed):
                self.CD_maxSpeed = self.CD[idx]
                self.CL_maxSpeed = self.CL[idx]
                self.maxSpeed_idx = idx
        print("max Speed, Cd = %f @ Cl = %f" %
                                  (self.CD_maxSpeed, self.CL_maxSpeed))


    def determineMaxGlide(self):
        # determine max-value for Cl/Cd (max glide) and corresponding Cl
        peak_height = 2.0
        self.maxGlide_idx = findPeak(self.CL_CD, peak_height)
        self.CL_maxGlide = self.CL[self.maxGlide_idx]
        self.alpha_maxGlide = self.alpha[self.maxGlide_idx]
        self.CL_CD_maxGlide = self.CL_CD[self.maxGlide_idx]

        print("max Glide, Cl/Cd = %f @ Cl = %f" %
                                  (self.CL_CD_maxGlide, self.CL_maxGlide))


    def determineMaxLift(self):
        # determine max lift-value and corresponding alpha
        peak_height = 0.025
        self.maxLift_idx = findPeak(self.CL, peak_height)
        self.CL_maxLift = self.CL[self.maxLift_idx]
        self.alpha_maxLift = self.alpha[self.maxLift_idx]

        # also calculate opPoint before MaxLift that can be reached by the
        # optimizer
        self.pre_CL_maxLift = self.CL_maxLift * 0.99
        self.pre_CD_maxLift = self.find_CD(self.pre_CL_maxLift)
        print(self.Re, self.pre_CL_maxLift, self.pre_CD_maxLift) #Debug

        self.pre_maxLift_idx = self.find_index(self.pre_CL_maxLift)
        self.pre_alpha_maxLift = self.alpha[self.pre_maxLift_idx]

        print("max Lift, Cl = %f @ alpha = %f" %
                                  (self.CL_maxLift, self.alpha_maxLift))


    def analyze(self):
        # yy_sg = savgol_filter(itp(xx), window_size, poly_order) TODO smoothing
        print("analysing polar...")
        self.determineMaxSpeed()
        self.determineMaxGlide()
        self.determineMaxLift()
        print("done.\n")


    def find_CD(self, CL):
        # calculate corresponding CD
        # reduce list of CL, CD-values up to CL_max. No duplicate CL-values are
        # allowed!
        for i in range(len(self.CL)):
            if (self.CL[i] >= CL):
                return self.CD[i]
        return None

##        x = []
##        y = []
##        for i in range(self.maxLift_idx):
##            x.append(self.CL[i])
##            y.append(self.CD[i])
##
##
##        #double append the last value
##        x.append(self.CL[i])
##        y.append(self.CD[i])
##
##        # interpolate the values
##        CD = np.interp( CL, x, y)
##        return CD


    def find_index(self, CL):
        for i in range(len(self.CL)):
            if (self.CL[i] >= CL):
                return i
        return None

    def find_CL(self, alpha):
        for i in range(len(self.alpha)):
            if (self.alpha[i] >= alpha):
                return self.CL[i]

    def find_alpha(self, CL):
        for i in range(len(self.CL)):
            if (self.CL[i] >= CL):
                return self.alpha[i]
        return None

    def find_CL_CD(self, CL):
        # calculate corresponding CL_CD
        # reduce list of CL, CL_CD-values up to CL_max. No duplicate CL-values are
        # allowed!
        x = []
        y = []
        for i in range(self.maxLift_idx):
            x.append(self.CL[i])
            y.append(self.CL_CD[i])

        #double append the last value
        x.append(self.CL[i])
        y.append(self.CL_CD[i])

        # interpolate the values
        CL_CD = np.interp(CL, x, y)
        return CL_CD



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

        foilName = (foilName + "%s-%03dk.dat") % (suffix,(Re/1000))

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
# function that generates commandlines to run Xoptfoil
def generate_commandlines(params):
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

    # change current working dir to output folder
    commandline = "cd %s\n\n" % params.outputFolder
    commandLines.append(commandline)

    if (params.operatingMode != 'matchpolarfoils'):
        # copy root-foil to airfoil-folder as it can be used
        # as the root airfoil without optimization
        commandline = ("copy %s %s" + bs + "%s\n\n") % \
        (get_FoilName(params, 0), params.airfoilFolder, get_FoilName(params, 0))
        commandLines.append(commandline)

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

        # set input-file name for Xoptfoil
        iFile = params.inputFileNames[i]

        # generate Xoptfoil-commandline
        commandline = "xoptfoil-jx -i %s -r %d -a %s -o %s\n" %\
                        (iFile, ReList[i], previousFoilname,
                          strakFoilName.strip('.dat'))
        commandLines.append(commandline)

        # smooth the airfoil
        inputFilename = getPresetInputFileName('Smooth')

        # compose commandline for smoothing the airfoil
        commandline = "xfoil_worker.exe -w smooth -i %s -a %s -o %s\n" % \
                       (inputFilename, strakFoilName, strakFoilName.strip('.dat'))
        commandLines.append(commandline)


        #copy strak-airfoil to airfoil-folder
        commandline = ("copy %s %s" + bs +"%s\n\n") % \
            (strakFoilName , params.airfoilFolder, strakFoilName)
        commandLines.append(commandline)

    # change current working dir back
    commandline = "cd..\n"
    commandLines.append(commandline)

    print("Done.")
    return commandLines


################################################################################
# function that generates a Xoptfoil-batchfile
def generate_batchfile(batchFileName, commandlines):
    try:
        # create a new file
        outputfile = open(batchFileName, "w+")
    except:
        print ('Error, file %s could not be opened' % batchFileName)
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
    Re = str(params.ReNumbers[idx])

    # change current working dir to output folder
    strak_commandlines.append("cd %s\n\n" % params.outputFolder)

    for line_idx in range(len(commandlines)):
        if (idx == 0):
            # root-airfoil
            if (commandlines[line_idx].find("copy ")>=0):
                # append only one line
                strak_commandlines.append(commandlines[line_idx])
                # everything found
                break
        else:
            if (commandlines[line_idx].find(Re)>=0):
                # append three lines
                strak_commandlines.append(commandlines[line_idx])
                strak_commandlines.append(commandlines[line_idx+1])
                strak_commandlines.append(commandlines[line_idx+2])
                # everything found
                break

    # change back directory
    strak_commandlines.append("cd..")
    return strak_commandlines


################################################################################
# function that generates a Xoptfoil-batchfile for one strak airfoil
def generate_strak_batchfiles(params, commandlines):
    if (params.operatingMode == 'matchpolarfoils'):
        # nothing to do
        return

    for i in range(0, len(params.ReNumbers)):
        k_Re = (params.ReNumbers[i]/1000)
        batchFileName = "make_%dk.bat" % (k_Re)

        try:
            # create a new file
            outputfile = open(batchFileName, "w+")
        except:
            print ('Error, file %s could not be opened' % batchFileName)
            return
        # get commandlines to generate the strak-airfoil
        strak_commandlines = get_strak_commandlines(params, commandlines, i)

        # write commandlines to outputfile
        for element in strak_commandlines:
            outputfile.write(element)

        # close the outputfile
        outputfile.close()


def generate_visu_batchfiles(params):
   # determine start-index
    if (params.operatingMode == 'matchpolarfoils'):
        startidx = 0
    else:
        startidx = 1

    for i in range(startidx, len(params.ReNumbers)):
        visuFileName = "visu_%dk.bat" % (params.ReNumbers[i]/1000)
        airfoilName = get_FoilName(params, i)
        airfoilName = airfoilName.strip('.dat')

        try:
            # create a new file
            outputfile = open(visuFileName, "w+")
        except:
            print ('Error, file %s could not be opened' % visuFileName)
            return

        # write commandlines
        outputfile.write("cd build\n")
        outputfile.write("xoptfoil_visualizer-jx.py -o 3 -c %s\n" % airfoilName)

        # close the outputfile
        outputfile.close()

################################################################################
# function that gets the name of the strak-machine-data-file
def getInFileName(args):

    if args.input:
        inFileName = args.input
    else:
        # use Default-name
        inFileName = 'ressources/strakdata'

    inFileName = inFileName + '.txt'
    print("filename for strak-machine input-data is: %s" % inFileName)
    return inFileName


################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')

    parser.add_argument("-input", "-i", help="filename of strak-machine input"\
                        "-file (e.g. strak_data)")

    # read arguments from the command line
    args = parser.parse_args()
    return (getInFileName(args))


################################################################################
# function that gets parameters from dictionary
def getParameters(dict):

    params = strakData()

    try:
        params.inputFolder = dict["inputFolder"]
    except:
        print ('inputFolder not specified, using default-value \'%s\''\
         % params.inputFolder)

    try:
        params.outputFolder = dict["outputFolder"]
    except:
        print ('outputFolder not specified, using default-value \'%s\''\
         % params.outputFolder)

    try:
        params.batchfileName = dict["batchfileName"]
    except:
        print ('batchfileName not specified, setting default-filename \'%s\'.'\
                % params.batchfileName)

    try:
        params.xmlFileName = dict["XMLfileName"]
    except:
        print ('XMLfileName not specified, assuming no xml-file shall be used.')
        params.xmlFileName = None

    try:
        params.strakInputFileName = dict["strakInputFileName"]
    except:
        print ('strakInputFileName not found, setting default-filename \'%s\'.'\
                % params.strakInputFileName)

    try:
        params.ReNumbers = dict["ReNumbers"]
    except:
        print ('Error, ReNumbers not specified!')
        exit(1)

    try:
        params.additionalOpPoints[0] = dict["additionalOpPoints"]
    except:
        print ('additionalOpPoints not specified, using no additional'
               'op-points')

    try:
        params.maxReFactor = dict["maxReFactor"]
    except:
        print ('maxReFactor not specified, using default-value %f'
                 % params.maxReFactor)

    try:
        params.seedFoilName = dict["seedFoilName"]
    except:
        print ('Error, seedFoilName not specified!')
        exit(1)

    try:
        params.strakType = dict["strakType"]
    except:
        print ('Error, strakType not specified!')
        exit(1)

    try:
        params.operatingMode = dict["operatingMode"]
    except:
        print ('operatingMode not specified, using default value \'%s\'.'\
                % params.operatingMode)

    # get matchpolarfoilname only, if operating-mode is set to "matchpolarfoils"
    if (params.operatingMode == 'matchpolarfoils'):
        try:
            params.matchPolarFoilName = dict["matchPolarFoilName"]
            params.useAlwaysRootfoil = True
        except:
            print ('Error, matchPolarFoilName not specified!')
            exit(1)
    else:
        try:
            if (dict["useAlwaysRootfoil"] == 'true'):
                params.useAlwaysRootfoil = True
            else:
                params.useAlwaysRootfoil = False
        except:
            if params.useAlwaysRootfoil:
                string = 'True'
            else:
                string = 'False'
            print ('useAlwaysRootfoil not specified, using default value \'%s\'.'\
                    % string)

    try:
        if (dict["adaptInitialPerturb"] == 'true'):
            params.adaptInitialPerturb = True
        else:
            params.adaptInitialPerturb = False
    except:
        if params.adaptInitialPerturb:
            string = 'True'
        else:
            string = 'False'
        print ('adaptInitialPerturb not specifiedusing default value \'%s\'.'\
                    % string)

    try:
        params.weighting_mode = dict["weighting_mode"]
        if ((params.weighting_mode != 'linear_progression') &
            (params.weighting_mode != 'constant') &
            (params.weighting_mode != 'sinus')):
            params.weighting_mode = 'constant'
            print ('weighting_mode %s not allowed' % params.weighting_mode)
    except:
        print ('weighting_mode not specified, assuming constant weighting')
        params.weighting_mode = 'constant'

    try:
        params.min_weight = dict["min_weight"]
    except:
        print ('min_weight not specified')

    try:
        params.max_weight = dict["max_weight"]
    except:
        print ('max_weight not specified')

    try:
        if (dict["showTargetPolars"] == 'true'):
            params.showTargetPolars = True
        else:
            params.showTargetPolars = False
    except:
        print ('showTargetPolars not specified')

    try:
        if (dict["smoothSeedfoil"] == 'true'):
            params.smoothSeedfoil = True
        else:
            params.smoothSeedfoil = False
    except:
        print ('smoothSeedfoil not specified')

    try:
        if (dict["smoothMatchPolarFoil"] == 'true'):
            params.smoothMatchPolarFoil = True
        else:
            params.smoothMatchPolarFoil = False
    except:
        print ('smoothMatchPolarFoil not specified')

    try:
        params.maxGlideLoss = dict["maxGlideLoss"]
    except:
        print ('maxGlideLoss not specified')

    try:
        params.maxSpeedGain = dict["maxSpeedGain"]
    except:
        print ('maxSpeedGain not specified')

    try:
        params.maxLiftGain = dict["maxLiftGain"]
    except:
        print ('maxLiftGain not specified')

    try:
        params.numOpPoints = dict["numOpPoints"]
        if (params.numOpPoints < 5):
            params.numOpPoints = 5
    except:
        print ('numOpPoints not specified')

    try:
        params.Cl_min = dict["Cl_min"]
    except:
        print ('Cl_min not specified, using default value \'%f\'.'\
                    % params.Cl_min)

    return params


def getListOfFiles(dirName):
    # create a list of files in the given directory
    listOfFile = os.listdir(dirName)
    allFiles = list()

    # Iterate over all the entries
    for entry in listOfFile:
        # Create full path
        fullPath = os.path.join(dirName, entry)
        allFiles.append(fullPath)

    return allFiles


def getwingDataFromXML(params):

    xmlFileName = params.inputFolder + '/' + params.xmlFileName
    try:
        planeData = read_planeDataFile(xmlFileName)
    except:
        print("Error, file \"%s\" could not be opened.") % xmlFileName
        exit(-1)

    # return data
    return planeData[0]


def copyAndSmoothAirfoil(srcName, srcPath, destName, smooth):
    srcfoilNameAndPath = srcPath + bs + srcName + '.dat'

    if (smooth):
        print("Smoothing airfoil \'%s\', creating airfoil \'%s\'\n" %\
                       (srcName, destName))
        # smooth, rename and copy the airfoil
        inputFilename = getPresetInputFileName('Smooth')

        # compose system-string for smoothing the seed-airfoil
        systemString = "xfoil_worker.exe -w smooth -i %s -a %s -o %s" % \
                       (inputFilename, srcfoilNameAndPath, destName)

        # execute xfoil-worker / create the smootehed root-airfoil
        os.system(systemString)
    else:
        print("Renaming airfoil \'%s\' to \'%s\'\n" % (srcName, destName))
        # only reanme and copy the airfoil
        systemString = "change_airfoilname.py -i %s -o %s" %\
                              (srcfoilNameAndPath, destName + '.dat')
        os.system(systemString)

    print("Done.")


def copy_matchpolarfoils(params):
    # get the name of the matchfoil
    matchfoilName = params.matchPolarFoilName

    # get name of seed-airfoil
    seedFoilName = params.seedFoilName

    # get the path where the airfoil can be found
    srcPath = ".." + bs + params.inputFolder

    # copy and smooth the matchfoil
    copyAndSmoothAirfoil(matchfoilName, srcPath, matchfoilName,
                                             params.smoothMatchPolarFoil)

    # copy and smooth the seedfoil
    copyAndSmoothAirfoil(seedFoilName, srcPath, seedFoilName,
                                             params.smoothSeedfoil)

    print("Done.")
    return matchfoilName


def generate_rootfoil(params):
    # get name of seed-airfoil
    seedFoilName = params.seedFoilName

    # get name of root-airfoil
    rootfoilName = get_FoilName(params, 0).strip('.dat')

    # get the path where the seed-airfoil can be found
    srcPath = ".." + bs + params.inputFolder

    # copy and smooth the airfoil, also rename
    copyAndSmoothAirfoil(seedFoilName, srcPath, rootfoilName,
                                           params.smoothSeedfoil)

    return rootfoilName


def generate_inputFiles(params):
    print("Generating inputfiles...")

    # get root-polar
    rootPolar = params.merged_polars[0]
    num_polars = len(params.ReNumbers)

    # generate files for all Re-numbers
    for i in range(num_polars):
        # get strak-polar
        strakPolar = params.merged_polars[i]

        # create new inputfile
        newFile = inputFile(params.strakType)

        # generate a fresh list of equally distributed op-Points
        num_opPoints = params.numOpPoints + len(params.additionalOpPoints)

        # get the target-values
        targets = params.targets
        CL_maxLift = targets["CL_maxLift"][i]
        alpha_maxLift = targets["alpha_maxLift"][i]

        # generate op-points in the range Cl_min..Cl_max
        newFile.generateOpPoints(params.numOpPoints, params.Cl_min,
                               CL_maxLift, alpha_maxLift)

        # distribute main opPoints, also set the target-values
        newFile.distributeMainOpPoints(targets, i)

        # insert additional opPoints (if there are any):
        if len(params.additionalOpPoints[0])>0:
            newFile.insertAdditionalOpPoints(params.additionalOpPoints[i])

        # now distribute the opPoints between the main opPoints and additional
        # oppoints equally
        newFile.distributeIntermediateOpPoints(rootPolar)
        #newFile.printOpPoints()#debug

        # set the target-values of all intermediate-op-points now
        newFile.SetIntermediateOpPointTargetValues(targets, rootPolar,
                                                    strakPolar, i)

        # set the importance / weightings of the op-points
        newFile.SetWeightings(params)

        if params.adaptInitialPerturb:
            # also adapt the initial perturb according to the change in
            # Re-number
            if (params.useAlwaysRootfoil):
                # difference always calculated to Re-number of root-airfoil
                ReDiff = params.ReNumbers[0] - params.ReNumbers[i]
            else:
                # difference calculated to Re-number of previous-airfoil
                ReDiff = params.ReNumbers[i-1] - params.ReNumbers[i]

            newFile.setInitialPerturb(ReDiff)

        # copy operating-conditions to polar, so they can be plotted in the graph
        opConditions = newFile.getOperatingConditions()
        strakPolar.addOperatingConditions(opConditions)

        # physically create the file
        newFile.writeToFile(params.inputFileNames[i])

        # append to params
        params.inputFiles.append(newFile)

    print("Done.")


def generate_polars(params, workingDir, rootfoilName):
    # generate polars of seedfoil / root-airfoil:
    print("Generating polars for airfoil %s..." % rootfoilName)

    # compose polar-dir
    polarDir = workingDir + bs + rootfoilName + '_polars'

    # create polars, polar-file-Names and input-file-names from Re-Numbers
    for ReIdx in range(len(params.ReNumbers)):
        # get Re, maxRe
        Re = params.ReNumbers[ReIdx]
        maxRe = params.maxReNumbers[ReIdx]

        # create polar-file-Name T1-polar from maxRe-Number
        polarFileName_T1 = "T1_Re0.%03d_M0.00_N9.0.txt" % (maxRe/1000)
        polarFileNameAndPath_T1 = polarDir + bs + polarFileName_T1
        params.polarFileNames_T1.append(polarFileNameAndPath_T1)

        # create polar-file-Name T2-polar from Re-Number
        polarFileName_T2 = "T2_Re0.%03d_M0.00_N9.0.txt" % (Re/1000)
        polarFileNameAndPath_T2 = polarDir + bs + polarFileName_T2
        params.polarFileNames_T2.append(polarFileNameAndPath_T2)

        # generate inputfilename from Re-number
        inputFilename = params.strakInputFileName.strip('.txt')
        inputFilename = inputFilename + ("_%03dk.txt" % (Re/1000))
        params.inputFileNames.append(inputFilename)

        # compose string for system-call of XFOIL-worker for T1-polar generation
        airfoilName = rootfoilName + '.dat'
        inputFilename = getPresetInputFileName(T1_polarInputFile)
        systemString_T1 = "xfoil_worker.exe -i \"%s\" -o \"%s\" -w polar -a \"%s\" -r %d" %\
                              (inputFilename, rootfoilName, airfoilName, maxRe)

        # compose string for system-call of XFOIL-worker for T2-polar generation
        inputFilename = getPresetInputFileName(T2_polarInputFile)
        systemString_T2 = "xfoil_worker.exe -i \"%s\" -o \"%s\" -w polar -a \"%s\" -r %d" %\
                                 (inputFilename, rootfoilName, airfoilName, Re)

        # import polar type 1
        newPolar_T1 = polarData()
        try:
            newPolar_T1.importFromFile(polarFileNameAndPath_T1)
        except:
            # execute xfoil-worker / create T1 polar-file
            print("Generating polar %s" % polarFileName_T1)
            os.system(systemString_T1)
            newPolar_T1.importFromFile(polarFileNameAndPath_T1)

        params.T1_polars.append(newPolar_T1)

        # import polar type 2
        newPolar_T2 = polarData()
        try:
            newPolar_T2.importFromFile(polarFileNameAndPath_T2)
        except:
            # execute xfoil-worker / create T2 polar-file
            print("Generating polar %s" % polarFileName_T2)
            os.system(systemString_T2)
            newPolar_T2.importFromFile(polarFileNameAndPath_T2)

        params.T2_polars.append(newPolar_T2)

        # merge T1/T2 polars at Cl switching-point
        mergedPolar = newPolar_T2.merge(newPolar_T1,
                         params.Cl_switchpoint_Type2_Type1_polar, maxRe)

        # analyze merged polar
        mergedPolar.analyze()

        # set name
        mergedPolar.polarName = 'mergedPolar T1/T2, ReSqrt(Cl) = %.0f, Re = %0.f' %\
                (newPolar_T2.Re, newPolar_T1.Re)

        # add merged polar to params
        params.merged_polars.append(mergedPolar)

        # write merged polars to file
        polarFileNameAndPath = polarDir + bs + polarFileName_T1.strip('.txt') +\
                               '_' +polarFileName_T2
        print(polarFileNameAndPath) #Debug

        mergedPolar.writeToFile(polarFileNameAndPath)

    print("Done.")


def setPolarDataFromInputFile(polarData, rootPolar, inputFile, airfoilname, Re):
    polarData.polarName = 'target-polar for airfoil %s' % airfoilname
    polarData.airfoilname = airfoilname
    polarData.polarType = 12
    polarData.Re = Re
    polarData.NCrit = 0.0

    # get operating-conditions from inputfile
    operatingConditions = inputFile.getOperatingConditions()

    target_values = operatingConditions["target_value"]
    op_points = operatingConditions["op_point"]
    op_modes =  operatingConditions["op_mode"]

    numOpPoints = len(op_points)

    for i in range(numOpPoints):
        # check if the op-mode is 'spec-cl'
        if (op_modes[i] == 'spec-cl'):
            # get alpha from root-polar (dummy-value)
            alpha = rootPolar.find_alpha(op_points[i])
            polarData.alpha.append(alpha)
            polarData.CL.append(op_points[i])
            polarData.CD.append(target_values[i])
        else:
            # op_mode is 'spec-al'
            polarData.alpha.append(op_points[i])
            polarData.CL.append(target_values[i])
            polarData.CD.append(0.0)

        polarData.CDp.append(0.0)
        polarData.Cm.append(0.0)
        polarData.Top_Xtr.append(0.0)
        polarData.Bot_Xtr.append(0.0)


def generate_target_polars(params, workingDir):
    # local variable
    targetPolars = params.target_polars
    inputFiles = params.inputFiles
    Re = params.ReNumbers
    numTargetPolars = len(Re)
    rootPolar = params.merged_polars[0]

    for i in range(numTargetPolars):
        # get name of the airfoil
        airfoilName = (get_FoilName(params, i)).strip('.dat')
        # get inputfile
        inputFile = inputFiles[i]

        print("Generating target polar for airfoil %s..." % airfoilName)

        # create new target polar
        targetPolar = polarData()

        # put the necessary data into the polar
        setPolarDataFromInputFile(targetPolar, rootPolar, inputFile, airfoilName, Re[i])

        # append the new target polar to list of target_polars
        params.target_polars.append(targetPolar)

        # compose polar-dir
        polarDir = workingDir + bs + airfoilName + '_polars'

        # check if output-folder exists. If not, create folder.
        if not os.path.exists(polarDir):
            os.makedirs(polarDir)

        # compose filename and path
        polarFileNameAndPath = polarDir + bs + airfoilName + '_target_polar.txt'
        #print(polarFileNameAndPath) #Debug

        # write polar to file
        targetPolar.writeToFile(polarFileNameAndPath)

    print("Done.")


################################################################################
# Main program
if __name__ == "__main__":

    # get command-line-arguments or user-input
    strakDataFileName = getArguments()

    # get real path of the script
    pathname = os.path.dirname(sys.argv[0])
    scriptPath = os.path.abspath(pathname)

##    #debug
##    out_file = open("strakdata.txt",'w')
##    json.dump(strakdata, out_file, indent = 6)
##    out_file.close()

    # try to open .json-file
    try:
        strakDataFile = open(strakDataFileName)
    except:
        print('Error, failed to open file %s' % strakDataFileName)
        exit(-1)

    # load dictionary from .json-file
    try:
        strakdata = json.load(strakDataFile)
        strakDataFile.close()
    except:
        print('Error, failed to read data from file %s' % strakDataFileName)
        strakDataFile.close()
        exit(-1)

    # get strak-machine-parameters from dictionary
    params = getParameters(strakdata)

    # read plane-data from XML-File, if requested //TODO: only wing-data
    if (params.xmlFileName != None):
        params.wingData = getwingDataFromXML(params)

    # calculate further values like max Re-numbers etc.
    params.calculateDependendValues()

    # compose name of the folder, where the airfoils shall be stored
    params.airfoilFolder = 'airfoils'

    # get current working dir
    workingDir = os.getcwd()

    # check if output-folder exists. If not, create folder.
    if not os.path.exists(params.outputFolder):
        os.makedirs(params.outputFolder)

    # check if airfoil-folder exists. If not, create folder.
    if not os.path.exists(params.outputFolder + '\\' + params.airfoilFolder):
        os.makedirs(params.outputFolder + '\\' + params.airfoilFolder)

    # create an instance of polar graph
    graph = polarGraph()

    # change working-directory
    os.chdir(workingDir + bs + params.outputFolder)

    # get current working dir again
    workingDir = os.getcwd()

    # get name of root-airfoil according to operating-mode
    if (params.operatingMode == 'matchpolarfoils'):
        rootfoilName = copy_matchpolarfoils(params)
    else:
        rootfoilName = generate_rootfoil(params)

    # generate polars of root-airfoil, also analyze
    generate_polars(params, workingDir, rootfoilName)

    # calculate target-values for the main op-points
    params.calculateMainTargetValues()

    # calculate the additional op-points
    params.calculateAdditionalOpPoints()

    # generate input-Files
    generate_inputFiles(params)

    # generate target polars and write to file
    generate_target_polars(params, workingDir)

    # generate Xoptfoil command-lines
    commandlines = generate_commandlines(params)

    # change working-directory
    os.chdir(".." + bs)

    # generate batchfile
    print("Generating batchfile...")
    if (params.generateBatch == True):
        print ('generating batchfile \'%s\'' % params.batchfileName)
        generate_batchfile(params.batchfileName, commandlines)
        print ('generating visu-batchfiles')
        generate_visu_batchfiles(params)
        print ('generating batchfiles for each single airfoil of the strak')
        generate_strak_batchfiles(params, commandlines)
    print("Done.")

    # show graph
    graph.draw(scriptPath, params)

    print("Ready.")
