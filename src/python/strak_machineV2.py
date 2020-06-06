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
import pip
import f90nml
from copy import deepcopy

# paths and separators
bs = "\\"
presetsPath = 'ressources' + bs + 'presets'
imagesPath = 'ressources' + bs + 'images'
logoName = 'strakmachine.png'

# fonts
csfont = {'fontname':'Segoe Print'}

# number of decimals in the generated input-files
Cl_decimals = 3 # lift
Cd_decimals = 5 # drag
Al_decimals = 3 # alpha

# fontsizes
fs_infotext = 10

# colours
cl_infotext = 'aqua'


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
            # Re-numbers of the strak
            "ReNumbers": [150000, 130000, 110000, 90000],
            # list of chord-lenghts
            "chordlengths": [],
            # ReSqrtCl of root airfoil
            "ReSqrtCl": '150000',
            # root airfoil name
            "seedFoilName": 'rg15.dat',
            # type of the strak that shall be developed
            "strakType":  'F3F',
             # name of the xoptfoil-inputfile for strak-airfoil(s)
            "strakInputFileName": 'i-strak.txt',
            # generate batchfile for running Xoptfoil
            "generateBatchfile" : 'true',
            # name of the batchfile
            "batchfileName" : 'make_strak.bat',
            # operating-mode for strakmachine
            "operatingMode" : 'targetPolar',
            # use always root-airfoil or use predecessing airfoil
            "useAlwaysRootfoil" : 'false'
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

        # get real path of the script
        pathname = os.path.dirname(sys.argv[0])
        scriptPath = os.path.abspath(pathname)
        presetInputFiles = getListOfFiles(scriptPath + bs + presetsPath)
        self.getInputFileName(presetInputFiles, strakType)

        # read input-file as a Fortan namelist
        self.values = f90nml.read(self.presetInputFileName)

        # clean-up file
        self.removeDeactivatedOpPoints()

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

    def getPresetInputFileName(self):
        return self.presetInputFileName


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


    def adaptMaxLift(self, polarData):
        # get new values from polar
        ClMaxLift = polarData.CL_maxLift
        CdMaxLift = polarData.CD[polarData.maxLift_idx]
        alphaMaxLift = polarData.alpha_maxLift

        # set new values
        self.changeOpPoint("alphaClmax", alphaMaxLift)
        self.changeTargetValue("alphaClmax", ClMaxLift)
        self.changeOpPoint("Clmax", ClMaxLift)
        self.changeTargetValue("Clmax", CdMaxLift)

    def adaptMaxSpeed(self, polarData):
        # get new values from polar
        CL_maxSpeed = polarData.CL_maxSpeed

        # calculate difference (=shift-value)
        diff = CL_maxSpeed - self.getOpPoint("maxSpeed")

        # create List of opPoints to be affected. These oppoints will be
        # "shifted", according to the calculated difference
        opPointList = ['preSpeed', 'maxSpeed', 'keepSpeed']

        # shift all opPoints according to the difference
        self.shiftOpPoints(diff, opPointList)

        # now adapt target-values of shifted opPoints to polar
        for opPointName in opPointList:
            try:
                self.adaptTargetValueToPolar(opPointName, polarData)
            except:
                print("opPoint %s was skipped" % opPointName)


    # adapts Max-Glide and dependend values to polar
    def adaptMaxGlide(self, polarData):
        # get polar values, Cl MaxGlide, alpha Max-Glide
        AlphaMaxGlide = polarData.alpha_maxGlide
        CL_maxGlide = polarData.CL_maxGlide

        # calculate difference (=shift-value)
        diff = CL_maxGlide - self.getOpPoint("maxGlide")

        # create List of opPoints to be affected. These oppoints will be
        # "shifted", according to the calculated difference
        opPointList = ['helperPreGlide', 'preGlide', 'maxGlide', 'slopeMaxGlide',
                       'helperKeepGlide']

        # shift all opPoints according to the difference
        self.shiftOpPoints(diff, opPointList)

        # now adapt target-values of shifted opPoints to polar
        for opPointName in opPointList:
            try:
                self.adaptTargetValueToPolar(opPointName, polarData)
            except:
                print("opPoint %s was skipped" % opPointName)
        try:
            # do not shift the "keep glide" oppoint, but adapt to polar
            self.adaptTargetValueToPolar('keepGlide', polarData)
        except:
                print("opPoint \"keepGlide\" was skipped")

        # set new OpPoint / target-value for alphaMaxGlide
        self.changeOpPoint("alphaMaxGlide", AlphaMaxGlide)
        self.changeTargetValue("alphaMaxGlide", CL_maxGlide)


    # shifts a list of oppoints by a certain difference. Does not change the
    # target-values
    def shiftOpPoints(self, diff, opPointList):
         # shift all oppoints in list
        for opPointName in opPointList:
            try:
                opPointValue = self.getOpPoint(opPointName)
                opPointValue = opPointValue + diff
                self.changeOpPoint(opPointName, opPointValue)
            except:
                print("opPoint %s was skipped" % opPointName)

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

        # what kind of value is it?
        opPointType = self.getOpPointType(opPointName)

        if (opPointType == 'spec-al'):
            # oppoint is alpha-value, get target-value from polar
            targetValue = polar.find_alpha(opPointValue)
        else:
            # oppoint is Cl-value, get target-value from polar
            targetValue = polar.find_CD(opPointValue)

        # set new target-value of oppoint
        self.changeTargetValue(opPointName, targetValue)

    # All "maxlift"-dependend opPoints in the inputfile will be "transferred"
    # according to the polar of the strak-airfoil in polardata. The oppoints in
    # the inputfile exactly match the polar of the root-airfoil. For physical
    # reasons (lower Re-number) it is not possible to completely restore the
    # polar of the root-airfoil at the Re-number of the strak-airfoil. But the
    # polar should come as close as possible to the polar of the root-airfoil in
    # some specified points. So the target opPoints for the strak-airfoil, that
    # will be calculated here are a mixture between the polar of the root-
    # airfoil and the polar of the not optimized strak-airfoil.
    def transferMaxLift(self, params, polarData):
        try:
            # Clmax
            # get opPoint-values (polar of root-airfoil)
            CL_maxLift = self.getOpPoint("Clmax")
            CD_maxLift = self.getTargetValue("Clmax")

            # get polar-values (polar of strak-airfoil, not optimized yet)
            CL_Polar = polarData.CL[polarData.maxLift_idx]
            CD_Polar = polarData.CD[polarData.maxLift_idx]

            # determine new value from "gain" (=improvement), a mixture of root
            # polar and strak polar.
            CL_maxLift = ((CL_maxLift * params.maxLiftGain) + # part coming from root-airfoil
                          (CL_Polar * (1.0 - params.maxLiftGain)))# part coming from not optimized strak-airfoil

            CD_maxLift = ((CD_maxLift * params.maxLiftGain ) + # part coming from root-airfoil
                          (CD_Polar * (1.0 - params.maxLiftGain))) # part coming from not optimized strak-airfoil

            # set new values
            self.changeOpPoint("Clmax", CL_maxLift)
            self.changeTargetValue("Clmax", CD_maxLift)
        except:
            print("opPoint Clmax was skipped")

        try:
            # alphaClmax
            # get opPoint-values
            alpha_maxLift = self.getOpPoint("alphaClmax")
            CL_alpha_maxLift = self.getTargetValue("alphaClmax")

            # get polar-values
            alpha_maxLift_Polar = polarData.alpha[polarData.maxLift_idx]
            CL_alpha_maxLift_Polar = polarData.CL[polarData.maxLift_idx]

            # new value is value between root-polar and strak polar
            CL_alpha_maxLift = ((CL_alpha_maxLift * params.maxLiftGain) +
                                (CL_alpha_maxLift_Polar * (1.0 - params.maxLiftGain)))

            alpha_maxLift = ((alpha_maxLift * params.maxLiftGain) +
                             (alpha_maxLift_Polar * (1.0 - params.maxLiftGain)))

            # set new values
            self.changeOpPoint("alphaClmax", alpha_maxLift)
            self.changeTargetValue("alphaClmax", CL_alpha_maxLift)
        except:
            print("opPoint alphaClmax was skipped")


    # all target-values will be shifted "downward" according
    # to the difference in CL_CD_MaxGlide
    def transferMaxGlide(self, params, polarData):
        # calculate difference between oppoint maxGlide and polar maxGlide
        CL_maxGlide = self.getOpPoint("maxGlide")
        CD_maxGlide = self.getTargetValue("maxGlide")

        CL_CD_maxGlide = CL_maxGlide / CD_maxGlide
        factor = (CL_CD_maxGlide) / (polarData.CL_CD_max * (1.00 - params.maxGlideLoss))

        # Calculate CD-Difference
        CD_PolarMaxGlide = factor * CD_maxGlide
        diff = CD_maxGlide - CD_PolarMaxGlide

        # create List of opPoints to be affected. The target-values of these
        # oppoints will be "shifted", according to the calculated difference
        opPointList = ['preGlide','helperPreGlide', 'maxGlide','helperKeepGlide',
                       'keepGlide']#, 'Clmax' ]
        #self.shiftTargetValues(diff, opPointList)
        self.scaleTargetValues(factor, opPointList)


    def transferMaxSpeed(self, params, polarData):
        try:
            # get polar-values of root-airfoil
            CL_keepSpeed = self.getOpPoint("keepSpeed")
            CD_keepSpeed = self.getTargetValue("keepSpeed")
            CD_Polar = polarData.find_CD(CL_keepSpeed)

            # new target-value is value between root-polar and strak polar
            CD_keepSpeed = ((CD_keepSpeed * params.maxSpeedGain) + # part coming from root-airfoil
                            (CD_Polar * (1.0 - params.maxSpeedGain)))# part coming from not optimized strak-airfoil

            self.changeTargetValue("keepSpeed", CD_keepSpeed)
        except:
            print("opPoint keepSpeed was skipped")

        try:
            CL_maxSpeed = self.getOpPoint("maxSpeed")
            CD_maxSpeed = self.getTargetValue("maxSpeed")
            CD_Polar = polarData.find_CD(CL_maxSpeed)

            # new target-value is value between root-polar and strak polar
            CD_maxSpeed = ((CD_maxSpeed * params.maxSpeedGain) +
                           (CD_Polar * (1.0 - params.maxSpeedGain)))
# TODO Test
            self.changeTargetValue("maxSpeed", -1)#CD_maxSpeed)
        except:
            print("opPoint maxSpeed was skipped")

        try:
            # set pre speed to polar-value
            CL_preSpeed = self.getOpPoint("preSpeed")
            CD_Polar = polarData.find_CD(CL_preSpeed)
            self.changeTargetValue("preSpeed", -1)#CD_Polar)
        except:
            print("opPoint preSpeed was skipped")


    # adapt all oppoints and also the target-values to the given polar-data
    def adaptAllOppointsToPolar(self, polarData):
        # adapt maxSpeed to polar.
        self.adaptMaxSpeed(polarData)
        # adapt maxLift to polar.
        self.adaptMaxLift(polarData)
        # adapt maxGlide and dependend values to polar
        self.adaptMaxGlide(polarData)


    # transfer oppoints to a new polar, keeping the shape of the original polar/
    # oppoints
    def transferOppointsKeepShape(self, params, polarData):
        # transfer maxLift-values
        self.transferMaxLift(params, polarData)

        # transfer maxGlide-values
        self.transferMaxGlide(params, polarData)

        # transfer maxSpeed-values
        self.transferMaxSpeed(params, polarData)


    def deleteAllOpPoints(self, operatingConditions):
        # clear operating conditions
        operatingConditions["name"] = []
        operatingConditions["op_mode"] = []
        operatingConditions["op_point"] = []
        operatingConditions["optimization_type"] = []
        operatingConditions["target_value"] = []
        operatingConditions["weighting"] = []
        operatingConditions['noppoint'] = 0


    def clearGeoTargets(self):
        if 'geometry_targets' in self.values:
            del self.values['geometry_targets']



    def addOppoint(self, name, op_mode, op_point, optimization_type,
                                            target_value, weighting):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]
        # append new oppoint
        operatingConditions["name"].append(name)
        operatingConditions["op_mode"].append(op_mode)
        operatingConditions["op_point"].append(op_point)
        operatingConditions["optimization_type"].append(optimization_type)
        operatingConditions["target_value"].append(target_value)
        operatingConditions["weighting"].append(weighting)
        operatingConditions['noppoint'] = operatingConditions['noppoint'] + 1


    # add a "target-drag" oppoint to operating-conditions
    def addTargetPolarOppoint(self, Cl, Cd):
        self.addOppoint('target_polar', 'spec-cl', Cl, 'target-drag', Cd, 1.0)


    # delete all existing oppoints and set new ones from polar-data
    def SetOppointsFromPolar(self, polarData, numOppoints):
        Cl_min = polarData.CL[0]
        Cl_max = polarData.CL_maxLift
        Cl_increment = (Cl_max - Cl_min) / numOppoints

        # clear operating conditions
        self.deleteAllOpPoints(self.values["operating_conditions"])

        # clear any existing geo-targets
        self.clearGeoTargets()

        # add new oppoints
        for i in range (numOppoints):
            Cl = round(Cl_min + (i * Cl_increment), Cl_decimals)
            Cd = round(polarData.find_CD(Cl), Cd_decimals)
            #print "Cl:%f, Cd:%f" % (Cl, Cd) #Debug
            self.addTargetPolarOppoint(Cl, Cd)


    def getOperatingConditions(self):
         # get operating-conditions from dictionary
        operatingConditions = self.values["operating_conditions"]

        return operatingConditions


    def getOppointText(self):
        return "Dies ist ein Text"


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
        self.useWingPlanform = True
        self.generateBatch = True
        self.batchfileName = 'make_strak.bat'
        self.wingData = None
        self.strakType = "F3F"
        self.operatingMode = 'default'
        self.useAlwaysRootfoil = False
        self.adaptInitialPerturb = True
        self.seedFoilName = ""
        self.ReNumbers = []
        self.polarFileNames = []
        self.inputFileNames = []
        self.polars = []
        self.targetPolars = []
        self.maxGlideLoss = 0.008
        self.maxSpeedGain = 0.5
        self.maxLiftGain = 0.3



################################################################################
#
# polarGraph class
#
################################################################################
class polarGraph:
    def __init__(self):
        self.polars = []
        self.targetPolars = []

    def addPolar(self, polarData):
        self.polars.append(polarData)

    def plotLogo(self, ax, scriptDir):
        image = mpimg.imread(scriptDir + bs + imagesPath + bs + logoName)
        ax.imshow(image)
        ax.set_axis_off()

    def plotLiftDragOptimizationPoints(self, ax, polar):
        if (polar.operatingConditions != None):
            idx = 0
            operatingConditions = polar.operatingConditions
            validNames = ['preSpeed', 'maxSpeed', 'keepSpeed']

            for Name in operatingConditions["name"]:
                for validName in validNames:
                    if (Name == validName):
                        # get CD
                        x = operatingConditions["target_value"][idx]
                        # get CL
                        y = operatingConditions["op_point"][idx]
                        ax.plot(x, y, 'yo')
                idx = idx + 1

    def plotLiftDragPolar(self, ax):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl, Cd', 'Cd', 'Cl')

        # get polar of root-airfoil
        rootPolar = self.polars[0]

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.2, max(rootPolar.CL) + 0.2)

        # all polars
        for polar in self.polars:
            # plot CL, CD
            ax.plot(polar.CD, polar.CL, 'b-')

            # plot optimization points
            self.plotLiftDragOptimizationPoints(ax, polar)

            # plot max_speed
            x = polar.CD[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o',color=cl_infotext)
                ax.annotate('maxSpeed (root) @ Cl = %.2f, Cd = %.4f' % (y, x),
                 xy=(x,y), xytext=(20,0), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)
            else:
                ax.plot(x, y, 'ro')


            # plot max_glide
            x = polar.CD[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxGlide (root) @ Cl = %.2f, Cd = %.4f' % (y, x),
                 xy=(x,y), xytext=(20,0), textcoords='offset points',
                      fontsize = fs_infotext, color=cl_infotext)
            else:
                ax.plot(x, y, 'ro')

            # plot max lift
            x = polar.CD[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]

            # additonal text for root polar only
            if (polar == rootPolar):
                ax.plot(x, y, marker='o', color=cl_infotext)
                ax.annotate('maxLift (root) @ Cl = %.2f, Cd = %.4f' %(y,x),
                  xy=(x,y), xytext=(10,10), textcoords='offset points',
                    fontsize = fs_infotext, color=cl_infotext)
            else:
                ax.plot(x, y, 'ro')

                # plot additional markers for root polar only
                #ax.plot(polar.CD_Markers, polar.CL_Markers,'ro')

        # all target polars
        for polar in self.targetPolars:
            # plot CL, CD
            ax.plot(polar.CD, polar.CL, 'r-')

            # plot max_glide
            x = polar.CD[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]
            ax.plot(x, y, 'yo')

            # plot max lift
            x = polar.CD[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]
            ax.plot(x, y, 'yo')

    def plotLiftOverAlphaOptimizationPoints(self, ax, polar):
        if (polar.operatingConditions != None):
            idx = 0
            operatingConditions = polar.operatingConditions
            for optimization_type in operatingConditions["optimization_type"]:
                if (optimization_type == 'target-lift'):
                    x = operatingConditions["op_point"][idx]
                    y = operatingConditions["target_value"][idx]
                    ax.plot(x, y, 'yo')
                idx = idx +1


    def plotLiftOverAlphaPolar(self, ax):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl, alpha', 'alpha', 'Cl')

        # get polar of root-airfoil
        rootPolar = self.polars[0]

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL) - 0.1, max(rootPolar.CL) + 0.2)

        # all polars
        for polar in self.polars:
            # plot CL, alpha
            ax.plot(polar.alpha, polar.CL, 'b-')

            self.plotLiftOverAlphaOptimizationPoints(ax, polar)

            # plot max Speed
            x = polar.alpha[polar.maxSpeed_idx]
            y = polar.CL[polar.maxSpeed_idx]
            ax.plot(x, y, 'ro')
            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(40,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max Glide
            x = polar.alpha[polar.maxGlide_idx]
            y = polar.CL[polar.maxGlide_idx]
            ax.plot(x, y, 'ro')
            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(40,0), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

            # plot max lift
            x = polar.alpha[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]
            ax.plot(x, y, 'ro')
            # additonal text for root polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @ alpha = %.2f, Cl = %.2f' %\
                  (x, y), xy=(x,y),
                  xytext=(-80,15), textcoords='offset points',
                  fontsize = fs_infotext, color=cl_infotext)

        # all target polars
        for polar in self.targetPolars:
             # plot CL, alpha
            ax.plot(polar.alpha, polar.CL, 'r-')

            # plot max lift
            x = polar.alpha[polar.maxLift_idx]
            y = polar.CL[polar.maxLift_idx]
            ax.plot(x, y, 'yo')

    def setAxesAndLabels(self, ax, title, xlabel, ylabel):

        # set title of the plot
        text = (title)
        #ax.set_title(text, fontsize = 30, color="darkgrey")

        # set axis-labels
        ax.set_xlabel(xlabel, fontsize = 20, color="darkgrey")
        ax.set_ylabel(ylabel, fontsize = 20, color="darkgrey")

        # customize grid
        ax.grid(True, color='darkgrey',  linestyle='-.', linewidth=0.7)


    def plotLiftDragOverLiftOptimizationPoints(self, ax, polar):
        if (polar.operatingConditions != None):
            idx = 0
            operatingConditions = polar.operatingConditions
            validNames = ['preSpeed','maxSpeed', 'keepSpeed', 'preGlide',
                     'helperPreGlide','maxGlide','helperKeepGlide', 'keepGlide','Clmax']

            for Name in operatingConditions["name"]:
                for validName in validNames:
                    if (Name == validName):
                        # get CL
                        x = operatingConditions["op_point"][idx]
                        #y = polar.find_CL_CD(x)
                        # get CD
                        Cd = operatingConditions["target_value"][idx]
                        # calculate Cl/Cd
                        y = x/Cd

                        if (Name.find('helper') >= 0):
                            ax.plot(x, y, 'y.')
                        else:
                            ax.plot(x, y, 'yo')
                idx = idx + 1


    def plotLiftDragOverLiftPolar(self, ax):
        # set axes and labels
        self.setAxesAndLabels(ax, 'Cl/Cd, Cl', 'Cl', 'Cl/Cd')

        # get polar of root-airfoil
        rootPolar = self.polars[0]

        # set y-axis manually
        ax.set_ylim(min(rootPolar.CL_CD) - 10, max(rootPolar.CL_CD) + 10)

        # all polars
        for polar in self.polars:
            # plot CL/CD, alpha
            ax.plot(polar.CL, polar.CL_CD, 'b-')

            # plot max_speed
            x = polar.CL[polar.maxSpeed_idx]
            y = polar.CL_CD[polar.maxSpeed_idx]
            ax.plot(x, y, 'ro')
            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxSpeed (root) @ Cl = %.2f, Cl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(20,0), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)

            # plot max_glide
            x = polar.CL[polar.maxGlide_idx]
            y = polar.CL_CD[polar.maxGlide_idx]
            ax.plot(x, y, 'ro')
            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxGlide (root) @ Cl = %.2f, Cl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(10,10), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)

            # plot max Lift
            x = polar.CL[polar.maxLift_idx]
            y = polar.CL_CD[polar.maxLift_idx]
            ax.plot(x, y, 'ro')
            # add text for root Polar only
            if (polar == rootPolar):
                ax.annotate('maxLift (root) @ Cl = %.2f, Cl/Cd = %.2f' % (x, y), xy=(x,y),
                   xytext=(10,10), textcoords='offset points', fontsize = fs_infotext, color=cl_infotext)

            # plot optimizationPoints
            self.plotLiftDragOverLiftOptimizationPoints(ax, polar)



        # all target polars
        for polar in self.targetPolars:
            # plot CL/CD, alpha
            ax.plot(polar.CL, polar.CL_CD, 'r-')

            # plot max_glide
            x = polar.CL[polar.maxGlide_idx]
            y = polar.CL_CD[polar.maxGlide_idx]
            ax.plot(x, y, 'yo')


    def draw(self, scriptDir):

        # get polar of root-airfoil
        rootPolar = self.polars[0]

        print("plotting polar of airfoil %s at Re = %.0f..."
                       % (rootPolar.airfoilname, rootPolar.Re))

        # set 'dark' style
        plt.style.use('dark_background')

        # setup subplots
        fig, (upper,lower) = plt.subplots(2,2)

        # compose diagram-title
        text = ("Analysis of airfoil \"%s\" at " % rootPolar.airfoilname)

        if (rootPolar.polarType == 2):
            text = text + "ReSqrt(Cl) = "
        else:
            text = text + "Re = "

        # add Re-numbers
        for polar in self.polars:
            text = text + ("%d, " %polar.Re)

        text = text + ("Type %d polars" % rootPolar.polarType)


        fig.suptitle(text, fontsize = 20, color="darkgrey", **csfont)

        # first figure, display strak-machine-logo
        self.plotLogo(upper[0], scriptDir)

        # second figure, display the Lift / Drag-Polar
        self.plotLiftDragPolar(lower[0])

        # third figure, display the Lift / alpha-Polar
        self.plotLiftOverAlphaPolar(upper[1])

        # fourth figure, display the lift/drag /Lift polar
        self.plotLiftDragOverLiftPolar(lower[1])

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
        self.airfoilname = "airfoil"
        self.polarType = 2
        self.Re = 0
        self.NCrit = 9.0
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
        self.CL_CD_max = 0.0
        self.maxGlide_idx = 0
        self.alpha_maxGlide= 0.0
        self.CL_maxGlide = 0.0
        self.CL_maxLift = 0.0
        self.alpha_maxLift = 0.0
        self.maxLift_idx = 0
        self.CL_Markers = []
        self.CD_Markers = []
        self.textstr = ""
        self.operatingConditions = None

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

    def makeSimilar(self, ideal):
        print("making similar")
        # find out differences
        maxGlide_factor = self.CL_CD_max / ideal.CL_CD_max *0.985
        maxGlideAlpha = self.alpha[self.maxGlide_idx]
        idealmaxGlideAlpha = ideal.alpha[ideal.maxGlide_idx]
        maxGlideAlphaFaktor = maxGlideAlpha / idealmaxGlideAlpha
        maxLift_factor  = self.CL_maxLift / ideal.CL_maxLift
        maxAlpha_factor = self.alpha_maxLift / ideal.alpha_maxLift
        #print factor
##        for i in range(self.maxGlide_idx):
##            self.alpha[i] = self.alpha[i] / maxGlideAlphaFaktor

        # clear existing data
        self.alpha = []
        self.CL = []
        self.CD = []
        self.CL_CD = []
        self.CDp = []
        self.Cm = []
        self.Top_Xtr = []
        self.Bot_Xtr= []

        # build up new data
        for i in range(len(ideal.CL)):
            CL_CD = ideal.CL_CD[i]*maxGlide_factor
            alpha = ideal.alpha[i]#*maxAlpha_factor
            CL = ideal.CL[i]
            CD = CL / CL_CD

            self.CL_CD.append(CL_CD)
            self.CL.append(CL)
            self.CD.append(CD)
            self.alpha.append(alpha)

        #analyze new data
        self.analyze()
        # correct CL-values
        # find out differences

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
        self.CL_CD_max = self.CL_CD[self.maxGlide_idx]

        print("max Glide, Cl/Cd = %f @ Cl = %f" %
                                  (self.CL_CD_max, self.CL_maxGlide))


    def determineMaxLift(self):
        # determine max lift-value and corresponding alpha
        peak_height = 0.025
        self.maxLift_idx = findPeak(self.CL, peak_height)
        self.CL_maxLift = self.CL[self.maxLift_idx]
        self.alpha_maxLift = self.alpha[self.maxLift_idx]

        print("max Lift, Cl = %f @ alpha = %f" %
                                  (self.CL_maxLift, self.alpha_maxLift))


    def analyze(self):
        print("analysing polar...")
        self.determineMaxSpeed()
        self.determineMaxGlide()
        self.determineMaxLift()
        print("done.\n")


    def find_CD(self, CL):
        # calculate corresponding CD
        CD = np.interp( CL, self.CL, self.CD)
        return CD

    def find_CL(self, alpha):
        # calculate corresponding CL
        CL = np.interp( alpha, self.alpha, self.CL)
        return CL

    def find_CL_CD(self, CL):
        # calculate corresponding CL_CD
        CL_CD = np.interp( CL, self.CL, self.CL_CD)
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
        # strip .dat ending
        foilName = params.seedFoilName.strip('.dat')

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
# function that returns a list of Re-numbers
def get_ReList(params):
    list = []
    # is there wingdata available ?
    if (params.wingData != None):
        # get list of all chord-lengths
        chordLengths = params.wingData.get('chordLengths')
        # get Re-number of root-airfoil
        rootRe = params.ReNumbers[0]
        # get chord-length of root-airfoil
        rootChord = chordLengths[0]
        # calculate list of Re-numbers
        for chord in chordLengths:
            Re = (rootRe * chord) / rootChord
            list.append(Re)
    else:
        # get list of ReNumbers from params
        list = params.ReNumbers

    return list

################################################################################
# function that generates commandlines to run Xoptfoil
def generate_commandlines(params):

    # create an empty list of commandlines
    commandLines = []

    # do some initializations / set local variables
    seedFoilName = params.seedFoilName.strip('.dat') +'.dat'
    numFoils = get_NumberOfAirfoils(params)
    ReList = get_ReList(params)

    # change current working dir to output folder
    commandline = "cd %s\n" % params.outputFolder
    commandLines.append(commandline)

    # make directory for polars of root-airfoil
    root_polar_dir = "%s_polars" % (get_FoilName(params, 0).strip('.dat'))
    commandline = ("md %s\n") % root_polar_dir
    commandLines.append(commandline)

    # copy rootfoil polars
    commandline = ("copy .."+bs+"foil_polars"+bs+"*.* %s"+bs+"*.*\n") %\
                   root_polar_dir
    commandLines.append(commandline)

    # copy seedFoil with its original name to output-folder
    commandline = ("copy .." + bs +"%s"+ bs + "%s %s\n") % \
    (params.inputFolder, seedFoilName, seedFoilName)
    commandLines.append(commandline)

    for i in range (1, (numFoils)):
        # copy input-file to output-folder
        inputfile = params.inputFileNames[i]
        commandline = ("copy .." + bs +"%s"+ bs + "%s %s\n") % \
                             (params.inputFolder, inputfile, inputfile)
        commandLines.append(commandline)

    # rename seedfoil inside outputfolder
    commandline = ("change_airfoilname.py -i .." + bs + params.inputFolder
                + bs +"%s -o %s\n") % (seedFoilName, get_FoilName(params, 0))
    commandLines.append(commandline)

    # copy (renamed) seedFoil to airfoil-folder as it can be used
    # as the root airfoil without optimization
    commandline = ("copy %s %s" + bs + "%s\n") % \
    (get_FoilName(params, 0), params.airfoilFolder, get_FoilName(params, 0))
    commandLines.append(commandline)

    # store seedfoilname
    strakFoilName = seedFoilName
    previousFoilname = seedFoilName

    # add command-lines for each strak-airfoil
    # skip the root airfoil (as it was already copied)
    for i in range (1, numFoils):

        if (params.useAlwaysRootfoil == False):
            # store previous airfoil-name
            previousFoilname = strakFoilName

        # get name of the airfoil
        strakFoilName = get_FoilName(params, i)

        # set input-file name for Xoptfoil
        iFile = params.inputFileNames[i]

        # generate Xoptfoil-commandline
        commandline = "xoptfoil-jx -i %s -r %d -a %s -o %s\n" %\
                        (iFile, ReList[i], previousFoilname.strip('.dat') + '.dat',
                         strakFoilName.strip('.dat'))
        commandLines.append(commandline)

        #copy strak-airfoil to airfoil-folder
        commandline = ("copy %s %s" + bs +"%s\n") % \
            (strakFoilName , params.airfoilFolder, strakFoilName)
        commandLines.append(commandline)

    # change current working dir back
    commandline = "cd..\n"
    commandLines.append(commandline)

    return commandLines, ReList


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


def generate_visu_batchfiles(params):
    for i in range(1, len(params.ReNumbers)):
        visuFileName = "visu_%dk.bat" % (params.ReNumbers[i]/1000)
        airfoilName = get_FoilName(params, i).strip('.dat')

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
        print ('inputFolder not specified, assuming no input-folder shall be used.')

    try:
        params.outputFolder = dict["outputFolder"]
    except:
        print ('outputFolder not specified, assuming no output-folder shall be used.')

    try:
        params.batchfileName = dict["batchfileName"]
    except:
        print ('batchfileName not found, setting default-filename \'%s\'.'\
                % params.batchfileName)

    try:
        params.xmlFileName = dict["XMLfileName"]
    except:
        print ('XMLfileName not specified, assuming no xml-file shall be used.')

    try:
        params.strakInputFileName = dict["strakInputFileName"]
    except:
        print ('strakInputFileName not found, setting default-filename \'%s\'.'\
                % params.strakInputFileName)

    try:
        params.ReNumbers = dict["ReNumbers"]
    except:
        print ('ReNumbers not specified, using no list of ReNumbers')

    try:
        params.seedFoilName = dict["seedFoilName"].strip('.dat')
    except:
        print ('seedFoilName not specified')

    try:
        params.strakType = dict["strakType"]
    except:
        print ('strakType not specified')

    try:
        params.operatingMode = dict["operatingMode"]
    except:
        print ('operatingMode not specified')

    try:
        if (dict["useAlwaysRootfoil"] == 'true'):
            params.useAlwaysRootfoil = True
        else:
            params.useAlwaysRootfoil = False
    except:
        print ('useAlwaysRootfoil not specified')

    try:
        if (dict["adaptInitialPerturb"] == 'true'):
            params.adaptInitialPerturb = True
        else:
            params.adaptInitialPerturb = False
    except:
        print ('adaptInitialPerturb not specified')

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

def getwingDataFromParams(params):
    return
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
    # print strakdata

    # read plane-data from XML-File, if requested //TODO: only wing-data
    if (params.xmlFileName != None):
        params.wingData = getwingDataFromXML(params)

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

    # generate polars of seedfoil / root-airfoil:
    # get name of root-airfoil
    seedFoilName = params.seedFoilName.strip('.dat')+ '.dat'

    print("Generating polars for airfoil %s" % seedFoilName)

    # compose polar-dir
    polarDir = workingDir + bs + "foil_polars"

    idx = 0
    # create polars, polar-file-Names and input-file-names from Re-Numbers
    for Re in params.ReNumbers:
        # create polar-file-Name from Re-Number
        polarFileName = "T2_Re0.%03d_M0.00_N9.0.txt" % (Re/1000)
        polarFileName = polarDir + bs + polarFileName
        params.polarFileNames.append(polarFileName)

        # generate inputfilename
        inputFilename = params.strakInputFileName.strip('.txt')
        inputFilename = inputFilename + ("_%03dk.txt" % (Re/1000))
        params.inputFileNames.append(inputFilename)

        # compose string for system-call of XFOIL-worker
        airfoilName = workingDir + bs + params.inputFolder + bs + seedFoilName
        inputFilename = getPresetInputFileName(params.strakType)

        systemString = "xfoil_worker.exe -i %s -w polar -a %s -r %d" % (
                        inputFilename, airfoilName, Re)
        #print systemString #Debug

        # execute xfoil-worker / create polar-file
        os.system(systemString)

        # import polar
        newPolar = polarData()
        newPolar.importFromFile(polarFileName)
        newPolar.analyze()

        # add polar to params
        params.polars.append(newPolar)

        # also add polar to graph
        graph.addPolar(newPolar)

        if (params.operatingMode == 'targetPolar'):
            # add target polar for all airfoils that are not the root-airfoil
            if (idx == 0):
                rootPolar = newPolar
            else:
                targetPolar = deepcopy(newPolar)
                # make target polar "similar" too root polar
                targetPolar.makeSimilar(rootPolar)
                params.targetPolars.append(targetPolar)
                graph.targetPolars.append(targetPolar)

        idx = idx +1
    print("Done.")

    # Generate input-Files
    print("Generating inputfiles...")
    for i in range(0, len(params.ReNumbers)):
        # create new inputfile
        newFile = inputFile(params.strakType)

        # as a first step always adapt op-points according to polar of
        # root-airfoil
        newFile.adaptAllOppointsToPolar(params.polars[0])

        # as a second step,change oppoints again, but only "shift" them
        # matching the polar of the strak-airfoil
        if (i>0):
            newFile.transferOppointsKeepShape(params, params.polars[i])

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
        params.polars[i].addOperatingConditions(opConditions)

        # special mode 'target-polar'
        if (params.operatingMode == 'targetPolar'):
            # completely exchange oppoints of inputfile
            newFile.SetOppointsFromPolar(params.targetPolars[(i-1)], 10)#TODO number of oppoints

        # for all airfoils after the root-airfoil
        if (i>0):
            # physically create the file
            newFile.writeToFile(params.inputFolder + bs + params.inputFileNames[i])

    print("Done.")

    print("Generating commandlines...")
    # generate Xoptfoil command-lines
    commandlines, ReList = generate_commandlines(params)

    # debug-output
    for element in commandlines:
        print (element)
    print("Done.")

    # generate batchfile
    print("Generating batchfiles...")
    if (params.generateBatch == True):
        print ('generating batchfile \'%s\'' % params.batchfileName)
        generate_batchfile(params.batchfileName, commandlines)
        print ('generating visu-batchfiles')
        generate_visu_batchfiles(params)
    print("Done.")



    # show graph
    graph.draw(scriptPath)

    print("Ready.")
