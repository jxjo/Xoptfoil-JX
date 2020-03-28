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

import csv
from matplotlib import pyplot as plt
import numpy as np
import argparse
from sys import version_info

################################################################################

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
# function that reads particle-data from file
def read_particleFile(fileName):
    # open file containing particle-information position and velocity
    print ("Reading file %s.." % fileName)
    file = open(fileName, "r")

    # create empty list
    AllIterations = []

    # read file using csv-reader
    csv_reader = csv.reader(file, delimiter=";")

    #examine each row of csv-file
    for row in csv_reader:
        #init toggle flag and dict for first particle
        posVelToggle = 0
        particle =	{"pos": [], "vel": []}
        Iteration = []

        # examine each element in a row
        for element in row:
            # remove special characters
            element = element.strip("\r\n\t '")

            # an empty string is a separator between particles
            if (element == ''):
                if len(particle["pos"]) > 0:
                    # append actual particle to Iteration-List
                    Iteration.append(particle)
                # create new, empty particle-dict
                particle =	{"pos": [], "vel": []}
            else:
                floatNumber = float(element)
                if posVelToggle == 0:
                    particle["pos"].append(floatNumber)
                    posVelToggle = 1
                else:
                    particle["vel"].append(floatNumber)
                    posVelToggle = 0

        # Append Iteration, containig all particles to List of all Iterations
        AllIterations.append(Iteration)
    file.close()
    print ("success.")

    return AllIterations


################################################################################
# function that plots particle-data
def plot_particleData(data):

    # always start with iteration number 1
    iterationNum = 1
    iterationNumbers = []

    # determine number of particles
    firstIteration = data[0]
    NumParticles = len(firstIteration)
    print ("Number of particles: %d" % NumParticles)

    # determine number of dimensions
    firstParticle = firstIteration[0]
    NumDimensions = len(firstParticle["pos"])
    print ("Number of dimensions: %d" % NumDimensions)

    # create multi-dimensional, empty array
    posValues = []
    velValues = []

    # number of dimensions per particle
    for i in range(0, NumDimensions):
        posValues.append([])
        velValues.append([])

    # number of particles per dimension
    for i in range(0, NumParticles):
        for i in range(0, NumDimensions):
            # extend each dimension by the number of particles
            posValues[i-1].append([])
            velValues[i-1].append([])

    for iteration in data:
        particleNum = 0

        for particle in iteration:

            Num = 0
            for pos in particle["pos"]:
                posValues[Num][particleNum].append(pos)
                Num = Num +1

            Num = 0
            for vel in particle["vel"]:
                velValues[Num][particleNum].append(vel)
                Num = Num +1

            particleNum = particleNum + 1

        iterationNumbers.append(iterationNum)
        iterationNum = iterationNum + 1

    # plot all position-values
    for i in range(NumDimensions):
        plt.figure(i)

        # plot all position values
        for particle in range(0, NumParticles-1):
            plt.plot(iterationNumbers, posValues[i][particle])

        plt.xlabel('Iteration Number')
        plt.ylabel('position-value')
        text = ("position-values[%d]" % i)
        plt.title(text, fontsize = 14)
        plt.grid(True)

    plt.show()

    # clear figure
    plt.clf()

    # plot all velocity-values
    for i in range(NumDimensions):
        plt.figure(i)

        # plot all velocity values
        for particle in range(0, NumParticles-1):
            plt.plot(iterationNumbers, velValues[i][particle])

        plt.xlabel('Iteration Number')
        plt.ylabel('velocity-value')
        text = ("velocity-values[%d]" % i)
        plt.title(text, fontsize = 14)
        plt.grid(True)

    plt.show()


################################################################################
# Main particle_visualizer program
if __name__ == "__main__":

# initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("--input", "-i", help="filename containing particle-data (e.g. particles)")

  # read arguments from the command line
    args = parser.parse_args()

    if args.input:
        fileName = args.input
    else:
        message = "Enter the filename of the particle-file"\
        "(e.g., particles, which  is the default filename), "\
        "hit <enter> for default:"

        fileName = my_input(message)

        # set default filename in case there was no input
        if (fileName == ""):
            fileName = 'particles'

    fileName = fileName + '.csv'
    print("filename is %s" % fileName)

    try:
        particleData = read_particleFile(fileName)
    except:
        print("Error, file %s could not be opened." % fileName)
        exit -1

    plot_particleData(particleData)

    print("Ready.")
