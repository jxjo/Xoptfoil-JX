#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      matth
#
# Created:     23.05.2020
# Copyright:   (c) matth 2020
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import argparse
import change_airfoilname
from colorama import init
from termcolor import colored

# paths and separators
bs = "\\"
ressourcesPath = 'ressources'
buildPath = 'build'

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
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("-airfoil", "-a", help="airfoil-name")
    parser.add_argument("-number", "-n", help="number of airfoils")

    # read arguments from the command line
    args = parser.parse_args()
    return (args.airfoil, args.number)


def readPerformanceSummary(filename):
    file_content = None
    try:
        file = open(filename, 'r')
        file_content = file.readlines()
        file.close()
    except:
        ErrorMsg("File %s could not be opened %s" % filename)
        exit(-1)

    return file_content


def main():
    # get command-line-arguments
    (airfoilName, numCompetitors) = getArguments()

    max_improvement = 0.0
    bestCompetitor = airfoilName

    if (airfoilName == None) or (numCompetitors == None):
        ErrorMsg("airfoilName or numCompetitors not specified")
        exit(-1)

    for i in range(1, numCompetitors):
        fileName = airfoilName + ("_%d" %i)
        summary = readPerformanceSummary(fileName)

        for line in summary:
            if (line.find("improvement over seed") >=0):
                splitlines = line.split(":")
                improvement = int(splitlines[1])

                if (improvement > max_improvement):
                    max_improvement = improvement
                    bestCompetitor = fileName

    # print result
    print("Best competitor is: \'%s\'" % bestCompetitor)
    print("Renaming airfoil \'%s\' to \'%s\'\n" % (bestCompetitor, airfoilName))

    bestCompetitor = bestCompetitor + '.dat'
    airfoilName = airfoilName  + '.dat'

    # rename and copy the airfoil
    change_airfoilname.change_airfoilName(bestCompetitor, airfoilName)


if __name__ == '__main__':
    main()
